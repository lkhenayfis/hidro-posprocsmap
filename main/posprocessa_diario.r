suppressWarnings(suppressPackageStartupMessages(library(modprev)))
suppressWarnings(suppressPackageStartupMessages(library(dbrenovaveis)))
suppressWarnings(suppressPackageStartupMessages(library(data.table)))
suppressWarnings(suppressPackageStartupMessages(library(parallel)))
suppressWarnings(suppressPackageStartupMessages(library(future.apply)))
suppressWarnings(suppressPackageStartupMessages(library(logr)))

source("R/utils.r")
source("R/configuracao.r")
source("R/leitura.r")
source("R/transformacoes.r")
source("R/modelos.r")

INNER_EXEC <- function(mod_posproc, hor, mod_prec,
    erros, vazoes, previstos, assimilados, usina, CONF) {

    log_print(paste0("-    ", mod_posproc, " - h", hor))

    hor_s <- paste0("h", hor)
    jan   <- CONF$PARAMETROS$janela_hormod[[mod_prec]][[hor_s]][[mod_posproc]]
    jan_dados <- jan[1]
    jan_prev  <- jan[2]
    jan_assm  <- jan[3]

    jan_i <- strsplit(jan_dados, "/")[[1]][1]
    jan_i <- as.Date(jan_i)

    call <- CONF$MODELOS[[mod_posproc]]
    call$erros  <- aplica_subset(erros[dia_previsao <= hor], jan_dados, "data")
    call$vazoes <- aplica_subset(vazoes, jan_dados, "data")
    call$previstos   <- aplica_subset(previstos[dia_previsao <= hor], jan_prev, "data_previsao")
    call$assimilados <- aplica_subset(assimilados, jan_assm, "data")
    call$n.ahead <- hor + 1

    jm <- eval(call)
    jm <- lapply(jm, function(prev_i) {
        prev_i <- tail(prev_i, 1)
        tempo <- time(prev_i) - 1
        data  <- jan_i + as.numeric(tempo)
        dat   <- as.data.table(prev_i)
        dat[, data_previsao := data]
        dat[, sd := NULL]
        colnames(dat)[1] <- "erro"
        return(dat)
    })
    jm <- rbindlist(jm)
    jm[, c("dia_previsao", "id_usina", "id_modelo_previsao") := .(hor, usina, mod_prec)]
    jm[, erro := CONF$TRANSFORMACAO$inversa[[hor_s]](erro)]
    jm <- jm[complete.cases(jm)]
    setcolorder(jm, c("data_previsao", "dia_previsao", "erro", "id_usina",
            "id_modelo_previsao"))

    outarq <- paste(mod_posproc, usina, mod_prec, hor, sep = "_")
    outarq <- file.path(CONF$OUTDIR, paste0(outarq, ".csv"))
    jm[, id_modelo_correcao := mod_posproc]
    fwrite(jm, outarq)

    return(NULL)
}

main <- function(arq_conf) {

    timestamp <- format(Sys.time(), format = "%Y%m%d_%H%M%S")
    log_open(paste0("posprocessa_diario_", timestamp))

    # ARQUIVO DE CONFIGURACOES -------------------------------------------

    if (missing(arq_conf)) {
        args <- commandArgs(trailingOnly = TRUE)
        if ((length(args) > 0) && grepl("jsonc?$", args)) {
            arq_conf <- args[1]
        } else {
            arq_conf <- "conf/default/posprocessa_diario_default.jsonc"
        }
    }

    CONF <- le_conf_posproc_diario(arq_conf, !interactive(), !interactive())

    file.copy(arq_conf, file.path(CONF$OUTDIR, "conf_echo.jsonc"), overwrite = TRUE)

    # EXECUCAO PRINCIPAL -------------------------------------------------

    index_loop <- with(CONF$PARAMETROS, {
        l <- lapply(modelos, function(mod_prec) {
            lapply(elementos, function(elem) {
                list(elem, mod_prec, horizontes[[mod_prec]])
            })
        })
        unlist(l, recursive = FALSE)
    })

    #log_print(index_loop)
    #cat("\n")

    elem_0 <- mod_0 <- hor_0 <- ""

    for (item in index_loop) {

        elem_i <- item[[1]]
        mod_i  <- item[[2]]
        hor_i  <- item[[3]]
        max_hor_i <- max(hor_i)

        log_print(paste(elem_i, mod_i, paste0(hor_i, collapse = ","), sep = " -- "))

        jd <- CONF$PARAMETROS$janela_dados
        if (elem_i != elem_0) {
            vaz  <- le_vazoes(elem_i, jd[1])
            prev <- le_previstos(elem_i, jd[2], mod_i, seq_len(max_hor_i))
            assm <- le_assimilacao(elem_i, jd[3])
        } else if (mod_i != mod_0) {
            prev <- le_previstos(elem_i, jd[2], mod_i, seq_len(max_hor_i))
        }

        erros <- merge(
            vaz[, .(data, vazao)],
            prev[dia_previsao %in% hor_i, .(data_previsao, dia_previsao, data_execucao, vazao)],
            by.x = "data", by.y = "data_previsao")
        erros[, erro := vazao.x - vazao.y]

        transf_call <- CONF$TRANSFORMACAO$call
        erros <- split(erros, erros$dia_previsao)
        erros <- lapply(erros, function(d) {
            transf_call$erro <- d$erro
            aux <- eval(transf_call)
            d$erro <- aux[[1]]
            list(d, aux[[2]])
        })

        CONF$TRANSFORMACAO$inversa <- lapply(erros, "[[", 2)
        names(CONF$TRANSFORMACAO$inversa) <- paste0("h", names(CONF$TRANSFORMACAO$inversa))
        erros <- rbindlist(lapply(erros, "[[", 1))

        elem_0 <- elem_i
        mod_0  <- mod_i

        inner_index_loop <- expand.grid(names(CONF$MODELOS), hor_i, stringsAsFactors = FALSE)
        inner_index_loop <- split(inner_index_loop, seq_len(nrow(inner_index_loop)))

        if (CONF$PARALLEL$doparallel) {
            plan(multicore, workers = CONF$PARALLEL$nthreads)
            void <- future_sapply(inner_index_loop, function(v) {
                INNER_EXEC(v[[1]], v[[2]], mod_i, erros, vaz, prev, assm, elem_i, CONF)
            })
        } else {
            void <- lapply(inner_index_loop, function(v) {
                INNER_EXEC(v[[1]], v[[2]], mod_i, erros, vaz, prev, assm, elem_i, CONF)
            })
        }
    }

    on.exit(log_close())
}

main()
