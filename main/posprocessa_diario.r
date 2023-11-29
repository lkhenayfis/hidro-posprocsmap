suppressWarnings(suppressPackageStartupMessages(library(modprev)))
suppressWarnings(suppressPackageStartupMessages(library(dbrenovaveis)))
suppressWarnings(suppressPackageStartupMessages(library(data.table)))
suppressWarnings(suppressPackageStartupMessages(library(parallel)))
suppressWarnings(suppressPackageStartupMessages(library(logr)))

source("R/utils.r")
source("R/configuracao.r")
source("R/leitura.r")
source("R/transformacoes.r")
source("R/modelos.r")

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

    # EXECUCAO PRINCIPAL -------------------------------------------------

    index_loop <- with(CONF$PARAMETROS, {
        ll <- lapply(seq(modelos), function(i) {
            expand.grid(elem = elementos, mod = modelos[i], hor = horizontes[[i]],
                stringsAsFactors = FALSE)
        })
        do.call(rbind, ll)
    })
    setDT(index_loop)
    setorder(index_loop, elem, mod, hor)

    log_print(index_loop)
    cat("\n")

    elem_0 <- mod_0 <- hor_0 <- ""

    for (i in seq_len(nrow(index_loop))) {

        elem_i <- index_loop$elem[i]
        mod_i  <- index_loop$mod[i]
        hor_i  <- index_loop$hor[i]
        max_hor_i <- index_loop[(elem == elem_i) & (mod == mod_i), max(hor)]

        log_print(paste(elem_i, mod_i, hor_i, sep = " -- "))

        if (elem_i != elem_0) {
            vaz  <- le_vazoes(elem_i, CONF$PARAMETROS$janela_dados)
            prev <- le_previstos(elem_i, CONF$PARAMETROS$janela_dados, mod_i, seq_len(max_hor_i))
            assm <- le_assimilacao(elem_i, CONF$PARAMETROS$janela_dados)
        } else if (mod_i != mod_0) {
            prev <- le_previstos(elem_i, CONF$PARAMETROS$janela_dados, mod_i, seq_len(max_hor_i))
        }

        erros <- merge(
            vaz[, .(data, vazao)],
            prev[dia_previsao %in% seq_len(hor_i), .(data_previsao, dia_previsao, data_execucao, vazao)],
            by.x = "data", by.y = "data_previsao")
        erros[, erro := vazao.x - vazao.y]

        transf_call <- CONF$TRANSFORMACAO$call
        transf_call$erro <- erros$erro
        aux <- eval(transf_call)

        inv_transf_fun <- aux[[2]]
        erros[, erro := aux[[1]]]

        elem_0 <- elem_i
        mod_0  <- mod_i

        usina     <- getfromtabela(.DB_SCHEMA$subbacias, codigo = elem_i, "id")$id
        mod_prev  <- getfromtabela(.DB_SCHEMA$modelos, nome = mod_i, campos = "id")$id

        modelos <- mapply(CONF$MODELOS, CONF$PARAMETROS$janela_hormod[[hor_i]], FUN = function(mod, jan) {
            mod$erros  <- as.call(list(quote(aplica_subset), quote(erros), jan, "data"))
            mod$vazoes <- as.call(list(quote(aplica_subset), quote(vaz), jan, "data"))
            mod$previstos   <- as.call(list(quote(aplica_subset), quote(prev), jan, "data_execucao"))
            mod$assimilados <- as.call(list(quote(aplica_subset), quote(assm), jan, "data"))
            mod$n.ahead <- hor_i + 1
            mod
        })

        evalenv <- environment()

        EXEC <- function(mod) {

            log_print(paste0("-    ", mod))

            jan_hormod <- CONF$PARAMETROS$janela_hormod[[hor_i]][[mod]]
            jan_hormod_i <- strsplit(jan_hormod, "/")[[1]][1]
            jan_hormod_i <- as.Date(jan_hormod_i)

            jm <- eval(modelos[[mod]], envir = evalenv)
            jm <- lapply(jm, function(prev_i) {
                prev_i <- tail(prev_i, 1)
                tempo <- time(prev_i) - 1
                data  <- jan_hormod_i + as.numeric(tempo)
                dat   <- as.data.table(prev_i)
                dat[, data_previsao := data]
                dat[, sd := NULL]
                colnames(dat)[1] <- "erro"

                return(dat)
            })
            jm <- rbindlist(jm)
            jm[, c("dia_previsao", "id_usina", "id_modelo_previsao") := .(hor_i, usina, mod_prev)]
            jm[, erro := inv_transf_fun(erro)]
            jm <- jm[complete.cases(jm)]
            setcolorder(jm, c("data_previsao", "dia_previsao", "erro", "id_usina",
                    "id_modelo_previsao"))

            outarq <- paste(mod, as.character(elem_i), as.character(mod_i), hor_i, sep = "_")
            outarq <- file.path(CONF$OUTDIR, paste0(outarq, ".csv"))
            jm[, id_modelo_correcao := mod]
            fwrite(jm, outarq)

            return(NULL)
        }

        if (CONF$PARALLEL$doparallel) {
            clst <- makeCluster(CONF$PARALLEL$nthreads, "FORK")
            void <- parLapply(clst, names(modelos), EXEC)
            stopCluster(clst)
        } else {
            void <- lapply(names(modelos), EXEC)
        }
    }

    on.exit(log_close())
}

main()
