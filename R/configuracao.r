
#' Processa Arquivo De Configuracao Para Diario
#' 
#' Parse um json de configuracao adicionando informacoes processadas
#' 
#' @param arq caminho para um json de configuracao

le_conf_posproc_diario <- function(arq_conf, log = FALSE, print = FALSE, early = FALSE) {

    if (grepl("\\.(rds)$", arq_conf)) return(readRDS(arq_conf))

    CONF <- jsonlite::read_json(arq_conf, TRUE)

    if (print) cat(paste0("\n", prettyprint(CONF), "\n"))
    if (log) log_print(cat(paste0("\n", prettyprint(CONF), "\n")), console = FALSE)

    # OUTDIR -------------------------------------------------------------

    CONF$OUTDIR <- file.path("out", "posproc_diario", CONF$OUTDIR)
    if (!dir.exists(CONF$OUTDIR)) dir.create(CONF$OUTDIR, recursive = TRUE)

    if (early) return(CONF)

    # PROCESSA PARAMETROS ------------------------------------------------

    if (length(CONF$PARAMETROS$elementos) == 0) {
        CONF$PARAMETROS$elementos <- getfromtabela(.DB_SCHEMA$subbacias)[["codigo"]]
    }

    if (length(CONF$PARAMETROS$modelos) == 0) {
        CONF$PARAMETROS$modelos <- getfromtabela(.DB_SCHEMA$modelos)[["nome"]]
    }

    # os horizontes sao uma lista com um elemento para cada modelo
    # se veio vazio, usa todos os horizontes disponiveis para cada modelo especificado
    if (length(CONF$PARAMETROS$horizontes) == 0) {
        CONF$PARAMETROS$horizontes <- lapply(CONF$PARAMETROS$modelos, function(i) {
            maxhor <- getfromtabela(.DB_SCHEMA$modelos, nome = i, campos = "horizonte_previsao")[[1]]
            v <- seq_len(maxhor)
            v <- c("D", paste0("D", v))
            v
        })
    } else if (!is.list(CONF$PARAMETROS$horizontes)) {
        vec <- CONF$PARAMETROS$horizontes
        CONF$PARAMETROS$horizontes <- lapply(seq_along(CONF$PARAMETROS$modelos), function(i) vec)
    }
    names(CONF$PARAMETROS$horizontes) <- CONF$PARAMETROS$modelos

    all_hors <- unique(unlist(CONF$PARAMETROS$horizontes))
    max_hor <- max(all_hors)

    max_janela <- lapply(CONF$MODELOS, "[[", "janela")
    max_janela <- max(sapply(max_janela, tail, 1))

    janela <- CONF$PARAMETROS$janela
    janela <- dbrenovaveis:::parsedatas(janela, "", FALSE)

    janela_i <- as.Date(janela[[1]][1])

    CONF$PARAMETROS$janela_hormod <- lapply(seq_along(CONF$PARAMETROS$modelos), function(mp) {
        l <- lapply(CONF$PARAMETROS$horizontes[[mp]], function(h) {
            lapply(CONF$MODELOS, function(m) {
                jj <- tail(m$janela, 1)
                janela_i <- janela_i - (jj + h)
                janela_f <- as.POSIXct(janela[[2]][2], tz = "GMT") - 1

                janela_i_assm <- janela_i - h
                janela_i_prev <- janela_i - h + 1

                paste0(c(janela_i, janela_i_prev, janela_i_assm), "/", janela_f)
            })
        })
        names(l) <- paste0("h", CONF$PARAMETROS$horizontes[[mp]])
        l
    })
    names(CONF$PARAMETROS$janela_hormod) <- CONF$PARAMETROS$modelos

    # TRANSFORMACAO DE DADOS ---------------------------------------------

    janela_transf <- CONF$TRANSFORMACAO$janela
    if (is.null(janela_transf)) janela_transf <- CONF$PARAMETROS$janela
    janela_transf <- dbrenovaveis:::parsedatas(janela_transf, "", FALSE)
    janela_transf <- c(as.Date(janela_transf[[1]][1]), as.Date(janela_transf[[2]][2]) - 1)

    janela_max_i <- min(janela_i - (max_janela + max_hor), janela_transf[1])
    janela_max_f <- as.Date(janela[[2]][2]) - 1

    subset <- seq.Date(janela_max_i, janela_max_f, "days") <= janela_transf[2]

    transf_call <- as.symbol(paste0("transforma_", CONF$TRANSFORMACAO$tipo))
    transf_call <- as.call(c(list(transf_call), CONF$TRANSFORMACAO$parametros))
    transf_call$subset <- subset

    CONF$TRANSFORMACAO <- list(call = transf_call, janela = janela_transf)

    janela_max_i_assm <- janela_max_i - max_hor
    janela_max_i_prev <- janela_max_i - max_hor + 1
    v_jans <- c(janela_max_i, janela_max_i_prev, janela_max_i_assm)
    CONF$PARAMETROS$janela_dados <- paste0(v_jans, "/", janela_max_f)

    # MONTA CALLS DOS MODELOS ---------------------------------------

    CONF$MODELOS <- lapply(CONF$MODELOS, function(mod) {
        mod[[1]] <- as.symbol(mod[[1]])
        as.call(mod)
    })

    # RETORNO -------------------------------------------------------

    return(CONF)
}

#' Processa Arquivo De Configuracao Para Semanal
#' 
#' Parse um json de configuracao adicionando informacoes processadas
#' 
#' @param arq caminho para um json de configuracao

le_conf_posproc_semanal <- function(arq_conf, log = FALSE, print = FALSE, early = FALSE) {

    if (grepl("\\.(rds)$", arq_conf)) return(readRDS(arq_conf))

    CONF <- jsonlite::read_json(arq_conf, TRUE)

    if (print) cat(paste0("\n", prettyprint(CONF), "\n"))
    if (log) log_print(cat(paste0("\n", prettyprint(CONF), "\n")), console = FALSE)

    # OUTDIR -------------------------------------------------------------

    CONF$OUTDIR <- file.path("out", "posproc_semanal", CONF$OUTDIR)
    if (!dir.exists(CONF$OUTDIR)) dir.create(CONF$OUTDIR, recursive = TRUE)

    if (early) return(CONF)

    # PROCESSA PARAMETROS ------------------------------------------------

    if (length(CONF$PARAMETROS$elementos) == 0) {
        CONF$PARAMETROS$elementos <- getfromtabela(.DB_SCHEMA$subbacias)[["codigo"]]
    }

    if (length(CONF$PARAMETROS$modelos) == 0) {
        CONF$PARAMETROS$modelos <- getfromtabela(.DB_SCHEMA$modelos)[["nome"]]
    }

    # os horizontes sao uma lista com um elemento para cada modelo
    # se veio vazio, usa todos os horizontes disponiveis para cada modelo especificado
    if (length(CONF$PARAMETROS$horizontes) == 0) {
        CONF$PARAMETROS$horizontes <- lapply(CONF$PARAMETROS$modelos, function(i) {
            maxhor <- getfromtabela(.DB_SCHEMA$modelos, nome = i, campos = "horizonte_previsao")[[1]]
            v <- seq_len(maxhor)
            v
        })
    } else if (!is.list(CONF$PARAMETROS$horizontes)) {
        vec <- CONF$PARAMETROS$horizontes
        CONF$PARAMETROS$horizontes <- lapply(seq_along(CONF$PARAMETROS$modelos), function(i) vec)
    }
    names(CONF$PARAMETROS$horizontes) <- CONF$PARAMETROS$modelos

    CONF$PARAMETROS$horizontes_dia <- lapply(CONF$PARAMETROS$horizontes, function(v) 7 * v + 1)

    all_hors <- unique(unlist(CONF$PARAMETROS$horizontes))
    max_hor <- max(all_hors)

    max_janela <- lapply(CONF$MODELOS, "[[", "janela")
    max_janela <- max(sapply(max_janela, tail, 1))

    janela <- CONF$PARAMETROS$janela
    janela <- dbrenovaveis:::parsedatas(janela, "", FALSE)

    janela_i <- corrige_dia_semana(as.Date(janela[[1]][1]), "inicio")
    janela_f <- corrige_dia_semana(as.Date(janela[[2]][2]) - 1, "fim")

    CONF$PARAMETROS$janela_hormod <- lapply(seq_along(CONF$PARAMETROS$modelos), function(mp) {
        l <- lapply(CONF$PARAMETROS$horizontes[[mp]], function(h) {
            lapply(CONF$MODELOS, function(m) {
                jj <- tail(m$janela, 1)
                janela_i <- janela_i - (jj + h) * 7 # semanal multiplica por sete

                janela_i_assm <- janela_i - 7 * (h)
                janela_i_prev <- janela_i - 7 * (h + 1)

                paste0(c(janela_i, janela_i_prev, janela_i_assm), "/", janela_f)
            })
        })
        names(l) <- paste0("h", CONF$PARAMETROS$horizontes[[mp]])
        l
    })
    names(CONF$PARAMETROS$janela_hormod) <- CONF$PARAMETROS$modelos

    # TRANSFORMACAO DE DADOS ---------------------------------------------

    janela_transf <- CONF$TRANSFORMACAO$janela
    if (is.null(janela_transf)) janela_transf <- CONF$PARAMETROS$janela
    janela_transf <- dbrenovaveis:::parsedatas(janela_transf, "", FALSE)
    janela_transf <- c(
        corrige_dia_semana(as.Date(janela_transf[[1]][1]), "inicio"),
        corrige_dia_semana(as.Date(janela_transf[[2]][2]) - 1, "fim")
    )

    janela_max_i <- min(janela_i - 7 * (max_janela + max_hor), janela_transf[1])
    janela_max_f <- janela_f

    subset <- seq.Date(janela_max_i, janela_max_f, "7 days") <= janela_transf[2]

    transf_call <- as.symbol(paste0("transforma_", CONF$TRANSFORMACAO$tipo))
    transf_call <- as.call(c(list(transf_call), CONF$TRANSFORMACAO$parametros))
    transf_call$subset <- subset

    CONF$TRANSFORMACAO <- list(call = transf_call, janela = janela_transf)

    janela_max_i_assm <- janela_max_i - 7 * (max_hor)
    janela_max_i_prev <- janela_max_i - 7 * (max_hor + 1)
    v_jans <- c(janela_max_i, janela_max_i_prev, janela_max_i_assm)
    CONF$PARAMETROS$janela_dados <- paste0(v_jans, "/", janela_max_f)

    # MONTA CALLS DOS MODELOS ---------------------------------------

    CONF$MODELOS <- lapply(CONF$MODELOS, function(mod) {
        mod[[1]] <- as.symbol(mod[[1]])
        as.call(mod)
    })

    # RETORNO -------------------------------------------------------

    return(CONF)
}

# HELPERS ------------------------------------------------------------------------------------------

prettyprint <- function(lista) {
    string <- as.character(jsonlite::toJSON(lista, pretty = TRUE))
    string <- strsplit(string, "\n")[[1]]
    string <- string[-c(1, length(string))]
    string <- sub(",$", "", string)
    string <- sub(",$", "", string)
    string <- gsub("\"", "", string)
    string <- paste0(string, collapse = "\n")
    return(string)
}

#' Modifica Um Dia Para Inicio/Fim De Semana
#' 
#' Dada uma determinada data, identifica a data do inicio ou fim de semana operativa mais proximo
#' 
#' Para uma 'data' qualquer, se 'tipo' == "inicio" retorna o primeiro sabado apos 'data'. Se 
#' 'tipo' == "fim" retorna a ultima sexta feira antes de 'data'
#' 
#' @param data uma data para modificar
#' @param tipo um de 'inicio' ou 'fim'. Ver Detalhes

corrige_dia_semana <- function(data, tipo = c("inicio", "fim")) {

    diasem <- data.table::wday(data)

    if (tipo == "inicio") {
        out <- data + (7 - diasem)
    } else {
        out <- as.Date(ifelse(diasem == 7, data - 1, data - diasem - 1), origin = "1970-01-01")
    }

    return(out)
}
