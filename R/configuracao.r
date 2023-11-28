
#' Processa Arquivo De Configuracao
#' 
#' Parse um json de configuracao adicionando informacoes processadas
#' 
#' @param arq caminho para um json de configuracao

le_conf_posproc_diario <- function(arq_conf, log = FALSE, print = FALSE, early = FALSE) {

    if (grepl("\\.(rds)$", arq_conf)) return(readRDS(arq_conf))

    CONF <- jsonlite::read_json(arq_conf, TRUE)

    if (print) cat(paste0("\n", prettyprint(CONF), "\n"))
    if (log) log_print(prettyprint(CONF), console = FALSE)

    # OUTDIR -------------------------------------------------------------

    CONF$OUTDIR <- file.path("out", "posproc_diario", CONF$OUTDIR)
    if (!dir.exists(CONF$OUTDIR)) dir.create(CONF$OUTDIR, recursive = TRUE)

    if (early) return(CONF)

    # PROCESSA PARAMETROS ------------------------------------------------

    if (length(CONF$PARAMETROS$elementos) == 0) {
        CONF$PARAMETROS$elementos <- getfromtabela(.DB_SCHEMA$subbacias, "codigo")[[1]]
    }

    if (length(CONF$PARAMETROS$modelos) == 0) {
        CONF$PARAMETROS$modelos <- getfromtabela(.DB_SCHEMA$modelos, "nome")[[1]]
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

    all_hors <- unique(unlist(CONF$PARAMETROS$horizontes))
    max_hor <- max(all_hors)

    max_janela <- lapply(CONF$MODELOS, "[[", "janela")
    max_janela <- max(sapply(max_janela, tail, 1))

    janela <- CONF$PARAMETROS$janela
    janela <- dbrenovaveis:::parsedatas(janela, "", FALSE)

    janela_i <- as.Date(janela[[1]][1])

    janela_max_i <- janela_i - (max_janela + max_hor)
    janela_max_f <- as.Date(janela[[2]][2]) - 1

    CONF$PARAMETROS$janela_dados <- paste0(janela_max_i, "/", janela_max_f)

    CONF$PARAMETROS$janela_hormod <- lapply(all_hors, function(h) {
        lapply(CONF$MODELOS, function(m) {
            jj <- tail(m$janela, 1)
            janela_i <- janela_i - (jj + h)
            janela_f <- as.POSIXct(janela[[2]][2], tz = "GMT") - 1

            paste0(janela_i, "/", janela_f)
        })
    })

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
