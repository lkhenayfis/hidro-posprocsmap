suppressWarnings(suppressPackageStartupMessages(library(dbrenovaveis)))
suppressWarnings(suppressPackageStartupMessages(library(data.table)))
suppressWarnings(suppressPackageStartupMessages(library(ggplot2)))
suppressWarnings(suppressPackageStartupMessages(library(logr)))

source("R/utils.r")
source("R/configuracao.r")
source("R/leitura.r")

main <- function(arq_conf) {

    timestamp <- format(Sys.time(), format = "%Y%m%d_%H%M%S")
    log_open(paste0("analisa_diario_", timestamp))

    # ARQUIVO DE CONFIGURACOES -------------------------------------------

    if (missing(arq_conf)) {
        args <- commandArgs(trailingOnly = TRUE)
        if ((length(args) > 0) && grepl("jsonc?$", args)) {
            arq_conf <- args[1]
        } else {
            arq_conf <- "conf/default/posprocessa_diario_default.jsonc"
        }
    }

    CONF <- le_conf_posproc_diario(arq_conf, !interactive(), !interactive(), TRUE)

    index_loop <- with(CONF$PARAMETROS, {
        ll <- lapply(seq(modelos), function(i) {
            expand.grid(elem = elementos, mod = modelos[i], hor = horizontes,
                stringsAsFactors = FALSE)
        })
        do.call(rbind, ll)
    })
    setDT(index_loop)
    setorder(index_loop, elem, mod, hor)

    log_print(index_loop)
    cat("\n")

    elem_0 <- mod_0 <- hor_0 <- ""

    dat_barplot <- list()

    for (i in seq_len(nrow(index_loop))) {

        elem_i <- index_loop$elem[i]
        mod_i  <- index_loop$mod[i]
        hor_i  <- index_loop$hor[i]
        max_hor_i <- index_loop[(elem == elem_i) & (mod == mod_i), max(hor)]

        log_print(paste(elem_i, mod_i, hor_i, sep = " -- "))

        if (elem_i != elem_0) {
            vaz  <- le_vazoes(elem_i, CONF$PARAMETROS$janela)
            prev <- le_previstos(elem_i, CONF$PARAMETROS$janela, mod_i, seq_len(max_hor_i))
        } else if (mod_i != mod_0) {
            prev <- le_previstos(elem_i, CONF$PARAMETROS$janela, mod_i, seq_len(max_hor_i))
        }

        elem_0 <- elem_i
        mod_0  <- mod_i

        erros <- merge(
            vaz[, .(data, vazao)],
            prev[dia_previsao == hor_i, .(data_previsao, dia_previsao, data_execucao, vazao)],
            by.x = "data", by.y = "data_previsao")
        erros[, erro := vazao.x - vazao.y]

        arqs_posproc <- index_loop[i, paste0(c(elem, mod, hor), collapse = "_")]
        arqs_posproc <- paste0(names(CONF$MODELOS), "_", arqs_posproc, ".csv")
        arqs_posproc <- file.path(CONF$OUTDIR, arqs_posproc)
        posprocs <- lapply(arqs_posproc, fread)
        posprocs <- rbindlist(posprocs)
        posprocs[, data_previsao := as.Date(data_previsao)]

        dplot <- rbind(
            cbind(erros[, list(data, erro)], id_modelo_correcao = "original"),
            posprocs[, list(data_previsao, erro, id_modelo_correcao)],
            use.names = FALSE
        )
        dplot[, id_modelo_correcao :=
                factor(id_modelo_correcao, levels = unique(id_modelo_correcao))]

        cores <- viridisLite::viridis(length(unique(posprocs$id_modelo_correcao)))
        cores <- c("black", cores)

        lineplot <- ggplot(dplot, aes(data, erro, color = id_modelo_correcao)) +
            geom_line() +
            scale_color_manual(values = cores) +
            facet_wrap(~ year(data), ncol = 1, scales = "free") +
            theme_bw()
        arq_lineplot <- index_loop[i, paste0(c("lineplot", elem, mod, hor), collapse = "_")]
        ggsave(file.path(CONF$OUTDIR, paste0(arq_lineplot, ".jpeg")), lineplot,
            width = 16, height = 9)

        metricas <- merge(
            erros[, .(data, dia_previsao, erro)],
            posprocs[, .(data_previsao, dia_previsao, erro, id_modelo_correcao)],
            by.x = "data", by.y = "data_previsao"
        )
        metricas[, valor := erro.x - erro.y]
        metricas <- metricas[,
            .(
                ME2 = mean(valor)^2 / mean(erro.x)^2,
                VAR = var(valor) / var(erro.x),
                MSE = mean(valor^2) / mean(erro.x^2)
            ),
            by = id_modelo_correcao]
        metricas[, c("usina", "modelo", "horizonte") := .(elem_i, mod_i, hor_i)]

        dat_barplot <- c(dat_barplot, list(metricas))
    }

    dat_barplot <- rbindlist(dat_barplot)
    dat_barplot[, horizonte := factor(horizonte)]
    dat_barplot[, id_modelo_correcao :=
            factor(id_modelo_correcao, levels = unique(id_modelo_correcao))]
    dat_barplot <- melt(dat_barplot, id.vars = c(1, 5, 6, 7), variable.name = "metrica")
    dat_barplot[, variable := factor(variable, levels = c("MSE", "ME2", "VAR"))]

    cores <- viridisLite::viridis(length(unique(dat_barplot$id_modelo_correcao)))

    barplot <- ggplot(dat_barplot, aes(horizonte, value, fill = id_modelo_correcao)) +
        geom_col(color = NA, position = "dodge") +
        geom_hline(yintercept = 1, linetype = 2, color = 1) +
        scale_fill_manual(values = cores) +
        scale_y_continuous(breaks = seq(0, 5, .2), minor_breaks = seq(.1, 5, .2)) +
        coord_cartesian(ylim = c(0, 2)) +
        facet_grid(metrica ~ usina) +
        theme_bw() + theme(legend.position = "bottom")

    ggsave(file.path(CONF$OUTDIR, "barplot.jpeg"), barplot, width = 16, height = 9)
}

main()