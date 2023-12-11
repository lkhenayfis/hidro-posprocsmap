######################################## FUNCOES AUXILIARES ########################################

#' Aplicacao Dos Pesos Temporais
#' 
#' Calcula media movel temporal de uma serie, opcionalmente replicando ao final com media
#' 
#' Esta funcao permite que o vetor \code{x} seja completado com mais duas observacoes ao final dele
#' cujos valores sao iguais a media do vetor completo. Isto busca mimetizar o comportamento do SMAP
#' durante a previsao
#' 
#' @param x a serie sobre a qual passar media movel
#' @param pesos vetor de kts
#' @param pad booleano indicando se a serie deve ser padded com sua media ao final. Ver Detalhes

aplicakts <- function(x, pesos, pad = FALSE) {
    N <- length(pesos)
    if (pad) {
        x <- c(x, rep(mean(x), 2))
        out <- zoo::rollapply(x, N, function(v) weighted.mean(v, pesos), align = "center")
        out <- c(rep(NA_real_, N - 3), out)
    } else {
        out <- zoo::rollapply(x, N, function(v) weighted.mean(v, pesos), align = "center")
        out <- c(rep(NA_real_, N - 3), out, rep(NA_real_, 2))
    }
    return(out)
}

#' Expansao De Subset
#' 
#' Funcoes para expandir string de janela no formato dbrenovaveis para lista de datas
#' 
#' @param subset uma string indicando faixa de datas no formato dbrenovaveis
#' 
#' @return lista de contendo o intervalo de datas do subset [data_ini, data_fim)

expande_subset <- function(subset = NULL) {
    if (!is.null(subset)) {
        subset <- dbrenovaveis:::parsedatas(subset, "", FALSE)
        subset <- list(subset[[1]][1], subset[[2]][2])
    } else {
        subset <- list(dbrenovaveis:::expandedatahora("1000")[1],
            dbrenovaveis:::expandedatahora("3000")[2])
    }
    subset <- lapply(subset, function(x) as.Date(as.character(x)))

    return(subset)
}

#' Aplica Subset De Datas
#' 
#' Aplica um \code{subset} de datahoras em um dado
#' 
#' @param dat data.table contendo uma coluna de datahora na qual aplicar subset
#' @param subset uma string indicando faixa de datas no formato dbrenovaveis
#' @param col_datahora nome da coluna de datahora
#' 
#' @return dado \code{dat} subsetado

aplica_subset <- function(dat, subset, col_datahora = "data_hora") {
    subset <- expande_subset(subset)
    expr <- paste0(col_datahora, c(" >= subset[[1]]", " < subset[[2]]"))
    expr <- lapply(expr, str2lang)
    sub <- lapply(expr, function(e) eval(e, envir = dat))
    sub <- Reduce("&", sub)
    dat <- dat[sub]

    return(dat)
}

#' Agregacao Semanal De Observados
#' 
#' Agrega um dado de observados em valores semanais
#' 
#' @param vazoes dado de observados lido do banco

agrega_semanal_vazoes <- function(vazoes) {

    vaz_sem <- copy(vazoes)
    vaz_sem[, semana := rep(seq_len(.N / 7), each = 7)]
    vaz_sem <- vaz_sem[,
        .(
            "vazao" = mean(vazao),
            "data" = data[1]
        ),
        by = semana
    ][, -1]
}

#' Agregacao Semana De Previsoes
#' 
#' Agrega um dado de previsoes em valores semanais
#' 
#' @param previstos dado de previstos lido do banco

agrega_semanal_previstos <- function(previstos) {

    prev_sem <- copy(previstos)

    nsems <- (max(prev_sem$dia_previsao) - 1) / 7

    setorder(prev_sem, data_execucao, dia_previsao)
    prev_sem[, tira := .N != 7 * max(nsems), by = data_execucao]
    prev_sem <- prev_sem[tira == FALSE]

    vsems <- rep(seq_len(nsems), each = 7)
    prev_sem[, semana_previsao := rep(sems, length.out = .N)]

    prev_sem <- prev_sem[,
        .(
            "data_previsao" = data_previsao[1],
            "precipitacao" = sum(precipitacao),
            "vazao" = mean(vazao)
        ),
        by = .(data_execucao, semana_previsao)
    ]

    prev_sem <- prev_sem[wday(data_previsao) == 7]

    return(prev_sem)
}