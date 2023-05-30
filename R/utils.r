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
    if(pad) {
        x <- c(x, rep(mean(x), 2))
        out <- zoo::rollapply(x, N, function(v) weighted.mean(v, pesos), align = "center")
        out <- c(rep(NA_real_, N - 3), out)
    } else {
        out <- zoo::rollapply(x, N, function(v) weighted.mean(v, pesos), align = "center")
        out <- c(rep(NA_real_, N - 3), out)
    }
    return(out)
}
