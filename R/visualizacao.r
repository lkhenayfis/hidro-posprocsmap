################################# FUNCOES DE VISUALIZACAO DOS DADOS ################################

library(data.table)
library(ggplot2)
library(ggExt)

#' Plots De Series Temporais
#' 
#' Realiza o plot de series temporais em um dado, separando em facets ou nao
#' 
#' @param dado o dado contendo as series a plotar e uma coluna de datas as indexando
#' @param variaveis vetor de strings com os nomes das variaveis a serem plotadas
#' @param janela janela de dados a plotar no formato xts
#' @param facet booleano indicando se o plot deve ser facetado ou nao
#' 
#' @examples 
#' 
#' # plotando dado previsto em multiplos horizontes
#' prev <- le_previstos("AVERMELHA", "2015")
#' prev <- dcast(prev, data_previsao ~ dia_previsao, value.var = "vazao", fun.aggregate = mean)
#' plota_serie(prev, variaveis = as.character(1:10))
#' 
#' @return objeto ggplot contendo o grafico gerado

plota_serie <- function(dado, variaveis, janela, facet = TRUE) {
    if(missing("variaveis")) stop("forneca ao menos um nome de coluna para plot")

    if(missing("janela")) janela <- "1000/3000"
    janela <- dbrenovaveis:::parsedatas(janela, "", FALSE)
    janela <- c(janela[[1]][1], janela[[2]][2])
    janela <- lapply(janela, as.Date)

    coldata <- colnames(dado)[which(sapply(dado, class) == "Date")]

    dplot <- dado[(dado[[coldata]] >= janela[[1]]) & (dado[[coldata]] < janela[[2]]), .SD,
        .SDcols = c(coldata, variaveis)]
    dplot <- melt(dplot, id.vars = coldata, variable.name = "variavel", value.name = "valor")
    colnames(dplot)[1] <- "X"

    gg <- ggplot(dplot, aes(X, valor)) +
        labs(x = coldata) +
        theme_bw()
    if(facet) {
        gg <- gg + geom_line() + facet_wrap(~ variavel, dir = "v")
    } else {
        gg <- gg + geom_line(aes(color = variavel))
    }

    return(gg)
}
