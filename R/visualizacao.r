################################# FUNCOES DE VISUALIZACAO DOS DADOS ################################

library(data.table)
library(ggplot2)
library(ggExt)

#' Plots De Series Temporais
#' 
#' Realiza o plot de series temporais em um dado, separando em facets ou nao
#' 
#' O argumento \code{facet} controla a forma como \code{variaveis} sao diferenciadas entre si. Se
#' for TRUE, cada variavel vai para uma facet diferente. Caso contrario, um plot normal e gerado 
#' destacando variaveis por cores. 
#' 
#' O argumento \code{by} permite maior controle do dado. Caso seja omitido, a funcao se comporta
#' de acordo com o valor de \code{facet}. Do contrario, um grafico facetado necessariamente sera
#' realizado, utilizando as colunas em \code{by} para cada quadro.  
#' 
#' @param dado o dado contendo as series a plotar e uma coluna de datas as indexando
#' @param variaveis vetor de strings com os nomes das variaveis a serem plotadas
#' @param coldata coluna com as datas para indexacao. Caso omitido, tenta advinhar pelo dado
#' @param janela janela de dados a plotar no formato xts
#' @param by vetor de nomes de colunas pelas quais facetar. Ver Detalhes
#' @param facet booleano indicando se o plot deve ser facetado ou nao. Ver Detalhes
#' 
#' @examples 
#' 
#' # plotando dado previsto em multiplos horizontes
#' dprev <- le_previstos("AVERMELHA", "2015")
#' plota_serie(dprev, "vazao", by = "dia_previsao")
#' 
#' @return objeto ggplot contendo o grafico gerado

plota_serie <- function(dado, variaveis, coldata, janela, by = NULL, facet = TRUE) {
    if(missing("variaveis")) stop("forneca ao menos um nome de coluna para plot")

    if(missing("janela")) janela <- "1000/3000"
    janela <- dbrenovaveis:::parsedatas(janela, "", FALSE)
    janela <- c(janela[[1]][1], janela[[2]][2])
    janela <- lapply(janela, as.Date)

    if(missing("coldata")) coldata <- colnames(dado)[which(sapply(dado, class) == "Date")][1]

    dplot <- dado[(dado[[coldata]] >= janela[[1]]) & (dado[[coldata]] < janela[[2]]), .SD,
        .SDcols = c(coldata, variaveis, by)]
    dplot <- melt(dplot, id.vars = c(coldata, by), variable.name = "variavel", value.name = "valor")
    colnames(dplot)[1] <- "X"

    gg <- ggplot(dplot, aes(X, valor)) + labs(x = coldata) + theme_bw()

    if(!is.null(by)) {
        hasby <- TRUE
        byexpr <- as.formula(paste0("~ ", paste0(by, collapse = " + ")))
        gg <- gg + geom_line(aes(color = variavel)) + facet_wrap(byexpr, dir = "v")
    } else if(facet) {
        gg <- gg + geom_line() + facet_wrap(~ variavel, dir = "v")
    } else {
        gg <- gg + geom_line(aes(color = variavel))
    }

    return(gg)
}
