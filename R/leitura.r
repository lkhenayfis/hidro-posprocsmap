################################### FUNCOES PARA ACESSO AO BANCO ###################################

library(dbrenovaveis)
library(data.table)

source("R/utils.r")

#' Le Parametros Do SMAP
#' 
#' Leitura dos parametros calibrados para uma determinada subbacia
#' 
#' @param subbacia codigo no banco da subbacia a ser lida
#' 
#' @return lista dos parametros do SMAP

le_parametros <- function(subbacia) {
    idsb <- getfromtabela(.DB_SCHEMA$subbacias, codigo = subbacia)$id
    conn <- .DB_SCHEMA$conn

    if(class(conn)[1] == "local") {
        params <- jsonlite::read_json(file.path(conn[[1]], "parametros.json"), simplifyVector = TRUE)
    } else if(class(conn)[1] == "buckets3") {
        params <- aws.s3::s3read_using(jsonlite::read_json, simplifyVector = TRUE,
            object = file.path(conn[[1]], "parametros.json"))
    }

    params <- params[unlist(lapply(params, "[[", "id_subbacia")) == idsb]

    return(params)
}

#' Leitores De Dados Do Banco
#' 
#' Funcoes individuais para acesso das informacoes armazenadas no banco
#' 
#' Esta funcao sempre retornara, no minimo, o PSAT canonico utilizado pelo SMAP em sua rodada, isto 
#' e, a chuva combinada entre postos e ao longo do tempo segundo kts. Alem disso, atraves do 
#' argumento \code{retorna.extra} podem ser especificadas outras variaveis de saida. As opcoes de
#' retorno extra sao
#' 
#' \describe{
#' \item{postos}{saida inclui todos os postos associados a \code{subbacia}}
#' \item{comb_espaco}{saida inclui a compbinacao espacial dos postos associados}
#' }
#' 
#' @param subbacia codigo no banco da subbacia a ser lida
#' @param retorna.extra um vetor de strings indicando o que deve ser retornado. Deve conter um ou
#'     mais de \code{c("postos", "comb_espaco")}. Ver Detalhes
#' @param modelo codigo no banco do modelo de cujos resultados deve ser lido. Default "PMEDIA"
#' @param horizonte vetor de horizontes a ler. Default 1:10
#' 
#' @return data.table contendo os dados requisitados

le_psat <- function(subbacia, retorna.extra = c("postos", "comb_espaco")) {
    psat    <- getfromtabela(.DB_SCHEMA$psat, subbacia = subbacia)
    postos  <- getfromtabela(.DB_SCHEMA$postosplu, subbacia = subbacia)

    psat      <- merge(psat, postos[, .(id, codigo, peso)], by.x = "id_postoplu", by.y = "id")
    psatmedio <- psat[, .(precipitacao = sum(peso * precipitacao)), by = data]

    psat <- dcast(psat[, .(data, codigo, precipitacao)], data ~ codigo, value.var = "precipitacao")
    psat <- merge(psat, psatmedio)

    parametros <- le_parametros(subbacia, .DB_SCHEMA$conn)[[1]]
    psat[, precipitacao2 := aplicakts(precipitacao, parametros$kt)]

    mantem <- "precipitacao2"
    if("comb_espaco" %in% retorna.extra) mantem <- c("precipitacao", mantem)
    if("postos" %in% retorna.extra) mantem <- c(postos$codigo, mantem)

    psat <- psat[, .SD, .SDcols = c("data", mantem)]

    return(psat)
}

le_vazoes <- function(subbacia) {
    vazoes <- getfromtabela(.DB_SCHEMA$vazoes, subbacia = subbacia)
    return(vazoes)
}


le_assimilacao <- function(subbacia) {
    assimilacao <- getfromtabela(.DB_SCHEMA$assimilacao, subbacia = subbacia)
    return(assimilacao)
}

le_previstos <- function(subbacia, modelo = "PMEDIA", horizonte = seq_len(10)) {
    previstos <- getfromtabela(.DB_SCHEMA$previstos, subbacia = subbacia, modelo = modelo,
        dia_previsao = horizonte)
    return(previstos)
}
