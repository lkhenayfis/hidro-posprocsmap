################################### FUNCOES PARA ACESSO AO BANCO ###################################

library(dbrenovaveis)
library(data.table)

#' Le Dados De Uma Subbacia
#' 
#' Le todos os dados pertinentes a uma subbacia especifica, compondo a prec media de acordo
#' 
#' @param subbacia nome da subbacia (em caixa alta de acordo com os codigos do banco)
#' 
#' @return lista contendo todos os dados relevantes contidos no banco

ledados <- function(subbacia) {
    NA
}

# AUXILIARES ---------------------------------------------------------------------------------------

#' Le Parametros Do SMAP
#' 
#' Leitura dos parametros calibrados para uma determinada subbacia
#' 
#' @param subbacia codigo no banco da subbacia a ser lida
#' @param conn conexao com o banco de onde ler
#' 
#' @return lista dos parametros do SMAP

le_parametros <- function(subbacia, conn) {
    idsb <- getfromtabela(.DB_SCHEMA$subbacias, codigo = subbacia)$id

    if(class(conn)[1] == "local") {
        params <- jsonlite::read_json(file.path(conn[[1]], "parametros.json"), simplifyVector = TRUE)
    } else if(class(conn)[1] == "buckets3") {
        params <- aws.s3::s3read_using(jsonlite::read_json, simplifyVector = TRUE, object = x, bucket = conn[[1]])
    }

    params <- params[unlist(lapply(params, "[[", "id_subbacia")) == idsb]

    return(params)
}

#' Le PSAT
#' 
#' Leitura do PSAT no banco, combinando espacial e temporalmente
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
#' 
#' @return data.table contendo as saidas especificadas de PSAT

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

#' Le Dados Verificados
#' 
#' Leitura no banco dos dados verificados de uma determinada subbacia

le_verificado <- function(subbacia, retorna.psat = c("postos", "comb_espaco", "comb_tempo")) {
    vazoes <- getfromtabela(.DB_SCHEMA$vazoes, subbacia = subbacia)
    psat   <- 
    return(out)
}
