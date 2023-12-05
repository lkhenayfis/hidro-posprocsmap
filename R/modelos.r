####################################################################################################

#' Funcoes Para Pos-Processamento Diario
#' 
#' Funcoes application specific para aplicacao dos modelos de correcao de vies
#'
#' Todas as novas especificacoes devem ser adicionadas aqui. Os nomes internos dessas funcoes nao 
#' importam muito, mas para melhor entendimento devem ser dados de forma interpretavel.
#' Estas funcoes sao necessarias para aplicacao de quaisquer transformacoes necessarias ao dado
#' antes da execucao das funcoes do modprev. No caso mais simples, sao um wrapper dummy que 
#' simplesmente chama janelamovel
#' 
#' Qualquer funcao declarada aqui deve seguir alguns padroes de implementacao. Todas precisam 
#' receber quatro argumentos nomeados: "erros", "vazoes", "previstos" e "assimilados", 
#' correspondendo a dados que sao passados automaticamente por dentro da execucao principal. A 
#' seguir sao descritos os formatos de cada um destes dados
#' 
#' * erros
#' 
#'        data | data_execucao | dia_previsao |        erro
#'   ---       |    ---        |     ---      |     ---
#'  2013-01-01 |    2012-12-31 |            1 |   -8.882009
#'  2013-01-02 |    2013-01-01 |            1 |  -10.123693
#'  2013-01-03 |    2013-01-02 |            1 |  -11.997165
#'  2013-01-04 |    2013-01-03 |            1 |  -11.694910
#'  2013-01-05 |    2013-01-04 |            1 |  -11.177255
#' 
#' Quando pos processando, por exemplo, cinco dias a frente, este dado contera os erros nas datas
#' a serem corrigidas em todos os horizontes de previzao até cinco
#' 
#' * previstos 
#' 
#'  data_previsao | data_execucao | dia_previsao | precipitacao |  vazao
#'     ---        |       ---     |     ---      |     ---      |   ---
#'  2012-12-28    | 2012-12-27    |        1     |     0.02     |  116.41
#'  2012-12-28    | 2012-12-26    |        2     |     0.02     |  118.42
#'  2012-12-28    | 2012-12-25    |        3     |     0.02     |  123.74
#'  2012-12-28    | 2012-12-24    |        4     |     0.02     |  125.21
#'  2012-12-28    | 2012-12-23    |        5     |     0.02     |  123.81
#' 
#' Mesmo esquema de dados, porem contendo H - 1 datas a mais, ateriores ao inicio de 'erros'. Desta
#' forma, 'previstos' pode conter todos os trajetos de previsao H passos a frente ate cada uma das
#' datas em 'erros' a serem pos processadas
#' 
#' * assimilados
#' 
#'  data       |  dia_assimilacao |    rsolo  |     rsub  |    rsup     | rsup2
#'     ---     |      ---         |    ---    |    ---    |    ---      |  ----
#'  2012-12-27 |         1        | 21.295739 | 173.71275 | 0.009991589 | 0
#'  2012-12-28 |         1        | 22.197630 | 174.55094 | 0.015860659 | 0
#'  2012-12-29 |         1        | 22.928107 | 175.39317 | 0.025177226 | 0
#'  2012-12-30 |         1        | 23.687665 | 176.23946 | 0.039966355 | 0
#'  2012-12-31 |         1        | 24.456952 | 177.08984 | 0.063442634 | 0
#' 
#' As datas em 'assimilados' correspondem as DATAS DE EXECUCAO EM PREVISTOS.

####################################################################################################

# MODELOS ARMA -------------------------------------------------------------------------------------

ARMA_RAW <- function(erros, vazoes, previstos, assimilados,
    janela, passo, n.ahead, refit.cada, ...) {

    serie <- ts(erros[dia_previsao == max(dia_previsao)]$erro)
    jm    <- janelamovel(serie, "sarima", janela, passo, n.ahead, refit.cada,
        allowdrift = FALSE, ...)

    return(jm)
}

# MODELOS GAM --------------------------------------------------------------------------------------

process_previstos <- function(previstos) {
    max_hor <- max(previstos$dia_previsao)

    # acumula apenas as previsoes com trajeto completo
    acumulados <- previstos[, .("acumulado" = ifelse(.N == max_hor, sum(precipitacao), NA_real_)),
        by = .(data_execucao)]
    acumulados <- acumulados[complete.cases(acumulados)]

    previstos[, precipitacao := precipitacao /
            ifelse(sum(precipitacao) == 0, 1, sum(precipitacao)),
        by = data_execucao]
    previstos <- dcast(previstos, data_execucao ~ dia_previsao, value.var = "precipitacao")
    previstos <- previstos[complete.cases(previstos)]
    colnames(previstos)[-1] <- paste0("h", colnames(previstos)[-1])

    previstos <- merge(previstos, acumulados)
    previstos[, max_data_previsao := data_execucao + max_hor]

    return(previstos)
}

process_lags_erro <- function(erros, lags) {

    hors <- sort(unique(erros$dia_previsao))

    erros <- dcast(erros, data_execucao ~ dia_previsao, value.var = "erro")
    colnames(erros)[-1] <- paste0("d_", colnames(erros)[-1])

    out <- lapply(seq_along(hors), function(i) {
        h <- hors[i]
        dlag <- lapply(lags, function(l) data.table::shift(erros[[1 + i]], h + l))
        dlag <- as.data.table(dlag)
        colnames(dlag) <- paste0("h_", h, "_l_", lags)
        dlag
    })
    out <- do.call(cbind, out)
    out[, data_execucao := erros$data_execucao]

    return(out)
}
