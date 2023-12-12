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

ARMA <- function(erros, vazoes, previstos, assimilados,
    janela, passo, n.ahead, refit.cada, ...) {

    serie <- ts(erros[dia_previsao == max(dia_previsao)]$erro)
    jm    <- janelamovel(serie, "sarima", janela, passo, n.ahead, refit.cada,
        allowdrift = FALSE, ...)

    return(jm)
}

ARMAX <- function(erros, vazoes, previstos, assimilados,
    janela, passo, n.ahead, refit.cada, formula = "~ .", ...) {

    formula <- as.formula(formula)

    aux <- prepara_dados(previstos, assimilados, erros, 1, max(erros$dia_previsao))
    scales  <- aux[[1]]
    regdata <- aux[[2]]
    serie   <- aux[[3]]
    
    # Para facilitar a especificacao de formula pelo json, renomeia as colunas de lag de erro
    colnames(regdata) <- sub("h_([[:digit:]]+)_l_([[:digit:]]+)", "l_\\2", colnames(regdata))

    jm <- janelamovel(serie, "sarimax", janela, passo, n.ahead, refit.cada,
        allowdrift = FALSE, formula = formula, regdata = regdata, ...)

    jm <- lapply(jm, function(j) {
        j[, 1] <- j[, 1] * scales$erro[[2]][2] + scales$erro[[2]][1]
        j
    })

    return(jm)
}

# MODELOS GAM --------------------------------------------------------------------------------------

GAM <- function(erros, vazoes, previstos, assimilados,
    janela, passo, n.ahead, refit.cada,
    formula, lags_erro = seq(10), ...) {

    formula <- as.formula(formula)

    aux <- prepara_dados(previstos, assimilados, erros, lags_erro, max(erros$dia_previsao))
    scales  <- aux[[1]]
    regdata <- aux[[2]]
    serie   <- aux[[3]]
    
    # Para facilitar a especificacao de formula pelo json, renomeia as colunas de lag de erro
    colnames(regdata) <- sub("h_([[:digit:]]+)_l_([[:digit:]]+)", "l_\\2", colnames(regdata))

    jm <- janelamovel(serie, "GAM", janela, passo, n.ahead, refit.cada,
        regdata = regdata, formula = formula, ...)
    
    jm <- lapply(jm, function(j) {
        j[, 1] <- j[, 1] * scales$erro[[2]][2] + scales$erro[[2]][1]
        j
    })

    return(jm)
}

# BOOST DE MODELOS REGRESSIVOS ---------------------------------------------------------------------

BOOST_REG <- function(erros, vazoes, previstos, assimilados,
    janela, passo, n.ahead, refit.cada,
    formula = "~ .", mstop = 2000, family = "Gaussian",
    lags_erro = seq(10), horizontes = c("atual", "todos"), ...) {

    hors <- match.arg(horizontes)
    hors <- if (hors == "todos") unique(erros$dia_previsao) else max(erros$dia_previsao)

    formula <- process_formula(formula, lags_erro, hors)
    usevars <- strsplit(as.character(formula)[3], " \\+ ")[[1]]

    aux <- prepara_dados(previstos, assimilados, erros, lags_erro, hors)
    scales  <- aux[[1]]
    regdata <- aux[[2]][, .SD, .SDcols = usevars]
    serie   <- aux[[3]]

    jm <- janelamovel(serie, "BOOST", janela, passo, n.ahead, refit.cada,
        regdata = regdata, family = family,
        control = mboost::boost_control(mstop = mstop), ...)

    jm <- lapply(jm, function(j) {
        j[, 1] <- j[, 1] * scales$erro[[2]][2] + scales$erro[[2]][1]
        j
    })

    return(jm)
}

# AUXILIARES ---------------------------------------------------------------------------------------

process_formula <- function(form_string, lags_erro, hors) {

    quaishor <- regmatches(form_string, regexpr("_\\$(H|h)_", form_string))
    if (length(quaishor) == 0) {
        form_hors <- ""
    } else if (quaishor == "_$H_") form_hors <- hors else form_hors <- max(hors) 

    quaislag <- regmatches(form_string, regexpr("_\\$L[[:digit:]]{1-2}", form_string))
    if (length(quaislag) == 0) {
        form_lags <- ""
    } else if (quaislag == "_$L") {
        form_lags <- lags_erro
    } else {
        lags <- as.numeric(sub("_\\$L", "", quaislag))
        form_lags <- seq_len(lags)
    }

    aux <- outer(form_hors, form_lags, function(h, l) paste0("h_", h, "_l_", l))
    aux <- paste0(aux, collapse = " + ")
    out <- sub("h_\\$(H|h)_l_\\$L[[:digit:]]*", aux, form_string)
    out <- paste0("Y ", out)
    out <- as.formula(out)

    return(out)
}

process_previstos <- function(previstos) {
    max_hor <- max(previstos$dia_previsao)
    previstos <- copy(previstos)

    refdatas <- previstos[dia_previsao == max(dia_previsao),
        .(data_execucao, max_data_previsao = data_previsao)]

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
    previstos <- merge(previstos, refdatas)

    return(previstos)
}

process_lags_erro <- function(erros, lags, hors) {

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

process_regs <- function(previstos, assimilados, erros, lags, hors, ...) {

    previstos <- process_previstos(previstos)
    lags_erro <- process_lags_erro(erros, lags, hors)
    assimilados <- assimilados[dia_assimilacao == 1, .SD, .SDcols = -c("dia_assimilacao")]

    varex <- merge(previstos, lags_erro, by = "data_execucao")
    varex <- merge(varex, assimilados, by.x = "data_execucao", by.y = "data")
    
    # tira o perfil de previsao -- causa algumas instabilidades e tambem nao sera usado no 
    # operacional
    varex <- varex[, .SD, .SDcols = -paste0("h", unique(erros$dia_previsao))]

    setcolorder(varex, c("max_data_previsao", "data_execucao"))

    return(varex)
}

prepara_dados <- function(previstos, assimilados, erros, lags, hors, ...) {
    varex   <- process_regs(previstos, assimilados, erros, lags, hors)
    regdata <- merge(
        erros[dia_previsao == max(dia_previsao), .(data, erro)],
        varex,
        by.x = "data", by.y = "max_data_previsao"
    )
    regdata[, data_execucao := NULL]

    scales <- lapply(regdata, function(v) {
        v <- scale(v)
        list(v[, 1], c(attr(v, "scaled:center"), attr(v, "scaled:scale")))
    })
    regdata <- as.data.table(do.call("cbind", lapply(scales, "[[", 1)))[, -1]
    colsna  <- sapply(regdata, function(x) all(is.na(x)))
    regdata <- regdata[, .SD, .SDcols = !colsna]

    serie <- regdata$erro
    serie <- ts(serie)
    regdata <- regdata[, -1]

    out <- list(scales, regdata, serie)
    return(out)
}
