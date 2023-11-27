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

####################################################################################################

# MODELOS ARMA -------------------------------------------------------------------------------------

ARMA_RAW <- function(erros, vazoes, previstos, assimilados,
    janela, passo, n.ahead, refit.cada) {

    serie <- ts(erros$erro)
    jm    <- janelamovel(serie, "sarima", janela, passo, n.ahead, refit.cada)

    return(jm)
}
