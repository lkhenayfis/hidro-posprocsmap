################################# FUNCOES DE TRANSFORMACAO DE DADOS ################################

#' Transformacoes De Dados
#' 
#' Metodologias para transformacao e reversao das mesmas 
#' 
#' Todas as funcoes de transformacao devem respeitar determinadas regras a respeito de seus 
#' argumentos de entrada e retorno.
#' 
#' Ha apenas um argumento nomeado obrigatorio a todas as funcoes, chamado "erro". Este sera um vetor
#' numerico para transformar. Alem de "erro", deve existir um argumento "subset" indicando valores
#' in-sample de "erro". Tanto "erro" quanto "subset" sao supridos automaticamente dentro da execucao
#' principal. As funcoes podem receber mais quaisquer argumentos, que serao definidos no arquivo de
#' configuracao, e devem sempre ter o generico "...".
#' 
#' Quanto ao retorno, deve ser uma lista de dois elementos. O primeiro deve corresponder ao erro 
#' transformado, um vetor numerico de mesmo comprimento que o original. O segundo deve ser uma
#' funcao que realiza a transformada inversa. Tal como as funcoes de ida, as inversas devem sempre
#' possuir um único argumento nomeado chamado "erro_transf" e o generico "...".

transforma_identidade <- function(erro, subset = rep(TRUE, length(erro)), ...) {
    inversa <- function(erro_transf, ...) return(erro_transf)
    out <- list(erro, inversa)
    return(out)
}

transforma_boxcox_simples <- function(erro, lambda = "auto", subset = rep(TRUE, length(erro)), ...) {
    erro_insample <- erro[subset]
    lambda <- forecast::BoxCox(erro_insample, lambda)
    lambda <- attr(lambda, "lambda")

    erro_transf <- forecast::BoxCox(erro, lambda)
    inversa <- function(erro_transf, ...) forecast::InvBoxCox(erro_transf, lambda)
    out <- list(erro_transf, inversa)
    return(out)
}
