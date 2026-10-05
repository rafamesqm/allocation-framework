#' Formata proporção como percentual em pt-BR, com vírgula decimal
#'
#' Vive em `R/` e não no preâmbulo dos documentos porque o mesmo número
#' aparece em três lugares que não podem divergir: a tabela salva em
#' `outputs/tables/`, o texto corrido do bloco, e o enunciado do achado. Uma
#' função só garante que os três saiam do mesmo cálculo.
#'
#' @param x Proporção entre 0 e 1.
#' @param casas Casas decimais.
#' @return Texto no formato `"48,4%"`.
formatar_pct <- function(x, casas = 1) {
  paste0(formatC(100 * x, format = "f", digits = casas, decimal.mark = ","), "%")
}

#' Formata número inteiro com separador de milhar em pt-BR
#'
#' @param x Número.
#' @return Texto no formato `"50.946"`.
formatar_inteiro <- function(x) {
  # decimal.mark explícito mesmo sem casas decimais: sem ele o separador
  # decimal continua sendo o ponto e o R avisa que os dois marcadores
  # coincidem, o que em pt-BR seria de fato ambíguo.
  formatC(x, format = "d", big.mark = ".", decimal.mark = ",")
}

#' Formata número com casas decimais em pt-BR
#'
#' @param x Número.
#' @param casas Casas decimais.
#' @return Texto com vírgula decimal e ponto de milhar.
formatar_decimal <- function(x, casas = 2) {
  formatC(x, format = "f", digits = casas, big.mark = ".", decimal.mark = ",")
}

#' Formata diferença relativa com sinal explícito
#'
#' O sinal importa mais que a magnitude na leitura de um confronto contra o
#' agregado real: diz se o sintético está acima ou abaixo do observado.
#'
#' @param x Diferença relativa, como proporção.
#' @param casas Casas decimais.
#' @return Texto no formato `"+12,3%"`, ou `"n/a"` quando indefinido.
formatar_diferenca <- function(x, casas = 1) {
  ifelse(
    is.na(x), "n/a",
    paste0(ifelse(x >= 0, "+", ""), formatar_pct(x, casas))
  )
}
