#' Escreve uma diferença de proporções em pontos percentuais, com concordância
#'
#' "Ponto" no singular para valores com módulo menor que 2, depois do
#' arredondamento que vai para o texto, e "pontos" nos demais: "0,3 ponto",
#' "1,9 ponto", "2,0 pontos". Usado só nos textos da wave 3.
#'
#' @param x Diferença em proporção, como 0,003 para 0,3 ponto.
#' @param casas Casas decimais.
#' @param percentual Se `TRUE`, acrescenta "percentual" ou "percentuais".
#' @return Texto, como "0,3 ponto".
formatar_pontos <- function(x, casas = 1, percentual = FALSE) {
  valor <- round(100 * x, casas)
  singular <- abs(valor) < 2
  unidade <- ifelse(singular, "ponto", "pontos")
  if (percentual) unidade <- paste(unidade, ifelse(singular, "percentual", "percentuais"))
  paste(formatar_decimal(valor, casas), unidade)
}

#' Intervalo de uma grandeza em pontos percentuais, com concordância
#'
#' A unidade concorda com a mediana, que é o primeiro número do texto.
#'
#' @param estabilidade tibble de `resumir_estabilidade()`.
#' @param medida Nome da grandeza, em proporção.
#' @return Texto, como "0,3 (-2,4 a 2,4) ponto".
formatar_intervalo_em_pontos <- function(estabilidade, medida) {
  mediana <- estabilidade$mediana[estabilidade$medida == medida]
  paste(
    formatar_intervalo(estabilidade, medida, \(x) formatar_decimal(100 * x, 1)),
    ifelse(abs(round(100 * mediana, 1)) < 2, "ponto", "pontos")
  )
}
