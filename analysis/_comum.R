# Preâmbulo compartilhado por todos os blocos da EDA.
#
# Cada bloco do capítulo é um .Rmd independente, que renderiza sozinho. Este
# arquivo é o que evita que o preâmbulo seja copiado em catorze lugares e
# passe a divergir entre eles. Carregue com:
#
#     source(here::here("analysis", "_comum.R"))
#
# Não carrega dado: cada bloco pede as tabelas de que precisa, via
# carregar_dados(tabelas = ...), porque avaliacoes_desempenho sozinha tem
# cerca de 70 MB e quase nenhum bloco a usa.

suppressPackageStartupMessages({
  library(here)
  library(tidyverse)
})

# Mesma forma de carregar funções que o _targets.R usa com tar_source("R") e
# que os testes usam no helper: uma fonte só de verdade sobre o que existe.
invisible(lapply(
  list.files(here::here("R"), pattern = "[.]R$", recursive = TRUE, full.names = TRUE),
  source
))

aplicar_padrao_monografia()

knitr::opts_chunk$set(
  echo = FALSE,
  message = FALSE,
  warning = FALSE,
  fig.align = "center",
  dpi = 150
)

#' Formata proporção como percentual em pt-BR, com vírgula decimal
#'
#' Usada no texto corrido dos blocos, para o número do parágrafo sair do
#' mesmo cálculo que o da tabela e nunca divergir dela.
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
  # decimal.mark explícito mesmo sem casas decimais: sem ele, o separador
  # decimal continua sendo o ponto e o R avisa que os dois marcadores
  # coincidem, o que em pt-BR seria de fato ambíguo.
  formatC(x, format = "d", big.mark = ".", decimal.mark = ",")
}
