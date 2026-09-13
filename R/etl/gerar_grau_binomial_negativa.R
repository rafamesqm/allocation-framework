#' Sorteia uma sequência de graus via Binomial Negativa, calibrada por
#' casamento de momentos
#'
#' Grau de alocação, nos dois lados de `R`, é sobredisperso (variância bem
#' maior que a média, ver docs/mapeamento_schema.md, "Distribuição de grau em
#' R"), o que descarta Poisson como modelo de contagem. Calibra `size`/`prob`
#' da Binomial Negativa pela parametrização de momentos:
#' `size = media^2 / (variancia - media)`, `prob = size / (size + media)`.
#'
#' @param n Quantos graus sortear (um por nó).
#' @param media Média real do grau.
#' @param variancia Variância real do grau. Precisa ser maior que `media`,
#'   senão a Binomial Negativa não está definida (nesse caso a distribuição
#'   correta seria Poisson, não esta função).
#' @return Vetor inteiro de graus, um por nó.
gerar_grau_binomial_negativa <- function(n, media, variancia) {
  stopifnot(variancia > media)
  size <- media^2 / (variancia - media)
  prob <- size / (size + media)
  stats::rnbinom(n, size = size, prob = prob)
}
