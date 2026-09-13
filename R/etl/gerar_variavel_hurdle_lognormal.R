#' Gera uma variável numérica via modelo hurdle (Bernoulli + log-normal)
#'
#' Vários campos reais (`valor_contrato`, receita e custo por frente-mês,
#' ver docs/mapeamento_schema.md) têm o mesmo formato: uma fração exata de
#' zeros mais uma cauda positiva extremamente assimétrica. Uma log-normal
#' comum não acomoda massa em zero exato, por isso o modelo hurdle: uma
#' Bernoulli decide se o valor é zero, e uma log-normal gera a parte
#' positiva.
#'
#' Os parâmetros da log-normal (`meanlog`, `sdlog`) são calibrados por
#' casamento de quantis (P25/P50/P75 reais), não de média/desvio-padrão: com
#' assimetria extrema, a média e o desvio brutos são dominados pela cauda e
#' são estimadores instáveis. `sdlog` é a média dos dois `sdlog` implícitos
#' (cauda inferior e superior), já que uma log-normal não encaixa
#' exatamente nos três quantis ao mesmo tempo.
#'
#' @param n Quantos valores gerar.
#' @param prob_zero Probabilidade de o valor ser zero.
#' @param mediana_positivos Mediana real entre os valores positivos.
#' @param p25_positivos,p75_positivos 1º e 3º quartil reais entre os
#'   positivos, usados para calibrar `sdlog`.
#' @return Vetor numérico, um valor por posição.
gerar_variavel_hurdle_lognormal <- function(n,
                                             prob_zero,
                                             mediana_positivos,
                                             p25_positivos,
                                             p75_positivos) {
  meanlog <- log(mediana_positivos)
  sdlog_superior <- log(p75_positivos / mediana_positivos) / stats::qnorm(0.75)
  sdlog_inferior <- log(mediana_positivos / p25_positivos) / stats::qnorm(0.75)
  sdlog <- mean(c(sdlog_superior, sdlog_inferior))

  eh_zero <- stats::rbinom(n, size = 1, prob = prob_zero) == 1
  valor_positivo <- stats::rlnorm(n, meanlog = meanlog, sdlog = sdlog)
  ifelse(eh_zero, 0, valor_positivo)
}
