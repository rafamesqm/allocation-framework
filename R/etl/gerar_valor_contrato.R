#' Gera valor de contrato via modelo hurdle (Bernoulli + log-normal)
#'
#' `valor_contrato` tem 14,7% de registros exatamente zero e o restante segue
#' uma distribuição extremamente assimétrica à direita (assimetria 12,68, ver
#' docs/mapeamento_schema.md). Uma log-normal comum não acomoda massa em
#' zero exato, por isso o modelo hurdle: uma Bernoulli decide se o valor é
#' zero, e uma log-normal gera a parte positiva.
#'
#' Os parâmetros da log-normal (`meanlog`, `sdlog`) são calibrados por
#' casamento de quantis (P25/P50/P75 reais), não de média/desvio-padrão: com
#' assimetria tão extrema, a média e o desvio brutos são dominados pela
#' cauda e são estimadores instáveis. `sdlog` é a média dos dois `sdlog`
#' implícitos (cauda inferior e superior), já que uma log-normal não encaixa
#' exatamente nos três quantis ao mesmo tempo.
#'
#' @param n Número de projetos.
#' @param prob_zero Probabilidade de o contrato ser zero.
#' @param mediana_positivos Mediana do valor de contrato entre os positivos.
#' @param p25_positivos,p75_positivos 1º e 3º quartil entre os positivos,
#'   usados para calibrar `sdlog`.
#' @return Vetor numérico de valor de contrato, um por projeto.
gerar_valor_contrato <- function(n,
                                  prob_zero = 0.147,
                                  mediana_positivos = 108300,
                                  p25_positivos = 34814,
                                  p75_positivos = 375054) {
  meanlog <- log(mediana_positivos)
  sdlog_superior <- log(p75_positivos / mediana_positivos) / stats::qnorm(0.75)
  sdlog_inferior <- log(mediana_positivos / p25_positivos) / stats::qnorm(0.75)
  sdlog <- mean(c(sdlog_superior, sdlog_inferior))

  eh_zero <- stats::rbinom(n, size = 1, prob = prob_zero) == 1
  valor_positivo <- stats::rlnorm(n, meanlog = meanlog, sdlog = sdlog)
  ifelse(eh_zero, 0, valor_positivo)
}
