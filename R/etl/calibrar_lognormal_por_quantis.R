#' Calibra os parâmetros de uma log-normal por casamento de quantis
#'
#' Vários campos reais deste projeto são positivos e fortemente assimétricos
#' à direita: `valor_contrato` (assimetria 12,68), receita e custo por
#' projeto-mês, duração de projeto (3,6) e duração de alocação (3,97), todos
#' em docs/mapeamento_schema.md. Para todos eles calibrar por média e desvio
#' é instável, porque os dois são dominados pela cauda; os quantis são
#' robustos e são o que está documentado.
#'
#' `meanlog` sai direto da mediana. `sdlog` não encaixa exatamente nos três
#' quantis ao mesmo tempo, então é a média dos dois `sdlog` implícitos, o da
#' cauda inferior (P25) e o da superior (P75).
#'
#' @param mediana,p25,p75 Mediana e quartis reais, na escala original.
#' @return Lista com `meanlog` e `sdlog`, prontos para `stats::rlnorm()`.
calibrar_lognormal_por_quantis <- function(mediana, p25, p75) {
  sdlog_superior <- log(p75 / mediana) / stats::qnorm(0.75)
  sdlog_inferior <- log(mediana / p25) / stats::qnorm(0.75)

  list(
    meanlog = log(mediana),
    sdlog = mean(c(sdlog_superior, sdlog_inferior))
  )
}
