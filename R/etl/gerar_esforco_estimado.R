#' Gera esforço estimado do projeto (horas/mês), Normal truncada em zero
#'
#' `esforco_estimado` é quase simétrico na base real (assimetria -0,05, ver
#' docs/mapeamento_schema.md), por isso uma Normal simples é aproximação
#' razoável, truncada em zero porque esforço não pode ser negativo. Média e
#' desvio calibrados pela mediana e pelo desvio-padrão reais de `horas_mes`
#' (mediana usada como proxy de média, já que a distribuição é quase
#' simétrica e a média exata não foi levantada nesta rodada).
#'
#' @param n Número de projetos.
#' @param media Horas por mês esperadas.
#' @param desvio Desvio-padrão de horas por mês.
#' @return Vetor numérico de esforço estimado, um por projeto.
gerar_esforco_estimado <- function(n, media = 120, desvio = 66.90) {
  esforco <- stats::rnorm(n, mean = media, sd = desvio)
  pmax(esforco, 0)
}
