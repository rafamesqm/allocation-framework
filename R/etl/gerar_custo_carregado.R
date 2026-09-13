#' Gera custo carregado por consultor (parte de `c`, Camada 3)
#'
#' `custo_carregado` é, na fonte real, uma tabela de taxa (rate card): uma
#' taxa determinística por `Sigla Cargo` vigente, não uma distribuição com
#' dispersão real entre pessoas do mesmo cargo (ver docs/mapeamento_schema.md,
#' "Quinta rodada"). O gerador sintético só tem `nivel_senioridade` (N4-N0),
#' não a sigla de cargo granular da fonte real, então mapeia cada nível a
#' uma banda de siglas reais e reamostra dentro da banda: preserva a
#' dispersão real observada (as taxas reais de cada sigla) sem inventar uma
#' distribuição contínua onde a fonte não tem uma.
#'
#' O mapeamento de banda (nível para conjunto de siglas) é decisão de
#' projeto, não leitura direta do dado: a fonte real não documenta qual
#' sigla corresponde a qual nível N4-N0, só a progressão de cargo dentro da
#' `Sigla Cargo`. A banda segue a progressão plausível de carreira em
#' consultoria (estagiário/analista, consultor, team leader, gerente,
#' diretoria).
#'
#' @param consultores tibble de `gerar_consultores()`.
#' @param semente Inteiro para reprodutibilidade.
#' @return tibble com `consultor_id`, `custo_carregado` (R$/h).
gerar_custo_carregado <- function(consultores, semente = 1) {
  set.seed(semente)

  bandas <- list(
    N4 = c(91.45, 112.84, 112.84, 196.69, 196.69, 237.70, 268.22, 268.22, 294.52, 294.52, 311.74, 330.26),
    N3 = c(331.35, 331.35, 380.88, 386.66, 386.66, 444.09),
    N2 = c(452.23, 452.23, 518.90),
    N1 = c(499.06, 499.06, 567.11, 674.69, 674.69, 751.33, 834.51, 834.51, 940.38),
    N0 = c(944.62, 944.62, 1095.98, 1095.98, 1196.38, 1267.02, 1337.65, 1337.65, 1471.25, 1503.15, 1627.98, 1722.61, 1859.73, 2066.00)
  )

  custo_carregado <- vapply(as.character(consultores$nivel_senioridade), function(nivel) {
    sample(bandas[[nivel]], size = 1)
  }, numeric(1))

  tibble::tibble(consultor_id = consultores$consultor_id, custo_carregado = unname(custo_carregado))
}
