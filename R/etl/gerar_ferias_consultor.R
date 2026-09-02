#' Gera período de férias de cada consultor (Camada 3)
#'
#' Sem calibração real de datas de férias levantada até esta rodada (ver
#' docs/mapeamento_schema.md): usa uma taxa plausível de dias por ano (30,
#' padrão CLT), suposição documentada, não dado observado. O início do
#' período é sorteado uniformemente dentro do ano de referência.
#'
#' @param consultores tibble de `gerar_consultores()`.
#' @param dias_ferias Duração do período de férias, em dias.
#' @param ano_referencia Ano em que o período de férias é sorteado.
#' @param semente Inteiro para reprodutibilidade.
#' @return tibble com `consultor_id`, `inicio_ferias`, `fim_ferias`.
gerar_ferias_consultor <- function(consultores, dias_ferias = 30, ano_referencia = 2026, semente = 1) {
  set.seed(semente)

  inicio_ano <- as.Date(sprintf("%d-01-01", ano_referencia))
  fim_ano <- as.Date(sprintf("%d-12-31", ano_referencia))
  amplitude_dias <- as.integer(fim_ano - inicio_ano - dias_ferias)

  inicio_ferias <- inicio_ano + sample.int(amplitude_dias, nrow(consultores), replace = TRUE)

  tibble::tibble(
    consultor_id = consultores$consultor_id,
    inicio_ferias = inicio_ferias,
    fim_ferias = inicio_ferias + dias_ferias
  )
}
