#' Agrega ocupação de cada consultor a partir das alocações (Camada 3)
#'
#' `calendario_alocacao`/`pct_ocupacao` não são sorteados de forma
#' independente: são derivados da matriz `R` já gerada por
#' `gerar_alocacoes()`. Como `alocacoes` não carrega uma dimensão temporal
#' (não há calendário por aresta neste gerador), a agregação é um retrato
#' agregado por consultor, soma de `pct_alocacao` e de `horas_mes` entre
#' todas as alocações, não uma série temporal mês a mês.
#'
#' Consultor sem nenhuma alocação (cold start) fica de fora do resultado,
#' não aparece com zero: quem consome o resultado deve tratar ausência como
#' zero explicitamente, se for o caso.
#'
#' @param alocacoes tibble de `gerar_alocacoes()`.
#' @return tibble com `consultor_id`, `pct_alocacao_total`,
#'   `horas_mes_total`, `n_alocacoes`.
agregar_ocupacao_consultor <- function(alocacoes) {
  alocacoes |>
    dplyr::summarise(
      pct_alocacao_total = sum(pct_alocacao),
      horas_mes_total = sum(horas_mes),
      n_alocacoes = dplyr::n(),
      .by = consultor_id
    )
}
