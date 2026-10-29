#' Mede a discordância entre as horas e o percentual das alocações
#'
#' Cada alocação traz `horas_mes` e `pct_alocacao`, e as duas medem a mesma
#' carga: um percentual corresponde a `capacidade` vezes ele em horas. A
#' folga do segundo nível usa as horas, e a ocupação do Capítulo 4 usa o
#' percentual, então a discordância entre elas é uma limitação do segundo
#' nível.
#'
#' @param alocacoes tibble com `horas_mes` e `pct_alocacao`.
#' @param capacidade Horas de um mês cheio, a mesma de
#'   `medir_folga_de_capacidade()`.
#' @return tibble de uma linha com `alocacoes`, `correlacao` (entre as horas
#'   e as horas que o percentual implica) e `acima_da_capacidade` (fração com
#'   mais horas que `capacidade`).
medir_discordancia_horas_percentual <- function(alocacoes, capacidade = 168) {
  correlacao <- stats::cor(alocacoes$horas_mes, capacidade * alocacoes$pct_alocacao)
  acima_da_capacidade <- mean(alocacoes$horas_mes > capacidade)
  tibble::tibble(
    alocacoes = nrow(alocacoes), correlacao = correlacao, acima_da_capacidade = acima_da_capacidade
  )
}

#' Ocupação do recomendado mais frequente nos meses dos casos lidos
#'
#' Confere, pelas duas medidas de carga, a sobrecarga do consultor que mais
#' aparece como recomendado comparado num ramo: a folga em horas do segundo
#' nível e a ocupação pelo percentual, a do Capítulo 4, nos meses dos casos
#' sorteados para a leitura qualitativa em que ele é o recomendado.
#'
#' @param plausibilidade Lista de `avaliar_plausibilidade_das_discordancias()`.
#' @param dados Lista nomeada de tibbles, com `ocupacao_consultor`.
#' @param ramo Ramo do recomendado.
#' @return tibble com `discordancia_id`, `consultor_id`, `mes`,
#'   `pct_ocupacao` e `horas_alem_da_capacidade`, um caso lido por linha.
medir_sobrecarga_do_mais_recomendado <- function(plausibilidade, dados, ramo = "colaborativo") {
  do_ramo <- plausibilidade$classificadas[plausibilidade$classificadas$ramo == ramo, ]
  contagem <- table(do_ramo$consultor_recomendado)
  consultor <- names(contagem)[which.max(contagem)]

  lidos <- plausibilidade$rubrica[
    plausibilidade$rubrica$ramo == ramo & plausibilidade$rubrica$consultor_recomendado == consultor,
  ]
  meses <- plausibilidade$discordancias |>
    dplyr::filter(discordancia_id %in% lidos$discordancia_id) |>
    dplyr::transmute(discordancia_id, consultor_id = consultor, mes = format(data_inicio, "%Y-%m"))

  meses |>
    dplyr::left_join(
      dados$ocupacao_consultor |> dplyr::select(consultor_id, ano_mes, pct_ocupacao),
      by = c("consultor_id", mes = "ano_mes")
    ) |>
    dplyr::left_join(
      do_ramo |> dplyr::select(discordancia_id, folga_recomendado),
      by = "discordancia_id"
    ) |>
    dplyr::transmute(
      discordancia_id, consultor_id, mes, pct_ocupacao,
      horas_alem_da_capacidade = -folga_recomendado
    )
}
