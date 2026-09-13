#' Calcula o balanço de senioridade de um time (Camada 4)
#'
#' Agregação sobre a pirâmide de senioridade dos consultores de um time:
#' proporção de cada `nivel_senioridade`, para checar restrição de mix (ex.:
#' time sem nenhum sênior) na formação de equipes.
#'
#' @param ids_consultores Vetor de `consultor_id` do time.
#' @param consultores tibble de `gerar_consultores()`.
#' @return tibble com `nivel_senioridade`, `n`, `proporcao`.
agregar_balanco_senioridade <- function(ids_consultores, consultores) {
  consultores |>
    dplyr::filter(consultor_id %in% ids_consultores) |>
    dplyr::count(nivel_senioridade, name = "n") |>
    dplyr::mutate(proporcao = n / sum(n))
}
