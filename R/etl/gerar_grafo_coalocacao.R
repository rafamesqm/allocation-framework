#' Deriva o grafo de coalocação (`G`, Camada 4) a partir de `R`
#'
#' `G` não é sorteado, é derivado do histórico de alocação: duas pessoas no
#' mesmo projeto viram uma aresta. O peso aqui é a frequência de coalocação
#' (quantos projetos em comum), não uma medida de "coalocação boa": esse
#' refinamento fica para a EDA decidir (ver docs/dicionario_de_dados.md,
#' "Montagem de G"), fora do escopo desta função.
#'
#' @param alocacoes tibble de `gerar_alocacoes()`.
#' @return tibble com `consultor_a`, `consultor_b`, `n_projetos_em_comum`,
#'   um par por linha (a mesma dupla nunca aparece em ordem trocada).
gerar_grafo_coalocacao <- function(alocacoes) {
  consultores_por_projeto <- alocacoes |>
    dplyr::summarise(consultores = list(consultor_id), .by = projeto_id) |>
    dplyr::filter(lengths(consultores) >= 2)

  purrr::map_dfr(consultores_por_projeto$consultores, function(ids) {
    pares <- utils::combn(sort(unique(ids)), 2)
    tibble::tibble(consultor_a = pares[1, ], consultor_b = pares[2, ])
  }) |>
    dplyr::count(consultor_a, consultor_b, name = "n_projetos_em_comum")
}
