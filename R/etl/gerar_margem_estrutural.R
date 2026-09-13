#' Calcula margem estrutural por projeto (candidato (d) da construção do
#' rótulo, Camada 3)
#'
#' Candidato (d) de docs/dicionario_de_dados.md, "Construção do rótulo":
#' margem calculada estruturalmente a partir de custo vezes horas contra o
#' valor de contrato, em vez de herdada do agregado de frente/mês. Para
#' cada projeto, soma `custo_carregado * horas_mes` de todo consultor
#' alocado nele e subtrai do `valor_contrato` do projeto.
#'
#' Projeto sem nenhuma alocação (cold start do lado do projeto) tem custo
#' total zero, não fica de fora: a margem estrutural nesse caso é
#' simplesmente o `valor_contrato` inteiro, coerente com "não houve custo
#' de entrega ainda".
#'
#' @param alocacoes tibble de `gerar_alocacoes()`.
#' @param custo_carregado tibble de `gerar_custo_carregado()`.
#' @param projetos tibble de `gerar_projetos()`.
#' @return tibble com `projeto_id`, `custo_total`, `valor_contrato`,
#'   `margem_estrutural`.
gerar_margem_estrutural <- function(alocacoes, custo_carregado, projetos) {
  custo_por_projeto <- alocacoes |>
    dplyr::left_join(custo_carregado, by = "consultor_id") |>
    dplyr::mutate(custo_alocacao = custo_carregado * horas_mes) |>
    dplyr::summarise(custo_total = sum(custo_alocacao), .by = projeto_id)

  projetos |>
    dplyr::select(projeto_id, valor_contrato) |>
    dplyr::left_join(custo_por_projeto, by = "projeto_id") |>
    dplyr::mutate(
      custo_total = dplyr::coalesce(custo_total, 0),
      margem_estrutural = valor_contrato - custo_total
    )
}
