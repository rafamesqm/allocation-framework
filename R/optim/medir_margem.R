#' Margem dos projetos com assento sob uma solução, por caminho independente
#'
#' A margem não é objetivo do problema: com os mesmos assentos, a receita de
#' cada projeto é fixa, e margem = receita - custo. Esta função recalcula a
#' margem de uma solução sem passar pelo vetor de custo da matriz, a partir
#' do ocupante de cada assento, das horas dele nos meses de teste e do custo
#' por hora da tabela de custo, para o teste poder conferir que a variação da
#' margem entre duas soluções é o negativo da variação do custo.
#'
#' A receita é o valor do contrato. Qualquer receita fixa por projeto daria a
#' mesma variação.
#'
#' @param problema Lista de `montar_problema_da_otimizacao()`.
#' @param x Vetor de `completar_solucao()`.
#' @param insumos Lista de `preparar_insumos_da_otimizacao()`.
#' @param projetos tibble com `projeto_id` e `valor_contrato`.
#' @param custo_carregado tibble com `consultor_id` e `custo_carregado`.
#' @return tibble com `projeto_id`, `receita`, `custo` e `margem`.
medir_margem_dos_projetos <- function(problema, x, insumos, projetos, custo_carregado) {
  ocupantes <- problema$pares[x[seq_len(nrow(problema$pares))] > 0.5, c("assento_id", "consultor_id")]

  insumos$meses_dos_assentos |>
    dplyr::inner_join(ocupantes, by = "assento_id") |>
    dplyr::left_join(custo_carregado |> dplyr::select(consultor_id, custo_carregado), by = "consultor_id") |>
    dplyr::mutate(projeto_id = insumos$assentos$projeto_id[match(assento_id, insumos$assentos$assento_id)]) |>
    dplyr::summarise(custo = sum(custo_carregado * carga_horas), .by = projeto_id) |>
    dplyr::mutate(receita = projetos$valor_contrato[match(projeto_id, projetos$projeto_id)]) |>
    dplyr::transmute(projeto_id, receita, custo, margem = receita - custo)
}
