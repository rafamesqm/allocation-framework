#' Calcula margem estrutural por projeto (candidato (d) da construção do
#' rótulo, Camada 3)
#'
#' Candidato (d) de docs/dicionario_de_dados.md, "Construção do rótulo":
#' margem calculada estruturalmente a partir de custo vezes horas contra o
#' valor de contrato, em vez de herdada do agregado de frente/mês. Para
#' cada projeto, integra o custo de cada consultor alocado ao longo da
#' duração da alocação e subtrai do `valor_contrato` do projeto.
#'
#' **A integração pela duração não é refinamento, é a unidade certa.** A
#' primeira versão calculava `custo_carregado * horas_mes` e subtraía isso
#' do valor de contrato. Os dois lados estavam em unidades diferentes:
#' `custo_carregado` é R$ por hora e `horas_mes` é hora por mês, então o
#' produto é R$ por **mês**, enquanto `valor_contrato` é um total. O bloco
#' II.5 encontrou o efeito disso ao conferir a plausibilidade dos valores, e
#' não só dos zeros: 41,5% dos projetos saíam com margem negativa e o décimo
#' percentil ficava em menos 8,5 vezes o valor do contrato. A alocação
#' mediana dura 0,39 mês e a média 3,96, então o erro não tinha nem sinal
#' constante: superestimava custo em alocação curta e subestimava em longa.
#'
#' Projeto sem nenhuma alocação (cold start do lado do projeto) tem custo
#' total zero, não fica de fora: a margem estrutural nesse caso é
#' simplesmente o `valor_contrato` inteiro, coerente com "não houve custo
#' de entrega ainda".
#'
#' @param alocacoes tibble de `gerar_alocacoes()`, com `duracao_dias`.
#' @param custo_carregado tibble de `gerar_custo_carregado()`.
#' @param projetos tibble de `gerar_projetos()`.
#' @param dias_por_mes Divisor que converte duração em dias para meses.
#'   365,25 dividido por 12, para o ano médio fechar.
#' @return tibble com `projeto_id`, `custo_total`, `valor_contrato`,
#'   `margem_estrutural`.
gerar_margem_estrutural <- function(alocacoes, custo_carregado, projetos,
                                     dias_por_mes = 365.25 / 12) {
  custo_por_projeto <- alocacoes |>
    dplyr::left_join(custo_carregado, by = "consultor_id") |>
    dplyr::mutate(
      meses_alocado = duracao_dias / dias_por_mes,
      custo_alocacao = custo_carregado * horas_mes * meses_alocado
    ) |>
    dplyr::summarise(custo_total = sum(custo_alocacao), .by = projeto_id)

  projetos |>
    dplyr::select(projeto_id, valor_contrato) |>
    dplyr::left_join(custo_por_projeto, by = "projeto_id") |>
    dplyr::mutate(
      custo_total = dplyr::coalesce(custo_total, 0),
      margem_estrutural = valor_contrato - custo_total
    )
}
