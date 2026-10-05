#' Agrega ocupação de cada consultor por mês, a partir das alocações (Camada 3)
#'
#' `calendario_alocacao`/`pct_ocupacao` não são sorteados de forma
#' independente: derivam da matriz `R` já gerada por `gerar_alocacoes()`.
#' Como cada alocação carrega sua própria janela de datas, a agregação é uma
#' série temporal por consultor e mês, que é a granularidade que o vetor `k`
#' (capacidade por pessoa e período) exige no dicionário.
#'
#' **Mês parcial entra ponderado pelos dias cobertos**, não inteiro. Com
#' mediana de alocação de 18 dias, contar cada mês tocado como mês cheio
#' superestimaria a ocupação de forma sistemática. `pct_ocupacao` fica então
#' interpretável como fração de capacidade usada naquele mês, e valor acima
#' de 1 passa a significar superalocação de verdade, não artefato de soma.
#'
#' Consultor sem nenhuma alocação (cold start) fica de fora do resultado, e
#' mês sem nenhuma alocação não vira linha de zero: quem consome deve tratar
#' ausência como zero explicitamente, se for o caso.
#'
#' @param alocacoes tibble de `gerar_alocacoes()`, com `data_inicio` e
#'   `data_fim` por alocação.
#' @return tibble com `consultor_id`, `ano_mes`, `pct_ocupacao`,
#'   `horas_mes`, `n_alocacoes`, ordenado por consultor e mês.
agregar_ocupacao_consultor <- function(alocacoes) {
  meses <- expandir_para_meses(alocacoes$data_inicio, alocacoes$data_fim)

  meses |>
    dplyr::mutate(
      consultor_id = alocacoes$consultor_id[linha],
      fracao_do_mes = dias_ativos / dias_no_mes,
      pct_ocupacao = alocacoes$pct_alocacao[linha] * fracao_do_mes,
      horas_mes = alocacoes$horas_mes[linha] * fracao_do_mes
    ) |>
    dplyr::summarise(
      pct_ocupacao = sum(pct_ocupacao),
      horas_mes = sum(horas_mes),
      n_alocacoes = dplyr::n(),
      .by = c(consultor_id, ano_mes)
    ) |>
    dplyr::arrange(consultor_id, ano_mes)
}
