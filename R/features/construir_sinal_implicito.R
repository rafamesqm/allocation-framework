#' Constrói o sinal de interação implícito de cada par consultor e projeto
#'
#' Segue Hu, Koren e Volinsky (2008): a ocorrência da alocação vira uma
#' preferência binária, `r_ij = 1`, e as horas do consultor no projeto viram
#' uma confiança, `w_ij = 1 + alfa * log(1 + h_ij)`. O par não observado não
#' tem linha aqui: ele é ausência de sinal, e não preferência zero, e é
#' `construir_matriz_r()` que o representa como célula vazia.
#'
#' `alfa = 0` dá o binário puro, com confiança 1 em todo par observado. A
#' comparação entre `alfa = 0` e `alfa = 1` fica registrada no bloco II.5
#' antes de existir modelo, e é decidida pelo teste pareado do protocolo do
#' bloco II.7: se a confiança por horas não superar o binário, fica o
#' binário.
#'
#' As horas seguem a convenção da margem: `horas_mes` vezes a duração da
#' alocação em meses. Um par com mais de uma alocação soma as horas de todas.
#'
#' @param alocacoes tibble com `consultor_id`, `projeto_id`, `horas_mes` e
#'   `duracao_dias`.
#' @param alfa Peso do logaritmo das horas na confiança. Não negativo.
#' @param dias_por_mes Divisor que converte a duração em dias para meses.
#' @return tibble com uma linha por par observado: `consultor_id`,
#'   `projeto_id`, `horas`, `preferencia` e `confianca`.
construir_sinal_implicito <- function(alocacoes, alfa = 0, dias_por_mes = 365.25 / 12) {
  if (!is.numeric(alfa) || length(alfa) != 1 || is.na(alfa) || alfa < 0) {
    stop("`alfa` precisa ser um número não negativo, e veio ", deparse(alfa), ".", call. = FALSE)
  }

  alocacoes |>
    dplyr::mutate(horas = horas_mes * duracao_dias / dias_por_mes) |>
    dplyr::summarise(horas = sum(horas), .by = c(consultor_id, projeto_id)) |>
    dplyr::mutate(
      preferencia = 1,
      confianca = 1 + alfa * log1p(horas)
    )
}
