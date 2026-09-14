#' Expande intervalos de datas para uma linha por mês-calendário coberto
#'
#' Ponte entre os objetos que nascem com janela de datas (alocação, projeto)
#' e os que a Camada 3 exige por pessoa-período ou frente-mês: `pct_ocupacao`
#' e `capacidade_periodo` alimentam `k`, e a margem real existe por frente e
#' mês (ver docs/dicionario_de_dados.md, Camada 3).
#'
#' Devolve também quantos dias do mês o intervalo de fato cobre, para quem
#' consome poder ponderar o mês parcial em vez de contá-lo inteiro. Um
#' intervalo que começa dia 28 não ocupa o mês todo, e tratar como se
#' ocupasse superestimaria a ocupação justamente nas alocações curtas, que
#' são a maioria (mediana real de 18 dias).
#'
#' O intervalo é fechado nos dois extremos: uma alocação que começa e termina
#' no mesmo dia cobre 1 dia, não zero.
#'
#' @param data_inicio,data_fim Vetores de `Date`, de mesmo comprimento.
#'   `data_fim` nunca antes de `data_inicio`.
#' @return tibble com `linha` (índice da posição de entrada, para juntar de
#'   volta), `ano_mes` (texto `"AAAA-MM"`), `dias_ativos` e `dias_no_mes`.
expandir_para_meses <- function(data_inicio, data_fim) {
  stopifnot(length(data_inicio) == length(data_fim), all(data_fim >= data_inicio))

  primeiro_mes <- lubridate::floor_date(data_inicio, "month")
  ultimo_mes <- lubridate::floor_date(data_fim, "month")

  n_meses <- lubridate::interval(primeiro_mes, ultimo_mes) %/% months(1) + 1
  linha <- rep(seq_along(data_inicio), times = n_meses)
  mes <- primeiro_mes[linha] + months(sequence(n_meses) - 1)
  fim_do_mes <- mes + months(1) - 1

  inicio_no_mes <- pmax(data_inicio[linha], mes)
  fim_no_mes <- pmin(data_fim[linha], fim_do_mes)

  tibble::tibble(
    linha = linha,
    ano_mes = format(mes, "%Y-%m"),
    dias_ativos = as.integer(fim_no_mes - inicio_no_mes) + 1L,
    dias_no_mes = as.integer(fim_do_mes - mes) + 1L
  )
}
