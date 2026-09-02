#' Gera uma variável numérica correlacionada ao nível de senioridade, por
#' casamento de momentos
#'
#' Generaliza a técnica de `gerar_tempo_casa()` para qualquer variável com
#' correlação alvo conhecida contra `nivel_senioridade` (aqui, `pct_alocacao`
#' e `horas_mes`, ver docs/mapeamento_schema.md, "Correlações extras"):
#' constrói a variável como combinação linear padronizada do nível
#' (`correlacao_alvo * nivel_padronizado`) mais ruído normal independente, na
#' proporção exata que reproduz a correlação de Pearson alvo.
#'
#' @param nivel_senioridade Factor ordenado, do mais júnior ao mais sênior.
#' @param correlacao_alvo Correlação de Pearson alvo com o nível numérico
#'   (pode ser negativa, ex.: `horas_mes`).
#' @param media Média da variável gerada.
#' @param desvio_variavel Desvio-padrão alvo da variável gerada, antes de
#'   qualquer corte por `minimo`/`maximo`.
#' @param minimo,maximo Limites opcionais (ex.: `pct_alocacao` não passa de
#'   1). `NULL` desativa o corte correspondente.
#' @return Vetor numérico, um valor por consultor.
gerar_variavel_por_nivel <- function(nivel_senioridade, correlacao_alvo, media,
                                      desvio_variavel, minimo = NULL, maximo = NULL) {
  nivel_numerico <- as.numeric(nivel_senioridade) - 1
  nivel_padronizado <- (nivel_numerico - mean(nivel_numerico)) / stats::sd(nivel_numerico)

  sinal <- correlacao_alvo * desvio_variavel * nivel_padronizado
  desvio_ruido <- desvio_variavel * sqrt(1 - correlacao_alvo^2)
  ruido <- stats::rnorm(length(nivel_numerico), mean = 0, sd = desvio_ruido)

  variavel <- media + sinal + ruido
  if (!is.null(minimo)) variavel <- pmax(variavel, minimo)
  if (!is.null(maximo)) variavel <- pmin(variavel, maximo)
  variavel
}
