#' Gera tempo de casa correlacionado ao nível de senioridade
#'
#' Modelo linear simples (nível numérico + ruído normal, piso em zero),
#' calibrado por casamento de momentos para reproduzir a correlação real
#' entre `nivel_senioridade` e tempo de casa (+0,34, ver
#' docs/mapeamento_schema.md). Não é cópula formal: para um único par de
#' variáveis, o pacote `copula` seria abstração prematura, e a regra do
#' projeto é não criar abstração antes de haver dois ou três usos concretos.
#' Cópula entra quando houver 3+ variáveis a correlacionar de uma vez
#' (Alocações, Camada 3).
#'
#' `media_anos`, `incremento_por_nivel` e `piso_anos` são suposições
#' documentadas: só a correlação com o nível foi calibrada contra dado real,
#' a escala absoluta (média e desvio de tempo de casa) não tinha fonte
#' agregada levantada.
#'
#' @param nivel_senioridade Factor ordenado, do mais júnior ao mais sênior.
#' @param correlacao_alvo Correlação de Pearson alvo com o nível numérico.
#' @param media_anos Tempo de casa médio, em anos, no nível mais júnior.
#' @param incremento_por_nivel Anos adicionais de tempo de casa esperados a
#'   cada nível de senioridade acima.
#' @param piso_anos Valor mínimo de tempo de casa, nunca negativo.
#' @return Vetor numérico de tempo de casa em anos, um valor por consultor.
gerar_tempo_casa <- function(nivel_senioridade,
                              correlacao_alvo = 0.34,
                              media_anos = 4,
                              incremento_por_nivel = 1,
                              piso_anos = 0.1) {
  nivel_numerico <- as.numeric(nivel_senioridade) - 1
  variancia_nivel <- stats::var(nivel_numerico)

  sinal <- incremento_por_nivel * nivel_numerico
  desvio_sinal <- incremento_por_nivel * sqrt(variancia_nivel)
  desvio_ruido <- desvio_sinal * sqrt(1 / correlacao_alvo^2 - 1)

  ruido <- stats::rnorm(length(nivel_numerico), mean = 0, sd = desvio_ruido)
  tempo_casa <- media_anos + sinal + ruido
  pmax(tempo_casa, piso_anos)
}
