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
#' Valores fora dos limites são **reamostrados**, não recortados, ver o
#' comentário no corpo para o porquê.
#'
#' @param minimo,maximo Limites opcionais (ex.: `pct_alocacao` não passa de
#'   1). `NULL` desativa o limite correspondente.
#' @param max_tentativas Teto de rodadas de reamostragem antes de recorrer ao
#'   recorte, que existe só para garantir terminação no caso degenerado.
#' @return Vetor numérico, um valor por consultor.
gerar_variavel_por_nivel <- function(nivel_senioridade, correlacao_alvo, media,
                                      desvio_variavel, minimo = NULL, maximo = NULL,
                                      max_tentativas = 100) {
  nivel_numerico <- as.numeric(nivel_senioridade) - 1
  nivel_padronizado <- (nivel_numerico - mean(nivel_numerico)) / stats::sd(nivel_numerico)

  sinal <- correlacao_alvo * desvio_variavel * nivel_padronizado
  desvio_ruido <- desvio_variavel * sqrt(1 - correlacao_alvo^2)

  limite_inferior <- if (is.null(minimo)) -Inf else minimo
  limite_superior <- if (is.null(maximo)) Inf else maximo

  # Reamostra por rejeição em vez de recortar. Recortar com pmax/pmin empilha
  # no limite toda a massa que caiu fora dele, criando um pico artificial que
  # a calibração nunca pediu: na versão anterior, 7,2% das alocações saíam com
  # pct_alocacao exatamente 1,00 e 4,2% com horas_mes exatamente 0, valores
  # que a fonte real não reporta. A rejeição devolve a normal truncada que a
  # calibração sempre pretendeu, sem ponto de massa.
  #
  # Só o ruído é redesenhado, nunca o sinal: é o sinal que carrega a
  # correlação com o nível, e redesenhá-lo destruiria a calibração.
  variavel <- media + sinal + stats::rnorm(length(sinal), mean = 0, sd = desvio_ruido)

  for (tentativa in seq_len(max_tentativas)) {
    fora <- variavel < limite_inferior | variavel > limite_superior
    if (!any(fora)) break
    variavel[fora] <- media + sinal[fora] +
      stats::rnorm(sum(fora), mean = 0, sd = desvio_ruido)
  }

  # Recorte de último recurso, para o caso degenerado em que a faixa é
  # estreita demais ou o ruído é nulo (correlação alvo de módulo 1) e a
  # rejeição nunca convergiria. Mantém a função total em vez de entrar em
  # laço infinito, e o teste verifica que este caminho não é exercitado na
  # calibração real.
  pmin(pmax(variavel, limite_inferior), limite_superior)
}
