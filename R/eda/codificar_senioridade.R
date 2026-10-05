#' Níveis de senioridade, da base ao topo
#'
#' A ordem é contraintuitiva e já causou erro de sinal neste projeto: no
#' rótulo real, `N0` é o **topo** da hierarquia e `N4` a base, ou seja, o
#' número decresce conforme a senioridade cresce. A reconciliação original
#' leu o rótulo ao pé da letra, codificou N0 como 0, e obteve correlação de
#' -0,90 entre nível e custo, que só foi reconhecida como invertida porque o
#' sinal não fazia sentido de negócio (ver docs/mapeamento_schema.md,
#' Segunda rodada).
#'
#' Todos os alvos de correlação registrados no projeto pressupõem esta
#' escala ascendente de senioridade.
#'
#' @return Vetor de texto com os cinco níveis, da base (`N4`) ao topo (`N0`).
niveis_senioridade <- function() {
  c("N4", "N3", "N2", "N1", "N0")
}

#' Codifica senioridade como fator ordenado, da base ao topo
#'
#' Necessário porque a ida e volta pelo CSV perde a ordenação: o gerador
#' produz um fator ordenado, o arquivo guarda texto, e a leitura devolve
#' texto. Correlacionar o texto exigiria uma conversão implícita, que em R
#' sairia em ordem alfabética (`N0` < `N1` < ... < `N4`), exatamente a
#' inversão que produziria o sinal errado.
#'
#' @param x Vetor de texto com os níveis.
#' @return Fator ordenado com os níveis de `niveis_senioridade()`.
codificar_senioridade <- function(x) {
  desconhecidos <- setdiff(unique(x), niveis_senioridade())
  if (length(desconhecidos) > 0) {
    stop(
      "Nível de senioridade fora da escala conhecida: ",
      paste(desconhecidos, collapse = ", "),
      call. = FALSE
    )
  }
  factor(x, levels = niveis_senioridade(), ordered = TRUE)
}

#' Converte senioridade para escala numérica ascendente
#'
#' `N4` vira 0 e `N0` vira 4, isto é, o número cresce com a senioridade. É
#' essa a direção em que os alvos de correlação do projeto foram registrados.
#'
#' @param x Vetor de texto ou fator com os níveis.
#' @return Vetor numérico de 0 a 4.
senioridade_numerica <- function(x) {
  as.numeric(codificar_senioridade(as.character(x))) - 1
}
