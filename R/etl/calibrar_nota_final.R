#' Calibra a nota final do gestor a partir da nota às cegas, puxando um
#' nível em direção à autoavaliação com uma dada probabilidade
#'
#' Simula o efeito de calibração descrito na plataforma real: o gestor
#' define a nota final depois de ver a autoavaliação ao lado da avaliação
#' às cegas que ele mesmo deu (ver docs/mapeamento_schema.md, "Fonte
#' externa: plataforma de avaliação de desempenho").
#'
#' @param nota_gestor_cegas,nota_individual Vetores paralelos de nota
#'   ordinal (`"ABAIXO"`/`"DENTRO"`/`"ACIMA"`).
#' @param prob_puxa Probabilidade de a nota final se mover um nível em
#'   direção à autoavaliação, a partir da nota às cegas.
#' @return Vetor de nota final, no mesmo domínio ordinal.
calibrar_nota_final <- function(nota_gestor_cegas, nota_individual, prob_puxa) {
  niveis <- c("ABAIXO", "DENTRO", "ACIMA")
  cegas_num <- match(nota_gestor_cegas, niveis)
  individual_num <- match(nota_individual, niveis)

  direcao <- sign(individual_num - cegas_num)
  puxa <- stats::runif(length(cegas_num)) < prob_puxa

  final_num <- cegas_num + ifelse(puxa, direcao, 0)
  niveis[final_num]
}
