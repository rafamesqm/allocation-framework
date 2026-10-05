#' Gera soft skills de cada consultor (parte de `C`), a partir da estrutura
#' de competências da avaliação de desempenho
#'
#' `soft_skills[]` é descrito no dicionário como "derivável das avaliações".
#' Usa a mesma estrutura de competências de `gerar_avaliacoes_desempenho()`
#' (`obter_subcompetencias_avaliacao()`) e a mesma escala de 3 níveis, para
#' as duas peças nunca divergirem. Todo consultor tem as 9 competências
#' avaliadas (não é esparso como `skills_hard[]`).
#'
#' A tabela gerada é o atributo latente, completo. A observação é parcial: o
#' crosswalk de schema (docs/mapeamento_schema.md) aponta `soft_skills[]`
#' para o bloco de habilidades de consultoria do Formulário de Skills 2.1, e
#' não para a base de avaliação de desempenho, cujo acesso nunca foi
#' liberado. A restrição aos respondentes é aplicada em `_targets.R` por
#' `restringir_a_respondentes()`, na fronteira do que vai para
#' `data/synthetic/`.
#'
#' @param consultores tibble de `gerar_consultores()`.
#' @param pesos_nivel Pesos para ABAIXO/DENTRO/ACIMA. Suposição documentada:
#'   maioria "dentro do esperado", sem calibração real de proporção
#'   populacional (só vimos um exemplo real, n=1, ver mapeamento_schema.md).
#' @return tibble longo: `consultor_id`, `competencia`, `nivel`.
gerar_soft_skills_consultor <- function(consultores, pesos_nivel = c(ABAIXO = 0.15, DENTRO = 0.6, ACIMA = 0.25)) {
  competencias <- obter_subcompetencias_avaliacao()

  purrr::map_dfr(consultores$consultor_id, function(id_consultor) {
    tibble::tibble(
      consultor_id = id_consultor,
      competencia = competencias,
      nivel = sample(names(pesos_nivel), length(competencias), replace = TRUE, prob = pesos_nivel)
    )
  })
}
