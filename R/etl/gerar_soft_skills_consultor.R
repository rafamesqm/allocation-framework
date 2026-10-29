#' Gera soft skills de cada consultor (parte de `C`)
#'
#' Cada consultor recebe um nível, numa escala de 3, em cada uma das 9
#' competências de `listar_competencias_soft_skills()`. Não é esparso como
#' `skills_hard[]`: todo consultor tem as 9.
#'
#' A tabela gerada é o atributo latente, completo. A observação é parcial: o
#' crosswalk de schema (docs/mapeamento_schema.md) aponta `soft_skills[]`
#' para o bloco de habilidades de consultoria do formulário de skills, e a
#' restrição aos respondentes é aplicada em `_targets.R` por
#' `restringir_a_respondentes()`, na fronteira do que vai para
#' `data/synthetic/`.
#'
#' @param consultores tibble de `gerar_consultores()`.
#' @param pesos_nivel Pesos para ABAIXO/DENTRO/ACIMA. Suposição documentada:
#'   maioria "dentro do esperado", sem calibração real de proporção
#'   populacional (só vimos um exemplo real, n=1, ver mapeamento_schema.md).
#' @return tibble longo: `consultor_id`, `competencia`, `nivel`.
gerar_soft_skills_consultor <- function(consultores, pesos_nivel = c(ABAIXO = 0.15, DENTRO = 0.6, ACIMA = 0.25)) {
  competencias <- listar_competencias_soft_skills()

  purrr::map_dfr(consultores$consultor_id, function(id_consultor) {
    tibble::tibble(
      consultor_id = id_consultor,
      competencia = competencias,
      nivel = sample(names(pesos_nivel), length(competencias), replace = TRUE, prob = pesos_nivel)
    )
  })
}
