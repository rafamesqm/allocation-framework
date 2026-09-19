#' Gera a expertise funcional primária de cada consultor (parte de `C`)
#'
#' Usa as categorias reais de `Offering` (ver docs/mapeamento_schema.md,
#' distribuição de `vw_crm_oportunidades_latest`) como espaço de expertise
#' funcional, já que não existe taxonomia própria do lado do consultor.
#' `expertise_funcional` é especificado no dicionário como categórico, não
#' vetor: cada consultor tem uma área primária só.
#'
#' Gera para todos os consultores porque a área de atuação existe
#' independentemente de ter sido declarada em formulário. A restrição aos
#' respondentes é aplicada em `_targets.R`, na fronteira do que vai para
#' `data/synthetic/`, por `restringir_a_respondentes()`. Ver
#' `sortear_respondentes_formulario()` para por que a cobertura é
#' compartilhada entre os sinais de formulário e não sorteada aqui.
#'
#' @param consultores tibble de `gerar_consultores()` (usa só `consultor_id`).
#' @return tibble com `consultor_id`, `expertise_funcional`.
gerar_expertise_funcional <- function(consultores) {
  offerings <- c(
    "Experience & Efficiency", "Strategy and M&A",
    "Data Analytics & Artificial Intelligence", "Supply Chain",
    "Tech Consulting", "Finance", "Talent & Organization",
    "Digital Products & Platforms", "Cyber Security", "Digital Factory",
    "Peers.AI", "Educação Pública"
  )
  pesos_offering <- c(991, 642, 530, 524, 427, 424, 192, 155, 119, 114, 49, 47)

  tibble::tibble(
    consultor_id = consultores$consultor_id,
    expertise_funcional = sample(offerings, nrow(consultores), replace = TRUE, prob = pesos_offering)
  )
}
