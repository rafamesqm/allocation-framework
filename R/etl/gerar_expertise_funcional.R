#' Gera a expertise funcional primária de cada consultor (parte de `C`)
#'
#' Usa as categorias reais de `Offering` (ver docs/mapeamento_schema.md,
#' distribuição da base de oportunidades do CRM) como espaço de expertise
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
  # Os nomes reais das ofertas ficam fora do repositório; os códigos seguem a
  # ordem decrescente dos pesos reais, e o sorteio é o mesmo que com os nomes.
  pesos_offering <- c(991, 642, 530, 524, 427, 424, 192, 155, 119, 114, 49, 47)
  offerings <- sprintf("oferta_%02d", seq_along(pesos_offering))

  tibble::tibble(
    consultor_id = consultores$consultor_id,
    expertise_funcional = sample(offerings, nrow(consultores), replace = TRUE, prob = pesos_offering)
  )
}
