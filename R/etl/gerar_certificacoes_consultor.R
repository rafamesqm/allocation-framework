#' Gera certificações de cada consultor (parte de `C`)
#'
#' Sem proporção real calibrada: as planilhas de skills têm campo de texto
#' livre para certificação, sem contagem agregada extraída nesta rodada (ver
#' docs/mapeamento_schema.md). Usa uma probabilidade de posse e um catálogo
#' genérico, ambos suposição documentada, não catálogo real da Peers.
#'
#' A pergunta de certificação está dentro dos três formulários de skills
#' (docs/mapeamento_schema.md), e não numa base corporativa à parte. Quem não
#' respondeu não tem certificação registrada, qualquer que seja a
#' certificação que de fato possua, e por isso a posse é condicional a ser
#' respondente. A cobertura resultante é `cobertura_populacional` vezes
#' `prob_possui`, isto é, um teto de 48,4% por construção, e não os 40%
#' aplicados à população inteira como antes desta correção.
#'
#' @param consultores tibble de `gerar_consultores()`.
#' @param respondentes Vetor de `consultor_id` de
#'   `sortear_respondentes_formulario()`. Quem está fora não aparece na
#'   tabela, independentemente de `prob_possui`.
#' @param prob_possui Probabilidade de o consultor respondente ter declarado
#'   ao menos 1 certificação. Suposição documentada: a proporção real nunca
#'   foi agregada e o acesso ao arquivo está encerrado.
#' @return tibble longo: `consultor_id`, `certificacao`. Consultor sem
#'   nenhuma certificação simplesmente não aparece na tabela.
gerar_certificacoes_consultor <- function(consultores,
                                           respondentes = NULL,
                                           prob_possui = 0.4) {
  catalogo_generico <- c(
    "PMP", "Scrum Master", "AWS Certified", "Six Sigma", "ITIL", "Prince2", "SAFe"
  )

  if (is.null(respondentes)) {
    respondentes <- sortear_respondentes_formulario(consultores)
  }
  consultores_respondentes <- intersect(consultores$consultor_id, respondentes)

  purrr::map_dfr(consultores_respondentes, function(id_consultor) {
    if (stats::runif(1) >= prob_possui) {
      return(tibble::tibble(consultor_id = character(), certificacao = character()))
    }
    n_certificacoes <- sample.int(3, size = 1)
    tibble::tibble(
      consultor_id = id_consultor,
      certificacao = sample(catalogo_generico, n_certificacoes)
    )
  })
}
