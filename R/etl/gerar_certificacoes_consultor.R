#' Gera certificações de cada consultor (parte de `C`)
#'
#' Sem proporção real calibrada: as planilhas de skills têm campo de texto
#' livre para certificação, sem contagem agregada extraída nesta rodada (ver
#' docs/mapeamento_schema.md). Usa uma probabilidade de posse e um catálogo
#' genérico, ambos suposição documentada, não catálogo real da Peers.
#'
#' @param consultores tibble de `gerar_consultores()`.
#' @param prob_possui Probabilidade de o consultor ter ao menos 1 certificação.
#' @return tibble longo: `consultor_id`, `certificacao`. Consultor sem
#'   nenhuma certificação simplesmente não aparece na tabela.
gerar_certificacoes_consultor <- function(consultores, prob_possui = 0.4) {
  catalogo_generico <- c(
    "PMP", "Scrum Master", "AWS Certified", "Six Sigma", "ITIL", "Prince2", "SAFe"
  )

  purrr::map_dfr(consultores$consultor_id, function(id_consultor) {
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
