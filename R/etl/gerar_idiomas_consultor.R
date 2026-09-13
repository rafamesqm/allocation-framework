#' Gera proficiência em idiomas de cada consultor (parte de `C`)
#'
#' Inglês é avaliado para todo consultor (prática comum em consultoria, e
#' quase universal na base real, 97,9% com ao menos básico). Espanhol é
#' calibrado pela Sexta rodada (docs/mapeamento_schema.md): presente em
#' algum nível para ~80% dos consultores (79,6% real), bem mais comum do que
#' a suposição inicial de 30%. Outro idioma segue suposição documentada, sem
#' fonte agregada real.
#'
#' @param consultores tibble de `gerar_consultores()`.
#' @param prob_espanhol Probabilidade de o consultor ter espanhol avaliado.
#' @param prob_outro_idioma Probabilidade de ter outro idioma além de
#'   inglês/espanhol.
#' @return tibble longo: `consultor_id`, `idioma`, `nivel`.
gerar_idiomas_consultor <- function(consultores, prob_espanhol = 0.8, prob_outro_idioma = 0.1) {
  niveis_idioma <- c("Básico", "Intermediário", "Avançado", "Fluente")

  purrr::map_dfr(consultores$consultor_id, function(id_consultor) {
    idiomas_do_consultor <- "Inglês"
    if (stats::runif(1) < prob_espanhol) {
      idiomas_do_consultor <- c(idiomas_do_consultor, "Espanhol")
    }
    if (stats::runif(1) < prob_outro_idioma) {
      idiomas_do_consultor <- c(idiomas_do_consultor, "Outro")
    }

    tibble::tibble(
      consultor_id = id_consultor,
      idioma = idiomas_do_consultor,
      nivel = sample(niveis_idioma, length(idiomas_do_consultor), replace = TRUE)
    )
  })
}
