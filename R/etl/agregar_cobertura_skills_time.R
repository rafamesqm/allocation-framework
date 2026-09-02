#' Calcula a cobertura de skills de um time (Camada 4)
#'
#' Agregação sobre `C` (skills por consultor): não é um objeto sorteado, é
#' calculado sob demanda para qualquer conjunto de consultores. Serve tanto
#' para caracterizar um time já formado (a partir de `alocacoes`) quanto
#' para avaliar um time candidato na formação futura de equipes.
#'
#' @param ids_consultores Vetor de `consultor_id` do time.
#' @param skills_consultor tibble de `gerar_skills_consultor()`.
#' @return tibble com `skill_id`, `n_consultores_com_skill`, uma linha por
#'   skill coberta por pelo menos um consultor do time.
agregar_cobertura_skills_time <- function(ids_consultores, skills_consultor) {
  skills_consultor |>
    dplyr::filter(consultor_id %in% ids_consultores) |>
    dplyr::count(skill_id, name = "n_consultores_com_skill")
}
