#' Gera soft skills de cada consultor (parte de `C`), a partir da estrutura
#' de competências da avaliação de desempenho
#'
#' `soft_skills[]` é descrito no dicionário como "derivável das avaliações".
#' Como a avaliação de desempenho ainda não foi gerada (peça 5 do roadmap),
#' usa-se diretamente a estrutura real de competências reconstruída da
#' plataforma (9 subcompetências em 5 categorias, ver
#' docs/dicionario_de_dados.md) e a mesma escala de 3 níveis, para manter
#' coerência com o que a peça 5 vai produzir depois. Todo consultor tem as 9
#' competências avaliadas (não é esparso como `skills_hard[]`).
#'
#' @param consultores tibble de `gerar_consultores()`.
#' @param pesos_nivel Pesos para ABAIXO/DENTRO/ACIMA. Suposição documentada:
#'   maioria "dentro do esperado", sem calibração real de proporção
#'   populacional (só vimos um exemplo real, n=1, ver mapeamento_schema.md).
#' @return tibble longo: `consultor_id`, `competencia`, `nivel`.
gerar_soft_skills_consultor <- function(consultores, pesos_nivel = c(ABAIXO = 0.15, DENTRO = 0.6, ACIMA = 0.25)) {
  competencias <- c(
    "Vivência e Disseminação", "Autodesenvolvimento", "Liderança",
    "Visão, Planejamento e Organização", "Gestão da Entrega",
    "Conhecimento Técnico e Experiência Prática", "Execução com Qualidade",
    "Inovação", "Comercial"
  )

  purrr::map_dfr(consultores$consultor_id, function(id_consultor) {
    tibble::tibble(
      consultor_id = id_consultor,
      competencia = competencias,
      nivel = sample(names(pesos_nivel), length(competencias), replace = TRUE, prob = pesos_nivel)
    )
  })
}
