#' Lista as 9 competências de soft skills
#'
#' Fonte única da lista usada por `gerar_soft_skills_consultor()`. Os nomes
#' são suposição documentada dentro de 5 categorias (Cultura,
#' Desenvolvimento, Gestão de Projeto, Entrega do Projeto, Geração de Valor):
#' a categoria tem respaldo na estrutura de competências da organização, o
#' nome exato de cada competência dentro dela não.
#'
#' @return Vetor de texto com as 9 competências.
listar_competencias_soft_skills <- function() {
  c(
    "Vivência e Disseminação", "Autodesenvolvimento", "Liderança",
    "Visão, Planejamento e Organização", "Gestão da Entrega",
    "Conhecimento Técnico e Experiência Prática", "Execução com Qualidade",
    "Inovação", "Geração de Receita"
  )
}
