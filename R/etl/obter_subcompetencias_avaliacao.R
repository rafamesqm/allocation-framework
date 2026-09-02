#' Lista as 9 subcompetências da avaliação de desempenho
#'
#' Fonte única da lista, usada tanto por `gerar_soft_skills_consultor()`
#' quanto por `gerar_avaliacoes_desempenho()`, para as duas nunca
#' divergirem (já divergiram uma vez nesta base: a mesma string "Comercial"
#' aparecia aqui e também como item de Performance em
#' `gerar_avaliacoes_desempenho()`, corrigida para "Geração de Receita").
#'
#' Os nomes das 9 subcompetências são suposição documentada dentro das 5
#' categorias reais (Cultura, Desenvolvimento, Gestão de Projeto, Entrega do
#' Projeto, Geração de Valor, ver docs/dicionario_de_dados.md): a estrutura
#' reconstruída da plataforma confirma a categoria, não o nome exato de
#' cada subcompetência dentro dela.
#'
#' @return Vetor de texto com as 9 subcompetências.
obter_subcompetencias_avaliacao <- function() {
  c(
    "Vivência e Disseminação", "Autodesenvolvimento", "Liderança",
    "Visão, Planejamento e Organização", "Gestão da Entrega",
    "Conhecimento Técnico e Experiência Prática", "Execução com Qualidade",
    "Inovação", "Geração de Receita"
  )
}
