#' Gera a matriz esparsa consultor × skill (parte de `C`: `skills_hard[]` e
#' `recencia_skill[]`)
#'
#' Cada consultor recebe 1 a 2 categorias principais (ver `gerar_espaco_skills`
#' para a taxonomia) e conhece a maior parte das skills dessas categorias,
#' mais um punhado ocasional fora delas. Isso emula a cobertura parcial e
#' concentrada por especialidade observada na prática (alguém de backend não
#' costuma saber RPA), não uma amostra uniforme do catálogo inteiro.
#'
#' Escala de proficiência usa os rótulos do Formulário de Skills 2.1
#' (Iniciante/Básico/Médio/Avançado). As planilhas reais têm duas escalas
#' diferentes que ainda precisam de de-para (ver docs/mapeamento_schema.md);
#' até isso ser resolvido, o sintético usa só uma, documentado aqui.
#'
#' `recencia_skill` é a quantidade de dias desde a última atualização
#' registrada da skill, amostrada uniformemente até `recencia_maxima_dias`.
#' Sem calibração real de decaimento, é suposição documentada.
#'
#' Cobertura populacional calibrada pela Sexta rodada
#' (docs/mapeamento_schema.md): só 48,4% dos consultores ativos (138 de 285)
#' têm ao menos uma resposta nas planilhas de skills, medido por interseção
#' de hashes de e-mail. Um consultor sorteado como não respondente não
#' aparece nesta tabela (cold start do lado do conteúdo), em vez de todo
#' consultor ter cobertura garantida como antes dessa calibração.
#'
#' @param consultores tibble produzido por `gerar_consultores()` (usa só
#'   `consultor_id`).
#' @param espaco_skills tibble produzido por `gerar_espaco_skills()`.
#' @param n_categorias_principais Quantas categorias de especialidade cada
#'   consultor tem.
#' @param cobertura_categoria_principal Proporção das skills da categoria
#'   principal que o consultor conhece.
#' @param cobertura_fora_categoria Proporção das skills fora da categoria
#'   principal que o consultor conhece (skill ocasional fora da especialidade).
#' @param recencia_maxima_dias Teto, em dias, para a recência da skill.
#' @param cobertura_populacional Proporção de consultores que respondeu
#'   alguma planilha de skills (ver detalhe acima); o resto não aparece na
#'   tabela retornada.
#' @return tibble longo: `consultor_id`, `skill_id`, `proficiencia`,
#'   `recencia_dias`. Um consultor sem nenhuma skill sorteada (cold start do
#'   lado do conteúdo) simplesmente não aparece nesta tabela.
gerar_skills_consultor <- function(consultores,
                                    espaco_skills,
                                    n_categorias_principais = 2,
                                    cobertura_categoria_principal = 0.6,
                                    cobertura_fora_categoria = 0.05,
                                    recencia_maxima_dias = 730,
                                    cobertura_populacional = 0.484) {
  escala_proficiencia <- c("Iniciante", "Básico", "Médio", "Avançado")
  categorias <- unique(espaco_skills$categoria)

  respondeu <- stats::runif(nrow(consultores)) < cobertura_populacional
  consultores_respondentes <- consultores$consultor_id[respondeu]

  purrr::map_dfr(consultores_respondentes, function(id_consultor) {
    categorias_principais <- sample(categorias, size = n_categorias_principais)
    dentro_da_especialidade <- espaco_skills$categoria %in% categorias_principais

    probabilidade_conhecer <- ifelse(
      dentro_da_especialidade,
      cobertura_categoria_principal,
      cobertura_fora_categoria
    )
    conhece <- stats::runif(nrow(espaco_skills)) < probabilidade_conhecer

    skills_conhecidas <- espaco_skills$skill_id[conhece]
    n_skills <- length(skills_conhecidas)

    tibble::tibble(
      consultor_id = id_consultor,
      skill_id = skills_conhecidas,
      proficiencia = sample(escala_proficiencia, n_skills, replace = TRUE),
      recencia_dias = sample.int(recencia_maxima_dias, n_skills, replace = TRUE)
    )
  })
}
