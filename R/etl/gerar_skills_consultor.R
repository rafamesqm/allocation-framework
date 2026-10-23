#' Gera a matriz esparsa consultor × skill (parte de `C`: `skills_hard[]` e
#' `recencia_skill[]`)
#'
#' Cada consultor recebe 1 a 2 categorias principais (ver `gerar_espaco_skills`
#' para a taxonomia) e conhece a maior parte das skills dessas categorias,
#' mais um punhado ocasional fora delas. Isso emula a cobertura parcial e
#' concentrada por especialidade observada na prática (alguém de backend não
#' costuma saber RPA), não uma amostra uniforme do catálogo inteiro.
#'
#' Escala de proficiência usa os rótulos do formulário geral de skills
#' (Iniciante/Básico/Médio/Avançado). As planilhas reais têm duas escalas
#' diferentes que ainda precisam de de-para (ver docs/mapeamento_schema.md);
#' até isso ser resolvido, o sintético usa só uma, documentado aqui.
#'
#' `recencia_skill` é a quantidade de dias desde a última atualização
#' registrada da skill. É constante dentro de cada consultor e agrupada em
#' poucas ondas, refletindo que a fonte real são três formulários aplicados em
#' campanha, e não um fluxo contínuo. Ver o comentário no corpo da função para
#' o que nisso é estrutura reproduzida e o que é suposição declarada.
#' Sem calibração real de decaimento, é suposição documentada.
#'
#' Cobertura populacional calibrada pela Sexta rodada
#' (docs/mapeamento_schema.md): só 48,4% dos consultores ativos (138 de 285)
#' têm ao menos uma resposta nas planilhas de skills, medido por interseção
#' de hashes de e-mail. Um consultor não respondente não aparece nesta
#' tabela (cold start do lado do conteúdo), em vez de todo consultor ter
#' cobertura garantida como antes dessa calibração.
#'
#' O conjunto de respondentes chega por argumento, e não é sorteado aqui,
#' por ser compartilhado com os demais sinais de formulário (ver
#' `sortear_respondentes_formulario()`). Sortear dentro de cada gerador
#' faria as ausências parecerem independentes entre si, quando na fonte real
#' elas são a mesma pessoa não tendo respondido a mesma campanha.
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
#' @param respondentes Vetor de `consultor_id` que respondeu alguma planilha
#'   de skills, de `sortear_respondentes_formulario()`; o resto não aparece
#'   na tabela retornada.
#' @return tibble longo: `consultor_id`, `skill_id`, `proficiencia`,
#'   `recencia_dias`. Um consultor sem nenhuma skill sorteada (cold start do
#'   lado do conteúdo) simplesmente não aparece nesta tabela.
gerar_skills_consultor <- function(consultores,
                                    espaco_skills,
                                    n_categorias_principais = 2,
                                    cobertura_categoria_principal = 0.6,
                                    cobertura_fora_categoria = 0.05,
                                    respondentes = NULL,
                                    recencia_maxima_dias = 730,
                                    ondas_recencia_dias = c(110, 365, 620),
                                    dispersao_onda_dias = 15) {
  escala_proficiencia <- c("Iniciante", "Básico", "Médio", "Avançado")
  categorias <- unique(espaco_skills$categoria)

  if (is.null(respondentes)) {
    respondentes <- sortear_respondentes_formulario(consultores)
  }
  consultores_respondentes <- intersect(consultores$consultor_id, respondentes)

  # Recência é sorteada por consultor, não por skill, e em torno de poucas
  # ondas. As duas coisas são estruturais, não cosméticas.
  #
  # Por consultor: na fonte real, as skills de uma pessoa vêm de uma única
  # resposta de formulário, então todas compartilham a mesma data de
  # atualização. A versão anterior sorteava recência independente por skill,
  # o que tornava a recência de uma pessoa internamente inconsistente.
  #
  # Em ondas: são três formulários distintos, aplicados em campanhas, e não um
  # fluxo contínuo de atualização. A versão anterior produzia recência
  # uniforme entre 1 e 730 dias, e o bloco I.4 registrou que isso é o oposto
  # do que a fonte real teria. Um peso de decaimento calibrado sobre recência
  # uniforme mediria uma suavidade que não existe.
  #
  # As datas das ondas são suposição declarada, não calibração: as planilhas
  # saíram da máquina antes de qualquer data ser agregada. O que se reproduz
  # aqui é a forma, isto é, poucos grupos apertados em vez de um contínuo, e é
  # só isso que a monografia pode afirmar.
  onda_do_consultor <- sample(
    seq_along(ondas_recencia_dias), size = length(consultores_respondentes),
    replace = TRUE
  )
  recencia_do_consultor <- pmin(
    pmax(
      round(
        ondas_recencia_dias[onda_do_consultor] +
          stats::rnorm(length(onda_do_consultor), mean = 0, sd = dispersao_onda_dias)
      ),
      1
    ),
    recencia_maxima_dias
  )
  names(recencia_do_consultor) <- consultores_respondentes

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
      recencia_dias = rep(recencia_do_consultor[[id_consultor]], n_skills)
    )
  })
}
