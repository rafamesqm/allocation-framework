#' Cabla consultores e projetos em pares via configuration model bipartido,
#' com viés de seleção assortativo plantado
#'
#' Implementa o mecanismo de stub-matching (Newman, 2010; van der Hofstad,
#' 2016, ver `remover_duplicatas_por_double_edge_swap()`): repete cada nó
#' tantas vezes quanto seu grau sorteado, embaralha um dos lados e casa
#' posição a posição. Isso preserva exatamente a sequência de graus
#' sorteada.
#'
#' O viés de seleção plantado (achado de pesquisa a caracterizar na EDA, ver
#' docs/dicionario_de_dados.md, "Nota sobre viés de seleção") é implementado
#' como mistura assortativa: com probabilidade `prob_vies`, o coto de um
#' consultor é casado com um coto de projeto do mesmo grupo de atributo
#' (aqui, setor); caso contrário, ou se não sobrar coto do mesmo grupo, casa
#' com um coto qualquer ainda disponível.
#'
#' Como as somas dos graus dos dois lados quase nunca batem exatamente
#' (grau sorteado independentemente em cada lado), o lado com menos cotos
#' recebe cotos extras antes da cablagem, sorteados por nó com peso
#' proporcional ao grau já sorteado (preservando o formato da distribuição,
#' e nunca dando aresta extra a quem tem grau zero, o que quebraria o cold
#' start): é a técnica padrão de "redistribuir o resíduo" para fechar a
#' cablagem quando os dois lados do configuration model não somam igual.
#'
#' A cablagem em si é um laço com custo O(n²) no pior caso (a cada aresta,
#' filtra os cotos de projeto ainda disponíveis). Aceitável na escala de
#' milhares de arestas deste projeto; não otimizado com estrutura de dados
#' mais sofisticada, corretude e clareza antes de velocidade.
#'
#' @param id_consultor,grau_consultor Vetores paralelos: id e grau sorteado
#'   de cada consultor (grau zero = cold start).
#' @param id_projeto,grau_projeto Vetores paralelos: id e grau sorteado de
#'   cada projeto (grau zero = cold start).
#' @param grupo_consultor,grupo_projeto Vetores paralelos aos ids acima,
#'   grupo de atributo usado para o viés assortativo (aqui, setor).
#' @param prob_vies Probabilidade de a cablagem preferir o mesmo grupo.
#' @return tibble de arestas `consultor_id`/`projeto_id`, uma linha por
#'   aresta, sem pares repetidos (após o reparo por double edge swap).
gerar_pares_configuration_model <- function(id_consultor, grau_consultor,
                                             id_projeto, grau_projeto,
                                             grupo_consultor, grupo_projeto,
                                             prob_vies = 0.7) {
  cotos_consultor <- rep(id_consultor, grau_consultor)
  grupo_dos_cotos_consultor <- rep(grupo_consultor, grau_consultor)

  cotos_projeto <- rep(id_projeto, grau_projeto)
  grupo_dos_cotos_projeto <- rep(grupo_projeto, grau_projeto)

  diferenca <- length(cotos_consultor) - length(cotos_projeto)
  if (diferenca > 0) {
    extra <- sample(seq_along(id_projeto), size = diferenca, replace = TRUE, prob = grau_projeto)
    cotos_projeto <- c(cotos_projeto, id_projeto[extra])
    grupo_dos_cotos_projeto <- c(grupo_dos_cotos_projeto, grupo_projeto[extra])
  } else if (diferenca < 0) {
    extra <- sample(seq_along(id_consultor), size = -diferenca, replace = TRUE, prob = grau_consultor)
    cotos_consultor <- c(cotos_consultor, id_consultor[extra])
    grupo_dos_cotos_consultor <- c(grupo_dos_cotos_consultor, grupo_consultor[extra])
  }

  n_arestas <- length(cotos_consultor)
  ordem <- sample.int(n_arestas)
  cotos_consultor <- cotos_consultor[ordem]
  grupo_dos_cotos_consultor <- grupo_dos_cotos_consultor[ordem]

  projeto_pareado <- rep(NA_character_, n_arestas)
  disponivel <- rep(TRUE, n_arestas)

  for (k in seq_len(n_arestas)) {
    candidatos <- which(disponivel)
    mesmo_grupo <- candidatos[grupo_dos_cotos_projeto[candidatos] == grupo_dos_cotos_consultor[k]]

    usar_mesmo_grupo <- length(mesmo_grupo) > 0 && stats::runif(1) < prob_vies
    pool_escolha <- if (usar_mesmo_grupo) mesmo_grupo else candidatos
    escolhido <- if (length(pool_escolha) == 1) pool_escolha else sample(pool_escolha, size = 1)

    projeto_pareado[k] <- cotos_projeto[escolhido]
    disponivel[escolhido] <- FALSE
  }

  arestas <- tibble::tibble(consultor_id = cotos_consultor, projeto_id = projeto_pareado)
  remover_duplicatas_por_double_edge_swap(arestas)
}
