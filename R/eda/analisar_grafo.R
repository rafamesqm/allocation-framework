#' Monta o grafo de coalocação como objeto igraph, com os isolados
#'
#' A tabela de arestas só contém quem dividiu projeto com alguém. Montar o
#' grafo apenas a partir dela descartaria em silêncio todo consultor sem par,
#' e as medidas globais passariam a descrever uma subpopulação escolhida pelo
#' próprio fenômeno em estudo: densidade e grau médio sairiam inflados,
#' porque o denominador teria perdido justamente os vértices de grau zero.
#'
#' Mesmo princípio de `construir_matriz_r()`, que cobre o grid completo para
#' o cold start ficar visível.
#'
#' @param grafo tibble com `consultor_a`, `consultor_b` e
#'   `n_projetos_em_comum`.
#' @param consultores tibble com `consultor_id`, que define o conjunto de
#'   vértices.
#' @param atributos tibble opcional com `consultor_id` e colunas a anexar
#'   como atributo de vértice.
#' @return Objeto `igraph` não direcionado e ponderado.
construir_grafo_coalocacao <- function(grafo, consultores, atributos = NULL) {
  vertices <- tibble::tibble(name = consultores$consultor_id)

  if (!is.null(atributos)) {
    vertices <- dplyr::left_join(
      vertices, dplyr::rename(atributos, name = consultor_id), by = "name"
    )
  }

  arestas <- grafo |>
    dplyr::transmute(from = consultor_a, to = consultor_b, weight = n_projetos_em_comum)

  igraph::graph_from_data_frame(arestas, directed = FALSE, vertices = vertices)
}

#' Resume a estrutura global de um grafo de coalocação
#'
#' `assortatividade_nivel` mede se pessoas de senioridade parecida tendem a
#' trabalhar juntas. Positivo indica semelhança, negativo indica
#' complementaridade, e zero indica indiferença. Num time de consultoria a
#' expectativa de negócio seria negativa, já que o time mistura senioridades,
#' mas o gerador não modela nada disso: a cablagem casa por setor, não por
#' nível.
#'
#' `agrupamento` é a transitividade global. **Ela é alta por construção em
#' qualquer projeção de grafo bipartido**, porque cada projeto transforma seu
#' time inteiro numa clique, e toda clique tem transitividade 1. Lida contra
#' um grafo aleatório de mesma densidade, ela "descobriria" uma necessidade
#' matemática. Por isso só faz sentido compará-la a um nulo que também seja
#' uma projeção bipartida com os mesmos graus (ver
#' `simular_nulo_coalocacao()`).
#'
#' @param g Objeto `igraph`.
#' @param atributo_nivel Nome do atributo numérico de vértice para a
#'   assortatividade, ou `NULL`.
#' @return tibble de uma linha com as medidas globais.
resumir_grafo <- function(g, atributo_nivel = NULL) {
  graus <- igraph::degree(g)
  componentes <- igraph::components(g)

  assortatividade <- if (is.null(atributo_nivel)) {
    NA_real_
  } else {
    igraph::assortativity(g, values = igraph::vertex_attr(g, atributo_nivel))
  }

  # Derivados resolvidos antes da tibble, pela convenção do projeto: dentro
  # de `tibble()` cada coluna sombreia a variável local de mesmo nome.
  n_componentes <- componentes$no
  tamanho_maior_componente <- max(componentes$csize)

  tibble::tibble(
    vertices = igraph::vcount(g),
    arestas = igraph::ecount(g),
    isolados = sum(graus == 0),
    densidade = igraph::edge_density(g),
    grau_medio = mean(graus),
    grau_mediano = stats::median(graus),
    grau_maximo = max(graus),
    componentes = n_componentes,
    maior_componente = tamanho_maior_componente,
    agrupamento = igraph::transitivity(g, type = "global"),
    assortatividade_nivel = assortatividade
  )
}

#' Simula a distribuição nula das medidas do grafo de coalocação
#'
#' O nulo certo aqui não é um grafo aleatório qualquer. É uma projeção
#' bipartida com **as mesmas sequências de grau** dos dois lados, cablada sem
#' o viés assortativo por setor. Só assim a comparação isola o que o viés
#' plantado acrescenta, em vez de redescobrir que projeção de bipartido gera
#' cliques.
#'
#' Reaproveita o próprio mecanismo do gerador
#' (`gerar_pares_configuration_model()` com `prob_vies = 0`), o que garante
#' que o nulo e o observado diferem apenas no que se quer testar.
#'
#' @param alocacoes tibble de alocações observadas.
#' @param consultores,projetos tibbles que definem os dois conjuntos.
#' @param atributos tibble opcional de atributos de vértice.
#' @param n_replicas Número de réplicas.
#' @param semente Inteiro para reprodutibilidade.
#' @return tibble com uma linha por réplica, mesmas colunas de
#'   `resumir_grafo()`.
simular_nulo_coalocacao <- function(alocacoes, consultores, projetos,
                                     atributos = NULL,
                                     n_replicas = 20, semente = 1) {
  set.seed(semente)

  grau_consultor <- dplyr::count(alocacoes, consultor_id)
  grau_projeto <- dplyr::count(alocacoes, projeto_id)

  graus_c <- grau_consultor$n[match(consultores$consultor_id, grau_consultor$consultor_id)]
  graus_c[is.na(graus_c)] <- 0
  graus_p <- grau_projeto$n[match(projetos$projeto_id, grau_projeto$projeto_id)]
  graus_p[is.na(graus_p)] <- 0

  sem_vies <- rep("indiferente", length(graus_c))

  purrr::map(seq_len(n_replicas), function(replica) {
    arestas <- gerar_pares_configuration_model(
      id_consultor = consultores$consultor_id, grau_consultor = graus_c,
      id_projeto = projetos$projeto_id, grau_projeto = graus_p,
      grupo_consultor = sem_vies,
      grupo_projeto = rep("outro", length(graus_p)),
      prob_vies = 0
    )

    grafo_nulo <- gerar_grafo_coalocacao(arestas)
    g <- construir_grafo_coalocacao(grafo_nulo, consultores, atributos)

    dplyr::mutate(
      resumir_grafo(g, if (is.null(atributos)) NULL else "nivel_num"),
      replica = replica,
      .before = 1
    )
  }) |>
    purrr::list_rbind()
}

#' Compara as medidas observadas com a distribuição nula
#'
#' Devolve, para cada medida, o valor observado, a média e o desvio das
#' réplicas nulas, e o escore padronizado. O escore responde à única pergunta
#' que importa aqui: o que o viés de seleção plantado acrescenta sobre o que
#' já viria de graça da sequência de graus.
#'
#' Escore próximo de zero significa que a medida é inteiramente explicada
#' pela estrutura de graus, e reportá-la como característica do fenômeno
#' seria reportar uma necessidade do método de construção.
#'
#' @param observado tibble de uma linha, de `resumir_grafo()`.
#' @param nulo tibble de réplicas, de `simular_nulo_coalocacao()`.
#' @param medidas Nomes das colunas a comparar.
#' @return tibble com `medida`, `observado`, `nulo_media`, `nulo_desvio` e
#'   `escore_z`.
comparar_com_nulo <- function(observado, nulo,
                               medidas = c(
                                 "arestas", "densidade", "agrupamento",
                                 "assortatividade_nivel", "maior_componente"
                               )) {
  purrr::map(medidas, function(nome_medida) {
    valor <- observado[[nome_medida]]
    replicas <- nulo[[nome_medida]]
    media_nula <- mean(replicas)
    desvio_nulo <- stats::sd(replicas)

    tibble::tibble(
      medida = nome_medida,
      observado = valor,
      nulo_media = media_nula,
      nulo_desvio = desvio_nulo,
      escore_z = if (desvio_nulo == 0) NA_real_ else (valor - media_nula) / desvio_nulo
    )
  }) |>
    purrr::list_rbind()
}

#' Distribuição do tamanho de time por projeto
#'
#' O tamanho de time é o grau do lado do projeto na matriz de alocação, e é
#' ele que determina quantas arestas cada projeto injeta na projeção: um time
#' de `k` pessoas produz `k(k-1)/2` pares. Times grandes dominam a contagem
#' de arestas de forma quadrática, o que explica por que poucos projetos
#' podem responder por boa parte do grafo.
#'
#' @param alocacoes tibble de alocações.
#' @return tibble com `projeto_id`, `tamanho_time` e `pares_gerados`.
distribuir_tamanho_time <- function(alocacoes) {
  alocacoes |>
    dplyr::count(projeto_id, name = "tamanho_time") |>
    dplyr::mutate(pares_gerados = tamanho_time * (tamanho_time - 1) / 2) |>
    dplyr::arrange(dplyr::desc(tamanho_time))
}
