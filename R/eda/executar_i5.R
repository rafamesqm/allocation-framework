#' Executa o bloco I.5 da EDA, estrutura relacional
#'
#' Mesmo contrato de `executar_i1()`: calcula, formata, desenha e conclui num
#' só lugar, e o documento apenas narra.
#'
#' A decisão de método que sustenta o bloco é a escolha do modelo nulo. Toda
#' projeção de grafo bipartido tem agrupamento alto por construção, porque
#' cada projeto vira uma clique. Comparar o grafo observado contra um grafo
#' aleatório de mesma densidade produziria a conclusão de que a rede é
#' "fortemente agrupada", que é verdade e não é informação: é consequência
#' aritmética da projeção. O nulo usado aqui é outra projeção bipartida, com
#' as mesmas sequências de grau, cablada sem o viés assortativo por setor,
#' o que isola exatamente o que o viés acrescenta.
#'
#' @param dados Lista nomeada de tibbles, de `carregar_dados()`.
#' @param n_replicas Réplicas do nulo.
#' @param semente Inteiro para reprodutibilidade.
#' @return Lista com `objetos`, `tabelas`, `figuras` e `achados`.
executar_i5 <- function(dados, n_replicas = 20, semente = 1) {
  atributos <- dados$consultores |>
    dplyr::transmute(consultor_id, nivel_num = senioridade_numerica(nivel_senioridade))

  g <- construir_grafo_coalocacao(dados$grafo_coalocacao, dados$consultores, atributos)
  observado <- resumir_grafo(g, "nivel_num")

  nulo <- simular_nulo_coalocacao(
    dados$alocacoes, dados$consultores, dados$projetos, atributos,
    n_replicas = n_replicas, semente = semente
  )
  comparacao <- comparar_com_nulo(observado, nulo)

  graus <- tibble::tibble(
    consultor_id = igraph::V(g)$name,
    grau = as.integer(igraph::degree(g))
  )
  pesos <- dados$grafo_coalocacao |>
    dplyr::count(n_projetos_em_comum, name = "n_arestas")
  times <- distribuir_tamanho_time(dados$alocacoes)

  pares_possiveis <- sum(times$pares_gerados)
  repeticoes_observadas <- pares_possiveis - observado$arestas
  repeticoes_nulas <- pares_possiveis - mean(nulo$arestas)
  excesso_repeticao <- repeticoes_observadas / repeticoes_nulas - 1

  agrupamento <- comparacao[comparacao$medida == "agrupamento", ]
  assortatividade <- comparacao[comparacao$medida == "assortatividade_nivel", ]
  arestas <- comparacao[comparacao$medida == "arestas", ]

  objetos <- list(
    observado = observado,
    nulo = nulo,
    comparacao = comparacao,
    graus = graus,
    pesos = pesos,
    times = times,
    pares_possiveis = pares_possiveis,
    repeticoes_observadas = repeticoes_observadas,
    excesso_repeticao = excesso_repeticao,
    agrupamento = agrupamento,
    assortatividade = assortatividade,
    arestas = arestas,
    times_com_par = sum(times$tamanho_time >= 2),
    n_projetos_alocados = nrow(times)
  )

  tabelas <- list(
    i5_resumo_grafo = observado |>
      dplyr::transmute(
        Vértices = vertices, Arestas = formatar_inteiro(arestas),
        Isolados = isolados, Densidade = formatar_decimal(densidade, 4),
        `Grau médio` = formatar_decimal(grau_medio, 2),
        `Grau máximo` = grau_maximo, Componentes = componentes,
        `Maior componente` = maior_componente,
        Agrupamento = formatar_decimal(agrupamento, 4),
        `Assortatividade por nível` = formatar_decimal(assortatividade_nivel, 4)
      ),
    i5_comparacao_nulo = comparacao |>
      dplyr::transmute(
        Medida = medida,
        Observado = formatar_decimal(observado, 4),
        `Nulo (média)` = formatar_decimal(nulo_media, 4),
        `Nulo (desvio)` = formatar_decimal(nulo_desvio, 4),
        `Escore z` = formatar_decimal(escore_z, 2)
      ),
    i5_peso_arestas = pesos |>
      dplyr::transmute(
        `Projetos em comum` = n_projetos_em_comum,
        Arestas = formatar_inteiro(n_arestas),
        `Fração` = formatar_pct(n_arestas / sum(n_arestas), 2)
      )
  )

  figuras <- list(
    i5_distribuicao_grau = figura_distribuicoes(
      graus, "grau",
      titulo = "Distribuição do grau no grafo de coalocação",
      subtitulo = paste(
        "Inclui os", observado$isolados, "consultores isolados, que teriam",
        "sumido se o grafo fosse montado só a partir das arestas"
      )
    ),
    i5_tamanho_time = figura_distribuicoes(
      times, "tamanho_time",
      titulo = "Distribuição do tamanho de time por projeto",
      subtitulo = "Um time de k pessoas injeta k(k-1)/2 pares no grafo projetado"
    )
  )

  achados <- registrar_achados("I.5", com_sintese(tibble::tribble(
    ~codigo, ~etiqueta, ~enunciado, ~implicacao,

    "I.5.1", "instrumento",
    paste0(
      "O agrupamento do grafo é ", formatar_decimal(observado$agrupamento, 3),
      " contra ", formatar_decimal(agrupamento$nulo_media, 3),
      " de uma projeção bipartida com as mesmas sequências de grau e sem ",
      "viés nenhum. A diferença é de apenas ",
      formatar_pct(observado$agrupamento / agrupamento$nulo_media - 1),
      " em magnitude, ainda que detectável (escore z ",
      formatar_decimal(agrupamento$escore_z, 1),
      "), porque a variância do nulo é muito pequena."
    ),
    paste0(
      "Praticamente todo o agrupamento vem da projeção, não do mecanismo de ",
      "alocação: cada projeto vira uma clique, e clique tem transitividade 1 ",
      "por definição. Comparar contra um nulo de mesma densidade, que é o ",
      "reflexo usual, acusaria agrupamento muito acima do esperado e ",
      "produziria uma conclusão vazia sobre química de time."
    ),

    "I.5.2", "consequência do gerador",
    paste0(
      "O viés de seleção plantado não aparece no agrupamento, e sim na ",
      "repetição de parceria: o grafo tem ", formatar_inteiro(observado$arestas),
      " arestas distintas contra ", formatar_inteiro(round(arestas$nulo_media)),
      " do nulo (escore z ", formatar_decimal(arestas$escore_z, 1),
      "), o que equivale a ", formatar_pct(excesso_repeticao),
      " mais coalocações repetidas."
    ),
    paste0(
      "É esse o canal por onde o viés de setor se manifesta na Camada 4. Um ",
      "peso de aresta baseado em contagem de projetos em comum está medindo, ",
      "em boa parte, o viés de alocação, não afinidade de time."
    ),

    "I.5.3", "consequência do gerador",
    paste0(
      "Não há assortatividade por senioridade: ",
      formatar_decimal(observado$assortatividade_nivel, 3),
      " contra ", formatar_decimal(assortatividade$nulo_media, 3),
      " no nulo, escore z ", formatar_decimal(assortatividade$escore_z, 1), "."
    ),
    paste0(
      "O gerador cabla por setor e ignora nível, então a ausência é premissa ",
      "do código. No real, a composição de time por senioridade é regra de ",
      "negócio conhecida, e sua ausência aqui limita o que a wave 4 pode ",
      "validar sobre restrição de pirâmide."
    ),

    "I.5.4", "instrumento",
    paste0(
      "O grafo tem ", observado$isolados, " vértices isolados em ",
      observado$vertices, ", e ", observado$componentes,
      " componentes, com o maior cobrindo ", observado$maior_componente,
      " consultores."
    ),
    paste0(
      "Montar o grafo apenas a partir da tabela de arestas descartaria os ",
      "isolados e inflaria densidade e grau médio, porque o denominador ",
      "perderia justamente os vértices de grau zero. Formação de time por ",
      "afinidade de grafo não tem o que dizer sobre esses consultores."
    )
  ), tibble::tribble(
    ~codigo, ~decisao, ~limitacao, ~proveniencia,

    "I.5.1", NA,
    paste0(
      "O agrupamento do grafo vem quase todo da projeção bipartida, porque ",
      "cada projeto vira uma clique e clique tem transitividade 1 por ",
      "definição, então ele não sustenta conclusão sobre química de time."
    ),
    "estrutural",

    "I.5.2", NA,
    paste0(
      "Peso de aresta baseado em contagem de projetos em comum mede em boa ",
      "parte o viés de alocação e não afinidade de time; a magnitude do ",
      "excesso de parceria repetida é do gerador, ainda que a cautela seja ",
      "geral."
    ),
    "depende do gerador",

    "I.5.3", NA,
    paste0(
      "O gerador cabla por setor e ignora nível, então a ausência de ",
      "assortatividade por senioridade é premissa do código e limita o que a ",
      "wave 4 pode validar sobre restrição de pirâmide."
    ),
    "depende do gerador",

    "I.5.4", NA,
    paste0(
      "Formação de time por afinidade de grafo não tem o que dizer sobre os ",
      "consultores isolados, e o fenômeno é real: 48 dos 285 ativos não ",
      "aparecem em nenhuma linha de alocação no dado da organização."
    ),
    "fato real"
  )))

  list(objetos = objetos, tabelas = tabelas, figuras = figuras, achados = achados)
}
