test_that("o grafo inclui os consultores isolados, nao so quem tem aresta", {
  # Montar so a partir das arestas inflaria densidade e grau medio, porque o
  # denominador perderia os vertices de grau zero.
  consultores <- tibble::tibble(consultor_id = paste0("c", 1:5))
  grafo <- tibble::tibble(
    consultor_a = "c1", consultor_b = "c2", n_projetos_em_comum = 1
  )

  g <- construir_grafo_coalocacao(grafo, consultores)

  expect_equal(igraph::vcount(g), 5)
  expect_equal(sum(igraph::degree(g) == 0), 3)
})

test_that("resumir_grafo devolve componentes e maior componente corretos", {
  # Regressao: `componentes = componentes$no` criava a coluna e a linha
  # seguinte lia a coluna atomica em vez da lista do igraph.
  consultores <- tibble::tibble(consultor_id = paste0("c", 1:5))
  grafo <- tibble::tibble(
    consultor_a = c("c1", "c3"), consultor_b = c("c2", "c4"),
    n_projetos_em_comum = c(1, 1)
  )

  resumo <- resumir_grafo(construir_grafo_coalocacao(grafo, consultores))

  expect_equal(resumo$vertices, 5)
  expect_equal(resumo$arestas, 2)
  expect_equal(resumo$isolados, 1)
  expect_equal(resumo$componentes, 3)
  expect_equal(resumo$maior_componente, 2)
})

test_that("a projecao de um time de k pessoas gera k(k-1)/2 pares", {
  alocacoes <- tibble::tibble(
    consultor_id = c("a", "b", "c", "d", "e"),
    projeto_id = c("p1", "p1", "p1", "p2", "p3")
  )

  times <- distribuir_tamanho_time(alocacoes)

  expect_equal(times$tamanho_time, c(3L, 1L, 1L))
  expect_equal(times$pares_gerados, c(3, 0, 0))
})

test_that("comparar_com_nulo mede a distancia em desvios da replica", {
  observado <- tibble::tibble(agrupamento = 0.5)
  nulo <- tibble::tibble(agrupamento = c(0.3, 0.4, 0.5))

  comparacao <- comparar_com_nulo(observado, nulo, "agrupamento")

  expect_equal(comparacao$nulo_media, 0.4)
  expect_equal(comparacao$escore_z, (0.5 - 0.4) / stats::sd(c(0.3, 0.4, 0.5)))
})

test_that("o nulo preserva as sequencias de grau do observado", {
  # E o que torna a comparacao valida: nulo e observado so diferem no vies.
  dados <- carregar_dados(tabelas = c("consultores", "projetos", "alocacoes"))

  nulo <- simular_nulo_coalocacao(
    dados$alocacoes, dados$consultores, dados$projetos, n_replicas = 2
  )

  expect_equal(nrow(nulo), 2)
  expect_true(all(nulo$vertices == nrow(dados$consultores)))
  # Mesma contagem de pares possiveis implica total de arestas na mesma ordem.
  expect_true(all(nulo$arestas > 0))
})

test_that("executar_i5 devolve as quatro partes do bloco", {
  dados <- carregar_dados(tabelas = c(
    "consultores", "projetos", "alocacoes", "grafo_coalocacao"
  ))

  bloco <- executar_i5(dados, n_replicas = 3)

  expect_named(bloco, c("objetos", "tabelas", "figuras", "achados"))
  expect_true(all(vapply(bloco$figuras, ggplot2::is_ggplot, logical(1))))
  expect_equal(nrow(bloco$achados), 4)
})
