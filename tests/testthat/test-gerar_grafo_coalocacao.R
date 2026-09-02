test_that("cria uma aresta por dupla de consultores no mesmo projeto, sem repetir ordem trocada", {
  alocacoes <- tibble::tibble(
    projeto_id = c("p1", "p1", "p1", "p2", "p2", "p3"),
    consultor_id = c("c1", "c2", "c3", "c2", "c1", "c4")
  )

  grafo <- gerar_grafo_coalocacao(alocacoes)

  # p1 tem 3 pessoas -> 3 pares (c1-c2, c1-c3, c2-c3); p2 repete c1-c2;
  # p3 tem só 1 pessoa, não gera aresta
  expect_equal(nrow(grafo), 3)
  expect_true(all(grafo$consultor_a < grafo$consultor_b))
  expect_equal(grafo$n_projetos_em_comum[grafo$consultor_a == "c1" & grafo$consultor_b == "c2"], 2)
})

test_that("consultor sozinho num projeto não aparece em nenhuma aresta", {
  alocacoes <- tibble::tibble(
    projeto_id = c("p1", "p2", "p2"),
    consultor_id = c("c1", "c2", "c3")
  )

  grafo <- gerar_grafo_coalocacao(alocacoes)

  expect_false("c1" %in% c(grafo$consultor_a, grafo$consultor_b))
})

test_that("funciona sobre uma alocação sintética real, sem erro", {
  consultores <- gerar_consultores(n_consultores = 100)
  projetos <- gerar_projetos(n_projetos = 100, data_referencia = as.Date("2026-01-01"))
  experiencia <- gerar_experiencia_setorial(consultores)
  alocacoes <- gerar_alocacoes(consultores, projetos, experiencia, semente = 1)

  grafo <- gerar_grafo_coalocacao(alocacoes)

  expect_true(all(grafo$consultor_a %in% consultores$consultor_id))
  expect_true(all(grafo$consultor_b %in% consultores$consultor_id))
  expect_true(all(grafo$n_projetos_em_comum >= 1))
})
