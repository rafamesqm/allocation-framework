test_that("receita, custo e margem ficam com o formato esperado", {
  projetos <- gerar_projetos(n_projetos = 500, data_referencia = as.Date("2026-01-01"))

  margem <- gerar_margem_frente_mes(projetos)

  expect_named(margem, c("projeto_id", "ano_mes", "receita", "custo", "margem"))
  expect_true(all(margem$receita >= 0))
  expect_true(all(margem$custo >= 0))
  expect_equal(margem$margem, margem$receita - margem$custo)
})

test_that("a margem é joinável contra os projetos, uma linha por projeto-mês", {
  projetos <- gerar_projetos(n_projetos = 500, data_referencia = as.Date("2026-01-01"))

  margem <- gerar_margem_frente_mes(projetos)

  expect_true(all(margem$projeto_id %in% projetos$projeto_id))
  expect_setequal(unique(margem$projeto_id), projetos$projeto_id)
  expect_false(any(duplicated(paste(margem$projeto_id, margem$ano_mes))))
  expect_true(nrow(margem) >= nrow(projetos))
})

test_that("o mês da margem cai dentro da janela do projeto", {
  projetos <- gerar_projetos(n_projetos = 300, data_referencia = as.Date("2026-01-01"))

  margem <- gerar_margem_frente_mes(projetos)
  do_projeto <- match(margem$projeto_id, projetos$projeto_id)

  expect_true(all(margem$ano_mes >= format(projetos$data_inicio[do_projeto], "%Y-%m")))
  expect_true(all(margem$ano_mes <= format(projetos$data_fim[do_projeto], "%Y-%m")))
})

test_that("proporção de zero de receita e custo aproxima a calibração real", {
  projetos <- gerar_projetos(n_projetos = 8000, data_referencia = as.Date("2026-01-01"))

  margem <- gerar_margem_frente_mes(projetos, semente = 2)

  expect_equal(mean(margem$receita == 0), 0.159, tolerance = 0.02)
  expect_equal(mean(margem$custo == 0), 0.447, tolerance = 0.02)
})

test_that("margem pode ser negativa (custo maior que receita)", {
  projetos <- gerar_projetos(n_projetos = 2000, data_referencia = as.Date("2026-01-01"))

  margem <- gerar_margem_frente_mes(projetos, semente = 3)

  expect_true(any(margem$margem < 0))
})
