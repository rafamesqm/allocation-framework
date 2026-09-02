test_that("receita, custo e margem ficam com o formato esperado", {
  set.seed(1)
  margem <- gerar_margem_frente_mes(20000)

  expect_named(margem, c("receita", "custo", "margem"))
  expect_true(all(margem$receita >= 0))
  expect_true(all(margem$custo >= 0))
  expect_equal(margem$margem, margem$receita - margem$custo)
})

test_that("proporção de zero de receita e custo aproxima a calibração real", {
  margem <- gerar_margem_frente_mes(20000, semente = 2)

  expect_equal(mean(margem$receita == 0), 0.159, tolerance = 0.02)
  expect_equal(mean(margem$custo == 0), 0.447, tolerance = 0.02)
})

test_that("margem pode ser negativa (custo maior que receita)", {
  margem <- gerar_margem_frente_mes(20000, semente = 3)
  expect_true(any(margem$margem < 0))
})
