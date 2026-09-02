test_that("gerar_grau_binomial_negativa aproxima média e variância alvo", {
  set.seed(1)
  n <- 20000
  grau <- gerar_grau_binomial_negativa(n, media = 9.00, variancia = 44.6)

  expect_length(grau, n)
  expect_true(all(grau >= 0))
  expect_true(all(grau == round(grau)))
  expect_equal(mean(grau), 9.00, tolerance = 0.05)
  expect_equal(var(grau), 44.6, tolerance = 0.1)
})

test_that("recusa variância menor ou igual à média", {
  expect_error(gerar_grau_binomial_negativa(10, media = 5, variancia = 5))
  expect_error(gerar_grau_binomial_negativa(10, media = 5, variancia = 3))
})
