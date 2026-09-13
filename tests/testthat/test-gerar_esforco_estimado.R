test_that("esforco_estimado é não negativo e aproxima média/desvio reais", {
  set.seed(1)
  n <- 5000
  esforco <- gerar_esforco_estimado(n)

  expect_length(esforco, n)
  expect_true(all(esforco >= 0))
  expect_equal(mean(esforco), 120, tolerance = 0.1)
  expect_equal(sd(esforco), 66.90, tolerance = 0.15)
})
