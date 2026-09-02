test_that("valor_contrato tem massa em zero e cauda positiva pesada", {
  set.seed(42)
  n <- 20000
  valores <- gerar_valor_contrato(n)

  expect_length(valores, n)
  expect_true(all(valores >= 0))

  proporcao_zero <- mean(valores == 0)
  expect_equal(proporcao_zero, 0.147, tolerance = 0.02)

  positivos <- valores[valores > 0]
  expect_equal(median(positivos), 108300, tolerance = 0.25)
  expect_true(max(positivos) > median(positivos) * 20)
})

test_that("prob_zero = 0 nunca produz contrato zerado", {
  valores <- gerar_valor_contrato(1000, prob_zero = 0)
  expect_true(all(valores > 0))
})
