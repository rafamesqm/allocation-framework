test_that("tem massa em zero e cauda positiva pesada, calibrada por quantis", {
  set.seed(42)
  n <- 20000
  valores <- gerar_variavel_hurdle_lognormal(
    n,
    prob_zero = 0.147, mediana_positivos = 108300,
    p25_positivos = 34814, p75_positivos = 375054
  )

  expect_length(valores, n)
  expect_true(all(valores >= 0))

  proporcao_zero <- mean(valores == 0)
  expect_equal(proporcao_zero, 0.147, tolerance = 0.02)

  positivos <- valores[valores > 0]
  expect_equal(median(positivos), 108300, tolerance = 0.25)
  expect_true(max(positivos) > median(positivos) * 20)
})

test_that("prob_zero = 0 nunca produz valor zerado", {
  valores <- gerar_variavel_hurdle_lognormal(
    1000,
    prob_zero = 0, mediana_positivos = 108300,
    p25_positivos = 34814, p75_positivos = 375054
  )
  expect_true(all(valores > 0))
})

test_that("serve para calibrações diferentes (ex.: receita por frente-mês)", {
  set.seed(1)
  n <- 20000
  valores <- gerar_variavel_hurdle_lognormal(
    n,
    prob_zero = 0.159, mediana_positivos = 136901.46,
    p25_positivos = 46105.5, p75_positivos = 268683.84
  )

  proporcao_zero <- mean(valores == 0)
  expect_equal(proporcao_zero, 0.159, tolerance = 0.02)
  expect_equal(median(valores[valores > 0]), 136901.46, tolerance = 0.25)
})
