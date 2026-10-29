test_that("horas iguais às que o percentual implica dão correlação 1", {
  alocacoes <- tibble::tibble(pct_alocacao = c(0.25, 0.5, 1), horas_mes = 168 * c(0.25, 0.5, 1))
  medida <- medir_discordancia_horas_percentual(alocacoes)

  expect_equal(medida$alocacoes, 3)
  expect_equal(medida$correlacao, 1)
  expect_equal(medida$acima_da_capacidade, 0)
})

test_that("a fração acima da capacidade conta só o que passa de 168 horas", {
  alocacoes <- tibble::tibble(pct_alocacao = c(0.5, 0.6, 0.7, 0.8), horas_mes = c(100, 168, 169, 200))
  expect_equal(medir_discordancia_horas_percentual(alocacoes)$acima_da_capacidade, 0.5)
})
