alocacoes_do_sinal <- function() {
  tibble::tibble(
    consultor_id = c("c1", "c1", "c2"),
    projeto_id = c("p1", "p1", "p2"),
    horas_mes = c(120, 60, 0),
    duracao_dias = c(365.25 / 12, 365.25 / 12, 10)
  )
}

test_that("com alfa zero o sinal e o binario puro", {
  sinal <- construir_sinal_implicito(alocacoes_do_sinal(), alfa = 0)

  expect_equal(nrow(sinal), 2)
  expect_true(all(sinal$preferencia == 1))
  expect_true(all(sinal$confianca == 1))
})

test_that("com alfa um a confianca e 1 mais o log das horas do par", {
  sinal <- construir_sinal_implicito(alocacoes_do_sinal(), alfa = 1)

  # O par c1 e p1 soma as duas alocacoes, um mes cheio cada.
  c1 <- sinal[sinal$consultor_id == "c1", ]
  expect_equal(c1$horas, 180)
  expect_equal(c1$confianca, 1 + log1p(180))
  # Par sem horas fica com a confianca minima, e nao com zero.
  expect_equal(sinal$confianca[sinal$consultor_id == "c2"], 1)
})

test_that("a confianca cresce com as horas e nunca fica abaixo de 1", {
  sinal <- construir_sinal_implicito(alocacoes_do_sinal(), alfa = 1)

  expect_true(all(sinal$confianca >= 1))
  expect_gt(
    sinal$confianca[sinal$consultor_id == "c1"],
    sinal$confianca[sinal$consultor_id == "c2"]
  )
})

test_that("alfa negativo e recusado", {
  expect_error(construir_sinal_implicito(alocacoes_do_sinal(), alfa = -1), "não negativo")
})
