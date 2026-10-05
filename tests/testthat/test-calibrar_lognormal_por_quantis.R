test_that("a mediana da log-normal calibrada bate com a mediana alvo", {
  parametros <- calibrar_lognormal_por_quantis(mediana = 18, p25 = 4, p75 = 60)

  expect_equal(exp(parametros$meanlog), 18)
  expect_true(parametros$sdlog > 0)
})

test_that("amostrar com os parâmetros calibrados reproduz os quantis alvo", {
  set.seed(1)
  parametros <- calibrar_lognormal_por_quantis(mediana = 18, p25 = 4, p75 = 60)
  amostra <- stats::rlnorm(2e5, meanlog = parametros$meanlog, sdlog = parametros$sdlog)

  # P25 e P75 saem aproximados de propósito: uma log-normal tem dois
  # parâmetros e não encaixa exatamente nos três quantis ao mesmo tempo,
  # por isso `sdlog` é a média dos dois implícitos (ver roxygen).
  expect_equal(unname(stats::quantile(amostra, 0.50)), 18, tolerance = 0.05)
  expect_equal(unname(stats::quantile(amostra, 0.25)), 4, tolerance = 0.30)
  expect_equal(unname(stats::quantile(amostra, 0.75)), 60, tolerance = 0.30)
})

test_that("quartis mais afastados da mediana produzem sdlog maior", {
  estreita <- calibrar_lognormal_por_quantis(mediana = 18, p25 = 12, p75 = 27)
  larga <- calibrar_lognormal_por_quantis(mediana = 18, p25 = 4, p75 = 81)

  expect_true(larga$sdlog > estreita$sdlog)
})
