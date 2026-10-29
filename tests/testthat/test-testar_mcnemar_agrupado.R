test_that("com um par por consultor, o agrupado coincide com o McNemar assintótico", {
  modelo <- c(1, 1, 1, 0, 0, 1, 0)
  piso <- c(0, 0, 0, 1, 0, 1, 0)
  agrupado <- testar_mcnemar_agrupado(modelo, piso, seq_along(modelo))
  # b = 3, c = 1: (b - c)^2 / (b + c) = 1.
  expect_equal(agrupado$estatistica, 1)
  expect_equal(agrupado$valor_p, stats::pchisq(1, 1, lower.tail = FALSE))
})

test_that("pares do mesmo consultor somam antes de entrar na estatística", {
  modelo <- c(1, 1, 1, 0)
  piso <- c(0, 0, 0, 1)
  grupo <- c("a", "a", "a", "b")
  agrupado <- testar_mcnemar_agrupado(modelo, piso, grupo)
  # d_a = 3, d_b = -1: (3 - 1)^2 / (9 + 1) = 0,4.
  expect_equal(agrupado$estatistica, 0.4)
  expect_false(agrupado$a_favor)
})

test_that("sem discordância, o agrupado não rejeita", {
  agrupado <- testar_mcnemar_agrupado(c(1, 0), c(1, 0), c("a", "b"))
  expect_equal(agrupado$estatistica, 0)
  expect_equal(agrupado$valor_p, 1)
})

test_that("a direção da rejeição segue o sinal da soma", {
  n <- 40
  agrupado <- testar_mcnemar_agrupado(rep(0, n), rep(1, n), seq_len(n))
  expect_true(agrupado$contra)
  expect_false(agrupado$a_favor)
})

test_that("a estatística padronizada é (b - c) / raiz(b + c), zero sem discordantes", {
  expect_equal(padronizar_mcnemar(9, 1), 8 / sqrt(10))
  expect_equal(padronizar_mcnemar(0, 0), 0)
})

test_that("ponto concorda em número depois do arredondamento", {
  expect_equal(formatar_pontos(0.003), "0,3 ponto")
  expect_equal(formatar_pontos(0.0199), "2,0 pontos")
  expect_equal(formatar_pontos(-0.019), "-1,9 ponto")
  expect_equal(formatar_pontos(0.025, percentual = TRUE), "2,5 pontos percentuais")
  expect_equal(formatar_pontos(0.01, percentual = TRUE), "1,0 ponto percentual")
})
