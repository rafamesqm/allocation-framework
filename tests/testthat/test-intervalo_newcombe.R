test_that("o intervalo de Wilson cobre a proporção e fica em [0, 1]", {
  w <- calcular_intervalo_wilson(0, 20)
  expect_equal(w[1], 0, tolerance = 1e-12)
  expect_gt(w[2], 0)
  w <- calcular_intervalo_wilson(7, 20)
  expect_lt(w[1], 0.35)
  expect_gt(w[2], 0.35)
})

test_that("o intervalo de Newcombe contém a diferença e é simétrico quando b = c", {
  i <- calcular_intervalo_newcombe_pareado(40, 9, 2, 49)
  expect_equal(i$diferenca, 7 / 100)
  expect_lt(i$inferior, i$diferenca)
  expect_gt(i$superior, i$diferenca)

  simetrico <- calcular_intervalo_newcombe_pareado(30, 6, 6, 58)
  expect_equal(simetrico$diferenca, 0)
  expect_equal(simetrico$inferior, -simetrico$superior, tolerance = 1e-12)
})

test_that("sem correlação entre os acertos, vira o intervalo de soma de quadrados", {
  # a d = b c dá phi = 0, e os limites são a raiz da soma dos quadrados das
  # distâncias de Wilson, o método 10 de Newcombe para amostras independentes.
  i <- calcular_intervalo_newcombe_pareado(10, 10, 10, 10)
  w <- calcular_intervalo_wilson(20, 40)
  esperado <- sqrt((0.5 - w[1])^2 + (w[2] - 0.5)^2)
  expect_equal(i$inferior, -esperado, tolerance = 1e-12)
  expect_equal(i$superior, esperado, tolerance = 1e-12)
})

test_that("contagens grandes não estouram o inteiro", {
  i <- intervalo_da_diferenca_pareada(rep(c(1L, 0L), c(800L, 500L)), rep(c(1L, 0L), c(780L, 520L)))
  expect_false(anyNA(unlist(i)))
})

test_that("a cobertura simulada fica perto de 95%", {
  n <- 300
  verdadeira <- 0.6 - (0.8 * 0.6 + 0.2 * 0.55)
  cobre <- withr::with_seed(1, replicate(1000, {
    modelo <- stats::runif(n) < 0.6
    piso <- ifelse(stats::runif(n) < 0.8, modelo, stats::runif(n) < 0.55)
    i <- intervalo_da_diferenca_pareada(modelo, piso)
    i$inferior <= verdadeira && verdadeira <= i$superior
  }))
  expect_gt(mean(cobre), 0.93)
  expect_lt(mean(cobre), 0.97)
})
