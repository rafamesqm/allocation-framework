test_that("reproduz a correlação alvo, positiva e negativa", {
  set.seed(1)
  niveis_ordenados <- c("N4", "N3", "N2", "N1", "N0")
  nivel <- factor(sample(niveis_ordenados, 5000, replace = TRUE), levels = niveis_ordenados, ordered = TRUE)
  nivel_numerico <- as.numeric(nivel)

  positiva <- gerar_variavel_por_nivel(nivel, correlacao_alvo = 0.52, media = 0.7, desvio_variavel = 0.2)
  negativa <- gerar_variavel_por_nivel(nivel, correlacao_alvo = -0.55, media = 120, desvio_variavel = 66.90)

  expect_equal(cor(nivel_numerico, positiva), 0.52, tolerance = 0.05)
  expect_equal(cor(nivel_numerico, negativa), -0.55, tolerance = 0.05)
})

test_that("respeita os limites mínimo e máximo", {
  set.seed(2)
  niveis_ordenados <- c("N4", "N3", "N2", "N1", "N0")
  nivel <- factor(sample(niveis_ordenados, 2000, replace = TRUE), levels = niveis_ordenados, ordered = TRUE)

  variavel <- gerar_variavel_por_nivel(
    nivel, correlacao_alvo = 0.52, media = 0.7, desvio_variavel = 0.2,
    minimo = 0.05, maximo = 1
  )

  expect_true(all(variavel >= 0.05 & variavel <= 1))
})
