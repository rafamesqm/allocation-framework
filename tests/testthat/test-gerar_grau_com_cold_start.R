test_that("fração de cold start produz exatamente essa fração de graus zero", {
  set.seed(1)
  n <- 5000
  grau <- gerar_grau_com_cold_start(n, media = 9.00, variancia = 44.6, fracao_cold_start = 0.17)

  expect_length(grau, n)
  expect_equal(mean(grau == 0), 0.17, tolerance = 0.01)
})

test_that("grau não-cold-start nunca é zero, mesmo com alta massa natural de zero", {
  set.seed(2)
  n <- 5000
  # média 2,51 / variância 8,65 tem ~28% de massa natural em zero sem truncar
  grau <- gerar_grau_com_cold_start(n, media = 2.51, variancia = 8.65, fracao_cold_start = 0.17)

  fracao_zero <- mean(grau == 0)
  expect_equal(fracao_zero, 0.17, tolerance = 0.02)
})

test_that("truncar em zero desloca a média dos positivos para cima, como documentado", {
  # Limitação conhecida (ver roxygen de gerar_grau_com_cold_start()): com
  # ~28% de massa natural em zero na calibração do projeto, truncar em zero
  # empurra a média condicional de 2,51 para ~3,50 (+39%). Este teste
  # verifica que o desvio documentado continua nessa ordem de grandeza, não
  # que a truncagem reproduz a média calibrada (ela não reproduz).
  set.seed(3)
  n <- 20000
  media_alvo <- 2.51
  grau <- gerar_grau_com_cold_start(n, media = media_alvo, variancia = 8.65, fracao_cold_start = 0.17)

  media_nao_zero <- mean(grau[grau > 0])
  expect_true(media_nao_zero > media_alvo)
  expect_equal(media_nao_zero, 3.50, tolerance = 0.1)
})
