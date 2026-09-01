test_that("gerar_tempo_casa nunca produz valor negativo", {
  nivel <- factor(sample(c("N4", "N3", "N2", "N1", "N0"), 500, replace = TRUE),
    levels = c("N4", "N3", "N2", "N1", "N0"), ordered = TRUE
  )
  tempo <- gerar_tempo_casa(nivel, piso_anos = 0.1)

  expect_length(tempo, 500)
  expect_true(all(tempo >= 0.1))
})

test_that("gerar_tempo_casa reproduz a correlação alvo com o nível, dentro de tolerância", {
  set.seed(42)
  nivel <- factor(sample(c("N4", "N3", "N2", "N1", "N0"), 2000,
    replace = TRUE, prob = c(42, 107, 71, 55, 8)
  ), levels = c("N4", "N3", "N2", "N1", "N0"), ordered = TRUE)
  tempo <- gerar_tempo_casa(nivel, correlacao_alvo = 0.34)

  corr_observada <- cor(as.numeric(nivel), tempo)
  expect_equal(corr_observada, 0.34, tolerance = 0.1)
})
