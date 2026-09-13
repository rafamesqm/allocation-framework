test_that("comentários variam de tamanho dentro da faixa pedida", {
  set.seed(1)
  comentarios <- gerar_comentario_avaliacao(500, min_palavras = 5, max_palavras = 260)

  n_palavras <- lengths(strsplit(comentarios, " "))

  expect_length(comentarios, 500)
  expect_true(all(n_palavras >= 5 & n_palavras <= 260))
  expect_true(dplyr::n_distinct(n_palavras) > 1)
})

test_that("comentários não são sempre idênticos", {
  set.seed(2)
  comentarios <- gerar_comentario_avaliacao(50)

  expect_true(dplyr::n_distinct(comentarios) > 1)
})
