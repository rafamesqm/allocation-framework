test_that("adiciona cotos quando o tamanho atual é menor que o alvo", {
  set.seed(1)
  ids <- c("a", "b", "c")
  grau <- c(2, 0, 3) # "b" tem grau zero, nunca deve receber coto extra
  cotos <- rep(ids, grau)
  grupo <- rep(c("g1", "g2", "g1"), grau)

  ajustado <- ajustar_tamanho_cotos(cotos, grupo, ids, grau, c("g1", "g2", "g1"), tamanho_alvo = 10)

  expect_length(ajustado$cotos, 10)
  expect_true(all(cotos %in% ajustado$cotos))
  expect_false("b" %in% ajustado$cotos)
})

test_that("remove cotos quando o tamanho atual é maior que o alvo", {
  set.seed(2)
  ids <- c("a", "b")
  grau <- c(4, 4)
  cotos <- rep(ids, grau)
  grupo <- rep(c("g1", "g2"), grau)

  ajustado <- ajustar_tamanho_cotos(cotos, grupo, ids, grau, c("g1", "g2"), tamanho_alvo = 5)

  expect_length(ajustado$cotos, 5)
  expect_true(all(ajustado$cotos %in% cotos))
})

test_that("não muda nada quando o tamanho já é o alvo", {
  cotos <- c("a", "b", "c")
  grupo <- c("g1", "g1", "g2")

  ajustado <- ajustar_tamanho_cotos(cotos, grupo, c("a", "b", "c"), c(1, 1, 1), c("g1", "g1", "g2"), tamanho_alvo = 3)

  expect_equal(ajustado$cotos, cotos)
  expect_equal(ajustado$grupo_dos_cotos, grupo)
})
