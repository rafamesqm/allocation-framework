test_that("com prob_puxa = 0, a nota final é sempre igual à nota às cegas", {
  cegas <- c("ABAIXO", "DENTRO", "ACIMA", "DENTRO")
  individual <- c("ACIMA", "ACIMA", "ABAIXO", "DENTRO")

  final <- calibrar_nota_final(cegas, individual, prob_puxa = 0)

  expect_equal(final, cegas)
})

test_that("com prob_puxa = 1, a nota final se move um nível em direção à autoavaliação", {
  cegas <- c("ABAIXO", "ACIMA", "DENTRO")
  individual <- c("ACIMA", "ABAIXO", "DENTRO")

  final <- calibrar_nota_final(cegas, individual, prob_puxa = 1)

  expect_equal(final, c("DENTRO", "DENTRO", "DENTRO"))
})

test_that("nunca sai do domínio ordinal, mesmo no limite da escala", {
  cegas <- c("ACIMA", "ABAIXO")
  individual <- c("ACIMA", "ABAIXO") # já no mesmo nível, não deveria ultrapassar a escala

  final <- calibrar_nota_final(cegas, individual, prob_puxa = 1)

  expect_true(all(final %in% c("ABAIXO", "DENTRO", "ACIMA")))
})
