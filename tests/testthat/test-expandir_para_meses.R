test_that("intervalo dentro de um único mês vira uma linha só", {
  meses <- expandir_para_meses(as.Date("2026-03-05"), as.Date("2026-03-20"))

  expect_equal(nrow(meses), 1)
  expect_equal(meses$ano_mes, "2026-03")
  expect_equal(meses$dias_ativos, 16L)
  expect_equal(meses$dias_no_mes, 31L)
})

test_that("intervalo de um dia só cobre um dia, não zero", {
  meses <- expandir_para_meses(as.Date("2026-03-05"), as.Date("2026-03-05"))

  expect_equal(nrow(meses), 1)
  expect_equal(meses$dias_ativos, 1L)
})

test_that("intervalo que atravessa meses gera uma linha por mês tocado", {
  meses <- expandir_para_meses(as.Date("2026-01-20"), as.Date("2026-03-10"))

  expect_equal(meses$ano_mes, c("2026-01", "2026-02", "2026-03"))
  expect_equal(meses$dias_ativos, c(12L, 28L, 10L))
  expect_equal(meses$dias_no_mes, c(31L, 28L, 31L))
})

test_that("dias_ativos somam a duração total do intervalo", {
  inicio <- as.Date("2025-11-14")
  fim <- as.Date("2026-04-03")

  meses <- expandir_para_meses(inicio, fim)

  expect_equal(sum(meses$dias_ativos), as.integer(fim - inicio) + 1L)
})

test_that("vários intervalos mantêm o índice de origem em linha", {
  meses <- expandir_para_meses(
    as.Date(c("2026-01-10", "2026-05-01")),
    as.Date(c("2026-02-02", "2026-05-31"))
  )

  expect_equal(meses$linha, c(1L, 1L, 2L))
  expect_equal(meses$ano_mes, c("2026-01", "2026-02", "2026-05"))
})

test_that("data_fim anterior a data_inicio é rejeitada", {
  expect_error(expandir_para_meses(as.Date("2026-03-10"), as.Date("2026-03-01")))
})
