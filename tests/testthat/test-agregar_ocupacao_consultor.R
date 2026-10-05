test_that("agrega ocupação por consultor e mês", {
  consultores <- gerar_consultores(n_consultores = 100)
  projetos <- gerar_projetos(n_projetos = 100, data_referencia = as.Date("2026-01-01"))
  experiencia <- gerar_experiencia_setorial(consultores)
  alocacoes <- gerar_alocacoes(consultores, projetos, experiencia, semente = 1)

  ocupacao <- agregar_ocupacao_consultor(alocacoes)

  expect_named(ocupacao, c("consultor_id", "ano_mes", "pct_ocupacao", "horas_mes", "n_alocacoes"))
  expect_false(anyNA(ocupacao))
  expect_true(all(ocupacao$n_alocacoes >= 1))
  expect_false(any(duplicated(paste(ocupacao$consultor_id, ocupacao$ano_mes))))
})

test_that("consultor em cold start não aparece no resultado", {
  consultores <- gerar_consultores(n_consultores = 100)
  projetos <- gerar_projetos(n_projetos = 100, data_referencia = as.Date("2026-01-01"))
  experiencia <- gerar_experiencia_setorial(consultores)
  alocacoes <- gerar_alocacoes(consultores, projetos, experiencia, semente = 1)

  ocupacao <- agregar_ocupacao_consultor(alocacoes)

  expect_true(dplyr::n_distinct(ocupacao$consultor_id) < nrow(consultores))
  expect_true(all(ocupacao$consultor_id %in% alocacoes$consultor_id))
})

test_that("mês parcial entra ponderado pelos dias cobertos", {
  # Meia alocação de 50% cobrindo 15 dos 31 dias de março ocupa
  # 0,5 * 15/31 do mês, não 0,5.
  alocacoes <- tibble::tibble(
    consultor_id = "consultor_0001",
    projeto_id = "projeto_0001",
    papel = "N3",
    pct_alocacao = 0.5,
    horas_mes = 120,
    data_inicio = as.Date("2026-03-01"),
    duracao_dias = 14L,
    data_fim = as.Date("2026-03-15")
  )

  ocupacao <- agregar_ocupacao_consultor(alocacoes)

  expect_equal(nrow(ocupacao), 1)
  expect_equal(ocupacao$ano_mes, "2026-03")
  expect_equal(ocupacao$pct_ocupacao, 0.5 * 15 / 31)
  expect_equal(ocupacao$horas_mes, 120 * 15 / 31)
})

test_that("alocações simultâneas somam no mesmo mês", {
  alocacoes <- tibble::tibble(
    consultor_id = c("consultor_0001", "consultor_0001"),
    projeto_id = c("projeto_0001", "projeto_0002"),
    papel = c("N3", "N3"),
    pct_alocacao = c(0.6, 0.7),
    horas_mes = c(100, 80),
    data_inicio = as.Date(c("2026-03-01", "2026-03-01")),
    duracao_dias = c(30L, 30L),
    data_fim = as.Date(c("2026-03-31", "2026-03-31"))
  )

  ocupacao <- agregar_ocupacao_consultor(alocacoes)

  expect_equal(nrow(ocupacao), 1)
  expect_equal(ocupacao$n_alocacoes, 2L)
  expect_equal(ocupacao$pct_ocupacao, 1.3)
})

test_that("alocação que atravessa meses vira uma linha por mês", {
  alocacoes <- tibble::tibble(
    consultor_id = "consultor_0001",
    projeto_id = "projeto_0001",
    papel = "N3",
    pct_alocacao = 1,
    horas_mes = 160,
    data_inicio = as.Date("2026-01-20"),
    duracao_dias = 49L,
    data_fim = as.Date("2026-03-10")
  )

  ocupacao <- agregar_ocupacao_consultor(alocacoes)

  expect_equal(ocupacao$ano_mes, c("2026-01", "2026-02", "2026-03"))
  expect_equal(ocupacao$pct_ocupacao[2], 1)
  expect_true(all(ocupacao$pct_ocupacao[c(1, 3)] < 1))
})
