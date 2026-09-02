test_that("agrega pct_alocacao e horas_mes por consultor", {
  consultores <- gerar_consultores(n_consultores = 100)
  projetos <- gerar_projetos(n_projetos = 100, data_referencia = as.Date("2026-01-01"))
  experiencia <- gerar_experiencia_setorial(consultores)
  alocacoes <- gerar_alocacoes(consultores, projetos, experiencia, semente = 1)

  ocupacao <- agregar_ocupacao_consultor(alocacoes)

  expect_named(ocupacao, c("consultor_id", "pct_alocacao_total", "horas_mes_total", "n_alocacoes"))
  expect_false(anyNA(ocupacao))
  expect_true(all(ocupacao$n_alocacoes >= 1))
  expect_equal(dplyr::n_distinct(ocupacao$consultor_id), nrow(ocupacao))
})

test_that("consultor em cold start não aparece no resultado", {
  consultores <- gerar_consultores(n_consultores = 100)
  projetos <- gerar_projetos(n_projetos = 100, data_referencia = as.Date("2026-01-01"))
  experiencia <- gerar_experiencia_setorial(consultores)
  alocacoes <- gerar_alocacoes(consultores, projetos, experiencia, semente = 1)

  ocupacao <- agregar_ocupacao_consultor(alocacoes)

  expect_true(nrow(ocupacao) < nrow(consultores))
  expect_true(all(ocupacao$consultor_id %in% alocacoes$consultor_id))
})
