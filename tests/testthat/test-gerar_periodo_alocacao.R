test_that("a alocação nunca escapa da janela do projeto", {
  set.seed(1)
  n <- 5000
  inicio_projeto <- as.Date("2026-01-01") + sample.int(500, n, replace = TRUE)
  duracao_projeto <- sample.int(400, n, replace = TRUE)

  periodo <- gerar_periodo_alocacao(inicio_projeto, duracao_projeto)

  expect_true(all(periodo$data_inicio >= inicio_projeto))
  expect_true(all(periodo$data_fim <= inicio_projeto + duracao_projeto))
  expect_equal(periodo$data_fim, periodo$data_inicio + periodo$duracao_dias)
})

test_that("duração fica entre 1 dia e a duração do projeto", {
  set.seed(2)
  n <- 5000
  duracao_projeto <- sample.int(400, n, replace = TRUE)

  periodo <- gerar_periodo_alocacao(rep(as.Date("2026-01-01"), n), duracao_projeto)

  expect_true(all(periodo$duracao_dias >= 1))
  expect_true(all(periodo$duracao_dias <= duracao_projeto))
})

test_that("projeto de 1 dia produz alocação de 1 dia, sem folga", {
  periodo <- gerar_periodo_alocacao(rep(as.Date("2026-01-01"), 50), rep(1L, 50))

  expect_true(all(periodo$duracao_dias == 1))
  expect_true(all(periodo$data_inicio == as.Date("2026-01-01")))
})

test_that("a cobertura média da janela do projeto bate a razão real de 0,75", {
  # Razão real entre média de duração de alocação (51,6) e média do span do
  # projeto (68,8), Sexta rodada de docs/mapeamento_schema.md. É o alvo de
  # calibração do default, ver roxygen.
  set.seed(3)
  n <- 100000
  duracao_projeto <- rep(1000L, n)

  periodo <- gerar_periodo_alocacao(rep(as.Date("2026-01-01"), n), duracao_projeto)

  expect_equal(mean(periodo$duracao_dias / duracao_projeto), 0.75, tolerance = 0.01)
})

test_that("concentração maior aproxima a alocação da cobertura total", {
  set.seed(4)
  n <- 20000
  inicio <- rep(as.Date("2026-01-01"), n)
  duracao_projeto <- rep(500L, n)

  frouxa <- gerar_periodo_alocacao(inicio, duracao_projeto, concentracao_cobertura = 2)
  apertada <- gerar_periodo_alocacao(inicio, duracao_projeto, concentracao_cobertura = 20)

  expect_true(mean(apertada$duracao_dias) > mean(frouxa$duracao_dias))
  expect_true(mean(apertada$duracao_dias / duracao_projeto) > 0.9)
})

test_that("sobre projetos com a distribuição real, a duração aproxima os quantis alvo", {
  # Alvo real de duração de alocação: mediana 18, P25 4, P75 60. Mediana e
  # P25 saem abaixo por arredondamento em projeto curto, efeito documentado
  # no roxygen; o teste fixa a faixa aceita, não finge que crava o alvo.
  set.seed(5)
  projetos <- gerar_projetos(n_projetos = 20000, data_referencia = as.Date("2026-01-01"))

  periodo <- gerar_periodo_alocacao(projetos$data_inicio, projetos$duracao_dias)

  expect_equal(stats::median(periodo$duracao_dias), 13, tolerance = 0.2)
  expect_equal(unname(stats::quantile(periodo$duracao_dias, 0.75)), 60, tolerance = 0.15)
})

test_that("é reprodutível sob a mesma semente", {
  inicio <- rep(as.Date("2026-01-01"), 100)
  duracao <- rep(90L, 100)

  set.seed(7)
  primeira <- gerar_periodo_alocacao(inicio, duracao)
  set.seed(7)
  segunda <- gerar_periodo_alocacao(inicio, duracao)

  expect_identical(primeira, segunda)
})
