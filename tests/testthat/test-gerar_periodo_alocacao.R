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

test_that("a duração das alocações fica perto dos quartis reais 4, 18 e 60", {
  # A calibração anterior acertava a fração da janela do projeto e errava a
  # duração da alocação, que é a grandeza com âncora real: quando a duração
  # de projeto mudou de 18 para 81 dias medianos, a mediana das alocações foi
  # a 56 dias contra 18 reais sem nenhum teste falhar. Este teste olha a
  # grandeza ancorada, sobre as durações de projeto do gerador.
  set.seed(5)
  projetos <- gerar_projetos(n_projetos = 20000, data_referencia = as.Date("2026-01-01"))

  periodo <- gerar_periodo_alocacao(projetos$data_inicio, projetos$duracao_dias)
  quartis <- stats::quantile(periodo$duracao_dias, c(0.25, 0.5, 0.75), names = FALSE)

  expect_gte(quartis[1], 2)
  expect_lte(quartis[1], 8)
  expect_gte(quartis[2], 12)
  expect_lte(quartis[2], 27)
  expect_gte(quartis[3], 42)
  expect_lte(quartis[3], 80)
})

test_that("os parâmetros padrão são os que a calibração devolve", {
  # Amarra o padrão de gerar_periodo_alocacao() à função que o produz, para
  # que uma mudança na duração de projeto não deixe o padrão órfão.
  set.seed(6)
  projetos <- gerar_projetos(n_projetos = 3000, data_referencia = as.Date("2026-01-01"))

  calibrado <- calibrar_cobertura_alocacao(projetos$duracao_dias)

  expect_equal(calibrado$forma_a, 0.426, tolerance = 0.25)
  expect_equal(calibrado$forma_b, 0.689, tolerance = 0.25)
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
