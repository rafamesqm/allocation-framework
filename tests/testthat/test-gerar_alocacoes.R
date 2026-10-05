test_that("gerar_alocacoes produz as colunas esperadas, sem NA", {
  consultores <- gerar_consultores(n_consultores = 150)
  projetos <- gerar_projetos(n_projetos = 150, data_referencia = as.Date("2026-01-01"))
  experiencia <- gerar_experiencia_setorial(consultores)

  alocacoes <- gerar_alocacoes(consultores, projetos, experiencia, semente = 1)

  expect_named(alocacoes, c(
    "consultor_id", "projeto_id", "papel", "pct_alocacao", "horas_mes",
    "data_inicio", "duracao_dias", "data_fim"
  ))
  expect_false(anyNA(alocacoes))
  expect_true(nrow(alocacoes) > 0)
})

test_that("a janela de cada alocação cabe dentro da janela do seu projeto", {
  consultores <- gerar_consultores(n_consultores = 150)
  projetos <- gerar_projetos(n_projetos = 150, data_referencia = as.Date("2026-01-01"))
  experiencia <- gerar_experiencia_setorial(consultores)

  alocacoes <- gerar_alocacoes(consultores, projetos, experiencia, semente = 6)
  do_projeto <- match(alocacoes$projeto_id, projetos$projeto_id)

  expect_true(all(alocacoes$data_inicio >= projetos$data_inicio[do_projeto]))
  expect_true(all(alocacoes$data_fim <= projetos$data_fim[do_projeto]))
  expect_true(all(alocacoes$duracao_dias >= 1))
})

test_that("não há par (consultor, projeto) repetido", {
  consultores <- gerar_consultores(n_consultores = 150)
  projetos <- gerar_projetos(n_projetos = 150, data_referencia = as.Date("2026-01-01"))
  experiencia <- gerar_experiencia_setorial(consultores)

  alocacoes <- gerar_alocacoes(consultores, projetos, experiencia, semente = 2)

  expect_false(any(duplicated(paste(alocacoes$consultor_id, alocacoes$projeto_id))))
})

test_that("ids de consultor e projeto existem nas tabelas de origem", {
  consultores <- gerar_consultores(n_consultores = 150)
  projetos <- gerar_projetos(n_projetos = 150, data_referencia = as.Date("2026-01-01"))
  experiencia <- gerar_experiencia_setorial(consultores)

  alocacoes <- gerar_alocacoes(consultores, projetos, experiencia, semente = 3)

  expect_true(all(alocacoes$consultor_id %in% consultores$consultor_id))
  expect_true(all(alocacoes$projeto_id %in% projetos$projeto_id))
})

test_that("pct_alocacao e horas_mes ficam em faixa válida", {
  consultores <- gerar_consultores(n_consultores = 150)
  projetos <- gerar_projetos(n_projetos = 150, data_referencia = as.Date("2026-01-01"))
  experiencia <- gerar_experiencia_setorial(consultores)

  alocacoes <- gerar_alocacoes(consultores, projetos, experiencia, semente = 4)

  expect_true(all(alocacoes$pct_alocacao >= 0.05 & alocacoes$pct_alocacao <= 1))
  expect_true(all(alocacoes$horas_mes >= 0))
})

test_that("cold start reduz a fração de consultores e projetos alocados", {
  # A fração final sem alocação é sempre >= fracao_cold_start (garantido por
  # construção: quem é forçado a grau zero nunca aparece), mais uma fração
  # de "zero natural" da própria Binomial Negativa. Essa fração natural é
  # bem maior do lado do projeto (média 2,51, tamanho~1,03, P(grau=0)~28%)
  # do que do lado do consultor (média 9,00, tamanho~2,28, P(grau=0)~3%),
  # por isso a tolerância superior é bem mais folgada para o projeto.
  consultores <- gerar_consultores(n_consultores = 300)
  projetos <- gerar_projetos(n_projetos = 300, data_referencia = as.Date("2026-01-01"))
  experiencia <- gerar_experiencia_setorial(consultores)

  alocacoes <- gerar_alocacoes(
    consultores, projetos, experiencia, semente = 5,
    fracao_cold_start_consultor = 0.3, fracao_cold_start_projeto = 0.3
  )

  fracao_consultor_sem_alocacao <- 1 - dplyr::n_distinct(alocacoes$consultor_id) / nrow(consultores)
  fracao_projeto_sem_alocacao <- 1 - dplyr::n_distinct(alocacoes$projeto_id) / nrow(projetos)

  expect_true(fracao_consultor_sem_alocacao >= 0.3 && fracao_consultor_sem_alocacao <= 0.45)
  expect_true(fracao_projeto_sem_alocacao >= 0.3 && fracao_projeto_sem_alocacao <= 0.6)
})

test_that("gerar_alocacoes é reprodutível com a mesma semente", {
  consultores <- gerar_consultores(n_consultores = 100)
  projetos <- gerar_projetos(n_projetos = 100, data_referencia = as.Date("2026-01-01"))
  experiencia <- gerar_experiencia_setorial(consultores)

  expect_identical(
    gerar_alocacoes(consultores, projetos, experiencia, semente = 42),
    gerar_alocacoes(consultores, projetos, experiencia, semente = 42)
  )
})
