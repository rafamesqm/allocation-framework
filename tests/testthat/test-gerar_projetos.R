test_that("gerar_projetos produz a dimensão e as colunas esperadas", {
  projetos <- gerar_projetos(n_projetos = 300)

  expect_equal(nrow(projetos), 300)
  expect_named(projetos, c(
    "projeto_id", "setor", "tipo_projeto", "valor_contrato",
    "esforco_estimado", "data_inicio", "duracao_dias", "data_fim"
  ))
  expect_false(anyNA(projetos))
  expect_equal(dplyr::n_distinct(projetos$projeto_id), 300)
})

test_that("setor e tipo_projeto ficam dentro do espaço real de categorias", {
  projetos <- gerar_projetos(n_projetos = 300)

  segmentos_esperados <- c(
    "Varejo", "Serviços", "Indústria", "Banking", "Educação", "Health",
    "Insurance", "Agronegócio", "Alimentos e Bebidas", "Private Equity"
  )
  offerings_esperados <- c(
    "Experience & Efficiency", "Strategy and M&A",
    "Data Analytics & Artificial Intelligence", "Supply Chain",
    "Tech Consulting", "Finance", "Talent & Organization",
    "Digital Products & Platforms", "Cyber Security", "Digital Factory",
    "Peers.AI", "Educação Pública"
  )
  expect_true(all(projetos$setor %in% segmentos_esperados))
  expect_true(all(projetos$tipo_projeto %in% offerings_esperados))
})

test_that("valor_contrato e esforco_estimado são não negativos", {
  projetos <- gerar_projetos(n_projetos = 500)

  expect_true(all(projetos$valor_contrato >= 0))
  expect_true(all(projetos$esforco_estimado >= 0))
})

test_that("datas são coerentes: data_fim nunca antes de data_inicio", {
  projetos <- gerar_projetos(n_projetos = 500)

  expect_true(all(projetos$data_fim >= projetos$data_inicio))
  expect_true(all(projetos$duracao_dias >= 0))
})

test_that("duracao_dias aproxima a mediana real calibrada por quantis (Sexta rodada)", {
  projetos <- gerar_projetos(n_projetos = 20000, semente = 3)

  expect_equal(median(projetos$duracao_dias), 18, tolerance = 0.25)
  expect_true(max(projetos$duracao_dias) > median(projetos$duracao_dias) * 10)
})

test_that("duracao_dias respeita o teto, mesmo quando a log-normal sorteia além dele", {
  projetos <- gerar_projetos(n_projetos = 20000, semente = 3)

  expect_true(all(projetos$duracao_dias <= 2368))
  expect_true(any(projetos$duracao_dias == 2368))
})

test_that("gerar_projetos é reprodutível com a mesma semente e data de referência", {
  data_ref <- as.Date("2026-09-01")
  expect_identical(
    gerar_projetos(semente = 7, data_referencia = data_ref),
    gerar_projetos(semente = 7, data_referencia = data_ref)
  )
})

test_that("default gera o número real de projetos em execução", {
  projetos <- gerar_projetos()
  expect_equal(nrow(projetos), 952)
})
