test_that("gerar_consultores produz a dimensão e as colunas esperadas", {
  consultores <- gerar_consultores(n_consultores = 285)

  expect_equal(nrow(consultores), 285)
  expect_named(consultores, c(
    "consultor_id", "nivel_senioridade", "tempo_casa", "tempo_nivel",
    "tipo_contrato", "localizacao", "fuso",
    "disposicao_viagem_nacional", "disposicao_viagem_internacional"
  ))
  expect_false(anyNA(consultores))
  expect_equal(dplyr::n_distinct(consultores$consultor_id), 285)
})

test_that("nivel_senioridade é ordinal e aproxima a pirâmide real", {
  consultores <- gerar_consultores(n_consultores = 5000)

  expect_true(is.ordered(consultores$nivel_senioridade))
  expect_equal(levels(consultores$nivel_senioridade), c("N4", "N3", "N2", "N1", "N0"))

  proporcao_observada <- prop.table(table(consultores$nivel_senioridade))
  proporcao_alvo <- prop.table(c(N4 = 42, N3 = 107, N2 = 71, N1 = 55, N0 = 8))
  expect_equal(as.numeric(proporcao_observada), as.numeric(proporcao_alvo), tolerance = 0.03)
})

test_that("tempo_casa e tempo_nivel são coerentes entre si", {
  consultores <- gerar_consultores(n_consultores = 500)

  expect_true(all(consultores$tempo_casa > 0))
  expect_true(all(consultores$tempo_nivel > 0))
  expect_true(all(consultores$tempo_nivel <= consultores$tempo_casa))
})

test_that("campos categóricos ficam dentro do domínio esperado", {
  consultores <- gerar_consultores(n_consultores = 500)

  expect_true(all(consultores$tipo_contrato %in% c("CLT", "PJ")))
  expect_true(all(consultores$fuso == "America/Sao_Paulo"))
  expect_type(consultores$disposicao_viagem_nacional, "logical")
  expect_type(consultores$disposicao_viagem_internacional, "logical")
})

test_that("gerar_consultores é reprodutível com a mesma semente", {
  expect_identical(gerar_consultores(semente = 7), gerar_consultores(semente = 7))
})
