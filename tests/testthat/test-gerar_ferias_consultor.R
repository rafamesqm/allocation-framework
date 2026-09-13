test_that("gerar_ferias_consultor produz um período válido por consultor", {
  consultores <- gerar_consultores(n_consultores = 200)
  ferias <- gerar_ferias_consultor(consultores, dias_ferias = 30, ano_referencia = 2026)

  expect_equal(nrow(ferias), nrow(consultores))
  expect_false(anyNA(ferias))
  expect_true(all(ferias$fim_ferias - ferias$inicio_ferias == 30))
  expect_true(all(format(ferias$inicio_ferias, "%Y") == "2026"))
  expect_true(all(format(ferias$fim_ferias, "%Y") == "2026"))
})

test_that("gerar_ferias_consultor é reprodutível com a mesma semente", {
  consultores <- gerar_consultores(n_consultores = 50)
  expect_identical(
    gerar_ferias_consultor(consultores, semente = 3),
    gerar_ferias_consultor(consultores, semente = 3)
  )
})
