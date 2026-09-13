test_that("gerar_pipeline_demandas produz um estágio válido por oportunidade", {
  pipeline <- gerar_pipeline_demandas(5000, semente = 1)

  estagios_esperados <- c("Lead", "Reunião", "Proposta", "Projeto")

  expect_equal(nrow(pipeline), 5000)
  expect_false(anyNA(pipeline))
  expect_true(all(pipeline$estagio %in% estagios_esperados))
  expect_equal(dplyr::n_distinct(pipeline$oportunidade_id), 5000)
})

test_that("taxas de avanço do funil de coorte aproximam as taxas reais da Sexta rodada", {
  pipeline <- gerar_pipeline_demandas(50000, semente = 2)

  alcancou_reuniao <- mean(pipeline$estagio %in% c("Reunião", "Proposta", "Projeto"))
  alcancou_proposta <- mean(pipeline$estagio %in% c("Proposta", "Projeto"))
  alcancou_projeto <- mean(pipeline$estagio == "Projeto")

  expect_equal(alcancou_reuniao, 0.813, tolerance = 0.02)
  expect_equal(alcancou_proposta, 0.813 * 0.797, tolerance = 0.02)
  expect_equal(alcancou_projeto, 0.813 * 0.797 * 0.363, tolerance = 0.02)
})

test_that("gerar_pipeline_demandas é reprodutível com a mesma semente", {
  expect_identical(
    gerar_pipeline_demandas(1000, semente = 5),
    gerar_pipeline_demandas(1000, semente = 5)
  )
})
