test_that("gerar_pipeline_demandas produz um estágio válido por oportunidade", {
  pipeline <- gerar_pipeline_demandas(5000, semente = 1)

  estagios_esperados <- c("Ideia", "Reunião Inicial", "Lead", "Proposta", "Projeto", "Recusada/Cancelada")

  expect_equal(nrow(pipeline), 5000)
  expect_false(anyNA(pipeline))
  expect_true(all(pipeline$estagio %in% estagios_esperados))
  expect_equal(dplyr::n_distinct(pipeline$oportunidade_id), 5000)
})

test_that("proporção por estágio aproxima o censo real", {
  pipeline <- gerar_pipeline_demandas(50000, semente = 2)

  proporcao_observada <- prop.table(table(pipeline$estagio))
  proporcao_alvo <- prop.table(c(
    "Ideia" = 92, "Reunião Inicial" = 123, "Lead" = 127,
    "Proposta" = 209, "Projeto" = 952, "Recusada/Cancelada" = 2799
  ))

  expect_equal(
    as.numeric(proporcao_observada[names(proporcao_alvo)]),
    as.numeric(proporcao_alvo),
    tolerance = 0.02
  )
})
