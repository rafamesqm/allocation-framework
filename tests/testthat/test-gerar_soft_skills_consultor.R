test_that("todo consultor tem as 9 competências", {
  consultores <- gerar_consultores(n_consultores = 100)
  soft_skills <- gerar_soft_skills_consultor(consultores)

  n_competencias_por_consultor <- dplyr::count(soft_skills, consultor_id)$n
  expect_true(all(n_competencias_por_consultor == 9))
  expect_equal(dplyr::n_distinct(soft_skills$competencia), 9)
})

test_that("nivel fica na escala de 3 níveis", {
  consultores <- gerar_consultores(n_consultores = 100)
  soft_skills <- gerar_soft_skills_consultor(consultores)

  expect_true(all(soft_skills$nivel %in% c("ABAIXO", "DENTRO", "ACIMA")))
  expect_false(anyNA(soft_skills))
})

test_that("consultor_id de soft_skills é subconjunto dos consultores", {
  consultores <- gerar_consultores(n_consultores = 100)
  soft_skills <- gerar_soft_skills_consultor(consultores)

  expect_setequal(soft_skills$consultor_id, consultores$consultor_id)
})
