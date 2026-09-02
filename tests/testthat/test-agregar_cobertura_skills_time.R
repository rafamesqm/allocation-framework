test_that("cobertura conta quantos consultores do time têm cada skill", {
  skills_consultor <- tibble::tibble(
    consultor_id = c("c1", "c1", "c2", "c3"),
    skill_id = c("s1", "s2", "s1", "s3")
  )

  cobertura <- agregar_cobertura_skills_time(c("c1", "c2"), skills_consultor)

  expect_setequal(cobertura$skill_id, c("s1", "s2"))
  expect_equal(cobertura$n_consultores_com_skill[cobertura$skill_id == "s1"], 2)
  expect_equal(cobertura$n_consultores_com_skill[cobertura$skill_id == "s2"], 1)
})

test_that("time sem nenhum consultor no catálogo de skills retorna vazio", {
  skills_consultor <- tibble::tibble(consultor_id = "c1", skill_id = "s1")

  cobertura <- agregar_cobertura_skills_time(c("c99"), skills_consultor)

  expect_equal(nrow(cobertura), 0)
})
