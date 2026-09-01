test_that("gerar_espaco_skills produz a taxonomia com a forma esperada", {
  skills <- gerar_espaco_skills()

  expect_named(skills, c("skill_id", "categoria", "nome"))
  expect_equal(nrow(skills), 140)
  expect_equal(dplyr::n_distinct(skills$skill_id), 140)
  expect_false(anyNA(skills))

  categorias_esperadas <- c(
    "backend", "tecnologia_de_dados", "frontend", "banco_de_dados", "devops",
    "agilidade", "mobile", "plataformas_corporativas", "testes", "rpa"
  )
  expect_setequal(unique(skills$categoria), categorias_esperadas)
})

test_that("gerar_espaco_skills é determinística", {
  expect_identical(gerar_espaco_skills(), gerar_espaco_skills())
})
