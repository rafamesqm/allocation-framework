test_that("gerar_skills_consultor produz uma tabela longa consistente", {
  consultores <- gerar_consultores(n_consultores = 50)
  skills <- gerar_espaco_skills()
  set.seed(1)
  skills_consultor <- gerar_skills_consultor(consultores, skills)

  expect_true(all(skills_consultor$consultor_id %in% consultores$consultor_id))
  expect_true(all(skills_consultor$skill_id %in% skills$skill_id))
  expect_true(all(skills_consultor$proficiencia %in% c("Iniciante", "Básico", "Médio", "Avançado")))
  expect_true(all(skills_consultor$recencia_dias >= 1 & skills_consultor$recencia_dias <= 730))
  expect_false(anyNA(skills_consultor))
})

test_that("cobertura de skills é parcial, não o catálogo inteiro", {
  consultores <- gerar_consultores(n_consultores = 100)
  skills <- gerar_espaco_skills()
  set.seed(2)
  skills_consultor <- gerar_skills_consultor(consultores, skills)

  n_skills_por_consultor <- dplyr::count(skills_consultor, consultor_id)$n
  expect_true(mean(n_skills_por_consultor) < nrow(skills) * 0.5)
})

test_that("skills conhecidas se concentram nas categorias principais do consultor, não espalhadas", {
  # Testa concentração pela contagem de skills, não pelo número de categorias
  # tocadas: com 10 categorias e cobertura ocasional fora da especialidade,
  # é esperado tocar várias categorias com 1-2 skills cada, o que não
  # contradiz concentração. O que importa é que a maioria das skills
  # conhecidas venha de poucas categorias.
  consultores <- gerar_consultores(n_consultores = 100)
  skills <- gerar_espaco_skills()
  set.seed(3)
  skills_consultor <- gerar_skills_consultor(consultores, skills) |>
    dplyr::left_join(skills, by = "skill_id")

  concentracao_por_consultor <- skills_consultor |>
    dplyr::count(consultor_id, categoria) |>
    dplyr::mutate(pct = n / sum(n), .by = consultor_id) |>
    dplyr::summarise(pct_top2 = sum(sort(pct, decreasing = TRUE)[1:2], na.rm = TRUE), .by = consultor_id)

  expect_true(mean(concentracao_por_consultor$pct_top2) > 0.7)
})
