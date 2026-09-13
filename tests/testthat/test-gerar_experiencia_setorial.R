test_that("gerar_experiencia_setorial produz uma tabela longa consistente", {
  consultores <- gerar_consultores(n_consultores = 200)
  experiencia <- gerar_experiencia_setorial(consultores)

  expect_true(all(experiencia$consultor_id %in% consultores$consultor_id))
  expect_true(all(experiencia$intensidade >= 0 & experiencia$intensidade <= 1))
  expect_false(anyNA(experiencia))
})

test_that("cada consultor acumula no máximo max_setores segmentos distintos", {
  consultores <- gerar_consultores(n_consultores = 200)
  experiencia <- gerar_experiencia_setorial(consultores, max_setores = 3)

  n_segmentos_por_consultor <- dplyr::count(experiencia, consultor_id)$n
  expect_true(all(n_segmentos_por_consultor >= 1 & n_segmentos_por_consultor <= 3))
})

test_that("segmento fica dentro do espaço real de segmentos", {
  consultores <- gerar_consultores(n_consultores = 200)
  experiencia <- gerar_experiencia_setorial(consultores)

  segmentos_esperados <- c(
    "Varejo", "Serviços", "Indústria", "Banking", "Educação", "Health",
    "Insurance", "Agronegócio", "Alimentos e Bebidas", "Private Equity"
  )
  expect_true(all(experiencia$segmento %in% segmentos_esperados))
})
