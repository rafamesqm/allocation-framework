test_that("todo consultor tem ao menos inglês avaliado", {
  consultores <- gerar_consultores(n_consultores = 200)
  idiomas <- gerar_idiomas_consultor(consultores)

  consultores_com_ingles <- idiomas |>
    dplyr::filter(idioma == "Inglês") |>
    dplyr::pull(consultor_id)
  expect_setequal(consultores_com_ingles, consultores$consultor_id)
})

test_that("idioma e nivel ficam dentro do domínio esperado", {
  consultores <- gerar_consultores(n_consultores = 200)
  idiomas <- gerar_idiomas_consultor(consultores)

  expect_true(all(idiomas$idioma %in% c("Inglês", "Espanhol", "Outro")))
  expect_true(all(idiomas$nivel %in% c("Básico", "Intermediário", "Avançado", "Fluente")))
  expect_false(anyNA(idiomas))
})

test_that("espanhol e outro idioma são ocasionais, não universais", {
  consultores <- gerar_consultores(n_consultores = 500)
  idiomas <- gerar_idiomas_consultor(consultores)

  pct_espanhol <- mean(consultores$consultor_id %in% idiomas$consultor_id[idiomas$idioma == "Espanhol"])
  pct_outro <- mean(consultores$consultor_id %in% idiomas$consultor_id[idiomas$idioma == "Outro"])
  expect_true(pct_espanhol > 0 && pct_espanhol < 1)
  expect_true(pct_outro > 0 && pct_outro < 1)
})

test_that("presença de espanhol aproxima o achado real da Sexta rodada (~80%)", {
  consultores <- gerar_consultores(n_consultores = 3000)
  idiomas <- gerar_idiomas_consultor(consultores)

  pct_espanhol <- mean(consultores$consultor_id %in% idiomas$consultor_id[idiomas$idioma == "Espanhol"])
  expect_equal(pct_espanhol, 0.8, tolerance = 0.03)
})
