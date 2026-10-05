test_that("a cobertura bate exatamente com o agregado real, sem flutuar", {
  # Sorteio de numero fixo, e nao Bernoulli por consultor: o agregado real de
  # 138 de 285 e conhecido, e nao ha por que devolver 134 ou 141 no lugar dele.
  consultores <- gerar_consultores(n_consultores = 285)

  respondentes <- sortear_respondentes_formulario(consultores)

  expect_length(respondentes, 138)
  expect_equal(length(respondentes) / nrow(consultores), 0.484, tolerance = 0.002)
  expect_true(all(respondentes %in% consultores$consultor_id))
  expect_false(anyDuplicated(respondentes) > 0)
})

test_that("a cobertura e parametrizavel e a restricao a respeita", {
  consultores <- gerar_consultores(n_consultores = 100)

  respondentes <- sortear_respondentes_formulario(consultores, cobertura_populacional = 0.3)

  expect_length(respondentes, 30)
})

test_that("restringir_a_respondentes remove exatamente quem nao respondeu", {
  tabela <- tibble::tibble(
    consultor_id = rep(paste0("c", 1:10), each = 2),
    valor = seq_len(20)
  )

  restrita <- restringir_a_respondentes(tabela, paste0("c", c(1, 3, 5)))

  expect_equal(sort(unique(restrita$consultor_id)), c("c1", "c3", "c5"))
  expect_equal(nrow(restrita), 6)
  expect_true(all(restrita$valor %in% tabela$valor))
})

test_that("restringir_a_respondentes exige a coluna de ligacao", {
  tabela <- tibble::tibble(projeto_id = "p1", valor = 1)

  expect_error(restringir_a_respondentes(tabela, "c1"))
})

test_that("os sinais de formulario nascem acoplados quando partilham o sorteio", {
  # E a propriedade que o bloco II.3 depende: quem nao respondeu nao aparece em
  # nenhum dos sinais, e nao num subconjunto sorteado a parte de cada vez.
  consultores <- gerar_consultores(n_consultores = 200)
  respondentes <- sortear_respondentes_formulario(consultores)

  skills <- gerar_skills_consultor(
    consultores, gerar_espaco_skills(), respondentes = respondentes
  )
  setorial <- restringir_a_respondentes(
    gerar_experiencia_setorial(consultores), respondentes
  )
  certificacoes <- gerar_certificacoes_consultor(
    consultores, respondentes = respondentes
  )

  expect_setequal(unique(skills$consultor_id), unique(setorial$consultor_id))
  expect_true(all(unique(certificacoes$consultor_id) %in% respondentes))
})
