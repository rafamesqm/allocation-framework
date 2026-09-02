test_that("balanço soma 1 e conta corretamente por nível", {
  consultores <- tibble::tibble(
    consultor_id = c("c1", "c2", "c3", "c4"),
    nivel_senioridade = factor(c("N4", "N4", "N3", "N0"), levels = c("N4", "N3", "N2", "N1", "N0"), ordered = TRUE)
  )

  balanco <- agregar_balanco_senioridade(c("c1", "c2", "c3", "c4"), consultores)

  expect_equal(sum(balanco$proporcao), 1)
  expect_equal(balanco$n[balanco$nivel_senioridade == "N4"], 2)
  expect_equal(balanco$proporcao[balanco$nivel_senioridade == "N4"], 0.5)
})

test_that("subconjunto do time calcula balanço só sobre quem foi filtrado", {
  consultores <- gerar_consultores(n_consultores = 50)
  ids_time <- consultores$consultor_id[1:5]

  balanco <- agregar_balanco_senioridade(ids_time, consultores)

  expect_equal(sum(balanco$n), 5)
  expect_equal(sum(balanco$proporcao), 1)
})
