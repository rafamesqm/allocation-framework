test_that("custo_carregado fica dentro da faixa real por nível", {
  consultores <- gerar_consultores(n_consultores = 500)
  custo <- gerar_custo_carregado(consultores)

  expect_named(custo, c("consultor_id", "custo_hora_base", "custo_carregado"))
  expect_equal(nrow(custo), nrow(consultores))
  expect_false(anyNA(custo))
  # Faixa da tabela de custo por cargo, nao do rate card: a fonte de custo
  # passou a ser a tabela de custo por cargo, salario base por hora.
  expect_true(all(custo$custo_hora_base >= 70 & custo$custo_hora_base <= 1093))
})

test_that("custo carregado e a base vezes o fator de carga", {
  consultores <- gerar_consultores(n_consultores = 200)

  sem_carga <- gerar_custo_carregado(consultores, fator_carga = 1)
  com_carga <- gerar_custo_carregado(consultores, fator_carga = 1.8)

  expect_equal(sem_carga$custo_carregado, sem_carga$custo_hora_base)
  expect_equal(com_carga$custo_carregado, com_carga$custo_hora_base * 1.8)
})

test_that("banda de um valor so devolve aquele valor", {
  # sample(271, 1) sorteia de 1 a 271 em vez de devolver 271. O nivel N2 tem
  # banda unitaria e saia com media 136 no lugar de 271.
  consultores <- tibble::tibble(
    consultor_id = paste0("c", 1:50),
    nivel_senioridade = factor(rep("N2", 50), levels = c("N4","N3","N2","N1","N0"), ordered = TRUE)
  )

  custo <- gerar_custo_carregado(consultores)

  expect_true(all(custo$custo_hora_base == 271))
})

test_that("nível mais sênior tem custo médio maior que o mais júnior", {
  consultores <- gerar_consultores(n_consultores = 2000)
  custo <- gerar_custo_carregado(consultores) |>
    dplyr::left_join(consultores[c("consultor_id", "nivel_senioridade")], by = "consultor_id")

  media_por_nivel <- custo |>
    dplyr::summarise(media = mean(custo_carregado), .by = nivel_senioridade) |>
    dplyr::arrange(nivel_senioridade)

  # níveis ordenados N4 < N3 < N2 < N1 < N0 (N0 mais sênior)
  expect_true(all(diff(media_por_nivel$media) > 0))
})

test_that("gerar_custo_carregado é reprodutível com a mesma semente", {
  consultores <- gerar_consultores(n_consultores = 100)
  expect_identical(
    gerar_custo_carregado(consultores, semente = 9),
    gerar_custo_carregado(consultores, semente = 9)
  )
})
