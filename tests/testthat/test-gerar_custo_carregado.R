test_that("custo_carregado fica dentro da faixa real por nível", {
  consultores <- gerar_consultores(n_consultores = 500)
  custo <- gerar_custo_carregado(consultores)

  expect_named(custo, c("consultor_id", "custo_carregado"))
  expect_equal(nrow(custo), nrow(consultores))
  expect_false(anyNA(custo))
  expect_true(all(custo$custo_carregado >= 91.45 & custo$custo_carregado <= 2066.00))
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
