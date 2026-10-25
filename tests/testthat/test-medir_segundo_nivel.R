test_that("as férias da realização não mudam o resto do sorteio", {
  sem_ferias <- gerar_realizacao(7)
  com_ferias <- gerar_realizacao_com_ferias(7)

  expect_identical(com_ferias[names(sem_ferias)], sem_ferias)
  expect_equal(nrow(com_ferias$ferias_consultor), nrow(sem_ferias$consultores))
  # Semente própria e derivada: mesma realização, mesmas férias.
  expect_identical(gerar_realizacao_com_ferias(7)$ferias_consultor, com_ferias$ferias_consultor)
  expect_false(identical(gerar_realizacao_com_ferias(8)$ferias_consultor, com_ferias$ferias_consultor))
})

test_that("gerar as férias não desloca o fluxo aleatório de quem chama", {
  # As férias saem por último e com o estado aleatório preservado: quem chama
  # encontra o fluxo como gerar_realizacao() o deixaria.
  set.seed(123)
  invisible(gerar_realizacao(7))
  depois_sem_ferias <- stats::runif(1)

  set.seed(123)
  invisible(gerar_realizacao_com_ferias(7))
  expect_identical(stats::runif(1), depois_sem_ferias)
})

test_that("a semente das férias fica longe das sementes das realizações", {
  expect_true(all(derivar_semente_de_ferias(1:100) > 100))
  expect_false(any(duplicated(derivar_semente_de_ferias(1:100))))
})

test_that("o segundo nível versionado reproduz a Etapa 5", {
  skip_if_not(dir.exists(here::here("_targets")), "store do targets ausente")
  store <- here::here("_targets")
  versionado <- targets::tar_read(w3_segundo_nivel_versionado, store = store)
  classificadas <- targets::tar_read(w3_plausibilidade, store = store)$classificadas

  colaborativo <- classificadas$classe[classificadas$ramo == "colaborativo"]
  expect_equal(versionado$colaborativo_recomendado_pior, mean(colaborativo == "recomendado pior"))
  expect_equal(versionado$discordancias_conteudo, sum(classificadas$ramo == "conteúdo"))
})
