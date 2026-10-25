test_that("o McNemar exato usa só os discordantes e coincide com binom.test", {
  modelo <- c(rep(1, 12), rep(0, 3), rep(1, 40), rep(0, 45))
  piso <- c(rep(0, 12), rep(1, 3), rep(1, 40), rep(0, 45))
  teste <- testar_mcnemar_exato(modelo, piso)

  expect_equal(teste$b, 12)
  expect_equal(teste$c, 3)
  expect_equal(teste$valor_p, binom.test(12, 15)$p.value)
  expect_true(teste$supera)
})

test_that("rejeitar a favor do piso não conta como superar", {
  teste <- testar_mcnemar_exato(c(rep(0, 15), 1), c(rep(1, 15), 1))
  expect_lt(teste$valor_p, 0.05)
  expect_false(teste$supera)
})

test_that("sem discordantes o valor-p é um e nada supera", {
  teste <- testar_mcnemar_exato(c(1, 0, 1), c(1, 0, 1))
  expect_equal(teste$valor_p, 1)
  expect_false(teste$supera)
})

test_that("a escolha que diverge da emenda para a pipeline", {
  registro <- ler_pre_registro()
  chave <- "hiperparametros_do_conteudo_na_realizacao_versionada"
  emenda <- purrr::detect(registro$emendas, \(e) !is.null(e[[chave]]))
  escolhidos <- purrr::map(emenda[[chave]], tibble::as_tibble) |> purrr::list_rbind()
  selecao <- list(escolhidos = escolhidos)

  expect_identical(conferir_escolha_registrada(selecao, registro), selecao)
  selecao$escolhidos$fatores[1] <- 99
  expect_error(conferir_escolha_registrada(selecao, registro), "difere")
})

test_that("a escolha da pipeline é a registrada na emenda", {
  skip_if_not(dir.exists(here::here("_targets")), "store do targets ausente")
  selecao <- targets::tar_read(w3_conteudo_selecao, store = here::here("_targets"))
  expect_no_error(conferir_escolha_registrada(selecao, ler_pre_registro()))
})
