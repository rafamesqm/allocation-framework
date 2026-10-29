test_that("a corrida completa grava em outputs/", {
  completa <- realizacoes_registradas_w3()
  expect_equal(diretorio_de_saida("tables", completa), here::here("outputs", "tables"))
  expect_equal(diretorio_de_saida("figures", completa), here::here("outputs", "figures"))
})

test_that("a corrida reduzida não grava em outputs/", {
  raiz <- withr::local_tempdir()
  withr::local_envvar(W3_REALIZACOES = "5", W3_SAIDA_REDUZIDA = raiz)

  tabelas <- diretorio_de_saida("tables")
  expect_equal(tabelas, file.path(raiz, "tables"))
  expect_true(dir.exists(tabelas))

  caminho <- salvar_tabela(tibble::tibble(x = 1), "teste_trava")
  expect_equal(normalizePath(dirname(caminho)), normalizePath(file.path(raiz, "tables")))
  expect_false(file.exists(here::here("outputs", "tables", "teste_trava.csv")))
})

test_that("a corrida reduzida para se a pasta temporária apontar para outputs/", {
  withr::local_envvar(W3_REALIZACOES = "5", W3_SAIDA_REDUZIDA = here::here("outputs"))
  expect_error(diretorio_de_saida("tables"), "não grava em outputs/")
})

test_that("W3_REALIZACOES fora do intervalo para a pipeline", {
  withr::local_envvar(W3_REALIZACOES = "0")
  expect_error(realizacoes_w3(), "W3_REALIZACOES")
  withr::local_envvar(W3_REALIZACOES = "abc")
  expect_error(realizacoes_w3(), "W3_REALIZACOES")
  withr::local_envvar(W3_REALIZACOES = as.character(realizacoes_registradas_w3() + 1))
  expect_error(realizacoes_w3(), "W3_REALIZACOES")
})
