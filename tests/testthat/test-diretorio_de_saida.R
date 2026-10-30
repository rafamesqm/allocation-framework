test_that("a corrida completa grava em outputs/", {
  completa <- realizacoes_registradas_w3()
  w4 <- list(reduzida = FALSE)
  expect_equal(diretorio_de_saida("tables", completa, w4), here::here("outputs", "tables"))
  expect_equal(diretorio_de_saida("figures", completa, w4), here::here("outputs", "figures"))
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

test_that("a wave 4 reduzida também não grava em outputs/", {
  raiz <- withr::local_tempdir()
  withr::local_envvar(W4_CORRIDA = "reduzida", W3_SAIDA_REDUZIDA = raiz)
  expect_equal(diretorio_de_saida("tables", realizacoes_registradas_w3()), file.path(raiz, "tables"))
})

test_that("W4_CORRIDA só aceita completa ou reduzida", {
  withr::local_envvar(W4_CORRIDA = "rapida")
  expect_error(parametros_da_corrida_w4(), "W4_CORRIDA")
})

test_that("a corrida completa deixa o desenho e as emendas intactos", {
  registro <- ler_pre_registro_otimizacao()
  desenho <- separar_desenho_registrado(registro)
  nsga2 <- localizar_emenda(registro, "emenda 1, troca para o NSGA-II")
  exploratoria <- localizar_emenda(registro, "emenda 2, referência do hipervolume e corrida exploratória")
  completa <- list(reduzida = FALSE)
  expect_identical(reduzir_corrida_w4(desenho, completa), desenho)
  expect_identical(reduzir_corrida_w4(nsga2, completa), nsga2)
  expect_identical(reduzir_corrida_w4(exploratoria, completa), exploratoria)
})

test_that("a corrida reduzida encurta o tempo-limite, as sementes e as gerações", {
  registro <- ler_pre_registro_otimizacao()
  withr::local_envvar(W4_CORRIDA = "reduzida")
  reduzida <- parametros_da_corrida_w4()
  desenho <- reduzir_corrida_w4(separar_desenho_registrado(registro), reduzida)
  nsga2 <- reduzir_corrida_w4(localizar_emenda(registro, "emenda 1, troca para o NSGA-II"), reduzida)
  exploratoria <- reduzir_corrida_w4(
    localizar_emenda(registro, "emenda 2, referência do hipervolume e corrida exploratória"), reduzida
  )
  expect_equal(desenho$metodo$tempo_limite_segundos, 60)
  expect_length(nsga2$sementes$variabilidade, 2)
  expect_equal(nsga2$sementes$principal, localizar_emenda(registro, "emenda 1, troca para o NSGA-II")$sementes$principal)
  expect_equal(exploratoria$corrida_exploratoria$geracoes, 2000)
})
