dados_do_mapa_de_teste <- function() {
  ids <- paste0("c", sprintf("%02d", 1:20))
  atributos <- withr::with_seed(1, matrix(
    stats::rbinom(20 * 6, 1, 0.5), 20, 6, dimnames = list(ids, paste0("a", 1:6))
  ))
  fatores <- withr::with_seed(2, matrix(stats::rnorm(20 * 2), 20, 2, dimnames = list(ids, NULL)))
  list(ids = ids, atributos = atributos, fatores = fatores)
}

test_that("os subgrupos dependem só dos identificadores e da semente", {
  ids <- paste0("c", 1:23)
  a <- atribuir_subgrupos(ids, 5, 1)
  b <- atribuir_subgrupos(rev(ids), 5, 1)
  expect_identical(a, b)
  expect_lte(diff(range(table(a))), 1)
})

test_that("a previsão de quem está na estimação não usa o próprio fator", {
  m <- dados_do_mapa_de_teste()
  estimacao <- m$ids[1:15]
  base <- prever_fatores_cruzados(m$atributos, m$fatores, estimacao, estimacao, 0.1, 5, 1)

  # Mudar o fator de c03 não muda a previsão de c03, que sai de um mapa
  # ajustado sem o subgrupo dele.
  alterado <- m$fatores
  alterado["c03", ] <- c(100, -100)
  nova <- prever_fatores_cruzados(m$atributos, alterado, estimacao, estimacao, 0.1, 5, 1)
  expect_equal(nova$previstos["c03", ], base$previstos["c03", ])
  # Dentro da amostra, a previsão de c03 muda, que é o defeito da emenda 1.
  expect_false(isTRUE(all.equal(nova$dentro_da_amostra["c03", ], base$dentro_da_amostra["c03", ])))
})

test_that("quem não está na estimação recebe o mapa com toda a estimação", {
  m <- dados_do_mapa_de_teste()
  estimacao <- m$ids[1:15]
  previsao <- prever_fatores_cruzados(m$atributos, m$fatores, estimacao, estimacao, 0.1, 5, 1)
  completo <- prever_fatores(ajustar_mapeamento(m$atributos, m$fatores, estimacao, 0.1), m$atributos)

  fora <- m$ids[16:20]
  expect_equal(previsao$previstos[fora, ], completo[fora, ])
  expect_equal(previsao$dentro_da_amostra, completo)
})

test_that("a emenda 2 do conteúdo é achada pelo nome", {
  emenda <- emenda_2_do_conteudo(ler_pre_registro())
  expect_equal(emenda$previsoes_cruzadas$subgrupos, 5)
  expect_true(emenda$posterior_ao_resultado_da_emenda_1)
})
