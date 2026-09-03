test_that("produz a tabela de comparação com o formato esperado", {
  comparacao <- validar_utilidade_sintetico(semente = 1)

  expect_named(comparacao, c("categoria", "medida", "real", "sintetico", "diferenca_relativa"))
  expect_false(anyNA(comparacao))
  expect_setequal(comparacao$categoria, c("geral", "especifica"))
})

test_that("correlações sintéticas têm o mesmo sinal das reais", {
  comparacao <- validar_utilidade_sintetico(semente = 2)

  correlacoes <- comparacao[grepl("correlação", comparacao$medida), ]
  expect_true(all(sign(correlacoes$real) == sign(correlacoes$sintetico)))
})

test_that("cold start e densidade ficam na ordem de grandeza real", {
  comparacao <- validar_utilidade_sintetico(semente = 3)

  cold_start <- comparacao$sintetico[comparacao$medida == "fração de cold start (consultor)"]
  densidade <- comparacao$sintetico[comparacao$medida == "densidade de R"]

  expect_true(cold_start > 0.05 && cold_start < 0.35)
  expect_true(densidade > 0 && densidade < 0.05)
})

test_that("viés de seleção plantado supera a taxa esperada ao acaso", {
  comparacao <- validar_utilidade_sintetico(semente = 4)

  linha <- comparacao[comparacao$medida == "taxa de pareamento por mesmo setor", ]
  expect_true(linha$sintetico > linha$real)
})
