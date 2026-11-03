# A avaliação da emenda 2 na instância pequena. A realizada é (A, B, C):
# afinidade 2,0, custo 57.000, sobrecarga 0,1. Com os outros dois objetivos
# presos nela, à mão:
#
# * afinidade máxima 2,0: as de 2,5 custam 66.000 ou mais;
# * custo mínimo 51.000, em (B, A, C);
# * sobrecarga mínima 0, em (B, A, C).

test_that("os problemas do item 10.c da instância pequena são os calculados à mão", {
  skip_if_not_installed("highs")
  problema <- montar_problema_da_otimizacao(instancia_pequena_otimizacao(), "percentual")
  exatos <- resolver_problemas_da_realizada(problema, ler_pre_registro_otimizacao())
  linha <- \(o) exatos$problemas[exatos$problemas$objetivo == o, ]

  expect_true(all(exatos$problemas$status == "Optimal"))
  expect_equal(linha("afinidade")$afinidade, 2)
  expect_equal(linha("custo")$custo, 51000)
  expect_equal(linha("sobrecarga")$sobrecarga, 0)
})

test_that("o hipervolume na caixa da realizada vai de zero a um", {
  ideal <- c(afinidade = 10, custo = 0, sobrecarga = 0)
  realizada <- c(afinidade = 0, custo = 10, sobrecarga = 10)
  ponto <- \(a, c, s) cbind(afinidade = a, custo = c, sobrecarga = s)

  expect_equal(calcular_hipervolume_da_realizada(ponto(10, 0, 0), ideal, realizada), 1)
  expect_equal(calcular_hipervolume_da_realizada(ponto(5, 5, 5), ideal, realizada), 0.125)
  expect_equal(calcular_hipervolume_da_realizada(ponto(0, 10, 10), ideal, realizada), 0)
})

test_that("a ocupação conta zona morta, populares e repetição", {
  problema <- montar_problema_da_otimizacao(instancia_pequena_otimizacao(), "percentual")
  problema$pares$origem[problema$pares$consultor_id == "C"] <- "zona morta"
  estimacao <- tibble::tibble(consultor_id = c("B", "B", "A"))
  # (A, B, B): nenhum da zona morta, todos entre os populares, B em dois assentos.
  colunas <- rbind(c(1, 5, 8), c(3, 4, 9))
  ocupacao <- descrever_ocupacao(problema, colunas, estimacao)
  expect_equal(ocupacao$assentos_da_zona_morta, c(0, 2))
  expect_equal(ocupacao$maior_numero_de_assentos, c(2, 2))
  expect_equal(ocupacao$fracao_nos_10_mais_populares, c(1, 1 / 3))
})
