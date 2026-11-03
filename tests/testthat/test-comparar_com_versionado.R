test_that("cada arquivo recebe a regra declarada", {
  expect_equal(regra_de_comparacao("data/synthetic/projetos.csv")$tipo, "numérica")
  expect_equal(regra_de_comparacao("outputs/tables/achados_ii6.csv")$tipo, "exata")
  expect_equal(regra_de_comparacao("outputs/tables/w3_estabilidade.csv")$tipo, "exata")
  expect_equal(regra_de_comparacao("outputs/tables/w4_conferencia.csv")$tipo, "exata")
  expect_equal(regra_de_comparacao("outputs/tables/w4_payoff.csv")$tipo, "linhas")
  expect_equal(regra_de_comparacao("outputs/tables/w4_limite_individual.csv")$tipo, "relato")
  expect_equal(regra_de_comparacao("outputs/tables/achados_w4.csv")$tipo, "relato")
})

test_that("os dados sintéticos toleram a última casa de ponto flutuante, e não mais", {
  a <- data.frame(id = c("a", "b"), x = c("0.1234567890123456", "2"))
  quase <- data.frame(id = c("a", "b"), x = c("0.1234567890123457", "2"))
  longe <- data.frame(id = c("a", "b"), x = c("0.1234", "2"))
  rotulo <- data.frame(id = c("a", "c"), x = c("0.1234567890123456", "2"))
  expect_true(comparar_numericamente(a, quase, 1e-9)$igual)
  expect_false(comparar_numericamente(a, longe, 1e-9)$igual)
  expect_false(comparar_numericamente(a, rotulo, 1e-9)$igual)
})

test_that("nas tabelas com linhas exatas, só elas precisam bater", {
  antes <- c("Solução,Afinidade", "Afinidade máxima,\"663,1\"", "Sobrecarga mínima,\"552,6\"")
  outra_sobrecarga <- c("Solução,Afinidade", "Afinidade máxima,\"663,1\"", "Sobrecarga mínima,\"540,0\"")
  outra_afinidade <- c("Solução,Afinidade", "Afinidade máxima,\"660,0\"", "Sobrecarga mínima,\"552,6\"")
  regra <- regra_de_comparacao("outputs/tables/w4_payoff.csv")
  expect_true(comparar_conteudo(antes, outra_sobrecarga, regra, 1e-9)$igual)
  expect_false(comparar_conteudo(antes, outra_afinidade, regra, 1e-9)$igual)
})
