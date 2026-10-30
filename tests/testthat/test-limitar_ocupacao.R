# O limite individual da emenda 3 na instância pequena. Colunas dos pares:
# s1 A, B, C = 1, 2, 3; s2 = 4, 5, 6; s3 = 7, 8, 9. Cargas por consultor e
# mês, à mão, em fração de um mês cheio:
#
# * realizada (A, B, C): A tem 1,1 no mês 1 (0,5 fixa + 0,6); L = 1,1.
# * (B, A, B): A 1,0 e 0,5; B 0,6 e 1,0. Sem sobrecarga.
# * (A, B, B): A 1,1 no mês 1; B 1,5 no mês 2. Sobrecarga 0,1 em A e 0,5
#   em B.

test_that("o L da instância pequena é a carga de A no mês 1", {
  problema <- montar_problema_da_otimizacao(instancia_pequena_otimizacao(), "percentual")
  expect_equal(calcular_limite_individual(problema, instancia_pequena_otimizacao()), 1.1)
})

test_that("a concentração simultânea confere com as contas à mão", {
  problema <- montar_problema_da_otimizacao(instancia_pequena_otimizacao(), "percentual")
  colunas <- rbind(c(1, 5, 9), c(2, 4, 8), c(1, 5, 8))
  concentracao <- medir_concentracao_simultanea(problema, instancia_pequena_otimizacao(), colunas)
  expect_equal(concentracao$ocupacao_maxima, c(1.1, 1.0, 1.5))
  expect_equal(concentracao$meses_acima_de_2, c(0, 0, 0))
  expect_equal(concentracao$fracao_no_mais_sobrecarregado, c(1, NA, 5 / 6))
})

test_that("o limite individual mantém a realizada viável e corta quem passa dele", {
  problema <- montar_problema_da_otimizacao(instancia_pequena_otimizacao(), "percentual")
  limitado <- limitar_ocupacao(problema, 1.1)
  expect_equal(
    sum(limitado$tipo_da_restricao == "limite individual por consultor e mês"),
    sum(problema$tipo_da_restricao == "capacidade por consultor e mês")
  )
  expect_true(avaliar_solucao(limitado, solucao_realizada(limitado))$viavel)
  # (A, B, B) leva B a 1,5 no mês 2.
  expect_false(avaliar_solucao(limitado, completar_solucao(limitado, seq_len(9) %in% c(1, 5, 8)))$viavel)
})

test_that("os problemas limitados da instância pequena são os calculados à mão", {
  skip_if_not_installed("highs")
  problema <- montar_problema_da_otimizacao(instancia_pequena_otimizacao(), "percentual")
  desenho <- separar_desenho_registrado(ler_pre_registro_otimizacao())
  limitados <- resolver_problemas_limitados(limitar_ocupacao(problema, 1.1), desenho)

  expect_equal(limitados$payoff$payoff$linha, c("afinidade", "custo"))
  expect_equal(limitados$payoff$payoff$afinidade[1], 2.5)
  expect_equal(limitados$payoff$payoff$custo[2], 41000)
  expect_equal(limitados$realizada$problemas$afinidade[limitados$realizada$problemas$objetivo == "afinidade"], 2)

  # Com L = 0,9 ninguém cabe em s3, que tem carga 1,0 no mês 2.
  sem_solucao <- resolver_afinidade_com_limite(problema, 0.9, desenho)
  expect_null(sem_solucao$x)
  com_solucao <- resolver_afinidade_com_limite(problema, 1.1, desenho)
  expect_equal(com_solucao$valor, 2)
})

test_that("o L medido na realização versionada é o registrado na emenda 3", {
  montagem <- montagem_versionada_otimizacao()
  emenda <- localizar_emenda(montagem$registro_otimizacao, "emenda 3, limite individual calibrado")
  limite <- calcular_limite_individual(montagem$problema, montagem$insumos)
  expect_equal(conferir_limite_individual(limite, emenda), limite)
  expect_error(conferir_limite_individual(limite + 0.01, emenda), "não confere")
})
