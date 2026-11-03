# O solver contra a instância pequena, cuja tabela de payoff sai à mão de
# instancia_pequena_otimizacao(), com a ordem lexicográfica registrada:
#
# * afinidade, custo, sobrecarga: (B, A, B), afinidade 2,5, custo 66.000,
#   sobrecarga 0;
# * custo, afinidade, sobrecarga: custo 41.000; entre os quatro de custo
#   mínimo, afinidade 1,5 em (A, C, C) e (C, A, C); entre eles, sobrecarga
#   0,1 em (C, A, C);
# * sobrecarga, afinidade, custo: sobrecarga 0; afinidade 2,5 em (B, A, B),
#   custo 66.000.

test_that("a tabela de payoff da instância pequena é a calculada à mão", {
  skip_if_not_installed("highs")
  problema <- montar_problema_da_otimizacao(instancia_pequena_otimizacao(), "percentual")
  payoff <- resolver_tabela_de_payoff(problema, ler_pre_registro_otimizacao())

  expect_true(all(payoff$subproblemas$status == "Optimal"))
  expect_true(avaliar_tratabilidade_do_payoff(payoff)$payoff_otima)
  expect_true(all(payoff$payoff$viavel))

  tabela <- payoff$payoff
  linha <- \(nome) unlist(tabela[tabela$linha == nome, c("afinidade", "custo", "sobrecarga")])
  expect_equal(linha("afinidade"), c(afinidade = 2.5, custo = 66000, sobrecarga = 0))
  expect_equal(linha("custo"), c(afinidade = 1.5, custo = 41000, sobrecarga = 0.1))
  expect_equal(linha("sobrecarga"), c(afinidade = 2.5, custo = 66000, sobrecarga = 0))

  ocupantes <- \(x) paste(problema$pares$consultor_id[x[seq_len(nrow(problema$pares))] > 0.5], collapse = "")
  expect_equal(ocupantes(payoff$solucoes$afinidade), "BAB")
  expect_equal(ocupantes(payoff$solucoes$custo), "CAC")
})

test_that("as opções do solver são as registradas", {
  skip_if_not_installed("highs")
  registro_otimizacao <- ler_pre_registro_otimizacao()
  controle <- controlar_solver(registro_otimizacao)
  expect_equal(controle$threads, registro_otimizacao$metodo$threads)
  expect_equal(controle$time_limit, registro_otimizacao$metodo$tempo_limite_segundos)
  expect_equal(controle$mip_rel_gap, registro_otimizacao$metodo$mip_rel_gap)
})

test_that("um problema sem solução viável não devolve solução", {
  skip_if_not_installed("highs")
  # Afinidade máxima da instância pequena é 2,5; exigir 3 torna o problema
  # inviável, e o vetor primal que o HiGHS devolve não pode virar solução.
  problema <- montar_problema_da_otimizacao(instancia_pequena_otimizacao(), "percentual")
  resolvido <- resolver_objetivo(
    problema, "custo", list(list(objetivo = "afinidade", minimo = 3, maximo = Inf)),
    controlar_solver(ler_pre_registro_otimizacao())
  )
  expect_null(resolvido$x)
  expect_true(is.na(resolvido$valor))
})
