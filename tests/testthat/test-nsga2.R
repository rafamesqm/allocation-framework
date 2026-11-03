# O NSGA-II da emenda 1, peça por peça e inteiro, contra a instância pequena
# cuja fronteira se conhece por enumeração.

test_that("a ordenação não dominada separa as fronteiras conhecidas", {
  pontos <- rbind(c(1, 4), c(2, 2), c(4, 1), c(3, 3), c(5, 5))
  fronteira <- ordenar_nao_dominados(calcular_dominacao(pontos, rep(0, 5)))
  expect_equal(fronteira, c(1, 1, 1, 2, 3))
})

test_that("a dominação com restrições põe o viável à frente e a menor violação antes", {
  pontos <- rbind(c(0, 0), c(9, 9), c(1, 1))
  dominacao <- calcular_dominacao(pontos, c(2, 0, 1))
  expect_true(dominacao[2, 1])
  expect_true(dominacao[2, 3])
  expect_true(dominacao[3, 1])
  expect_equal(ordenar_nao_dominados(dominacao), c(3, 1, 2))
})

test_that("a distância de aglomeração é a de Deb et al. (2002)", {
  pontos <- rbind(c(1, 4), c(2, 2), c(4, 1))
  expect_equal(calcular_aglomeracao(pontos), c(Inf, 2, Inf))
  expect_equal(calcular_aglomeracao(pontos[1:2, ]), c(Inf, Inf))
})

test_that("o hipervolume confere com volumes calculados à mão", {
  expect_equal(calcular_hipervolume(rbind(c(0.5, 0.5, 0.5))), 0.125)
  # Duas caixas de 0,25 com interseção de 0,125; o ponto fora da referência não conta.
  pontos <- rbind(c(0, 0.5, 0.5), c(0.5, 0, 0.5), c(1.2, 0, 0))
  expect_equal(calcular_hipervolume(pontos), 0.375)
  expect_equal(calcular_hipervolume(rbind(c(1, 0, 0))), 0)
})

test_that("a avaliação da população coincide com avaliar_solucao()", {
  problema <- montar_problema_da_otimizacao(instancia_pequena_otimizacao(), "percentual")
  codificacao <- preparar_codificacao(problema)
  genes <- as.matrix(expand.grid(1:3, 1:3, 1:3))
  avaliacao <- avaliar_populacao(codificacao, genes)

  for (i in seq_len(nrow(genes))) {
    colunas <- genes[i, ] + codificacao$inicio - 1L
    referencia <- avaliar_solucao(problema, completar_solucao(problema, seq_len(nrow(problema$pares)) %in% colunas))
    expect_equal(unname(avaliacao$objetivos[i, ]), c(referencia$afinidade, referencia$custo, referencia$sobrecarga_atribuivel))
    expect_equal(avaliacao$violacao[i] == 0, referencia$viavel)
  }

  # Na realização versionada, para a realizada e indivíduos ao acaso.
  montagem <- montagem_versionada_otimizacao()
  codificacao <- preparar_codificacao(montagem$problema)
  sorteados <- withr::with_seed(1, t(replicate(5, ceiling(runif(codificacao$n_assentos) * codificacao$n_candidatos))))
  genes <- rbind(codificacao$gene_realizado, sorteados)
  avaliacao <- avaliar_populacao(codificacao, genes)
  for (i in seq_len(nrow(genes))) {
    colunas <- genes[i, ] + codificacao$inicio - 1L
    referencia <- avaliar_solucao(
      montagem$problema, completar_solucao(montagem$problema, seq_len(nrow(montagem$problema$pares)) %in% colunas)
    )
    expect_equal(unname(avaliacao$objetivos[i, ]), c(referencia$afinidade, referencia$custo, referencia$sobrecarga_atribuivel))
  }
  # O primeiro indivíduo é a realizada, viável.
  expect_equal(avaliacao$violacao[1], 0)
})

test_that("o reparo desfaz a duplicidade quando há candidato livre", {
  problema <- montar_problema_da_otimizacao(instancia_pequena_otimizacao(), "percentual")
  codificacao <- preparar_codificacao(problema)
  # A em s1 e em s2, do mesmo projeto: s1 mantém, s2 é reamostrado entre B e C.
  reparado <- withr::with_seed(1, reparar_individuo(codificacao, c(1L, 1L, 3L)))
  expect_equal(reparado[1], 1L)
  expect_true(reparado[2] %in% 2:3)
  expect_equal(avaliar_populacao(codificacao, rbind(reparado))$violacao, 0)
})

test_that("o NSGA-II acha a fronteira enumerada da instância pequena", {
  problema <- montar_problema_da_otimizacao(instancia_pequena_otimizacao(), "percentual")
  solucoes <- enumerar_solucoes(problema)
  viaveis <- as.matrix(solucoes[solucoes$viavel, c("afinidade", "custo", "sobrecarga")])
  nao_dominados <- ordenar_nao_dominados(calcular_dominacao(para_minimizacao(viaveis), rep(0, nrow(viaveis)))) == 1
  enumerada <- unique(round(viaveis[nao_dominados, , drop = FALSE], 9))

  execucao <- executar_nsga2(problema, tamanho = 20, geracoes = 30, probabilidade_de_cruzamento = 0.9,
                             probabilidade_de_mutacao = 1 / 3, semente = 1, registrar_a_cada = 10)
  encontrada <- as.matrix(extrair_fronteira(execucao)$objetivos)
  ordenar <- \(m) unname(m[do.call(order, as.data.frame(m)), , drop = FALSE])
  expect_equal(ordenar(round(encontrada, 9)), ordenar(enumerada))
  expect_length(execucao$historico, 3)
})

test_that("a mesma semente reproduz a execução", {
  problema <- montar_problema_da_otimizacao(instancia_pequena_otimizacao(), "percentual")
  rodar <- \(semente) executar_nsga2(problema, 10, 5, 0.9, 1 / 3, semente)$genes
  expect_identical(rodar(7), rodar(7))
})

test_that("a execução registrada recusa semente fora da emenda", {
  emenda <- localizar_emenda(ler_pre_registro_otimizacao(), "emenda 1, troca para o NSGA-II")
  problema <- montar_problema_da_otimizacao(instancia_pequena_otimizacao(), "percentual")
  expect_error(executar_nsga2_registrado(problema, emenda, 1), "fora das registradas")
})
