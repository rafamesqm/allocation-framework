alocacoes_de_teste <- function() {
  tibble::tibble(
    consultor_id = c("c1", "c1", "c2", "c3"),
    projeto_id = c("p1", "p2", "p1", "p3"),
    pct_alocacao = c(0.5, 1, 1, 1),
    horas_mes = c(80, 160, 160, 160),
    data_inicio = as.Date(c("2025-01-10", "2026-08-15", "2026-09-01", "2027-03-01")),
    duracao_dias = c(20L, 60L, 10L, 30L)
  ) |>
    dplyr::mutate(data_fim = data_inicio + duracao_dias)
}

test_that("a janela é semiaberta e vai de 2024-09-01 a 2026-09-01", {
  janela <- janela_de_analise()
  expect_equal(unname(janela), as.Date(c("2024-09-01", "2026-09-01")))
  expect_named(janela, c("inicio", "fim"))
})

test_that("só fica o que começa dentro da janela, e quem atravessa o fim é censurado", {
  restrita <- restringir_a_janela(alocacoes_de_teste())

  # A que começa no próprio dia do fim já está fora, por ser semiaberta.
  expect_equal(restrita$projeto_id, c("p1", "p2"))
  expect_true(all(restrita$data_fim <= as.Date("2026-08-31")))
  expect_equal(restrita$data_fim[2], as.Date("2026-08-31"))
  expect_equal(restrita$duracao_dias, as.integer(restrita$data_fim - restrita$data_inicio))
  # Quem termina dentro da janela não muda.
  expect_equal(restrita$duracao_dias[1], 20L)
})

test_that("a ocupação da janela não tem mês além do fim", {
  dados <- list(
    alocacoes = alocacoes_de_teste(),
    projetos = alocacoes_de_teste() |> dplyr::distinct(projeto_id, .keep_all = TRUE)
  )
  com_janela <- aplicar_janela_de_analise(dados)

  expect_true(all(com_janela$ocupacao_janela$ano_mes <= "2026-08"))
  expect_equal(nrow(com_janela$alocacoes), 4)
  # Idempotente: aplicar de novo não muda nada.
  expect_identical(aplicar_janela_de_analise(com_janela)$ocupacao_janela, com_janela$ocupacao_janela)
})

test_that("com_historico_da_janela troca alocações e ocupação pelas restritas", {
  dados <- list(
    alocacoes = alocacoes_de_teste(),
    projetos = alocacoes_de_teste() |> dplyr::distinct(projeto_id, .keep_all = TRUE)
  )
  historico <- com_historico_da_janela(dados)

  expect_identical(historico$alocacoes, historico$alocacoes_janela)
  expect_identical(historico$ocupacao_consultor, historico$ocupacao_janela)
})

test_that("no conjunto versionado, nenhuma leitura de calendário passa do fim da janela", {
  dados <- aplicar_janela_de_analise(carregar_dados(tabelas = c("alocacoes", "projetos")))

  expect_lt(max(dados$alocacoes_janela$data_fim), janela_de_analise()[["fim"]])
  expect_equal(dplyr::n_distinct(dados$ocupacao_janela$ano_mes), 24)
  expect_equal(nrow(dados$projetos_janela), nrow(dados$projetos))
})

test_that("a comparação estrutural mede as sete grandezas nas duas matrizes", {
  dados <- carregar_dados(tabelas = c("alocacoes", "consultores", "projetos", "experiencia_setorial"))
  comparacao <- comparar_leituras_estruturais_na_janela(dados)

  expect_equal(nrow(comparacao), 7)
  expect_false(anyNA(comparacao))
  alocacoes <- comparacao[comparacao$medida == "Alocações", ]
  expect_equal(alocacoes$conjunto_inteiro, nrow(dados$alocacoes))
  expect_lt(alocacoes$janela, alocacoes$conjunto_inteiro)
})
