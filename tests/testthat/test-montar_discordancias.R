test_that("a lista de recomendados põe o filtro antes e desempata pela chave", {
  consultores <- paste0("c", 1:4)
  contexto <- list(
    consultores = consultores, projetos = "p1", lista = 3,
    filtro = matrix(c(TRUE, TRUE, FALSE, TRUE), 4, 1, dimnames = list(consultores, "p1")),
    chave = matrix(c(0.4, 0.1, 0.05, 0.3), 4, 1, dimnames = list(consultores, "p1"))
  )
  # Sem escore: filtrados pela chave (c2, c4, c1), depois c3.
  expect_equal(ordenar_recomendados("p1", contexto), c("c2", "c4", "c1"))

  escores <- matrix(c(5, 0, 9, 1), 4, 1, dimnames = dimnames(contexto$filtro))
  # Com escore: c1 sobe; c3, fora do filtro, não passa à frente apesar do 9.
  expect_equal(ordenar_recomendados("p1", contexto, escores), c("c1", "c4", "c2"))
})

test_that("o desdobramento por critério conta quem vence, com desconhecido à parte", {
  classificadas <- tibble::tibble(
    ramo = c("conteúdo", "conteúdo"),
    folga_recomendado = c(10, 5), folga_realizado = c(5, 5),
    custo_recomendado = c(100, 200), custo_realizado = c(150, 100),
    setor_recomendado = c("compatível", "desconhecido"), setor_realizado = c("incompatível", "compatível"),
    senioridade_recomendado = c(1, 0), senioridade_realizado = c(0, 0)
  )
  tabela <- desdobrar_criterios_por_ramo(classificadas)
  linha <- \(cr) tabela[tabela$criterio == cr, ]

  expect_equal(linha("folga")$recomendado, 0.5)
  expect_equal(linha("folga")$empate, 0.5)
  expect_equal(linha("custo")$recomendado, 0.5)
  expect_equal(linha("custo")$realizado, 0.5)
  expect_equal(linha("setor")$desconhecido, 0.5)
  expect_equal(linha("senioridade")$realizado, 0.5)
})

test_that("as discordâncias da pipeline comparam com quem não estava no time", {
  skip_if_not(dir.exists(here::here("_targets")), "store do targets ausente")
  plausibilidade <- targets::tar_read(w3_plausibilidade, store = here::here("_targets"))
  classificadas <- plausibilidade$classificadas

  expect_false(any(classificadas$consultor_recomendado == classificadas$consultor_realizado))
  expect_setequal(unique(classificadas$ramo), c("conteúdo", "colaborativo"))
  expect_false(any(plausibilidade$sorteio$casos$na_zona_morta))
})

test_that("a planilha da rubrica sai em branco", {
  skip_if_not(dir.exists(here::here("_targets")), "store do targets ausente")
  rubrica <- targets::tar_read(w3_plausibilidade, store = here::here("_targets"))$rubrica
  campos <- c("disponibilidade_do_recomendado", "compatibilidade_de_setor", "justificativa_observavel", "veredito")
  for (campo in campos) expect_true(all(is.na(rubrica[[campo]])), info = campo)
})
