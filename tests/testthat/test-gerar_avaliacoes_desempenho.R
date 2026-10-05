test_that("gerar_avaliacoes_desempenho produz a estrutura de 3 etapas x 14 itens", {
  consultores <- gerar_consultores(n_consultores = 100)
  projetos <- gerar_projetos(n_projetos = 100, data_referencia = as.Date("2026-01-01"))
  experiencia <- gerar_experiencia_setorial(consultores)
  alocacoes <- gerar_alocacoes(consultores, projetos, experiencia, semente = 1)

  avaliacoes <- gerar_avaliacoes_desempenho(alocacoes, fracao_com_avaliacao = 0.5, semente = 1)

  expect_named(avaliacoes, c("consultor_id", "projeto_id", "item", "categoria_item", "etapa", "nota", "avaliador_id", "comentario"))
  expect_false(anyNA(avaliacoes))
  expect_setequal(avaliacoes$etapa, c("individual", "gestor_cegas", "gestor_final"))
  expect_equal(dplyr::n_distinct(avaliacoes$item), 14)
})

test_that("cada avaliação tem exatamente 9 subcompetências e 5 itens de performance, por etapa", {
  consultores <- gerar_consultores(n_consultores = 80)
  projetos <- gerar_projetos(n_projetos = 80, data_referencia = as.Date("2026-01-01"))
  experiencia <- gerar_experiencia_setorial(consultores)
  alocacoes <- gerar_alocacoes(consultores, projetos, experiencia, semente = 2)

  avaliacoes <- gerar_avaliacoes_desempenho(alocacoes, fracao_com_avaliacao = 0.6, semente = 2)

  contagem <- avaliacoes |>
    dplyr::count(consultor_id, projeto_id, etapa, categoria_item) |>
    dplyr::distinct(categoria_item, n)

  expect_true(all(contagem$n[contagem$categoria_item == "subcompetencia"] == 9))
  expect_true(all(contagem$n[contagem$categoria_item == "performance"] == 5))
})

test_that("nota fica sempre no domínio ordinal válido", {
  consultores <- gerar_consultores(n_consultores = 80)
  projetos <- gerar_projetos(n_projetos = 80, data_referencia = as.Date("2026-01-01"))
  experiencia <- gerar_experiencia_setorial(consultores)
  alocacoes <- gerar_alocacoes(consultores, projetos, experiencia, semente = 3)

  avaliacoes <- gerar_avaliacoes_desempenho(alocacoes, semente = 3)

  expect_true(all(avaliacoes$nota %in% c("ABAIXO", "DENTRO", "ACIMA")))
})

test_that("autoavaliação é mais leniente que a avaliação às cegas do gestor", {
  consultores <- gerar_consultores(n_consultores = 200)
  projetos <- gerar_projetos(n_projetos = 200, data_referencia = as.Date("2026-01-01"))
  experiencia <- gerar_experiencia_setorial(consultores)
  alocacoes <- gerar_alocacoes(consultores, projetos, experiencia, semente = 4)

  avaliacoes <- gerar_avaliacoes_desempenho(alocacoes, fracao_com_avaliacao = 0.8, semente = 4)

  pct_acima <- avaliacoes |>
    dplyr::filter(etapa %in% c("individual", "gestor_cegas")) |>
    dplyr::summarise(pct_acima = mean(nota == "ACIMA"), .by = etapa)

  pct_individual <- pct_acima$pct_acima[pct_acima$etapa == "individual"]
  pct_gestor_cegas <- pct_acima$pct_acima[pct_acima$etapa == "gestor_cegas"]

  expect_true(pct_individual > pct_gestor_cegas)
})

test_that("fração de alocações com avaliação aproxima o parâmetro pedido", {
  consultores <- gerar_consultores(n_consultores = 300)
  projetos <- gerar_projetos(n_projetos = 300, data_referencia = as.Date("2026-01-01"))
  experiencia <- gerar_experiencia_setorial(consultores)
  alocacoes <- gerar_alocacoes(consultores, projetos, experiencia, semente = 5)

  avaliacoes <- gerar_avaliacoes_desempenho(alocacoes, fracao_com_avaliacao = 0.4, semente = 5)

  n_alocacoes_avaliadas <- dplyr::n_distinct(avaliacoes[c("consultor_id", "projeto_id")])
  expect_equal(n_alocacoes_avaliadas, round(nrow(alocacoes) * 0.4))
})

test_that("gerar_avaliacoes_desempenho é reprodutível com a mesma semente", {
  consultores <- gerar_consultores(n_consultores = 60)
  projetos <- gerar_projetos(n_projetos = 60, data_referencia = as.Date("2026-01-01"))
  experiencia <- gerar_experiencia_setorial(consultores)
  alocacoes <- gerar_alocacoes(consultores, projetos, experiencia, semente = 6)

  expect_identical(
    gerar_avaliacoes_desempenho(alocacoes, semente = 42),
    gerar_avaliacoes_desempenho(alocacoes, semente = 42)
  )
})
