test_that("margem estrutural bate com valor_contrato menos custo alocado", {
  consultores <- gerar_consultores(n_consultores = 150)
  projetos <- gerar_projetos(n_projetos = 150, data_referencia = as.Date("2026-01-01"))
  experiencia <- gerar_experiencia_setorial(consultores)
  alocacoes <- gerar_alocacoes(consultores, projetos, experiencia, semente = 1)
  custo_carregado <- gerar_custo_carregado(consultores)

  margem <- gerar_margem_estrutural(alocacoes, custo_carregado, projetos)

  expect_named(margem, c("projeto_id", "valor_contrato", "custo_total", "margem_estrutural"))
  expect_equal(nrow(margem), nrow(projetos))
  expect_false(anyNA(margem))
  expect_equal(margem$margem_estrutural, margem$valor_contrato - margem$custo_total)
})

test_that("projeto sem alocação (cold start) tem custo total zero", {
  consultores <- gerar_consultores(n_consultores = 150)
  projetos <- gerar_projetos(n_projetos = 150, data_referencia = as.Date("2026-01-01"))
  experiencia <- gerar_experiencia_setorial(consultores)
  alocacoes <- gerar_alocacoes(consultores, projetos, experiencia, semente = 1)
  custo_carregado <- gerar_custo_carregado(consultores)

  margem <- gerar_margem_estrutural(alocacoes, custo_carregado, projetos)

  projetos_sem_alocacao <- setdiff(projetos$projeto_id, alocacoes$projeto_id)
  expect_true(length(projetos_sem_alocacao) > 0)

  custo_desses <- margem$custo_total[margem$projeto_id %in% projetos_sem_alocacao]
  expect_true(all(custo_desses == 0))
})
