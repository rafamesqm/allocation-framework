test_that("a matriz tem a dimensão do grid completo, não só de quem foi alocado", {
  consultores <- gerar_consultores(n_consultores = 120)
  projetos <- gerar_projetos(n_projetos = 200, data_referencia = as.Date("2026-01-01"))
  experiencia <- gerar_experiencia_setorial(consultores)
  alocacoes <- gerar_alocacoes(consultores, projetos, experiencia, semente = 1)

  matriz_r <- construir_matriz_r(alocacoes, consultores, projetos)

  expect_s4_class(matriz_r, "dgCMatrix")
  expect_equal(dim(matriz_r), c(nrow(consultores), nrow(projetos)))
  expect_equal(rownames(matriz_r), consultores$consultor_id)
  expect_equal(colnames(matriz_r), projetos$projeto_id)
})

test_that("cada alocação vira uma célula preenchida, e só ela", {
  consultores <- gerar_consultores(n_consultores = 120)
  projetos <- gerar_projetos(n_projetos = 200, data_referencia = as.Date("2026-01-01"))
  experiencia <- gerar_experiencia_setorial(consultores)
  alocacoes <- gerar_alocacoes(consultores, projetos, experiencia, semente = 2)

  matriz_r <- construir_matriz_r(alocacoes, consultores, projetos)

  expect_equal(Matrix::nnzero(matriz_r), nrow(alocacoes))
  expect_true(all(matriz_r@x == 1))
})

test_that("consultor em cold start fica como linha inteiramente vazia", {
  consultores <- gerar_consultores(n_consultores = 120)
  projetos <- gerar_projetos(n_projetos = 200, data_referencia = as.Date("2026-01-01"))
  experiencia <- gerar_experiencia_setorial(consultores)
  alocacoes <- gerar_alocacoes(consultores, projetos, experiencia, semente = 3)

  matriz_r <- construir_matriz_r(alocacoes, consultores, projetos)
  grau_por_linha <- Matrix::rowSums(matriz_r != 0)
  sem_alocacao <- setdiff(consultores$consultor_id, alocacoes$consultor_id)

  expect_true(length(sem_alocacao) > 0)
  expect_true(all(grau_por_linha[sem_alocacao] == 0))
})

test_that("o valor da célula pode ser ponderado por uma variável da alocação", {
  consultores <- gerar_consultores(n_consultores = 80)
  projetos <- gerar_projetos(n_projetos = 120, data_referencia = as.Date("2026-01-01"))
  experiencia <- gerar_experiencia_setorial(consultores)
  alocacoes <- gerar_alocacoes(consultores, projetos, experiencia, semente = 4)

  matriz_r <- construir_matriz_r(
    alocacoes, consultores, projetos,
    valor = alocacoes$pct_alocacao
  )

  expect_equal(sum(matriz_r), sum(alocacoes$pct_alocacao))
  expect_equal(Matrix::nnzero(matriz_r), nrow(alocacoes))
})

test_that("par repetido é rejeitado, não somado em silêncio", {
  consultores <- tibble::tibble(consultor_id = c("consultor_0001", "consultor_0002"))
  projetos <- tibble::tibble(projeto_id = c("projeto_0001", "projeto_0002"))
  alocacoes <- tibble::tibble(
    consultor_id = c("consultor_0001", "consultor_0001"),
    projeto_id = c("projeto_0001", "projeto_0001")
  )

  expect_error(construir_matriz_r(alocacoes, consultores, projetos), "repetido")
})

test_that("alocação com id órfão é rejeitada", {
  consultores <- tibble::tibble(consultor_id = "consultor_0001")
  projetos <- tibble::tibble(projeto_id = "projeto_0001")
  alocacoes <- tibble::tibble(
    consultor_id = "consultor_9999",
    projeto_id = "projeto_0001"
  )

  expect_error(construir_matriz_r(alocacoes, consultores, projetos), "não existe")
})

test_that("a densidade calculada da matriz bate com a contagem de alocações", {
  dados <- carregar_dados(tabelas = c("consultores", "projetos", "alocacoes"))

  matriz_r <- construir_matriz_r(dados$alocacoes, dados$consultores, dados$projetos)

  expect_equal(
    Matrix::nnzero(matriz_r) / prod(dim(matriz_r)),
    nrow(dados$alocacoes) / (nrow(dados$consultores) * nrow(dados$projetos))
  )
})
