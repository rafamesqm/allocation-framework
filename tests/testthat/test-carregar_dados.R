test_that("carrega todas as tabelas sintéticas do repositório", {
  dados <- carregar_dados()

  expect_type(dados, "list")
  expect_true(all(c("consultores", "projetos", "alocacoes", "espaco_skills") %in% names(dados)))
  expect_true(all(vapply(dados, tibble::is_tibble, logical(1))))
  expect_equal(names(dados), sort(names(dados)))
})

test_that("o argumento tabelas carrega só o que foi pedido", {
  dados <- carregar_dados(tabelas = c("consultores", "projetos"))

  expect_named(dados, c("consultores", "projetos"))
  expect_true(nrow(dados$consultores) > 0)
})

test_that("as colunas de data chegam como Date, não como texto", {
  dados <- carregar_dados(tabelas = c("alocacoes", "projetos"))

  expect_s3_class(dados$alocacoes$data_inicio, "Date")
  expect_s3_class(dados$alocacoes$data_fim, "Date")
  expect_s3_class(dados$projetos$data_inicio, "Date")
})

test_that("tabela inexistente falha com mensagem explícita", {
  expect_error(carregar_dados(tabelas = "nao_existe"), "não encontrada")
})

test_that("fonte databricks para de forma explícita, não devolve sintético calado", {
  expect_error(carregar_dados(fonte = "databricks"), "não está implementada")
})

test_that("fonte desconhecida é rejeitada", {
  expect_error(carregar_dados(fonte = "csv_solto"))
})

test_that("diretório sem csv falha com mensagem acionável", {
  diretorio_vazio <- tempfile("fonte_vazia_")
  dir.create(diretorio_vazio)
  on.exit(unlink(diretorio_vazio, recursive = TRUE))

  expect_error(carregar_dados(diretorio = diretorio_vazio), "tar_make")
})

test_that("par consultor e projeto repetido para a leitura", {
  diretorio <- withr::local_tempdir()
  alocacoes <- tibble::tibble(
    consultor_id = c("consultor_0001", "consultor_0001", "consultor_0002"),
    projeto_id = c("projeto_0001", "projeto_0001", "projeto_0001")
  )
  readr::write_csv(alocacoes, file.path(diretorio, "alocacoes.csv"))

  expect_error(carregar_dados(diretorio = diretorio), "repetido")
})

test_that("as alocações sintéticas não repetem par consultor e projeto", {
  expect_no_error(carregar_dados(tabelas = "alocacoes"))
})
