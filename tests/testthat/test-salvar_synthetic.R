test_that("salva um csv legível no diretório indicado", {
  tabela <- tibble::tibble(a = 1:3, b = c("x", "y", "z"))
  diretorio_temp <- tempfile("synthetic_teste_")
  dir.create(diretorio_temp)
  on.exit(unlink(diretorio_temp, recursive = TRUE))

  caminho <- salvar_synthetic(tabela, "teste", diretorio = diretorio_temp)

  expect_true(file.exists(caminho))
  lido <- tibble::as_tibble(readr::read_csv(caminho, show_col_types = FALSE))
  expect_equal(lido, tabela)
})
