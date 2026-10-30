test_that("estrutura de pastas do projeto existe", {
  expect_true(dir.exists(here::here("R")))
  expect_true(dir.exists(here::here("outputs", "results", "eda")))
  expect_true(dir.exists(here::here("outputs", "results", "scoring")))
  expect_true(dir.exists(here::here("outputs", "results", "optim")))
  expect_true(dir.exists(here::here("data", "synthetic")))
  expect_true(dir.exists(here::here("outputs", "figures")))
  expect_true(dir.exists(here::here("outputs", "tables")))
  expect_true(dir.exists(here::here("docs")))
})

test_that("arquivos de configuração do pipeline existem", {
  expect_true(file.exists(here::here("_targets.R")))
  expect_true(file.exists(here::here("renv.lock")))
  expect_true(file.exists(here::here("docs", "dicionario_de_dados.md")))
})
