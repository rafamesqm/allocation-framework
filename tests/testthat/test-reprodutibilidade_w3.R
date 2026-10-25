# Cada emenda do pré-registro invalida todos os alvos da wave 3, porque
# todos dependem dele, e as realizações que a emenda não toca são refeitas.
# Elas precisam sair idênticas às anteriores. As referências em
# `referencias/` foram guardadas a partir do store antes da emenda 2 do
# conteúdo, e este teste falha se alguma grandeza mudar além do
# arredondamento da ida e volta pelo CSV.

comparar_com_referencia <- function(alvo) {
  store <- here::here("_targets")
  atual <- as.data.frame(targets::tar_read_raw(alvo, store = store))
  referencia <- as.data.frame(readr::read_csv(
    testthat::test_path("referencias", paste0(alvo, ".csv")), show_col_types = FALSE
  ))

  expect_identical(names(atual), names(referencia), info = alvo)
  # A semente sai inteira do store e em ponto flutuante do CSV: compara o
  # valor, e não o tipo.
  expect_identical(as.integer(atual$semente), as.integer(referencia$semente), info = alvo)
  for (coluna in setdiff(names(referencia), "semente")) {
    expect_equal(atual[[coluna]], referencia[[coluna]], tolerance = 1e-12, info = paste(alvo, coluna))
  }
}

test_that("as realizações da emenda 1 do conteúdo se reproduzem", {
  skip_if_not(dir.exists(here::here("_targets")), "store do targets ausente")
  comparar_com_referencia("w3_conteudo_replicas")
})

test_that("as realizações do colaborativo se reproduzem", {
  skip_if_not(dir.exists(here::here("_targets")), "store do targets ausente")
  comparar_com_referencia("w3_colaborativo_replicas")
})

test_that("as realizações do segundo nível se reproduzem", {
  skip_if_not(dir.exists(here::here("_targets")), "store do targets ausente")
  comparar_com_referencia("w3_segundo_nivel_replicas")
})

test_that("os achados da emenda 1 ficam identificados e o W3.2.1 não decide", {
  skip_if_not(dir.exists(here::here("_targets")), "store do targets ausente")
  achados <- targets::tar_read(bloco_w3_conteudo, store = here::here("_targets"))$achados

  expect_true(all(startsWith(achados$enunciado, "**Análise conforme a emenda 1.**")))
  expect_true(is.na(achados$decisao[achados$codigo == "W3.2.1"]))
  expect_match(achados$enunciado[achados$codigo == "W3.2.1"], "p = 0,154", fixed = TRUE)
})
