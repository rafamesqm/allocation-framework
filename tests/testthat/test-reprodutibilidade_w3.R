# Cada emenda do pré-registro invalida todos os alvos da wave 3, porque
# todos dependem dele, e as realizações que a emenda não toca são refeitas.
# Elas precisam sair idênticas às anteriores. As referências em
# `referencias/` foram guardadas a partir do store antes da emenda 2 do
# conteúdo, no Windows. As contagens precisam bater exatamente. As grandezas
# contínuas são conferidas com tolerância relativa de 1e-5: no Linux da
# integração contínua, a correlação de Spearman que sai dos escores da
# fatoração difere da referência na sétima casa, por diferença de ponto
# flutuante na álgebra linear, e uma mudança real de modelo moveria esses
# valores muito mais que isso.

comparar_com_referencia <- function(alvo) {
  skip_if(realizacoes_w3() != realizacoes_registradas_w3(), "corrida reduzida: W3_REALIZACOES")
  store <- here::here("_targets")
  atual <- as.data.frame(targets::tar_read_raw(alvo, store = store))
  referencia <- as.data.frame(readr::read_csv(
    testthat::test_path("referencias", paste0(alvo, ".csv")), show_col_types = FALSE
  ))

  # A revisão acrescentou colunas descritivas às realizações; as que já
  # existiam precisam continuar presentes e iguais.
  expect_true(all(names(referencia) %in% names(atual)), info = alvo)
  # A semente sai inteira do store e em ponto flutuante do CSV: compara o
  # valor, e não o tipo.
  expect_identical(as.integer(atual$semente), as.integer(referencia$semente), info = alvo)
  for (coluna in setdiff(names(referencia), "semente")) {
    # Coluna de valores todos inteiros é contagem ou indicadora, e compara
    # exata; as demais são contínuas.
    valores <- referencia[[coluna]]
    if (all(is.na(valores) | valores == round(valores))) {
      expect_identical(as.numeric(atual[[coluna]]), as.numeric(valores), info = paste(alvo, coluna))
    } else {
      expect_equal(atual[[coluna]], valores, tolerance = 1e-5, info = paste(alvo, coluna))
    }
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
