test_that("o contrato cobre todas as tabelas versionadas, sem coluna solta", {
  especificacao <- especificacao_tabelas()
  dados <- carregar_dados()

  expect_setequal(unique(especificacao$tabela), names(dados))
  expect_false(any(duplicated(especificacao[c("tabela", "coluna")])))
  expect_true(all(especificacao$tipo %in% c("texto", "numerico", "data", "logico")))
  expect_true(all(especificacao$papel %in% c("chave", "atributo")))
})

test_that("toda tabela do contrato declara ao menos uma coluna de chave", {
  especificacao <- especificacao_tabelas()

  com_chave <- especificacao |>
    dplyr::summarise(tem_chave = any(papel == "chave"), .by = tabela)

  expect_true(all(com_chave$tem_chave))
})

test_that("toda referência aponta para uma coluna que existe no contrato", {
  especificacao <- especificacao_tabelas()
  declaradas <- paste(especificacao$tabela, especificacao$coluna, sep = ".")
  referencias <- stats::na.omit(especificacao$referencia)

  expect_true(all(referencias %in% declaradas))
})

test_that("classificar_tipo traduz as classes do R para o vocabulário do contrato", {
  expect_equal(classificar_tipo(c("a", "b")), "texto")
  expect_equal(classificar_tipo(factor("a")), "texto")
  expect_equal(classificar_tipo(1:3), "numerico")
  expect_equal(classificar_tipo(c(1.5, 2.5)), "numerico")
  expect_equal(classificar_tipo(Sys.Date()), "data")
  expect_equal(classificar_tipo(c(TRUE, FALSE)), "logico")
})

test_that("o dado sintético versionado está conforme o contrato", {
  # É o teste que dá conteúdo ao bloco I.1 do capítulo: se falhar, ou o
  # gerador mudou o esquema sem atualizar o dicionário, ou o contrário.
  divergencias <- verificar_conformidade(carregar_dados())

  expect_equal(nrow(divergencias), 0)
})

test_that("verificar_conformidade acusa coluna ausente, extra e de tipo errado", {
  especificacao <- tibble::tribble(
    ~tabela, ~coluna, ~tipo, ~papel, ~referencia,
    "t", "id", "texto", "chave", NA_character_,
    "t", "valor", "numerico", "atributo", NA_character_,
    "t", "quando", "data", "atributo", NA_character_
  )
  dados <- list(t = tibble::tibble(
    id = "a",
    valor = "isto deveria ser numerico",
    extra = 1
  ))

  divergencias <- verificar_conformidade(dados, especificacao)

  expect_setequal(
    divergencias$tipo_divergencia,
    c("coluna ausente", "coluna fora do contrato", "tipo divergente")
  )
  expect_equal(divergencias$coluna[divergencias$tipo_divergencia == "coluna ausente"], "quando")
  expect_equal(divergencias$coluna[divergencias$tipo_divergencia == "coluna fora do contrato"], "extra")
})

test_that("verificar_conformidade acusa tabela do contrato que não veio na fonte", {
  especificacao <- tibble::tribble(
    ~tabela, ~coluna, ~tipo, ~papel, ~referencia,
    "presente", "id", "texto", "chave", NA_character_,
    "sumida", "id", "texto", "chave", NA_character_
  )
  dados <- list(presente = tibble::tibble(id = "a"))

  divergencias <- verificar_conformidade(dados, especificacao)

  expect_equal(divergencias$tipo_divergencia, "tabela ausente")
  expect_equal(divergencias$tabela, "sumida")
})

test_that("o dado sintético versionado tem integridade referencial completa", {
  integridade <- verificar_integridade_referencial(carregar_dados())

  expect_true(all(integridade$integra))
  expect_true(nrow(integridade) > 0)
})

test_that("verificar_integridade_referencial conta os órfãos que existem", {
  especificacao <- tibble::tribble(
    ~tabela, ~coluna, ~tipo, ~papel, ~referencia,
    "pai", "id", "texto", "chave", NA_character_,
    "filho", "pai_id", "texto", "chave", "pai.id"
  )
  dados <- list(
    pai = tibble::tibble(id = c("a", "b")),
    filho = tibble::tibble(pai_id = c("a", "b", "fantasma", "outro_fantasma"))
  )

  integridade <- verificar_integridade_referencial(dados, especificacao)

  expect_equal(integridade$orfaos, 2)
  expect_false(integridade$integra)
  expect_equal(integridade$valores_distintos, 4)
})

test_that("inventariar_tabelas confere a granularidade declarada contra o dado", {
  especificacao <- tibble::tribble(
    ~tabela, ~coluna, ~tipo, ~papel, ~referencia,
    "unica", "id", "texto", "chave", NA_character_,
    "repetida", "id", "texto", "chave", NA_character_
  )
  dados <- list(
    unica = tibble::tibble(id = c("a", "b", "c")),
    repetida = tibble::tibble(id = c("a", "a", "b"))
  )

  inventario <- inventariar_tabelas(dados, especificacao)

  expect_true(inventario$chave_unica[inventario$tabela == "unica"])
  expect_false(inventario$chave_unica[inventario$tabela == "repetida"])
  expect_equal(inventario$linhas_duplicadas[inventario$tabela == "repetida"], 1)
})

test_that("toda chave declarada é de fato única no dado sintético", {
  inventario <- inventariar_tabelas(carregar_dados())

  expect_true(all(inventario$chave_unica))
  expect_equal(nrow(inventario), 18)
})

test_that("resumir_cobertura mede entidade sem registro, não célula vazia", {
  especificacao <- tibble::tribble(
    ~tabela, ~coluna, ~tipo, ~papel, ~referencia,
    "pessoas", "id", "texto", "chave", NA_character_,
    "satelite", "pessoa_id", "texto", "chave", "pessoas.id"
  )
  dados <- list(
    pessoas = tibble::tibble(id = c("a", "b", "c", "d")),
    satelite = tibble::tibble(pessoa_id = c("a", "a", "b"))
  )

  cobertura <- resumir_cobertura(dados, especificacao)

  expect_equal(cobertura$com_registro, 2)
  expect_equal(cobertura$total, 4)
  expect_equal(cobertura$sem_registro, 2)
  expect_equal(cobertura$cobertura, 0.5)
})

test_that("a figura de cobertura nomeia a entidade de destino em cada barra", {
  # Sem isso as barras parecem comparáveis e não são: cada uma tem seu
  # próprio denominador.
  cobertura <- tibble::tibble(
    tabela = c("satelite_a", "satelite_b"),
    entidade = c("consultores.consultor_id", "projetos.projeto_id"),
    coluna = c("consultor_id", "projeto_id"),
    com_registro = c(2L, 3L), total = c(4L, 4L),
    sem_registro = c(2L, 1L), cobertura = c(0.5, 0.75)
  )

  figura <- figura_cobertura_entidades(cobertura)
  rotulos <- as.character(figura$data$rotulo)

  expect_true(any(grepl("consultores", rotulos)))
  expect_true(any(grepl("projetos", rotulos)))
  expect_no_error(ggplot2::ggplot_build(figura))
})
