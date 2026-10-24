test_that("resumir_janela_temporal encontra os campos de data pelo contrato", {
  especificacao <- tibble::tribble(
    ~tabela, ~coluna, ~tipo, ~papel, ~referencia,
    "t", "id", "texto", "chave", NA_character_,
    "t", "quando", "data", "atributo", NA_character_,
    "t", "quanto", "numerico", "atributo", NA_character_
  )
  dados <- list(t = tibble::tibble(
    id = c("a", "b"),
    quando = as.Date(c("2026-01-01", "2026-03-02")),
    quanto = c(1, 2)
  ))

  janela <- resumir_janela_temporal(dados, especificacao)

  expect_equal(nrow(janela), 1)
  expect_equal(janela$coluna, "quando")
  expect_equal(janela$amplitude_dias, 60L)
})

test_that("serie_mensal_eventos preenche com zero o mes sem evento", {
  # Serie que pula mes vazio desenha uma continuidade que nao existe.
  datas <- as.Date(c("2026-01-15", "2026-01-20", "2026-04-02"))

  serie <- serie_mensal_eventos(datas)

  expect_equal(nrow(serie), 4)
  expect_equal(serie$n, c(2L, 0L, 0L, 1L))
})

test_that("serie_vigencias_mensais conta o intervalo em todo mes que ele cobre", {
  # Diferente de evento pontual: uma alocacao de tres meses aparece em tres.
  vigencias <- serie_vigencias_mensais(
    as.Date(c("2026-01-20", "2026-03-01")),
    as.Date(c("2026-03-10", "2026-03-31"))
  )

  expect_equal(format(vigencias$mes), c("2026-01-01", "2026-02-01", "2026-03-01"))
  expect_equal(vigencias$n_vigentes, c(1L, 1L, 2L))
})

test_that("o esperado da sazonalidade e ponderado pelos dias de cada mes", {
  # Testar contra doze proporcoes iguais acusaria sazonalidade onde so ha
  # calendario: fevereiro tem menos dias e a janela raramente cobre um
  # numero inteiro de anos.
  datas <- seq(as.Date("2026-01-01"), as.Date("2026-12-31"), by = "day")

  resultado <- testar_sazonalidade_mensal(datas)
  esperado_por_mes <- resultado$contagens$esperado

  expect_equal(esperado_por_mes[1], 31)
  expect_equal(esperado_por_mes[2], 28)
  expect_equal(esperado_por_mes[4], 30)
  expect_equal(sum(esperado_por_mes), length(datas))
})

test_that("a razao observado sobre esperado fica em torno de 1 sob uniformidade", {
  # Regressao: a razao era dividida por sum(observado) duas vezes, porque
  # dentro de tibble() a coluna `esperado` sombreava a variavel local de
  # mesmo nome. Saia na ordem de 0,001 em vez de 1.
  datas <- seq(as.Date("2026-01-01"), as.Date("2026-12-31"), by = "day")

  resultado <- testar_sazonalidade_mensal(datas)

  expect_true(all(abs(resultado$contagens$razao - 1) < 0.01))
  expect_equal(
    resultado$contagens$razao,
    resultado$contagens$observado / resultado$contagens$esperado
  )
})

test_that("a sazonalidade acusa concentracao real quando ela existe", {
  set.seed(1)
  datas <- c(
    as.Date("2026-01-01") + sample.int(28, 200, replace = TRUE),
    as.Date("2026-01-01") + sample.int(364, 100, replace = TRUE)
  )

  resultado <- testar_sazonalidade_mensal(datas)

  expect_lt(resultado$valor_p, 0.001)
  expect_gt(resultado$contagens$razao[1], 2)
})

test_that("executar_i4 devolve as quatro partes do bloco", {
  dados <- carregar_dados(tabelas = c(
    "projetos", "alocacoes", "ocupacao_consultor", "skills_consultor",
    "ferias_consultor", "consultores", "experiencia_setorial"
  ))

  bloco <- executar_i4(dados)

  expect_named(bloco, c("objetos", "tabelas", "figuras", "achados"))
  expect_equal(nrow(bloco$tabelas$i4_janela_estruturais), 7)
  expect_equal(nrow(bloco$objetos$vigencias), 24)
  expect_true(all(vapply(bloco$figuras, ggplot2::is_ggplot, logical(1))))
  expect_equal(nrow(bloco$achados), 4)
})
