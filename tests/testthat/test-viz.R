test_that("a paleta entrega os quatro papéis de cor esperados", {
  paleta <- paleta_monografia()

  expect_named(paleta, c("categorica", "ordinal_senioridade", "divergente_nota", "chrome"))
  expect_length(paleta$categorica, 5)
  expect_true(all(grepl("^#[0-9a-f]{6}$", unlist(paleta), ignore.case = TRUE)))
})

test_that("a rampa ordinal cobre os cinco níveis de senioridade, da base ao topo", {
  paleta <- paleta_monografia()

  expect_named(paleta$ordinal_senioridade, c("N4", "N3", "N2", "N1", "N0"))
  expect_length(unique(paleta$ordinal_senioridade), 5)
})

test_that("a rampa ordinal escurece de forma monotônica", {
  # É a luminosidade que carrega a ordem de senioridade, e é o que sobrevive
  # à impressão em preto e branco. Se deixar de ser monotônica, a figura
  # perde a informação principal.
  paleta <- paleta_monografia()
  luminosidade <- apply(grDevices::col2rgb(paleta$ordinal_senioridade), 2, mean)

  expect_true(all(diff(luminosidade) < 0))
})

test_that("a rampa ordinal é de uma matiz só", {
  paleta <- paleta_monografia()
  matiz <- grDevices::rgb2hsv(grDevices::col2rgb(paleta$ordinal_senioridade))["h", ]

  expect_lt(diff(range(matiz)) * 360, 40)
})

test_that("a escala divergente da nota cobre os três níveis e tem neutro no meio", {
  paleta <- paleta_monografia()
  rgb_nota <- grDevices::col2rgb(paleta$divergente_nota)

  expect_named(paleta$divergente_nota, c("ABAIXO", "DENTRO", "ACIMA"))
  expect_equal(rgb_nota[, "DENTRO"][["red"]], rgb_nota[, "DENTRO"][["green"]], tolerance = 0.05)
})

test_that("toda cor categórica alcança 3:1 de contraste contra o fundo", {
  # Piso de contraste WCAG para marca de gráfico. Foi o que travou a
  # suavização da paleta, ver roxygen de paleta_monografia().
  paleta <- paleta_monografia()

  luminancia_relativa <- function(cor) {
    canal <- grDevices::col2rgb(cor)[, 1] / 255
    linear <- ifelse(canal <= 0.04045, canal / 12.92, ((canal + 0.055) / 1.055)^2.4)
    sum(linear * c(0.2126, 0.7152, 0.0722))
  }
  contraste <- function(a, b) {
    valores <- sort(c(luminancia_relativa(a), luminancia_relativa(b)), decreasing = TRUE)
    (valores[1] + 0.05) / (valores[2] + 0.05)
  }

  contrastes <- vapply(
    paleta$categorica, contraste, numeric(1), b = paleta$chrome[["fundo"]]
  )
  expect_true(all(contrastes >= 3))
})

test_that("tema_monografia devolve um tema com moldura, grade e subgrade", {
  tema <- tema_monografia()

  expect_s3_class(tema, "theme")
  expect_s3_class(tema$panel.border, "element_rect")
  expect_s3_class(tema$panel.grid.major, "element_line")
  expect_s3_class(tema$panel.grid.minor, "element_line")
  # A hierarquia importa: subgrade mais fraca que grade, senão vira ruído.
  expect_lt(tema$panel.grid.minor$linewidth, tema$panel.grid.major$linewidth)
})

test_that("tema_monografia soma a um ggplot sem erro", {
  expect_no_error(
    ggplot2::ggplot(datasets::mtcars, ggplot2::aes(wt, mpg)) +
      ggplot2::geom_point() +
      tema_monografia()
  )
})

test_that("rotular_figura exige título e os dois eixos", {
  expect_error(rotular_figura(eixo_x = "x", eixo_y = "y"), "título")
  expect_error(rotular_figura("", eixo_x = "x", eixo_y = "y"), "título")
  expect_error(rotular_figura("Um titulo", eixo_y = "y"), "eixo_x e eixo_y")
})

test_that("rotular_figura aceita eixo omitido de propósito e traz a fonte", {
  rotulos <- rotular_figura("Um titulo", eixo_x = NULL, eixo_y = "Consultores")

  expect_equal(rotulos$title, "Um titulo")
  expect_equal(rotulos$y, "Consultores")
  expect_match(rotulos$caption, "sintéticos")
  expect_no_error(
    ggplot2::ggplot(datasets::mtcars, ggplot2::aes(wt, mpg)) +
      ggplot2::geom_point() +
      rotulos
  )
})

test_that("o título de legenda vai só para a estética pedida", {
  # Nomear fill e colour de uma vez faz o ggplot2 avisar sobre rótulo
  # desconhecido em toda figura que usa só uma das duas.
  por_preenchimento <- rotular_figura(
    "Titulo", eixo_x = "x", eixo_y = "y", legenda = "Segmento"
  )
  por_cor <- rotular_figura(
    "Titulo", eixo_x = "x", eixo_y = "y", legenda = "Segmento",
    legenda_estetica = "colour"
  )

  expect_equal(por_preenchimento$fill, "Segmento")
  expect_null(por_preenchimento$colour)
  expect_equal(por_cor$colour, "Segmento")
  expect_null(por_cor$fill)
})

test_that("sem legenda, nenhuma estética de cor é nomeada", {
  rotulos <- rotular_figura("Titulo", eixo_x = "x", eixo_y = "y")

  expect_null(rotulos$fill)
  expect_null(rotulos$colour)
})

test_that("salvar_figura recusa caminho que o Windows truncaria em silêncio", {
  figura <- ggplot2::ggplot(datasets::mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point()

  expect_error(
    salvar_figura(figura, strrep("nome_muito_comprido_", 20), diretorio = tempdir()),
    "truncaria"
  )
})

test_that("garantir_caminho_curto deixa passar o que cabe e barra o que não cabe", {
  expect_invisible(garantir_caminho_curto("curto.csv"))
  expect_equal(garantir_caminho_curto("curto.csv"), "curto.csv")
  expect_error(garantir_caminho_curto(strrep("a", 300)), "truncaria")
})

test_that("salvar_tabela grava csv legível e devolve o caminho", {
  diretorio_temp <- tempfile("tabelas_teste_")
  dir.create(diretorio_temp)
  on.exit(unlink(diretorio_temp, recursive = TRUE))

  tabela <- tibble::tibble(Medida = c("cobertura", "órfãos"), Valor = c("48,4%", "0"))
  caminho <- salvar_tabela(tabela, "teste", diretorio = diretorio_temp)

  expect_true(file.exists(caminho))
  expect_match(caminho, "teste\\.csv$")
  # as_tibble descarta a classe spec_tbl_df que readr anexa na leitura, e que
  # não é diferença de conteúdo. Mesmo padrão de test-salvar_synthetic.R.
  lido <- tibble::as_tibble(readr::read_csv(caminho, show_col_types = FALSE))
  expect_equal(lido, tabela)
})

test_that("salvar_tabela grava tabela vazia, que é evidência de verificação sem achado", {
  diretorio_temp <- tempfile("tabelas_vazias_")
  dir.create(diretorio_temp)
  on.exit(unlink(diretorio_temp, recursive = TRUE))

  vazia <- tibble::tibble(Tabela = character(0), Divergência = character(0))
  caminho <- salvar_tabela(vazia, "sem_divergencia", diretorio = diretorio_temp)

  expect_true(file.exists(caminho))
  expect_equal(nrow(readr::read_csv(caminho, show_col_types = FALSE)), 0)
})

test_that("salvar_tabela recusa caminho longo demais", {
  expect_error(
    salvar_tabela(tibble::tibble(a = 1), strrep("nome_comprido_", 25), diretorio = tempdir()),
    "truncaria"
  )
})

test_that("aplicar_padrao_monografia fixa o tema e a cor default da sessão", {
  tema_anterior <- ggplot2::theme_get()
  on.exit(ggplot2::theme_set(tema_anterior))

  aplicar_padrao_monografia()

  expect_s3_class(ggplot2::theme_get()$panel.border, "element_rect")
  expect_equal(
    ggplot2::GeomBar$default_aes$fill,
    paleta_monografia()$categorica[1]
  )
})

test_that("salvar_figura escreve o png e devolve o caminho", {
  diretorio_temp <- tempfile("figuras_teste_")
  dir.create(diretorio_temp)
  on.exit(unlink(diretorio_temp, recursive = TRUE))

  figura <- ggplot2::ggplot(datasets::mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point() +
    tema_monografia()

  caminho <- salvar_figura(figura, "teste", diretorio = diretorio_temp)

  expect_true(file.exists(caminho))
  expect_match(caminho, "teste\\.png$")
})

test_that("toda figura carrega o proprio tema, sem depender da sessao", {
  # Regressao real: apos o refactor, as figuras passaram a ser construidas
  # dentro do targets, onde aplicar_padrao_monografia() nunca roda. Saiam
  # com o tema cinza padrao do ggplot2 e o check de saude nao pegou, porque
  # olhava validade do PNG, nao aparencia. Tema explicito em cada figura
  # elimina a dependencia de estado de sessao.
  tema_anterior <- ggplot2::theme_get()
  ggplot2::theme_set(ggplot2::theme_grey())
  on.exit(ggplot2::theme_set(tema_anterior))

  cobertura <- tibble::tibble(
    tabela = "satelite", entidade = "consultores.consultor_id",
    coluna = "consultor_id", com_registro = 2L, total = 4L,
    sem_registro = 2L, cobertura = 0.5
  )
  comparacao <- tibble::tibble(
    categoria = c("x", "y"), proporcao_real = c(0.6, 0.4),
    proporcao_sintetica = c(0.55, 0.45), diferenca = c(-0.05, 0.05),
    rodada = "Teste"
  )
  confronto <- tibble::tibble(
    variavel_a = "a", variavel_b = "b", procedencia = "livre",
    medida = 0.02, valor_real = NA_real_, diferenca_do_alvo = NA_real_,
    atencao = FALSE, mecanismo = "teste", rodada = NA_character_
  )

  figuras <- list(
    figura_cobertura_entidades(cobertura),
    figura_proporcoes_comparadas(comparacao, "Titulo"),
    figura_distribuicoes(tibble::tibble(a = c(1, 10, 100)), "a", "Titulo"),
    figura_associacoes_por_procedencia(confronto, "Titulo")
  )

  for (figura in figuras) {
    expect_s3_class(figura$theme$panel.border, "element_rect")
    expect_s3_class(figura$theme$panel.grid.minor, "element_line")
  }
})
