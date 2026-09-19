test_that("registrar_achados exige as quatro colunas do registro", {
  incompleto <- tibble::tibble(codigo = "I.1.1", etiqueta = "instrumento")

  expect_error(registrar_achados("I.1", incompleto), "Faltam colunas")
})

test_that("registrar_achados recusa etiqueta epistemica fora do vocabulario", {
  achados <- tibble::tibble(
    codigo = "I.1.1", etiqueta = "achismo",
    enunciado = "algo", implicacao = "algo"
  )

  expect_error(registrar_achados("I.1", achados), "não reconhecida")
})

test_that("registrar_achados recusa codigo que nao pertence ao bloco", {
  achados <- tibble::tibble(
    codigo = "I.2.1", etiqueta = "instrumento",
    enunciado = "algo", implicacao = "algo"
  )

  expect_error(registrar_achados("I.1", achados), "fora do bloco")
})

test_that("registrar_achados recusa codigo repetido", {
  achados <- tibble::tibble(
    codigo = c("I.1.1", "I.1.1"), etiqueta = "instrumento",
    enunciado = "algo", implicacao = "algo"
  )

  expect_error(registrar_achados("I.1", achados), "repetido")
})

test_that("registrar_achados recusa achado sem enunciado ou sem implicacao", {
  # Achado solto, sem dizer o que habilita, e o tipo de conclusao que este
  # capitulo nao deve produzir.
  sem_implicacao <- tibble::tibble(
    codigo = "I.1.1", etiqueta = "instrumento",
    enunciado = "algo", implicacao = "   "
  )

  expect_error(registrar_achados("I.1", sem_implicacao), "sem enunciado ou sem")
})

test_that("registrar_achados devolve o registro validado com o bloco a frente", {
  achados <- tibble::tibble(
    codigo = c("I.1.2", "I.1.1"), etiqueta = c("instrumento", "âncora real"),
    enunciado = c("b", "a"), implicacao = c("b", "a")
  )

  registro <- registrar_achados("I.1", achados)

  expect_equal(names(registro)[1], "bloco")
  expect_equal(registro$bloco, c("I.1", "I.1"))
  expect_equal(registro$codigo, c("I.1.1", "I.1.2"))
})

test_that("executar_i1 devolve as quatro partes do bloco", {
  bloco <- executar_i1(carregar_dados())

  expect_named(bloco, c("objetos", "tabelas", "figuras", "achados"))
  expect_true(all(vapply(bloco$tabelas, is.data.frame, logical(1))))
  expect_true(all(vapply(bloco$figuras, ggplot2::is_ggplot, logical(1))))
  expect_equal(nrow(bloco$achados), 5)
})

test_that("executar_i2 devolve as quatro partes do bloco", {
  dados <- carregar_dados(tabelas = c(
    "consultores", "custo_carregado", "projetos", "alocacoes",
    "ocupacao_consultor", "margem_frente_mes", "margem_estrutural"
  ))

  bloco <- executar_i2(dados)

  expect_named(bloco, c("objetos", "tabelas", "figuras", "achados"))
  expect_true(all(vapply(bloco$tabelas, is.data.frame, logical(1))))
  expect_true(all(vapply(bloco$figuras, ggplot2::is_ggplot, logical(1))))
  expect_equal(nrow(bloco$achados), 4)
})

test_that("o enunciado dos achados embute os valores calculados, nao texto fixo", {
  # E o que impede que uma correcao no gerador mude o numero e o texto siga
  # afirmando o valor antigo.
  bloco <- executar_i1(carregar_dados())
  achado_cobertura <- bloco$achados[bloco$achados$codigo == "I.1.3", ]
  skills <- bloco$objetos$cobertura
  skills <- skills[skills$tabela == "skills_consultor" & skills$coluna == "consultor_id", ]

  expect_true(grepl(formatar_pct(skills$cobertura), achado_cobertura$enunciado, fixed = TRUE))
})

test_that("os nomes das tabelas do bloco viram os nomes dos arquivos", {
  diretorio_temp <- tempfile("bloco_teste_")
  dir.create(diretorio_temp)
  on.exit(unlink(diretorio_temp, recursive = TRUE))

  caminhos <- salvar_tabelas_bloco(
    list(primeira = tibble::tibble(a = 1), segunda = tibble::tibble(b = 2)),
    diretorio = diretorio_temp
  )

  expect_length(caminhos, 2)
  expect_true(all(file.exists(caminhos)))
  expect_match(caminhos[1], "primeira[.]csv$")
})

test_that("salvar_figuras_bloco respeita dimensao por figura e cai no padrao sem ela", {
  diretorio_temp <- tempfile("figuras_bloco_")
  dir.create(diretorio_temp)
  on.exit(unlink(diretorio_temp, recursive = TRUE))

  figura <- ggplot2::ggplot(datasets::mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()

  caminhos <- salvar_figuras_bloco(
    list(alta = figura, padrao = figura),
    dimensoes = list(alta = c(10, 20)),
    diretorio = diretorio_temp
  )

  expect_length(caminhos, 2)
  expect_true(all(file.exists(caminhos)))
})

test_that("titulo e subtitulo longos sao quebrados, e nao cortados na borda", {
  # O ggplot2 nao quebra nem avisa: texto mais largo que a area de desenho sai
  # pela borda direita do PNG e some sem erro. Teste que so verifica se o
  # arquivo e um PNG valido nao pega isso.
  subtitulo_longo <- paste(rep("palavra", 40), collapse = " ")

  rotulos <- rotular_figura(
    titulo = paste(rep("titulo", 30), collapse = " "),
    subtitulo = subtitulo_longo,
    eixo_x = "x", eixo_y = "y"
  )

  expect_true(grepl("\n", rotulos$title, fixed = TRUE))
  expect_true(grepl("\n", rotulos$subtitle, fixed = TRUE))
  expect_true(all(nchar(strsplit(rotulos$title, "\n", fixed = TRUE)[[1]]) <= 70))
  expect_true(all(nchar(strsplit(rotulos$subtitle, "\n", fixed = TRUE)[[1]]) <= 95))
})

test_that("rotulo curto e subtitulo ausente passam intactos", {
  rotulos <- rotular_figura(titulo = "Titulo curto", eixo_x = "x", eixo_y = NULL)

  expect_equal(rotulos$title, "Titulo curto")
  expect_null(rotulos$subtitle)
})

test_that("nenhuma figura da pipeline tem rotulo cortado na borda", {
  # Varre todos os blocos: qualquer linha acima do limite denuncia uma figura
  # que sairia com texto fora do PNG. Cobre nome de eixo e nao so titulo e
  # subtitulo, porque foi por eixo e legenda que a figura do bloco II.4
  # escapou da primeira versao deste teste.
  blocos <- c("bloco_i0", "bloco_i1", "bloco_i2", "bloco_i4", "bloco_i5",
              "bloco_ii1", "bloco_ii2", "bloco_ii3", "bloco_ii4")

  quebrar <- function(texto) strsplit(texto %||% "", "\n", fixed = TRUE)[[1]]

  # Verifica o rotulo como ele fica DEPOIS da requebra contra a largura real,
  # que e o que vai para o PNG. Testar o rotulo do objeto do bloco mediria a
  # quebra provisoria de rotular_figura e nao a que decide o corte. A largura
  # usada e a menor em uso no projeto, que e o caso mais apertado.
  largura_mais_estreita <- 16
  orcamento <- c(titulo = 3.4, subtitulo = 4.8, eixo = 3.6) * largura_mais_estreita

  excessos <- unlist(lapply(blocos, function(nome) {
    figuras <- targets::tar_read_raw(nome, store = here::here("_targets"))$figuras
    lapply(figuras, function(figura) {
      pronta <- requebrar_rotulos(figura, largura_mais_estreita)
      c(
        max(c(0, nchar(quebrar(pronta$labels$title)) - orcamento[["titulo"]])),
        max(c(0, nchar(quebrar(pronta$labels$subtitle)) - orcamento[["subtitulo"]])),
        max(c(0, nchar(quebrar(pronta$labels$x)) - orcamento[["eixo"]])),
        max(c(0, nchar(quebrar(pronta$labels$y)) - orcamento[["eixo"]]))
      )
    })
  }))

  # Uma palavra unica maior que o orcamento nao tem como ser quebrada, e
  # denuncia rotulo que precisa ser reescrito, nao requebrado.
  expect_true(all(excessos == 0))
})

test_that("requebrar_rotulos preserva eixo omitido de proposito", {
  # Atribuir NULL a elemento de lista o remove, e removido o ggplot2 volta ao
  # default e estampa o nome cru da coluna no eixo. A figura do bloco II.4
  # saiu com "cenario" escrito no eixo por causa disso.
  figura <- ggplot2::ggplot(
    tibble::tibble(a = 1:3, b = letters[1:3]), ggplot2::aes(a, b)
  ) +
    ggplot2::geom_col() +
    rotular_figura(titulo = "Titulo", eixo_x = "Eixo horizontal", eixo_y = NULL)

  pronta <- requebrar_rotulos(figura, 16)

  expect_true("y" %in% names(pronta$labels))
  expect_null(pronta$labels$y)
  expect_equal(pronta$labels$x, "Eixo horizontal")
})
