achado_minimo <- function(codigo = "I.1.1", ...) {
  base <- tibble::tibble(
    codigo = codigo, etiqueta = "instrumento",
    enunciado = "algo", implicacao = "algo"
  )
  extras <- list(...)
  for (nome in names(extras)) base[[nome]] <- extras[[nome]]
  base
}

test_that("as tres colunas de sintese aparecem mesmo quando o bloco nao as declara", {
  registrado <- registrar_achados("I.1", achado_minimo())

  expect_true(all(c("decisao", "limitacao", "proveniencia") %in% names(registrado)))
  expect_true(is.na(registrado$decisao))
  expect_true(is.character(registrado$proveniencia))
})

test_that("decisao ou limitacao sem proveniencia e recusada", {
  # E a lacuna que estes campos existem para fechar: e onde a banca pergunta
  # se a conclusao e da organização ou do gerador.
  expect_error(
    registrar_achados("I.1", achado_minimo(decisao = "decide algo")),
    "sem proveniência declarada"
  )
  expect_error(
    registrar_achados("I.1", achado_minimo(limitacao = "limita algo")),
    "sem proveniência declarada"
  )
})

test_that("proveniencia sem decisao nem limitacao e recusada", {
  expect_error(
    registrar_achados("I.1", achado_minimo(proveniencia = "fato real")),
    "sem decisão nem limitação"
  )
})

test_that("proveniencia fora do vocabulario e recusada", {
  expect_error(
    registrar_achados(
      "I.1", achado_minimo(decisao = "decide", proveniencia = "acho que sim")
    ),
    "Proveniência de achado não reconhecida"
  )
})

test_that("texto em branco na sintese conta como ausencia e nao dispara a exigencia", {
  # Sem isso, um campo preenchido com espacos passaria pela validacao e
  # apareceria na tabela da Parte III como linha vazia.
  registrado <- registrar_achados("I.1", achado_minimo(decisao = "   "))

  expect_true(is.na(registrado$decisao))
})

test_that("com_sintese recusa codigo que o bloco nao declara", {
  # Num left_join solto o erro de digitacao produziria NA silencioso, e a
  # Parte III perderia a linha sem ninguem notar.
  narrativo <- achado_minimo("I.1.1")
  sintese <- tibble::tibble(
    codigo = "I.1.10", decisao = "decide", proveniencia = "fato real"
  )

  expect_error(com_sintese(narrativo, sintese), "não declara")
})

test_that("ordem_de_leitura ordena por numero e nao por texto", {
  # Codigos hipoteticos: nenhum bloco tem dez achados, e o teste precisa de um.
  codigos <- c("II.9.10", "II.9.2", "I.1.1", "II.1.1", "III.1")

  expect_equal(
    codigos[ordem_de_leitura(codigos)],
    c("I.1.1", "II.1.1", "II.9.2", "II.9.10", "III.1")
  )
})

test_that("registrar_achados devolve o decimo achado depois do segundo", {
  dez <- achado_minimo(codigo = c("II.9.10", "II.9.2"))

  expect_equal(registrar_achados("II.9", dez)$codigo, c("II.9.2", "II.9.10"))
})

test_that("consolidar_achados recusa codigo reivindicado por dois blocos", {
  # `registrar_achados` enxerga um bloco por vez e nao detectaria isto.
  um <- registrar_achados("I.1", achado_minimo("I.1.1"))
  outro <- um

  expect_error(consolidar_achados(list(um, outro)), "mais de um bloco")
})

test_that("a sintese so traz achado que decide ou limita", {
  blocos <- list(registrar_achados("I.1", dplyr::bind_rows(
    achado_minimo("I.1.1", decisao = "decide", proveniencia = "fato real"),
    achado_minimo("I.1.2", limitacao = "limita", proveniencia = "estrutural"),
    achado_minimo("I.1.3")
  )))

  sintese <- tabular_sintese(consolidar_achados(blocos))

  expect_equal(sintese$codigo, c("I.1.1", "I.1.2"))
})

test_that("a contagem de limitacoes nao soma duas vezes o achado que tambem decide", {
  # A proveniencia se refere a decisao quando o achado tem as duas, pela regra
  # declarada em registrar_achados, e contar o mesmo achado nas duas colunas
  # sugeriria que a etiqueta vale para a limitacao tambem.
  blocos <- list(registrar_achados("I.1", dplyr::bind_rows(
    achado_minimo(
      "I.1.1", decisao = "decide", limitacao = "limita", proveniencia = "fato real"
    ),
    achado_minimo("I.1.2", limitacao = "limita", proveniencia = "fato real")
  )))

  contagem <- contar_proveniencia(consolidar_achados(blocos))
  fato_real <- contagem[contagem$proveniencia == "fato real", ]

  expect_equal(fato_real$decisoes, 1L)
  expect_equal(fato_real$limitacoes, 1L)
})

test_that("contar_proveniencia lista as tres proveniencias mesmo com zero", {
  blocos <- list(registrar_achados(
    "I.1", achado_minimo("I.1.1", decisao = "decide", proveniencia = "fato real")
  ))

  contagem <- contar_proveniencia(consolidar_achados(blocos))

  expect_equal(contagem$proveniencia, proveniencias_achado())
  expect_equal(sum(contagem$decisoes), 1L)
})

test_that("o resumo da proveniencia nomeia as decisoes que dependem do gerador", {
  blocos <- list(registrar_achados("I.1", dplyr::bind_rows(
    achado_minimo("I.1.1", decisao = "decide", proveniencia = "depende do gerador"),
    achado_minimo("I.1.2", decisao = "decide", proveniencia = "fato real"),
    achado_minimo("I.1.3", limitacao = "limita", proveniencia = "depende do gerador")
  )))

  resumo <- resumir_proveniencia_das_decisoes(consolidar_achados(blocos))

  expect_equal(resumo$n_decisoes, 2L)
  expect_equal(resumo$depende_do_gerador, 1L)
  expect_equal(resumo$codigos_dependentes, "I.1.1")
})

test_that("o cruzamento cobre as nove celulas mesmo com categoria vazia", {
  blocos <- list(registrar_achados(
    "I.1", achado_minimo("I.1.1", decisao = "decide", proveniencia = "fato real")
  ))

  cruzamento <- cruzar_etiqueta_e_proveniencia(consolidar_achados(blocos))

  expect_equal(nrow(cruzamento), length(etiquetas_achado()) * length(proveniencias_achado()))
  expect_equal(sum(cruzamento$n), 1L)
})

test_that("a Parte III nao inventa decisao que nenhum bloco sustenta", {
  # O capitulo consolida e nao conclui: achado da Parte III que habilitasse
  # decisao de modelagem seria decisao tomada sem bloco que a apoie.
  bloco <- targets::tar_read(bloco_iii, store = here::here("_targets"))

  expect_true(all(startsWith(bloco$achados$codigo, "III.")))
  expect_false(any(
    !is.na(bloco$achados$decisao) & bloco$achados$proveniencia == "fato real"
  ))
})

test_that("toda decisao da sintese tem proveniencia declarada", {
  bloco <- targets::tar_read(bloco_iii, store = here::here("_targets"))
  decisoes <- dplyr::filter(bloco$objetos$consolidados, !is.na(.data$decisao))

  expect_gt(nrow(decisoes), 0)
  expect_false(any(is.na(decisoes$proveniencia)))
  expect_true(all(decisoes$proveniencia %in% proveniencias_achado()))
})
