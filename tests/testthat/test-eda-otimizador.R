test_that("so a formulacao com tempo expressa a capacidade", {
  # Capacidade e por mes: sem o mes na variavel de decisao nao ha como
  # escrever a restricao, qualquer que seja o tamanho do modelo.
  dados <- carregar_dados()

  porte <- medir_porte_do_problema(
    dados$consultores, dados$projetos, dados$alocacoes, meses_ativos = 94
  )

  expect_equal(nrow(porte), 2)
  expect_false(porte$expressa_capacidade[porte$formulacao == "Par consultor e projeto"])
  expect_true(porte$expressa_capacidade[porte$formulacao == "Trio consultor, projeto e mês"])
  expect_gt(
    porte$variaveis[porte$formulacao == "Trio consultor, projeto e mês"],
    porte$variaveis[porte$formulacao == "Par consultor e projeto"]
  )
})

test_that("o filtro de conteudo reduz o porte", {
  # E a amarra entre o bloco II.6 e este: o filtro foi escolhido la por
  # filtrar e nao ordenar, e aqui o mesmo corte torna o inteiro tratavel.
  dados <- carregar_dados()
  filtro <- medir_reducao_pelo_filtro(
    dados$consultores, dados$projetos, dados$experiencia_setorial
  )

  porte <- medir_porte_do_problema(
    dados$consultores, dados$projetos, dados$alocacoes, meses_ativos = 94,
    fracao_candidatos = filtro$fracao_candidatos
  )

  expect_equal(nrow(porte), 3)
  expect_lt(
    porte$variaveis[porte$formulacao == "Trio, restrito ao conjunto candidato"],
    porte$variaveis[porte$formulacao == "Trio consultor, projeto e mês"]
  )
  expect_gt(filtro$fracao_candidatos, 0)
  expect_lt(filtro$fracao_candidatos, 1)
})

test_that("a fracao candidata sai das marginais e bate com a contagem direta", {
  consultores <- tibble::tibble(consultor_id = c("c1", "c2", "c3"))
  projetos <- tibble::tibble(
    projeto_id = c("p1", "p2"), setor = c("A", "B")
  )
  experiencia <- tibble::tibble(
    consultor_id = c("c1", "c2"), segmento = c("A", "B"), intensidade = c(1, 1)
  )

  filtro <- medir_reducao_pelo_filtro(consultores, projetos, experiencia)

  # c1 casa com p1 e c2 com p2. c3 nao declarou setor e por isso nao e
  # filtrado: passa nos dois projetos, como o hibrido chaveado manda. Sao 4 de
  # 6 pares. A versao anterior excluia c3 e contava 2 de 6.
  expect_equal(filtro$fracao_candidatos, 4 / 6)
  expect_equal(filtro$fracao_casamento, 2 / 4)
  expect_equal(filtro$fracao_excluindo_sem_setor, 2 / 6)
  expect_equal(filtro$consultores_sem_setor, 1 / 3)
})

test_that("a violacao de capacidade e medida contra o limite declarado", {
  ocupacao <- tibble::tibble(
    consultor_id = c("c1", "c1", "c2", "c3"),
    ano_mes = c("2026-01", "2026-02", "2026-01", "2026-01"),
    pct_ocupacao = c(0.5, 1.5, 2.0, 0.9)
  )

  v <- medir_violacao_de_capacidade(ocupacao, limite = 1)

  expect_equal(v$fracao_meses_acima, 0.5)
  expect_equal(v$consultores_com_violacao, 2 / 3)
  expect_equal(v$excesso_mediano, 0.75)
  expect_equal(v$ocupacao_maxima, 2.0)
})

test_that("sem violacao o excesso e zero e nao NA", {
  ocupacao <- tibble::tibble(
    consultor_id = "c1", ano_mes = "2026-01", pct_ocupacao = 0.8
  )

  v <- medir_violacao_de_capacidade(ocupacao)

  expect_equal(v$fracao_meses_acima, 0)
  expect_equal(v$excesso_mediano, 0)
})

test_that("o registro de insumos separa existir de ter escala", {
  # A distincao e a mesma que o bloco II.5 precisou fazer entre mecanismo e
  # calibracao: um objetivo pode ser calculavel e nao ter nivel utilizavel.
  insumos <- registrar_insumos_do_otimizador(
    margem_calibrada = FALSE, capacidade_respeitada = FALSE
  )

  expect_true(all(nzchar(insumos$observacao)))
  # Nada pode ser dimensionavel sem existir.
  expect_true(all(insumos$dimensionavel <= insumos$existe))
  expect_false(insumos$dimensionavel[insumos$insumo == "Margem"])
  expect_false(insumos$existe[insumos$insumo == "Demanda futura"])
})

test_that("o veredito dos insumos responde aos argumentos", {
  calibrado <- registrar_insumos_do_otimizador(
    margem_calibrada = TRUE, capacidade_respeitada = TRUE
  )

  expect_true(calibrado$dimensionavel[calibrado$insumo == "Margem"])
  expect_true(calibrado$dimensionavel[calibrado$insumo == "Capacidade mensal"])
})

test_that("executar_ii8 devolve as quatro partes do bloco", {
  dados <- carregar_dados()

  bloco <- executar_ii8(dados)

  expect_named(bloco, c("objetos", "tabelas", "figuras", "achados"))
  expect_true(all(vapply(bloco$tabelas, is.data.frame, logical(1))))
  expect_true(all(vapply(bloco$figuras, ggplot2::is_ggplot, logical(1))))
  expect_equal(nrow(bloco$achados), 4)
  expect_true(all(startsWith(bloco$achados$codigo, "II.8.")))
  # Poucos insumos tem escala utilizavel, e e esse o achado do bloco.
  expect_lt(bloco$objetos$dimensionaveis, bloco$objetos$existem)
})

test_that("a figura do bloco II.8 carrega o proprio tema", {
  tema_anterior <- ggplot2::theme_get()
  ggplot2::theme_set(ggplot2::theme_grey())
  on.exit(ggplot2::theme_set(tema_anterior))

  bloco <- executar_ii8(carregar_dados())

  for (figura in bloco$figuras) {
    expect_s3_class(figura$theme$panel.border, "element_rect")
    expect_no_error(ggplot2::ggplot_build(figura))
  }
})
