test_that("o numero de parametros cresce linearmente com os fatores", {
  consultores <- tibble::tibble(consultor_id = paste0("c", 1:10))
  projetos <- tibble::tibble(projeto_id = paste0("p", 1:20))
  alocacoes <- tibble::tibble(consultor_id = "c1", projeto_id = "p1")

  medida <- medir_identificabilidade(alocacoes, consultores, projetos, fatores = c(1, 3))

  expect_equal(medida$parametros, c(30, 90))
  expect_equal(medida$obs_por_parametro, c(1 / 30, 1 / 90))
})

test_that("entidade sem interacao conta como subdeterminada em qualquer k", {
  # E o cold start: ignora-lo no denominador esconderia o caso mais grave.
  consultores <- tibble::tibble(consultor_id = c("a", "b", "c", "d"))
  projetos <- tibble::tibble(projeto_id = c("p1", "p2"))
  alocacoes <- tibble::tibble(consultor_id = c("a", "b"), projeto_id = c("p1", "p1"))

  medida <- medir_identificabilidade(alocacoes, consultores, projetos, fatores = 1)

  # c e d nao tem interacao; p2 tampouco.
  expect_equal(medida$fracao_consultores_subdeterminados, 0.5)
  expect_equal(medida$fracao_projetos_subdeterminados, 0.5)
})

test_that("a subdeterminacao cresce de forma monotonica com o numero de fatores", {
  dados <- carregar_dados(tabelas = c("consultores", "projetos", "alocacoes"))

  medida <- medir_identificabilidade(dados$alocacoes, dados$consultores, dados$projetos)

  expect_true(all(diff(medida$fracao_consultores_subdeterminados) >= 0))
  expect_true(all(diff(medida$fracao_projetos_subdeterminados) >= 0))
  expect_true(all(diff(medida$obs_por_parametro) < 0))
})

test_that("a comparacao com referencias publicas inclui este projeto e os tres conjuntos", {
  dados <- carregar_dados(tabelas = c("consultores", "projetos", "alocacoes"))

  comparacao <- comparar_porte_com_referencias(
    dados$alocacoes, dados$consultores, dados$projetos, fatores = 10
  )

  expect_equal(nrow(comparacao), 4)
  expect_true(all(c("Netflix Prize", "MovieLens 1M", "MovieLens 100K") %in% comparacao$conjunto))
  # Este projeto tem menos evidencia por parametro que qualquer referencia.
  expect_equal(which.min(comparacao$obs_por_parametro), 1)
})

test_that("os numeros dos conjuntos publicos batem com os valores publicados", {
  referencias <- referencias_identificabilidade()
  netflix <- referencias[referencias$conjunto == "Netflix Prize", ]

  expect_equal(netflix$interacoes, 100480507)
  expect_equal(netflix$usuarios, 480189)
  expect_equal(netflix$itens, 17770)
})

test_that("executar_ii2 devolve as quatro partes do bloco", {
  dados <- carregar_dados(tabelas = c("consultores", "projetos", "alocacoes"))

  bloco <- executar_ii2(dados)

  expect_named(bloco, c("objetos", "tabelas", "figuras", "achados"))
  expect_true(all(vapply(bloco$figuras, ggplot2::is_ggplot, logical(1))))
  expect_equal(nrow(bloco$achados), 4)
  expect_true(all(startsWith(bloco$achados$codigo, "II.2.")))
})

test_that("as figuras do bloco II.2 carregam o proprio tema", {
  tema_anterior <- ggplot2::theme_get()
  ggplot2::theme_set(ggplot2::theme_grey())
  on.exit(ggplot2::theme_set(tema_anterior))

  dados <- carregar_dados(tabelas = c("consultores", "projetos", "alocacoes"))
  bloco <- executar_ii2(dados)

  for (figura in bloco$figuras) {
    expect_s3_class(figura$theme$panel.border, "element_rect")
    expect_no_error(ggplot2::ggplot_build(figura))
  }
})
