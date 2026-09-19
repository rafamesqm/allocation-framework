test_that("a cobertura e medida por sinal, nao em bloco", {
  # E a separacao por sinal que desfaz a leitura do achado I.1.3.
  dados <- carregar_dados()

  cobertura <- medir_cobertura_de_sinais(dados)

  expect_true(all(c("Consultor", "Projeto") %in% cobertura$lado))
  expect_true(all(c("conteúdo", "interação") %in% cobertura$tipo))
  expect_true(all(cobertura$cobertura >= 0 & cobertura$cobertura <= 1))
  # Ha sinal de conteudo universal e sinal de conteudo parcial ao mesmo tempo.
  conteudo <- cobertura[cobertura$tipo == "conteúdo", ]
  expect_true(any(conteudo$cobertura == 1))
  expect_true(any(conteudo$cobertura < 0.6))
})

test_that("os quadrantes somam a populacao inteira", {
  dados <- carregar_dados()

  cruzamento <- cruzar_cold_start(dados, "skills_consultor")

  expect_equal(sum(cruzamento$quadrantes$consultores), nrow(dados$consultores))
  expect_equal(sum(cruzamento$quadrantes$fracao), 1)
})

test_that("a zona morta depende do sinal de conteudo adotado", {
  # E o achado central do bloco: com experiencia setorial ela desaparece.
  dados <- carregar_dados()

  com_skills <- cruzar_cold_start(dados, "skills_consultor")
  com_setor <- cruzar_cold_start(dados, "experiencia_setorial")

  expect_gt(com_skills$zona_morta, 0)
  expect_equal(com_setor$zona_morta, 0)
})

test_that("cruzar_cold_start reporta o esperado sob independencia", {
  consultores <- tibble::tibble(consultor_id = paste0("c", 1:100))
  alocacoes <- tibble::tibble(
    consultor_id = paste0("c", 1:50), projeto_id = paste0("p", 1:50)
  )
  skills_consultor <- tibble::tibble(
    consultor_id = paste0("c", 1:80), skill_id = "s1"
  )
  dados <- list(
    consultores = consultores, alocacoes = alocacoes,
    skills_consultor = skills_consultor
  )

  cruzamento <- cruzar_cold_start(dados, "skills_consultor")

  # 50% sem interacao, 20% sem skills, esperado sob independencia = 10 de 100.
  expect_equal(cruzamento$zona_morta_esperada, 10)
})

test_that("a avaliacao das formas de hibrido e explicita e completa", {
  formas <- avaliar_formas_de_hibrido()

  expect_true(all(c("Ponderado", "Chaveado", "Cascata") %in% formas$forma))
  expect_type(formas$viavel, "logical")
  expect_false(any(is.na(formas$viavel)))
  # Toda forma declara exigencia e justificativa, senao a decisao nao e auditavel.
  expect_true(all(nzchar(formas$exige)))
  expect_true(all(nzchar(formas$justificativa)))
  # Cascata e chaveado sobrevivem; ponderado nao.
  expect_true(formas$viavel[formas$forma == "Cascata"])
  expect_false(formas$viavel[formas$forma == "Ponderado"])
})

test_that("executar_ii3 devolve as quatro partes do bloco", {
  dados <- carregar_dados()

  bloco <- executar_ii3(dados)

  expect_named(bloco, c("objetos", "tabelas", "figuras", "achados"))
  expect_true(all(vapply(bloco$figuras, ggplot2::is_ggplot, logical(1))))
  expect_equal(nrow(bloco$achados), 4)
  expect_true(all(startsWith(bloco$achados$codigo, "II.3.")))
})

test_that("a figura de cobertura por sinal carrega o proprio tema", {
  tema_anterior <- ggplot2::theme_get()
  ggplot2::theme_set(ggplot2::theme_grey())
  on.exit(ggplot2::theme_set(tema_anterior))

  cobertura <- medir_cobertura_de_sinais(carregar_dados())
  figura <- figura_cobertura_de_sinais(cobertura, "Titulo")

  expect_s3_class(figura$theme$panel.border, "element_rect")
  expect_no_error(ggplot2::ggplot_build(figura))
})
