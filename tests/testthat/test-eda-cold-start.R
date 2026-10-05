test_that("a cobertura e medida por sinal, nao em bloco", {
  dados <- carregar_dados()

  cobertura <- medir_cobertura_de_sinais(dados)

  expect_true(all(c("Consultor", "Projeto") %in% cobertura$lado))
  expect_true(all(c("conteúdo", "interação") %in% cobertura$tipo))
  expect_true(all(cobertura$cobertura >= 0 & cobertura$cobertura <= 1))
  # Ha sinal de conteudo universal e sinal de conteudo parcial ao mesmo tempo,
  # e e essa diferenca que a figura do bloco existe para mostrar.
  conteudo <- cobertura[cobertura$tipo == "conteúdo", ]
  expect_true(any(conteudo$cobertura == 1))
  expect_true(any(conteudo$cobertura < 0.6))
})

test_that("os sinais derivados de formulario cobrem as mesmas pessoas", {
  # Propriedade estrutural, nao coincidencia numerica: na fonte real os quatro
  # saem da mesma campanha de formularios, entao quem nao respondeu nao tem
  # nenhum deles. Foi a violacao disso que produziu a conclusao errada da
  # primeira versao do bloco II.3.
  dados <- carregar_dados()

  coincidencia <- verificar_coincidencia_de_sinais(dados)

  expect_true(coincidencia$coincidem)
  expect_length(unique(coincidencia$tamanhos), 1)
  expect_equal(length(coincidencia$uniao), length(coincidencia$intersecao))
})

test_that("certificacoes nao alcanca quem nao respondeu o formulario", {
  # A pergunta de certificacao esta dentro do proprio formulario, entao a
  # cobertura dela e um subconjunto da dos respondentes, nunca um conjunto a
  # parte sorteado sobre a populacao inteira.
  dados <- carregar_dados()

  respondentes <- unique(dados$experiencia_setorial$consultor_id)
  com_certificacao <- unique(dados$certificacoes_consultor$consultor_id)

  expect_true(all(com_certificacao %in% respondentes))
})

test_that("os quadrantes somam a populacao inteira", {
  dados <- carregar_dados()

  cruzamento <- cruzar_cold_start(dados, "skills_consultor")

  expect_equal(sum(cruzamento$quadrantes$consultores), nrow(dados$consultores))
  expect_equal(sum(cruzamento$quadrantes$fracao), 1)
})

test_that("a zona morta nao muda ao trocar o sinal de conteudo", {
  # Correcao do achado central da primeira versao do bloco, que afirmava o
  # oposto: trocar skills por experiencia setorial nao resgata consultor
  # nenhum, porque sao as mesmas pessoas que responderam.
  dados <- carregar_dados()

  com_skills <- cruzar_cold_start(dados, "skills_consultor")
  com_setor <- cruzar_cold_start(dados, "experiencia_setorial")

  expect_gt(com_skills$zona_morta, 0)
  expect_equal(com_setor$zona_morta, com_skills$zona_morta)
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

test_that("cobertura universal nao implica poder discriminante", {
  tabela <- tibble::tibble(
    consultor_id = rep(paste0("c", 1:10), each = 1),
    idioma = c(rep("Inglês", 8), "Espanhol", "Outro")
  )

  discriminacao <- medir_discriminacao_de_sinal(tabela, "idioma", 10)

  # Cobre os 10, mas separa em 3 classes e 8 deles ficam indistinguiveis.
  expect_equal(discriminacao$n_perfis, 3)
  expect_equal(discriminacao$n_maior_classe, 8)
  expect_equal(discriminacao$fracao_maior_classe, 0.8)
})

test_that("a avaliacao das formas de hibrido e explicita e completa", {
  formas <- avaliar_formas_de_hibrido(
    cobertura_conteudo = 0.484,
    cobertura_interacao_consultor = 0.832,
    cobertura_interacao_projeto = 0.787,
    colaborativo_identificavel = FALSE
  )

  expect_true(all(c("Ponderado", "Chaveado", "Cascata") %in% formas$forma))
  expect_type(formas$viavel, "logical")
  expect_false(any(is.na(formas$viavel)))
  # Toda forma declara exigencia e justificativa, senao a decisao nao e auditavel.
  expect_true(all(nzchar(formas$exige)))
  expect_true(all(nzchar(formas$justificativa)))
  # So o chaveado sobrevive ao padrao de cobertura medido.
  expect_true(formas$viavel[formas$forma == "Chaveado"])
  expect_false(formas$viavel[formas$forma == "Cascata"])
  expect_false(formas$viavel[formas$forma == "Ponderado"])
})

test_that("o veredito das formas responde a cobertura, e nao e fixo", {
  # Sem este teste, uma funcao que devolvesse FALSE para tudo passaria pelo
  # teste anterior e a decisao deixaria de ser dirigida pela medida.
  formas <- avaliar_formas_de_hibrido(
    cobertura_conteudo = 1,
    cobertura_interacao_consultor = 1,
    cobertura_interacao_projeto = 1,
    colaborativo_identificavel = TRUE
  )

  expect_true(formas$viavel[formas$forma == "Cascata"])
  expect_true(formas$viavel[formas$forma == "Ponderado"])
  expect_true(formas$viavel[formas$forma == "Meta-nível"])
})

test_that("executar_ii3 devolve as quatro partes do bloco", {
  dados <- carregar_dados()

  bloco <- executar_ii3(dados)

  expect_named(bloco, c("objetos", "tabelas", "figuras", "achados"))
  expect_true(all(vapply(bloco$figuras, ggplot2::is_ggplot, logical(1))))
  expect_equal(nrow(bloco$achados), 5)
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
