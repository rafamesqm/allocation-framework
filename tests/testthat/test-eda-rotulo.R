test_that("nota_numerica respeita a ordem e so admite os tres niveis", {
  expect_equal(nota_numerica(c("ABAIXO", "DENTRO", "ACIMA")), c(1, 2, 3))
  expect_true(is.na(nota_numerica("OUTRA")))
})

test_that("os itens de uma avaliacao compartilham estrutura", {
  # E o teste que faltava quando o gerador sorteava cada item de forma
  # independente: o alfa de Cronbach dava negativo e nada acusava. Um rotulo
  # sem estrutura comum entre itens nao tem sinal sobre a pessoa, e a wave 3
  # nao teria o que aprender dele.
  dados <- carregar_dados()

  dim <- medir_dimensionalidade_dos_itens(dados$avaliacoes_desempenho)

  expect_gt(dim$alfa_cronbach, 0.7)
  expect_gt(dim$correlacao_media_itens, 0.2)
  # O primeiro componente precisa explicar bem mais que o que itens
  # independentes dariam, que e 1 dividido pelo numero de itens.
  expect_gt(dim$variancia_primeiro_componente, 3 / dim$n_itens)
})

test_that("os avaliadores da mesma alocacao compartilham variancia", {
  # Dois avaliadores observam a mesma realidade: podem discordar muito, mas
  # nao podem ser independentes. A versao anterior do gerador os sorteava
  # separadamente e a correlacao saia em zero.
  dados <- carregar_dados()

  div <- medir_divergencia_entre_avaliadores(dados$avaliacoes_desempenho)

  expect_true(all(div$pares$correlacao > 0.1))
  # E a concordancia entre autoavaliacao e gestor fica na faixa baixa que as
  # meta-analises de self-other rating agreement reportam.
  auto <- div$pares[div$pares$etapa_a == "individual" &
    div$pares$etapa_b == "gestor_cegas", ]
  expect_lt(auto$correlacao, 0.5)
})

test_that("a autoavaliacao e mais leniente que a avaliacao as cegas", {
  dados <- carregar_dados()

  div <- medir_divergencia_entre_avaliadores(dados$avaliacoes_desempenho)
  individual <- div$por_etapa[div$por_etapa$etapa == "individual", ]
  cegas <- div$por_etapa[div$por_etapa$etapa == "gestor_cegas", ]

  expect_gt(individual$nivel_medio, cegas$nivel_medio)
  expect_gt(individual$fracao_acima, cegas$fracao_acima)
})

test_that("o escore tem sinal sobre a pessoa, e nao so sobre a alocacao", {
  dados <- carregar_dados()

  variancia <- decompor_variancia_do_escore(dados$avaliacoes_desempenho)

  expect_gt(variancia$fracao_entre_consultores, 0.1)
  expect_lte(variancia$fracao_entre_consultores, 1)
  expect_gt(variancia$consultores_com_mais_de_uma, 0)
})

test_that("cortar_latente_em_nota devolve exatamente as marginais pedidas", {
  # E o que permite mexer na decomposicao de variancia sem quebrar a
  # calibracao das marginais, que sao o unico numero com ancora aqui.
  set.seed(1)
  latente <- stats::rnorm(10000)
  pesos <- c(ABAIXO = 0.15, DENTRO = 0.60, ACIMA = 0.25)

  nota <- cortar_latente_em_nota(latente, pesos)

  observado <- as.numeric(prop.table(table(nota))[names(pesos)])

  expect_setequal(unique(nota), names(pesos))
  expect_equal(observado, unname(pesos), tolerance = 0.01)
})

test_that("sortear_efeito_por_grupo devolve o mesmo valor dentro do grupo", {
  set.seed(1)
  grupo <- c("a", "b", "a", "c", "b")

  efeito <- sortear_efeito_por_grupo(grupo, desvio = 1)

  expect_equal(efeito[1], efeito[3])
  expect_equal(efeito[2], efeito[5])
  expect_false(efeito[1] == efeito[2])
  expect_length(efeito, length(grupo))
})

test_that("a margem nao e atribuivel a uma alocacao individual", {
  dados <- carregar_dados()

  margem <- medir_atribuibilidade_da_margem(dados)

  expect_false(margem$tem_consultor_na_margem)
  expect_gt(margem$fracao_projetos_com_equipe, 0.5)
  # O custo zero da margem estrutural coincide com projeto sem alocacao, que e
  # o que separa o candidato (d) do (c).
  expect_true(margem$custo_zero_estrutural_sem_alocacao)
})

test_that("o veredito dos candidatos de rotulo responde as medidas", {
  medido <- avaliar_candidatos_de_rotulo(
    margem_atribuivel = FALSE, fracao_projetos_com_equipe = 0.685,
    custo_zero_frente_mes = 0.456, custo_zero_explicavel_estrutural = TRUE,
    alfa_cronbach = 0.9
  )
  hipotetico <- avaliar_candidatos_de_rotulo(
    margem_atribuivel = TRUE, fracao_projetos_com_equipe = 0,
    custo_zero_frente_mes = 0.01, custo_zero_explicavel_estrutural = TRUE,
    alfa_cronbach = 0.9
  )

  expect_equal(medido$candidato[medido$viavel], c("(a)", "(d)"))
  expect_true(all(hipotetico$viavel))
  expect_true(all(nzchar(medido$exige)))
  expect_true(all(nzchar(medido$justificativa)))
})

test_that("escala nao confiavel derruba o candidato (a)", {
  # Era o caso antes da correcao do gerador, e o veredito precisa acompanhar.
  sem_escala <- avaliar_candidatos_de_rotulo(
    margem_atribuivel = FALSE, fracao_projetos_com_equipe = 0.685,
    custo_zero_frente_mes = 0.456, custo_zero_explicavel_estrutural = TRUE,
    alfa_cronbach = -0.035
  )

  expect_false(sem_escala$viavel[sem_escala$candidato == "(a)"])
})

test_that("o comentario pode ser dispensado sem mexer nas notas", {
  alocacoes <- carregar_dados(tabelas = "alocacoes")$alocacoes

  com <- gerar_avaliacoes_desempenho(alocacoes, semente = 7)
  sem <- gerar_avaliacoes_desempenho(alocacoes, com_comentario = FALSE, semente = 7)

  expect_true("comentario" %in% names(com))
  expect_false("comentario" %in% names(sem))
  expect_equal(sem$nota, com$nota)
})

test_that("executar_ii5 devolve as quatro partes do bloco", {
  dados <- carregar_dados()

  bloco <- executar_ii5(dados)

  expect_named(bloco, c("objetos", "tabelas", "figuras", "achados"))
  expect_true(all(vapply(bloco$tabelas, is.data.frame, logical(1))))
  expect_true(all(vapply(bloco$figuras, ggplot2::is_ggplot, logical(1))))
  expect_equal(nrow(bloco$achados), 6)
  expect_true(all(startsWith(bloco$achados$codigo, "II.5.")))
  # A cobertura do rotulo e parametro do gerador, e o achado que a reporta
  # precisa estar etiquetado como tal.
  expect_equal(
    bloco$achados$etiqueta[bloco$achados$codigo == "II.5.6"],
    "consequência do gerador"
  )
})

test_that("as figuras do bloco II.5 carregam o proprio tema", {
  tema_anterior <- ggplot2::theme_get()
  ggplot2::theme_set(ggplot2::theme_grey())
  on.exit(ggplot2::theme_set(tema_anterior))

  bloco <- executar_ii5(carregar_dados())

  for (figura in bloco$figuras) {
    expect_s3_class(figura$theme$panel.border, "element_rect")
    expect_no_error(ggplot2::ggplot_build(figura))
  }
})
