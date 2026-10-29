test_that("a margem nao e atribuivel a uma alocacao individual", {
  dados <- carregar_dados()

  margem <- medir_atribuibilidade_da_margem(dados)

  expect_false(margem$tem_consultor_na_margem)
  expect_gt(margem$fracao_projetos_com_equipe, 0.5)
  # O custo zero da margem estrutural coincide com projeto sem alocacao, que e
  # o que separa a margem estrutural da mensal.
  expect_true(margem$custo_zero_estrutural_sem_alocacao)
})

test_that("o veredito das formas de margem responde as medidas", {
  medido <- avaliar_formas_da_margem(
    margem_atribuivel = FALSE, fracao_projetos_com_equipe = 0.685,
    tem_custo_mensal = FALSE, custo_zero_explicavel_estrutural = TRUE,
    desvio_da_mediana_alvo = 0.558, desvio_do_prejuizo_alvo = -0.013
  )
  hipotetico <- avaliar_formas_da_margem(
    margem_atribuivel = TRUE, fracao_projetos_com_equipe = 0,
    tem_custo_mensal = TRUE, custo_zero_explicavel_estrutural = TRUE,
    desvio_da_mediana_alvo = 0.02, desvio_do_prejuizo_alvo = 0.005
  )

  expect_equal(medido$forma[medido$viavel], "Estrutural")
  expect_true(all(hipotetico$viavel))
  expect_true(all(nzchar(medido$justificativa)))
})

test_that("mecanismo e calibracao sao eixos separados no veredito da margem", {
  # A margem estrutural e o caso que obriga a distincao: o mecanismo esta
  # certo e a escala do sintetico nao fecha.
  medido <- avaliar_formas_da_margem(
    margem_atribuivel = FALSE, fracao_projetos_com_equipe = 0.685,
    tem_custo_mensal = FALSE, custo_zero_explicavel_estrutural = TRUE,
    desvio_da_mediana_alvo = 0.558, desvio_do_prejuizo_alvo = -0.013
  )
  estrutural <- medido[medido$forma == "Estrutural", ]

  expect_true(estrutural$viavel)
  expect_false(estrutural$calibrado)

  calibrado <- avaliar_formas_da_margem(
    margem_atribuivel = FALSE, fracao_projetos_com_equipe = 0.685,
    tem_custo_mensal = FALSE, custo_zero_explicavel_estrutural = TRUE,
    desvio_da_mediana_alvo = 0.02, desvio_do_prejuizo_alvo = 0.005
  )
  expect_true(calibrado$calibrado[calibrado$forma == "Estrutural"])
})

test_that("a fracao de prejuizo certa nao basta para calibrar a margem", {
  # Olhar so a fracao de prejuizo marcaria a margem como calibrada com 0,7%
  # de prejuizo contra 2% reais enquanto a mediana fica a 55,8 pontos da real.
  centro_errado <- avaliar_formas_da_margem(
    margem_atribuivel = FALSE, fracao_projetos_com_equipe = 0.685,
    tem_custo_mensal = FALSE, custo_zero_explicavel_estrutural = TRUE,
    desvio_da_mediana_alvo = 0.558, desvio_do_prejuizo_alvo = -0.013
  )

  expect_false(centro_errado$calibrado[centro_errado$forma == "Estrutural"])
})

test_that("executar_ii5 devolve as quatro partes do bloco", {
  bloco <- executar_ii5(carregar_dados())

  expect_named(bloco, c("objetos", "tabelas", "figuras", "achados"))
  expect_true(all(vapply(bloco$tabelas, is.data.frame, logical(1))))
  expect_equal(nrow(bloco$achados), 7)
  expect_equal(bloco$achados$codigo, paste0("II.5.", 1:7))
})

test_that("acoplar_por_posto preserva a marginal exatamente", {
  set.seed(3)
  valores <- rlnorm(500)
  referencia <- rnorm(500)

  acoplado <- acoplar_por_posto(valores, referencia, forca = 0.7)

  expect_equal(sort(acoplado), sort(valores))
  expect_gt(cor(acoplado, referencia, method = "spearman"), 0.4)
})

test_that("forca zero no acoplamento nao induz correlacao", {
  set.seed(4)
  valores <- rlnorm(2000)
  referencia <- rnorm(2000)

  solto <- acoplar_por_posto(valores, referencia, forca = 0)

  expect_equal(sort(solto), sort(valores))
  expect_lt(abs(cor(solto, referencia, method = "spearman")), 0.1)
})

test_that("o acoplamento liga contrato e esforco ao esforco alocado", {
  # O defeito que motivou: na Camada 3 tudo era independente de tudo, e a
  # margem estrutural saia sem sentido.
  dados <- carregar_dados(tabelas = c("projetos", "alocacoes"))

  horas <- dados$alocacoes |>
    dplyr::mutate(h = horas_mes * duracao_dias / (365.25 / 12)) |>
    dplyr::summarise(horas = sum(h), .by = projeto_id)
  com_horas <- dplyr::inner_join(dados$projetos, horas, by = "projeto_id")

  expect_gt(
    cor(com_horas$valor_contrato, com_horas$horas, method = "spearman"), 0.3
  )
  expect_gt(
    cor(com_horas$esforco_estimado, com_horas$horas, method = "spearman"), 0.3
  )
})

test_that("o diagnostico de escala da margem mede sem decidir", {
  # A funcao existe para tornar a evidencia auditavel, nao para concluir que
  # a taxa e preco. O veredito depende de semantica de coluna que nao esta
  # registrada e cujo acesso encerrou.
  dados <- carregar_dados()

  escala <- diagnosticar_escala_da_margem(dados)

  expect_gt(escala$centro_da_razao, 0)
  # Guarda de regressao: com custo de verdade a razao centra bem abaixo de
  # 1. Se voltar a centrar em 1, alguem repos o rate card como custo.
  expect_false(escala$razao_centra_em_um)
  expect_lt(escala$centro_da_razao, 0.7)
  expect_equal(nrow(escala$custo_mensal), 5)
  expect_gt(escala$custo_mensal_mais_senior, escala$custo_mensal_mais_junior)
  # O custo mensal implicito e a taxa vezes a jornada, sem nada no meio.
  expect_equal(
    escala$custo_mensal$custo_mensal_implicito,
    escala$custo_mensal$taxa_mediana * escala$jornada_mensal
  )
})

test_that("as horas da margem saem do dado e a alocacao real aumenta o desacordo", {
  # O texto do II.5 dizia "cerca de metade das horas" digitado, e o numero
  # mudou para um quinto sem que nada acusasse quando a duracao da alocacao
  # foi recalibrada.
  dados <- carregar_dados(tabelas = c("projetos", "alocacoes"))
  sobredeterminacao <- medir_sobredeterminacao_economica()

  horas <- medir_horas_contra_contrato(dados, sobredeterminacao)

  expect_gt(horas$fracao_das_horas_implicadas, 0)
  expect_lt(horas$fracao_das_horas_implicadas, 1)
  expect_equal(horas$duracao_alocacao_dias, 18)
  expect_gt(horas$desacordo_com_alocacao, sobredeterminacao$desacordo)
})
