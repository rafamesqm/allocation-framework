dados_minimos_propensao <- function() {
  list(
    consultores = tibble::tibble(consultor_id = c("c1", "c2", "c3")),
    projetos = tibble::tibble(
      projeto_id = c("p1", "p2", "p3", "p4"),
      setor = c("Varejo", "Varejo", "Banking", "Banking")
    ),
    alocacoes = tibble::tibble(
      consultor_id = c("c1", "c1", "c2"),
      projeto_id = c("p1", "p2", "p1")
    ),
    experiencia_setorial = tibble::tibble(
      consultor_id = c("c1", "c2"),
      segmento = c("Varejo", "Banking"),
      intensidade = c(0.9, 0.8)
    )
  )
}

test_that("a propensao soma o numero de observacoes sobre a matriz inteira", {
  # E o que a torna distribuicao de exposicao, e nao escore arbitrario.
  d <- dados_minimos_propensao()

  propensao <- estimar_propensao_por_popularidade(d$alocacoes, d$consultores, d$projetos)

  expect_equal(sum(propensao$matriz), nrow(d$alocacoes))
  expect_equal(dim(propensao$matriz), c(nrow(d$consultores), nrow(d$projetos)))
})

test_that("entidade sem alocacao tem propensao zero em toda a sua linha ou coluna", {
  # E a falha de positividade: nenhum peso finito recupera esse estrato.
  d <- dados_minimos_propensao()

  propensao <- estimar_propensao_por_popularidade(d$alocacoes, d$consultores, d$projetos)

  expect_true(all(propensao$matriz["c3", ] == 0))
  expect_true(all(propensao$matriz[, "p3"] == 0))
  expect_true(all(propensao$matriz[, "p4"] == 0))
})

test_that("o diagnostico de positividade conta os zeros dos dois lados", {
  d <- dados_minimos_propensao()
  propensao <- estimar_propensao_por_popularidade(d$alocacoes, d$consultores, d$projetos)

  pos <- diagnosticar_positividade(propensao, d$alocacoes, d$consultores, d$projetos)

  expect_equal(pos$consultores_grau_zero, 1)
  expect_equal(pos$projetos_grau_zero, 2)
  # c3 inteira (4 celulas) mais p3 e p4 de c1 e c2 (4 celulas), sem dupla contagem.
  expect_equal(pos$pares_sem_positividade, 8)
  expect_equal(pos$fracao_pares_sem_positividade, 8 / 12)
})

test_that("o tamanho efetivo de amostra nunca passa do numero de observacoes", {
  # Propriedade do estimador de Kish: so vale a igualdade com pesos iguais.
  d <- dados_minimos_propensao()
  propensao <- estimar_propensao_por_popularidade(d$alocacoes, d$consultores, d$projetos)

  pos <- diagnosticar_positividade(propensao, d$alocacoes, d$consultores, d$projetos)

  expect_lte(pos$ess, nrow(d$alocacoes))
  expect_gt(pos$ess, 0)
  expect_equal(pos$ess_relativo, pos$ess / nrow(d$alocacoes))
})

test_that("pesos iguais devolvem tamanho efetivo igual ao nominal", {
  # Caso de contorno que fixa a escala do diagnostico: matriz simetrica, todos
  # os graus iguais, nenhuma informacao perdida na reponderacao.
  d <- list(
    consultores = tibble::tibble(consultor_id = c("c1", "c2")),
    projetos = tibble::tibble(projeto_id = c("p1", "p2")),
    alocacoes = tibble::tibble(
      consultor_id = c("c1", "c2"), projeto_id = c("p1", "p2")
    )
  )
  propensao <- estimar_propensao_por_popularidade(d$alocacoes, d$consultores, d$projetos)

  pos <- diagnosticar_positividade(propensao, d$alocacoes, d$consultores, d$projetos)

  expect_equal(pos$ess, 2)
  expect_equal(pos$ess_relativo, 1)
})

test_that("a aproximacao de probabilidade e verificada e nao suposta", {
  # d_i * d_j / m e contagem esperada, e so aproxima probabilidade sob
  # esparsidade. Sobre o dado do projeto a aproximacao vale; o teste existe
  # para acusar se deixar de valer.
  dados <- carregar_dados(tabelas = c("consultores", "projetos", "alocacoes"))
  propensao <- estimar_propensao_por_popularidade(
    dados$alocacoes, dados$consultores, dados$projetos
  )

  pos <- diagnosticar_positividade(
    propensao, dados$alocacoes, dados$consultores, dados$projetos
  )

  expect_true(pos$aproximacao_valida)
  expect_lt(pos$propensao_maxima, 1)
})

test_that("a cobertura da covariavel de propensao acompanha o formulario", {
  dados <- carregar_dados()
  propensao <- estimar_propensao_por_popularidade(
    dados$alocacoes, dados$consultores, dados$projetos
  )

  vies <- medir_vies_de_selecao(dados, propensao)

  # A covariavel vem de experiencia_setorial, restrita aos respondentes.
  expect_equal(vies$cobertura_covariavel_consultores, 0.484, tolerance = 0.01)
  expect_lt(vies$cobertura_covariavel_consultores, 1)
  expect_gt(vies$taxa_observada, vies$taxa_ao_acaso)
})

test_that("a propensao por popularidade e cega a afinidade setorial", {
  # Conclusao estrutural do bloco, e a que nao depende da magnitude do vies:
  # um modelo que so usa popularidade nao pode capturar afinidade.
  dados <- carregar_dados()
  propensao <- estimar_propensao_por_popularidade(
    dados$alocacoes, dados$consultores, dados$projetos
  )

  vies <- medir_vies_de_selecao(dados, propensao)

  expect_lt(
    abs(vies$taxa_sob_popularidade - vies$taxa_ao_acaso),
    abs(vies$taxa_sob_popularidade - vies$taxa_observada)
  )
})

test_that("nenhuma fonte que falsificaria a propensao esta disponivel", {
  amostras <- verificar_amostra_de_exposicao_aleatoria()

  expect_false(any(amostras$disponivel))
  expect_true(all(nzchar(amostras$observacao)))
  expect_gte(nrow(amostras), 3)
})

test_that("o veredito dos estimadores responde as medidas, e nao e fixo", {
  ruim <- avaliar_estimadores_de_propensao(
    positividade_completa = FALSE, propensao_captura_mecanismo = FALSE,
    cobertura_covariavel = 0.484, ess_relativo = 0.24,
    existe_amostra_aleatoria = FALSE
  )
  bom <- avaliar_estimadores_de_propensao(
    positividade_completa = TRUE, propensao_captura_mecanismo = TRUE,
    cobertura_covariavel = 1, ess_relativo = 0.9,
    existe_amostra_aleatoria = TRUE
  )

  expect_equal(sum(ruim$viavel), 1)
  expect_true(ruim$viavel[ruim$estimador == "Estimador sobre o observado, com viés declarado"])
  expect_true(all(bom$viavel))
  expect_true(all(nzchar(ruim$exige)))
  expect_true(all(nzchar(ruim$justificativa)))
})

test_that("posicao_no_intervalo acusa ponto fora do proprio intervalo", {
  estabilidade <- tibble::tibble(
    medida = "x", mediana = 0.3, desvio_mc = 0.01,
    inferior = 0.25, superior = 0.37, amplitude_relativa = 0.4, n_replicas = 100
  )

  expect_equal(posicao_no_intervalo(estabilidade, "x", 0.30), "dentro")
  expect_equal(posicao_no_intervalo(estabilidade, "x", 0.24), "abaixo")
  expect_equal(posicao_no_intervalo(estabilidade, "x", 0.40), "acima")
  expect_true(is.na(posicao_no_intervalo(estabilidade, "inexistente", 1)))
  expect_true(is.na(posicao_no_intervalo(NULL, "x", 1)))
})

test_that("executar_ii4 devolve as quatro partes do bloco", {
  dados <- carregar_dados()

  bloco <- executar_ii4(dados)

  expect_named(bloco, c("objetos", "tabelas", "figuras", "achados"))
  expect_true(all(vapply(bloco$tabelas, is.data.frame, logical(1))))
  expect_true(all(vapply(bloco$figuras, ggplot2::is_ggplot, logical(1))))
  expect_equal(nrow(bloco$achados), 5)
  expect_true(all(startsWith(bloco$achados$codigo, "II.4.")))
  # O achado de magnitude do vies precisa estar etiquetado como gerador, senao
  # a circularidade do bloco deixa de ficar declarada.
  expect_equal(
    bloco$achados$etiqueta[bloco$achados$codigo == "II.4.1"],
    "consequência do gerador"
  )
})

test_that("as figuras do bloco II.4 carregam o proprio tema", {
  tema_anterior <- ggplot2::theme_get()
  ggplot2::theme_set(ggplot2::theme_grey())
  on.exit(ggplot2::theme_set(tema_anterior))

  bloco <- executar_ii4(carregar_dados())

  for (figura in bloco$figuras) {
    expect_s3_class(figura$theme$panel.border, "element_rect")
    expect_no_error(ggplot2::ggplot_build(figura))
  }
})
