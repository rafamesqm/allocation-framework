test_that("gerar_variavel_por_nivel nao empilha massa nos limites", {
  # Defeito corrigido: pmax/pmin empilhava no limite toda a massa que caia
  # fora dele. Saiam 7,2% das alocacoes com pct_alocacao exatamente 1,00 e
  # 4,2% com horas_mes exatamente 0, valores sem contraparte no dado real.
  set.seed(1)
  nivel <- codificar_senioridade(sample(niveis_senioridade(), 5000, replace = TRUE))

  pct <- gerar_variavel_por_nivel(
    nivel, correlacao_alvo = 0.52, media = 0.7, desvio_variavel = 0.2,
    minimo = 0.05, maximo = 1
  )
  horas <- gerar_variavel_por_nivel(
    nivel, correlacao_alvo = -0.55, media = 120, desvio_variavel = 66.9,
    minimo = 0
  )

  expect_lt(mean(pct == 1), 0.001)
  expect_lt(mean(pct == 0.05), 0.001)
  expect_lt(mean(horas == 0), 0.001)
  expect_true(all(pct >= 0.05 & pct <= 1))
  expect_true(all(horas >= 0))
})

test_that("a reamostragem preserva a correlacao alvo com o nivel", {
  # A rejeicao so redesenha o ruido, nunca o sinal, justamente para nao
  # destruir a calibracao.
  set.seed(2)
  nivel <- codificar_senioridade(sample(niveis_senioridade(), 20000, replace = TRUE))

  pct <- gerar_variavel_por_nivel(
    nivel, correlacao_alvo = 0.52, media = 0.7, desvio_variavel = 0.2,
    minimo = 0.05, maximo = 1
  )

  expect_equal(stats::cor(senioridade_numerica(nivel), pct), 0.52, tolerance = 0.06)
})

test_that("gerar_variavel_por_nivel termina mesmo no caso degenerado", {
  # Ruido nulo: a rejeicao nunca convergiria, e o recorte de ultimo recurso
  # garante terminacao em vez de laco infinito.
  set.seed(3)
  nivel <- codificar_senioridade(rep(niveis_senioridade(), 20))

  valor <- gerar_variavel_por_nivel(
    nivel, correlacao_alvo = 1, media = 0.5, desvio_variavel = 0.2,
    minimo = 0.45, maximo = 0.55, max_tentativas = 5
  )

  expect_true(all(valor >= 0.45 & valor <= 0.55))
  expect_length(valor, length(nivel))
})

test_that("todo consultor do nivel de base e CLT", {
  # Ancora exata da Sexta rodada: 42 ESTAGIARIO coincide com 42 N4.
  set.seed(4)
  nivel <- codificar_senioridade(sample(niveis_senioridade(), 2000, replace = TRUE))

  contrato <- gerar_tipo_contrato(nivel)

  expect_true(all(contrato[as.character(nivel) == "N4"] == "CLT"))
  expect_true(any(contrato[as.character(nivel) != "N4"] == "PJ"))
})

test_that("gerar_tipo_contrato preserva a fracao total de CLT calibrada", {
  set.seed(5)
  nivel <- codificar_senioridade(sample(
    niveis_senioridade(), 20000, replace = TRUE, prob = c(42, 107, 71, 55, 8)
  ))

  contrato <- gerar_tipo_contrato(nivel, fracao_clt = 0.684)

  expect_equal(mean(contrato == "CLT"), 0.684, tolerance = 0.02)
})

test_that("gerar_tipo_contrato nao produz probabilidade negativa se a base excede o alvo", {
  nivel <- codificar_senioridade(rep("N4", 100))

  expect_no_error(gerar_tipo_contrato(nivel, fracao_clt = 0.5))
  expect_true(all(gerar_tipo_contrato(nivel, fracao_clt = 0.5) == "CLT"))
})

test_that("a recencia de skill e constante dentro de cada consultor", {
  # Na fonte real as skills de uma pessoa vem de uma resposta de formulario
  # so, logo compartilham a data de atualizacao.
  set.seed(6)
  consultores <- gerar_consultores(n_consultores = 200)
  skills <- gerar_skills_consultor(consultores, gerar_espaco_skills())

  por_consultor <- skills |>
    dplyr::summarise(valores = dplyr::n_distinct(recencia_dias), .by = consultor_id)

  expect_true(all(por_consultor$valores == 1))
})

test_that("a recencia se agrupa em poucas ondas, em vez de ser uniforme", {
  set.seed(7)
  consultores <- gerar_consultores(n_consultores = 285)
  skills <- gerar_skills_consultor(
    consultores, gerar_espaco_skills(),
    ondas_recencia_dias = c(110, 365, 620), dispersao_onda_dias = 15
  )

  recencias <- unique(skills$recencia_dias)
  perto_de_alguma_onda <- vapply(
    recencias,
    function(r) min(abs(r - c(110, 365, 620))) <= 60,
    logical(1)
  )

  expect_true(all(perto_de_alguma_onda))
  # Uniforme entre 1 e 730 teria valores espalhados por toda a faixa; em
  # ondas, faixas inteiras ficam vazias.
  expect_equal(length(unique(round(recencias / 100))), 3, tolerance = 1)
})
