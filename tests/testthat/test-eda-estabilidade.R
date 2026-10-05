test_that("medir_realizacao devolve uma linha com as grandezas dos achados", {
  medida <- medir_realizacao(semente = 1, n_consultores = 80, n_projetos = 120)

  expect_equal(nrow(medida), 1)
  expect_true(all(c(
    "densidade_R", "cold_start_consultor", "cobertura_skills",
    "cor_nivel_custo", "agrupamento_grafo", "acima_capacidade"
  ) %in% names(medida)))
  # O ganho minimo do colaborativo e NA por definicao quando ha pares de
  # menos para distinguir ate um modelo perfeito, que e o caso nesta
  # populacao reduzida. Na escala completa ha pares de sobra para isso.
  legitimamente_na <- c(
    "agrupamento_grafo", "ganho_minimo_colaborativo",
    "ganho_minimo_colaborativo_dominancia"
  )
  expect_false(anyNA(dplyr::select(medida, -dplyr::any_of(legitimamente_na))))
})

test_that("a mesma semente devolve a mesma realizacao", {
  expect_equal(
    medir_realizacao(7, n_consultores = 60, n_projetos = 90),
    medir_realizacao(7, n_consultores = 60, n_projetos = 90)
  )
})

test_that("sementes diferentes devolvem realizacoes diferentes", {
  # E o ponto do bloco inteiro: um numero reportado e um sorteio.
  primeira <- medir_realizacao(1, n_consultores = 80, n_projetos = 120)
  segunda <- medir_realizacao(2, n_consultores = 80, n_projetos = 120)

  expect_false(identical(primeira$densidade_R, segunda$densidade_R))
})

test_that("resumir_estabilidade calcula mediana e intervalo pedido", {
  replicas <- tibble::tibble(semente = 1:9, medida_teste = 1:9)

  resumo <- resumir_estabilidade(replicas, nivel = 0.8)

  expect_equal(resumo$mediana, 5)
  expect_equal(resumo$inferior, stats::quantile(1:9, 0.1, names = FALSE))
  expect_equal(resumo$superior, stats::quantile(1:9, 0.9, names = FALSE))
  expect_equal(resumo$n_replicas, 9)
})

test_that("resumir_estabilidade nao divide por zero em grandeza nula", {
  replicas <- tibble::tibble(semente = 1:5, sempre_zero = rep(0, 5))

  resumo <- resumir_estabilidade(replicas)

  expect_true(is.na(resumo$amplitude_relativa))
  expect_equal(resumo$desvio_mc, 0)
})

test_that("a fracao de cold start do consultor nao varia entre realizacoes", {
  # Invariancia aqui nao e robustez, e parametro imposto ao gerador. O teste
  # trava essa leitura: se um dia passar a variar, o achado I.0.3 muda.
  replicas <- simular_estabilidade(1:4, n_consultores = 285, n_projetos = 952)

  expect_equal(stats::sd(replicas$cold_start_consultor), 0)
})

test_that("formatar_intervalo devolve mediana e extremos, ou NA se nao medida", {
  estabilidade <- tibble::tibble(
    medida = "x", mediana = 0.5, desvio_mc = 0.1,
    inferior = 0.4, superior = 0.6, amplitude_relativa = 0.4, n_replicas = 10
  )

  expect_match(formatar_intervalo(estabilidade, "x"), "50,0%")
  expect_match(formatar_intervalo(estabilidade, "x"), "40,0% a 60,0%")
  expect_true(is.na(formatar_intervalo(estabilidade, "inexistente")))
})

test_that("toda grandeza medida esta mapeada a algum achado", {
  # Sem o mapa, a tabela de estabilidade e diagnostico solto: e ele que
  # transforma fragilidade de grandeza em afirmacao sobre achado.
  medidas <- names(medir_realizacao(1, n_consultores = 60, n_projetos = 90))
  medidas <- setdiff(medidas, "semente")

  expect_setequal(medidas, mapa_medida_achado()$medida)
})

test_that("executar_i0 classifica fragil, estavel e invariante", {
  replicas <- simular_estabilidade(1:5, n_consultores = 285, n_projetos = 952)

  bloco <- executar_i0(replicas)

  expect_named(bloco, c("objetos", "tabelas", "figuras", "achados"))
  expect_equal(nrow(bloco$achados), 4)
  expect_true(all(
    bloco$objetos$classificado$classe %in% c("frágil", "estável", "invariante")
  ))
  expect_true("cold_start_consultor" %in% bloco$objetos$invariantes$medida)
})

test_that("os blocos aceitam estabilidade e citam o intervalo no achado", {
  dados <- carregar_dados()
  replicas <- simular_estabilidade(1:4, n_consultores = 285, n_projetos = 952)
  estabilidade <- resumir_estabilidade(replicas)

  sem_intervalo <- executar_i1(dados)
  com_intervalo <- executar_i1(dados, estabilidade = estabilidade)

  achado_sem <- sem_intervalo$achados$enunciado[sem_intervalo$achados$codigo == "I.1.3"]
  achado_com <- com_intervalo$achados$enunciado[com_intervalo$achados$codigo == "I.1.3"]

  # Sem estabilidade o achado nao pode citar faixa entre realizacoes; com ela,
  # precisa citar. Testado pela mencao a realizacoes, e nao pela presenca do
  # conectivo " a ", que aparece na prosa comum do enunciado.
  expect_false(grepl("realizações", achado_sem, fixed = TRUE))
  expect_match(achado_com, "realizações")
  expect_true(nchar(achado_com) > nchar(achado_sem))
})

test_that("intervalo_separa_de_zero distingue intervalo que contem o zero", {
  # E o teste que o achado II.6.6 precisava e nao tinha: afirmar que uma
  # diferenca e positiva exige que o intervalo dela nao contenha o zero.
  estabilidade <- tibble::tibble(
    medida = c("positiva", "cruza", "negativa"),
    inferior = c(0.01, -0.01, -0.05),
    superior = c(0.06, 0.06, -0.02)
  )

  expect_true(intervalo_separa_de_zero(estabilidade, "positiva"))
  expect_false(intervalo_separa_de_zero(estabilidade, "cruza"))
  expect_true(intervalo_separa_de_zero(estabilidade, "negativa"))
  expect_true(is.na(intervalo_separa_de_zero(estabilidade, "inexistente")))
  expect_true(is.na(intervalo_separa_de_zero(NULL, "positiva")))
})

test_that("a folga do one-hot e medida como grandeza propria e bate com a diferenca", {
  medida <- medir_realizacao(1, n_consultores = 80, n_projetos = 120)

  expect_equal(
    medida$folga_one_hot_popularidade,
    medida$auc_one_hot - medida$auc_popularidade_discriminacao
  )
})

test_that("descrever_ponto_fora fala so do que caiu fora e cala no resto", {
  expect_equal(descrever_ponto_fora(c(a = "dentro", b = NA)), "")
  expect_equal(descrever_ponto_fora(NA_character_), "")

  uma <- descrever_ponto_fora(c("lista de 20" = "acima", "lista de 50" = "dentro"))
  expect_match(uma, "acima do intervalo em lista de 20")
  expect_no_match(uma, "lista de 50")

  sem_nome <- descrever_ponto_fora("abaixo")
  expect_match(sem_nome, "^; a realização versionada cai abaixo do intervalo")
})
