test_that("cada forma de divisao separa o que promete separar", {
  dados <- carregar_dados(tabelas = "alocacoes")

  por_consultor <- dividir_interacoes(dados$alocacoes, "por_consultor")
  por_projeto <- dividir_interacoes(dados$alocacoes, "por_projeto")
  temporal <- dividir_interacoes(dados$alocacoes, "temporal")

  # Em cada grupo, nenhum consultor do teste aparece no treino.
  for (grupo in por_consultor$grupos) {
    expect_length(intersect(grupo$teste$consultor_id, grupo$treino$consultor_id), 0)
  }
  expect_length(
    intersect(por_projeto$teste$projeto_id, por_projeto$treino$projeto_id), 0
  )
  # A divisao temporal nao cruza no tempo.
  expect_gte(min(temporal$teste$data_inicio), max(temporal$treino$data_inicio))
})

test_that("a divisao nao perde nem duplica alocacao", {
  dados <- carregar_dados(tabelas = "alocacoes")

  for (forma in c("temporal", "aleatoria", "por_projeto")) {
    divisao <- dividir_interacoes(dados$alocacoes, forma)
    expect_equal(
      nrow(divisao$treino) + nrow(divisao$teste), nrow(dados$alocacoes),
      info = forma
    )
  }

  por_consultor <- dividir_interacoes(dados$alocacoes, "por_consultor")
  for (grupo in por_consultor$grupos) {
    expect_equal(nrow(grupo$treino) + nrow(grupo$teste), nrow(dados$alocacoes))
  }
  # Reunidos, os testes cobrem cada alocação uma vez.
  expect_equal(sum(vapply(por_consultor$grupos, \(g) nrow(g$teste), integer(1))), nrow(dados$alocacoes))
  expect_equal(nrow(por_consultor$teste), nrow(dados$alocacoes))
})

test_that("os grupos por consultor não dependem da ordem das linhas", {
  # Era o defeito do sorteio único: tirar ou reordenar linhas trocava parte
  # dos consultores retidos.
  dados <- carregar_dados(tabelas = c("alocacoes", "consultores"))
  universo <- dados$consultores$consultor_id

  original <- dividir_por_consultor_em_grupos(dados$alocacoes, universo = universo)
  embaralhada <- withr::with_seed(3, dados$alocacoes[sample(nrow(dados$alocacoes)), ])
  reordenada <- dividir_por_consultor_em_grupos(embaralhada, universo = universo)
  sem_linhas <- dividir_por_consultor_em_grupos(dados$alocacoes[-(1:4), ], universo = universo)

  expect_identical(reordenada$grupo_do_consultor, original$grupo_do_consultor)
  expect_identical(sem_linhas$grupo_do_consultor, original$grupo_do_consultor)
  # Grupos balanceados em número de consultores.
  expect_lte(diff(range(table(original$grupo_do_consultor$grupo))), 1)
  expect_equal(dplyr::n_distinct(original$grupo_do_consultor$grupo), 5)
})

test_that("separar por entidade zera o vazamento e a avaliabilidade junto", {
  # E a oposicao que impede um protocolo unico: separar pessoas para nao
  # vazar atributo e exatamente criar cold start.
  dados <- carregar_dados()
  com_atributo <- unique(dados$experiencia_setorial$consultor_id)

  por_consultor <- medir_divisao(
    dividir_interacoes(dados$alocacoes, "por_consultor"), com_atributo
  )

  expect_equal(por_consultor$vazamento_consultor, 0)
  expect_equal(por_consultor$avaliavel_colaborativo, 0)
})

test_that("a divisao aleatoria infla a avaliabilidade do colaborativo", {
  # Ela so pontua porque deixa o mesmo projeto dos dois lados, o que em
  # producao nao acontece: projeto de consultoria aparece uma vez.
  dados <- carregar_dados()
  com_atributo <- unique(dados$experiencia_setorial$consultor_id)

  temporal <- medir_divisao(
    dividir_interacoes(dados$alocacoes, "temporal"), com_atributo
  )
  aleatoria <- medir_divisao(
    dividir_interacoes(dados$alocacoes, "aleatoria"), com_atributo
  )

  expect_gt(aleatoria$avaliavel_colaborativo, 5 * temporal$avaliavel_colaborativo)
  expect_gt(aleatoria$vazamento_projeto, 5 * temporal$vazamento_projeto)
})

test_that("nenhuma forma serve aos dois componentes", {
  dados <- carregar_dados()
  com_atributo <- unique(dados$experiencia_setorial$consultor_id)

  divisoes <- purrr::map(
    c("temporal", "aleatoria", "por_consultor", "por_projeto"),
    function(forma) {
      medir_divisao(dividir_interacoes(dados$alocacoes, forma), com_atributo)
    }
  ) |>
    purrr::list_rbind()

  protocolos <- avaliar_protocolos(divisoes)

  expect_false(any(protocolos$serve_colaborativo & protocolos$serve_conteudo))
})

test_that("a linha de base sob protocolo nao usa o teste para contar popularidade", {
  # Popularidade contada so no treino: se o teste inteiro for de um consultor
  # ausente do treino, ele fica no fim da lista e o acerto e zero.
  consultores <- tibble::tibble(consultor_id = paste0("c", 1:10))
  treino <- tibble::tibble(
    consultor_id = rep(paste0("c", 1:9), each = 3),
    projeto_id = "p1"
  )
  teste <- tibble::tibble(consultor_id = "c10", projeto_id = "p2")

  base <- medir_linha_base_sob_protocolo(treino, teste, consultores, tamanhos = c(5, 9))

  expect_equal(base$recall[base$n_recomendados == 5], 0)
})

test_that("o desempate da o acerto esperado e nao o acerto da posicao esperada", {
  # Dez consultores com a mesma popularidade: o focal cai com a mesma chance
  # em qualquer das dez posicoes. Desempate favoravel o poria em primeiro e
  # daria acerto em qualquer lista. Comparar a posicao media, 5,5, com o
  # tamanho da lista daria zero em lista de 4 e um em lista de 6, que era o
  # erro da versao anterior: o acerto esperado e a fracao do bloco que cabe
  # na lista.
  consultores <- tibble::tibble(consultor_id = paste0("c", 1:10))
  treino <- tibble::tibble(
    consultor_id = rep(paste0("c", 1:10), each = 2), projeto_id = "p1"
  )
  teste <- tibble::tibble(consultor_id = "c1", projeto_id = "p2")

  base <- medir_linha_base_sob_protocolo(treino, teste, consultores, tamanhos = c(4, 6))

  expect_equal(base$recall[base$n_recomendados == 4], 0.4)
  expect_equal(base$recall[base$n_recomendados == 6], 0.6)
})

test_that("executar_ii7 devolve as quatro partes do bloco", {
  dados <- carregar_dados()

  bloco <- executar_ii7(dados)

  expect_named(bloco, c("objetos", "tabelas", "figuras", "achados"))
  expect_true(all(vapply(bloco$tabelas, is.data.frame, logical(1))))
  expect_true(all(vapply(bloco$figuras, ggplot2::is_ggplot, logical(1))))
  expect_equal(nrow(bloco$achados), 8)
  expect_true(all(startsWith(bloco$achados$codigo, "II.7.")))
  expect_true(bloco$objetos$nenhuma_serve_aos_dois)
})

test_that("a figura do bloco II.7 carrega o proprio tema", {
  tema_anterior <- ggplot2::theme_get()
  ggplot2::theme_set(ggplot2::theme_grey())
  on.exit(ggplot2::theme_set(tema_anterior))

  bloco <- executar_ii7(carregar_dados())

  for (figura in bloco$figuras) {
    expect_s3_class(figura$theme$panel.border, "element_rect")
    expect_no_error(ggplot2::ggplot_build(figura))
  }
})

test_that("o piso de filtro com acaso da a fracao do conjunto filtrado que cabe na lista", {
  # Setor A tem dois consultores, setor B tem um, e c4 nao declarou setor.
  consultores <- tibble::tibble(consultor_id = paste0("c", 1:4))
  experiencia <- tibble::tibble(
    consultor_id = c("c1", "c2", "c3"), segmento = c("A", "A", "B"),
    intensidade = 1
  )
  projetos <- tibble::tibble(projeto_id = "p1", setor = "A")
  teste <- tibble::tibble(consultor_id = c("c1", "c3"), projeto_id = "p1")

  # Lista de um: c1 passa e divide a vaga com c2; c3 nao passa e nao sobra
  # vaga depois dos dois filtrados.
  um <- medir_piso_filtro_e_acaso(teste, consultores, projetos, experiencia, 1)
  expect_equal(um$recall, mean(c(0.5, 0)))
  expect_equal(um$fracao_passa, 0.5)

  # Lista de tres: c1 entra sempre; c3 disputa a vaga que sobra com c4.
  tres <- medir_piso_filtro_e_acaso(teste, consultores, projetos, experiencia, 3)
  expect_equal(tres$recall, mean(c(1, 0.5)))
})

test_that("a regiao critica e a do McNemar exato bilateral", {
  # Com cinco discordantes nem a unanimidade rejeita: 2 * 0,5^5 = 0,0625.
  # Com seis, so a unanimidade: 2 * 0,5^6 = 0,03125.
  expect_equal(calcular_regiao_critica_mcnemar_exato(c(0, 5, 6), 0.05), c(Inf, Inf, 6))
  # A forma fechada coincide com a varredura de todas as contagens.
  varredura <- function(d) {
    if (d == 0) return(Inf)
    b <- 0:d
    valor_p <- pmin(1, 2 * stats::pbinom(b - 1, d, 0.5, lower.tail = FALSE))
    rejeita <- b[valor_p <= 0.05 & b > d / 2]
    if (length(rejeita) == 0) Inf else min(rejeita)
  }
  expect_identical(
    calcular_regiao_critica_mcnemar_exato(0:1500, 0.05),
    vapply(0:1500, varredura, numeric(1))
  )
  # Coincide com binom.test em toda contagem.
  for (d in c(10, 25, 60)) {
    critico <- calcular_regiao_critica_mcnemar_exato(d, 0.05)
    expect_lte(binom.test(critico, d)$p.value, 0.05)
    expect_gt(binom.test(critico - 1, d)$p.value, 0.05)
  }
})

test_that("o ganho minimo da o poder pedido no teste exato e respeita os dois cenarios", {
  otimista <- calcular_ganho_minimo_detectavel(300, 0.2, "dominancia")
  conservador <- calcular_ganho_minimo_detectavel(300, 0.2, "independencia")

  # Dominancia e o minimo de discordancia possivel, entao e o cenario que
  # enxerga o menor ganho.
  expect_lt(otimista, conservador)
  expect_equal(calcular_poder_mcnemar_exato(300, 0.2, conservador, "independencia"), 0.8, tolerance = 1e-6)
  expect_equal(calcular_poder_mcnemar_exato(300, 0.2, otimista, "dominancia"), 0.8, tolerance = 1e-6)

  # Sob dominancia o poder tem forma fechada: rejeita quando ha seis ou mais
  # discordantes, todos a favor do modelo.
  expect_equal(
    calcular_poder_mcnemar_exato(30, 0.2, 0.1, "dominancia"),
    stats::pbinom(5, 30, 0.1, lower.tail = FALSE)
  )
})

test_that("mais pares deixam enxergar ganhos menores, e pares de menos nada", {
  ganhos <- vapply(
    c(50, 200, 800), calcular_ganho_minimo_detectavel, numeric(1), piso = 0.2
  )

  expect_true(all(diff(ganhos) < 0))
  expect_true(is.na(calcular_ganho_minimo_detectavel(3, 0.2)))
  expect_true(is.na(calcular_ganho_minimo_detectavel(0, 0.2)))
})

test_that("sob divisao por consultor o piso de popularidade do conteudo e zero", {
  # E consequencia da divisao, e nao do conjunto: nenhum consultor de teste
  # foi visto no treino. Se um dia sair de zero, a divisao vazou.
  dados <- carregar_dados()

  poder <- planejar_poder_por_componente(
    dados$alocacoes, dados$consultores, dados$projetos, dados$experiencia_setorial
  )
  popularidade_no_conteudo <- poder$valor_piso[
    poder$componente == "Conteúdo" & poder$piso == "Popularidade no treino"
  ]

  expect_equal(nrow(poder), 4)
  expect_equal(popularidade_no_conteudo, 0)
})
