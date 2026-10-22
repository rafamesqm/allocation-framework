alocacoes_de_teste <- function() {
  tibble::tibble(
    consultor_id = c("c1", "c1", "c2"),
    projeto_id = c("p1", "p2", "p3"),
    horas_mes = c(100, 40, 60),
    data_inicio = as.Date(c("2026-03-01", "2026-03-01", "2026-03-01")),
    data_fim = as.Date(c("2026-03-31", "2026-03-31", "2026-03-31"))
  )
}

sem_ferias <- function() {
  tibble::tibble(
    consultor_id = character(0),
    inicio_ferias = as.Date(character(0)),
    fim_ferias = as.Date(character(0))
  )
}

criterios_de_teste <- function(...) {
  base <- tibble::tibble(
    discordancia_id = 1L,
    folga_recomendado = 50, folga_realizado = 50,
    custo_recomendado = 200, custo_realizado = 200,
    setor_recomendado = "compatível", setor_realizado = "compatível",
    senioridade_recomendado = 0, senioridade_realizado = 0,
    tempo_casa_recomendado = 3, tempo_casa_realizado = 3
  )
  dplyr::mutate(base, ...)
}

test_that("a alocacao analisada nao entra na folga de capacidade", {
  # Sem a exclusao, o realizado pagaria em folga a propria alocacao julgada:
  # c1 tem 100 horas no projeto analisado e 40 em outro.
  folga <- medir_folga_de_capacidade(
    "c1", "2026-03", alocacoes_de_teste(), projeto_excluido = "p1",
    ferias = sem_ferias(), capacidade_mensal_horas = 168
  )

  expect_equal(folga, 168 - 40)
})

test_that("a folga do recomendado desconta todas as alocacoes dele no mes", {
  folga <- medir_folga_de_capacidade(
    c("c2", "c1"), c("2026-03", "2026-04"), alocacoes_de_teste(),
    projeto_excluido = c("p1", "p1"), ferias = sem_ferias(),
    capacidade_mensal_horas = 168
  )

  expect_equal(folga, c(168 - 60, 168))
})

test_that("as ferias no mes descontam a capacidade, ponderadas pelos dias", {
  # c2 tira 10 dos 31 dias de marco; a capacidade cai na mesma fracao, e as
  # 60 horas da outra alocacao saem depois.
  ferias <- tibble::tibble(
    consultor_id = "c2",
    inicio_ferias = as.Date("2026-03-01"),
    fim_ferias = as.Date("2026-03-10")
  )

  folga <- medir_folga_de_capacidade(
    c("c2", "c1"), c("2026-03", "2026-03"), alocacoes_de_teste(),
    projeto_excluido = c("p9", "p1"), ferias = ferias,
    capacidade_mensal_horas = 168
  )

  expect_equal(folga[1], 168 * (1 - 10 / 31) - 60)
  expect_equal(folga[2], 168 - 40)
})

test_that("o recomendado comparado e o primeiro fora do time no mes", {
  # c1 ja esta em p1 em marco, entao o comparado e o segundo da lista; em
  # abril c1 ja saiu do projeto e volta a ser o primeiro.
  escolhido <- selecionar_recomendado_comparado(
    recomendados = list(c("c1", "c3"), c("c1", "c3"), c("c1")),
    projeto_id = c("p1", "p1", "p1"),
    mes = c("2026-03", "2026-04", "2026-03"),
    alocacoes = alocacoes_de_teste()
  )

  expect_equal(escolhido, c("c3", "c1", NA_character_))
})

test_that("a distancia de senioridade usa a mediana das posicoes e e NA sem time", {
  expect_equal(medir_distancia_de_senioridade(3, c(1, 2, 4)), 1)
  expect_equal(medir_distancia_de_senioridade(0, c(1, 2)), 1.5)
  expect_true(is.na(medir_distancia_de_senioridade(2, numeric(0))))
})

test_that("setor nao declarado fica desconhecido, e nao incompativel", {
  expect_equal(
    comparar_setor(c("Varejo", "Banking", NA), "Varejo"),
    c("compatível", "incompatível", "desconhecido")
  )
})

test_that("projeto sem setor tambem fica desconhecido", {
  expect_equal(
    comparar_setor(c("Varejo", NA), NA_character_),
    c("desconhecido", "desconhecido")
  )
})

test_that("a dominancia separa melhor, pior e incomparaveis", {
  melhor <- criterios_de_teste(custo_recomendado = 150)
  pior <- criterios_de_teste(folga_recomendado = 10)
  incomparaveis <- criterios_de_teste(custo_recomendado = 150, folga_recomendado = 10)

  expect_equal(classificar_discordancia_por_dominancia(melhor)$classe, "recomendado melhor")
  expect_equal(classificar_discordancia_por_dominancia(pior)$classe, "recomendado pior")
  expect_equal(classificar_discordancia_por_dominancia(incomparaveis)$classe, "incomparáveis")
})

test_that("o empate nos quatro criterios e desfeito pelo maior tempo de casa", {
  veterano_recomendado <- criterios_de_teste(tempo_casa_recomendado = 8)
  veterano_realizado <- criterios_de_teste(tempo_casa_realizado = 8)
  empate <- criterios_de_teste()

  a <- classificar_discordancia_por_dominancia(veterano_recomendado)
  b <- classificar_discordancia_por_dominancia(veterano_realizado)
  c <- classificar_discordancia_por_dominancia(empate)

  expect_equal(a$classe, "recomendado melhor")
  expect_true(a$desempate_por_tempo_de_casa)
  expect_equal(b$classe, "recomendado pior")
  expect_equal(c$classe, "empate")
  expect_false(c$desempate_por_tempo_de_casa)
})

test_that("criterio desconhecido sai da comparacao daquele par", {
  # O realizado ganharia no setor se ele contasse; desconhecido para o
  # recomendado, o par e julgado so pelos outros tres, onde o recomendado
  # ganha no custo.
  par <- criterios_de_teste(
    setor_recomendado = "desconhecido", setor_realizado = "compatível",
    custo_recomendado = 150
  )

  classificado <- classificar_discordancia_por_dominancia(par)

  expect_equal(classificado$classe, "recomendado melhor")
  expect_equal(classificado$criterios_desconhecidos[[1]], "setor")
  expect_equal(
    contar_criterios_desconhecidos(classificado)$pares,
    c(0L, 0L, 1L, 0L)
  )
})

test_that("o sorteio estratifica, exclui a zona morta e reporta estrato vazio", {
  classificadas <- tibble::tibble(
    discordancia_id = 1:10,
    ramo = rep(c("conteúdo", "colaborativo"), each = 5),
    classe = c(rep("recomendado melhor", 4), "recomendado pior",
               rep("incomparáveis", 5)),
    na_zona_morta = c(rep(FALSE, 9), TRUE),
    desempate_por_tempo_de_casa = c(TRUE, TRUE, FALSE, FALSE, TRUE, rep(FALSE, 5))
  )

  sorteio <- sortear_casos_para_leitura(classificadas, por_estrato = 2, semente = 1)
  estratos <- sorteio$estratos

  expect_equal(nrow(estratos), 6)
  expect_false(10L %in% sorteio$casos$discordancia_id)
  expect_equal(
    estratos$sorteados[estratos$ramo == "conteúdo" & estratos$classe == "recomendado melhor"],
    2L
  )
  expect_true(estratos$vazio[estratos$ramo == "colaborativo" & estratos$classe == "recomendado melhor"])
  expect_identical(
    sorteio$casos,
    sortear_casos_para_leitura(classificadas, por_estrato = 2, semente = 1)$casos
  )
  # Dois dos quatro "melhor" de conteudo e o unico "pior" vieram do desempate.
  desempates <- sorteio$desempates_por_classe
  expect_equal(
    desempates$disponiveis_por_desempate[desempates$classe == "recomendado melhor"], 2L
  )
  expect_equal(
    desempates$disponiveis_por_desempate[desempates$classe == "recomendado pior"], 1L
  )
  expect_equal(
    desempates$disponiveis_por_desempate[desempates$classe == "incomparáveis"], 0L
  )
  expect_true(all(estratos$sorteados_por_desempate <= estratos$sorteados))
})

test_that("a rubrica traz os quatro campos em branco e os vereditos fixos", {
  casos <- tibble::tibble(
    discordancia_id = 1L, projeto_id = "p1", consultor_recomendado = "c2",
    consultor_realizado = "c1", ramo = "conteúdo", filtro = "Varejo",
    posicao_popularidade = 3L, classe = "recomendado melhor"
  )

  rubrica <- montar_rubrica_de_leitura(casos)

  expect_true(all(is.na(rubrica[c(
    "disponibilidade_do_recomendado", "compatibilidade_de_setor",
    "justificativa_observavel", "veredito"
  )])))
  expect_equal(levels(rubrica$veredito), listar_vereditos_de_leitura())
})

test_that("os criterios usam o time do mesmo mes e tratam projeto sem time como empate", {
  dados <- list(
    consultores = tibble::tibble(
      consultor_id = c("c1", "c2", "c3", "c4"),
      nivel_senioridade = c("N3", "N1", "N2", "N0"),
      tempo_casa = c(2, 6, 4, 9)
    ),
    projetos = tibble::tibble(projeto_id = c("p1", "p2"), setor = c("Varejo", "Banking")),
    custo_carregado = tibble::tibble(
      consultor_id = c("c1", "c2", "c3", "c4"), custo_carregado = c(200, 300, 250, 400)
    ),
    ferias_consultor = sem_ferias(),
    experiencia_setorial = tibble::tibble(
      consultor_id = c("c1", "c2"), segmento = c("Varejo", "Banking"), intensidade = c(1, 1)
    ),
    alocacoes = tibble::tibble(
      consultor_id = c("c1", "c3", "c4"),
      projeto_id = c("p1", "p1", "p1"),
      horas_mes = c(100, 80, 80),
      data_inicio = as.Date(c("2026-03-01", "2026-03-01", "2026-05-01")),
      data_fim = as.Date(c("2026-03-31", "2026-03-31", "2026-05-31"))
    )
  )
  discordancias <- tibble::tibble(
    discordancia_id = 1:2, projeto_id = c("p1", "p2"),
    data_inicio = as.Date(c("2026-03-01", "2026-03-01")),
    consultor_realizado = c("c1", "c1"), consultor_recomendado = c("c2", "c3")
  )

  criterios <- medir_criterios_da_discordancia(discordancias, dados)

  # No mes de marco o resto do time de p1 e so c3 (N2, posicao 2); c4 so
  # entra em maio e nao conta.
  expect_equal(criterios$senioridade_realizado[1], 1)
  expect_equal(criterios$senioridade_recomendado[1], 1)
  # p2 nao tem ninguem alocado: empate no criterio.
  expect_true(criterios$sem_time_no_mes[2])
  expect_equal(criterios$senioridade_recomendado[2], criterios$senioridade_realizado[2])
  # c3 nao declarou setor.
  expect_equal(criterios$setor_recomendado[2], "desconhecido")
  expect_equal(criterios$folga_realizado[1], 168)
})
