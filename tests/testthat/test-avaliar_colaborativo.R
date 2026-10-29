test_that("o contexto do colaborativo não filtra ninguém", {
  dados <- com_historico_da_janela(carregar_dados(tabelas = c("alocacoes", "projetos", "consultores")))
  contexto <- montar_contexto_do_colaborativo(dados, ler_pre_registro())
  expect_true(all(contexto$filtro))
  expect_equal(dim(contexto$chave), c(nrow(dados$consultores), nrow(dados$projetos)))
})

test_that("só são avaliáveis os pares com consultor e projeto na estimação", {
  treino <- tibble::tibble(consultor_id = c("c1", "c2"), projeto_id = c("p1", "p2"))
  teste <- tibble::tibble(consultor_id = c("c1", "c1", "c3"), projeto_id = c("p2", "p9", "p1"))
  expect_equal(selecionar_pares_avaliaveis(treino, teste)$projeto_id, "p2")
})

test_that("a popularidade conta alocações da estimação e é igual em todo projeto", {
  contexto <- list(consultores = c("c1", "c2", "c3"), projetos = c("p1", "p2"))
  treino <- tibble::tibble(consultor_id = c("c1", "c1", "c3"), projeto_id = c("p1", "p2", "p1"))
  escores <- escorar_popularidade(treino, contexto)
  expect_equal(unname(escores[, "p1"]), c(2, 0, 1))
  expect_equal(escores[, "p1"], escores[, "p2"])
})

test_that("o desempate da escolha do colaborativo é o mais parcimonioso", {
  validacao <- tibble::tibble(
    alfa = 0, fatores = c(5, 2, 2, 2), lambda = c(1, 0.1, 1, 1),
    iteracoes = c(10, 10, 20, 10), acertos = 15, pares = 58
  )
  escolhida <- escolher_configuracao_do_colaborativo(validacao)
  expect_equal(c(escolhida$fatores, escolhida$lambda, escolhida$iteracoes), c(2, 1, 10))
})

test_that("quem não tem fator vai para o fim da lista, em ordem pela chave", {
  # c1 tem escore negativo; c3 não tem fator e teria escore zero.
  contexto <- list(
    consultores = c("c1", "c2", "c3"), projetos = "p1", lista = 1,
    filtro = matrix(TRUE, 3, 1, dimnames = list(c("c1", "c2", "c3"), "p1")),
    chave = matrix(c(0.5, 0.9, 0.1), 3, 1, dimnames = list(c("c1", "c2", "c3"), "p1"))
  )
  escores <- matrix(c(-0.2, -0.5, 0), 3, 1, dimnames = dimnames(contexto$filtro))
  par <- tibble::tibble(consultor_id = "c1", projeto_id = "p1")
  treino <- tibble::tibble(consultor_id = c("c1", "c2"), projeto_id = "p0")

  # Sem a regra, o zero de c3 fica à frente do escore negativo de c1.
  expect_equal(acertar_na_lista(par, contexto, escores), 0L)
  restrito <- restringir_lista_a_quem_tem_fator(contexto, treino)
  expect_equal(unname(restrito$filtro[, "p1"]), c(TRUE, TRUE, FALSE))
  expect_equal(acertar_na_lista(par, restrito, escores), 1L)
})

test_that("a escolha do colaborativo que diverge da emenda para a pipeline", {
  registro <- ler_pre_registro()
  chave <- "hiperparametros_do_colaborativo_na_realizacao_versionada"
  emenda <- purrr::detect(registro$emendas, \(e) !is.null(e[[chave]]))
  selecao <- list(escolhidos = purrr::map(emenda[[chave]], tibble::as_tibble) |> purrr::list_rbind())

  expect_identical(conferir_escolha_do_colaborativo(selecao, registro), selecao)
  selecao$escolhidos$lambda[2] <- 0.1
  expect_error(conferir_escolha_do_colaborativo(selecao, registro), "difere")
})
