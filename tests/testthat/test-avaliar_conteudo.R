contexto_de_teste <- function(lista = 2) {
  consultores <- paste0("c", 1:6)
  projetos <- c("p1", "p2")
  # c1 a c4 declararam o setor A; c5 o setor B; c6 não declarou.
  setor <- c("A", "A", "A", "A", "B", NA)
  filtro <- outer(setor, c("A", "A"), `==`)
  filtro[is.na(filtro)] <- FALSE
  dimnames(filtro) <- list(consultores, projetos)
  chave <- matrix(seq(0.1, 0.6, by = 0.1), 6, 2, dimnames = list(consultores, projetos))
  list(consultores = consultores, projetos = projetos, filtro = filtro, chave = chave, lista = lista)
}

test_that("a lista ordena o filtro pelo escore e desempata pela chave", {
  contexto <- contexto_de_teste(lista = 2)
  pares <- tibble::tibble(consultor_id = c("c4", "c1"), projeto_id = "p1")

  # Sem escore, a chave decide: c1 e c2 ficam na frente, c4 fica fora.
  expect_equal(acertar_na_lista(pares, contexto), c(0L, 1L))

  escores <- matrix(0, 6, 2, dimnames = list(contexto$consultores, contexto$projetos))
  escores["c4", "p1"] <- 1
  # Com escore alto, c4 sobe para o primeiro lugar e c1 continua em segundo.
  expect_equal(acertar_na_lista(pares, contexto, escores), c(1L, 1L))
})

test_that("quem não passa o filtro só entra se o filtro deixar vaga", {
  pares <- tibble::tibble(consultor_id = c("c5", "c6"), projeto_id = "p1")

  # Lista de 2 com quatro filtrados: não sobra vaga.
  expect_equal(acertar_na_lista(pares, contexto_de_teste(lista = 2)), c(0L, 0L))
  # Lista de 5: sobra uma vaga, e a chave dá a vaga a c5, antes de c6.
  expect_equal(acertar_na_lista(pares, contexto_de_teste(lista = 5)), c(1L, 0L))
})

test_that("o escore não mexe em quem está fora do filtro", {
  contexto <- contexto_de_teste(lista = 5)
  pares <- tibble::tibble(consultor_id = c("c5", "c6"), projeto_id = "p1")
  escores <- matrix(0, 6, 2, dimnames = list(contexto$consultores, contexto$projetos))
  escores["c6", "p1"] <- 99
  expect_equal(acertar_na_lista(pares, contexto, escores), acertar_na_lista(pares, contexto))
})

test_that("na média dos sorteios, o piso binário é o piso esperado do bloco II.7", {
  consultores <- tibble::tibble(consultor_id = paste0("c", 1:30))
  experiencia <- tibble::tibble(
    consultor_id = paste0("c", 1:24), segmento = rep(c("A", "B"), each = 12), intensidade = 1
  )
  projetos <- tibble::tibble(projeto_id = c("p1", "p2"), setor = c("A", "B"))
  pares <- tibble::tibble(consultor_id = c("c1", "c13", "c25", "c2"), projeto_id = c("p1", "p1", "p1", "p2"))
  esperado <- medir_piso_filtro_e_acaso(pares, consultores, projetos, experiencia, 5)$recall

  filtro <- outer(
    experiencia$segmento[match(consultores$consultor_id, experiencia$consultor_id)],
    projetos$setor, `==`
  )
  filtro[is.na(filtro)] <- FALSE
  dimnames(filtro) <- list(consultores$consultor_id, projetos$projeto_id)
  sorteios <- vapply(1:4000, function(s) {
    chave <- withr::with_seed(s, matrix(stats::runif(60), 30, 2, dimnames = dimnames(filtro)))
    contexto <- list(consultores = consultores$consultor_id, projetos = projetos$projeto_id,
                     filtro = filtro, chave = chave, lista = 5)
    mean(acertar_na_lista(pares, contexto))
  }, numeric(1))

  expect_equal(mean(sorteios), esperado, tolerance = 0.01)
})

test_that("os atributos são só setor e competências declarados", {
  experiencia <- tibble::tibble(
    consultor_id = c("c1", "c1", "c2"), segmento = c("A", "B", "B"), intensidade = c(0.9, 0.2, 0.5)
  )
  skills <- tibble::tibble(consultor_id = c("c1", "c2", "c2"), skill_id = c("s1", "s1", "s2"),
                           proficiencia = c("Avançado", "Básico", "Médio"))
  espaco <- tibble::tibble(skill_id = c("s1", "s2", "s3"))
  atributos <- construir_atributos_declarados(experiencia, skills, espaco)

  expect_equal(rownames(atributos), c("c1", "c2"))
  expect_equal(colnames(atributos), c("setor:A", "setor:B", "skill:s1", "skill:s2", "skill:s3"))
  # Setor predominante, um só por pessoa; skill sem peso de proficiência.
  expect_equal(unname(atributos["c1", ]), c(1, 0, 1, 0, 0))
  expect_equal(unname(atributos["c2", ]), c(0, 1, 1, 1, 0))
})

test_that("o mapa recupera uma relação linear exata e usa só quem tem histórico", {
  atributos <- matrix(c(1, 0, 0, 1, 1, 1, 0, 0, 1, 0, 1, 0), 4, 3,
                      dimnames = list(paste0("c", 1:4), c("a", "b", "c")))
  coeficientes <- matrix(c(2, -1, 0.5, 1, 0, -2), 3, 2)
  fatores <- atributos %*% coeficientes + 0.3

  mapa <- ajustar_mapeamento(atributos, fatores, com_historico = paste0("c", 1:4), lambda = 1e-8)
  expect_equal(prever_fatores(mapa, atributos), fatores, tolerance = 1e-5)

  # Fator de quem não tem histórico não entra no ajuste.
  distorcido <- fatores
  distorcido["c4", ] <- 1000
  mapa_tres <- ajustar_mapeamento(atributos, distorcido, com_historico = paste0("c", 1:3), lambda = 1e-8)
  expect_lt(max(abs(mapa_tres$media_fatores)), 100)
})

test_that("o desempate da escolha é o mais parcimonioso", {
  validacao <- tibble::tibble(
    fatores = c(10, 5, 5, 5), lambda = c(1, 1, 10, 10), iteracoes = c(10, 10, 20, 10),
    lambda_mapeamento = c(1, 1, 1, 1), acerto = c(0.6, 0.6, 0.6, 0.6)
  )
  escolhida <- escolher_configuracao_do_conteudo(validacao)
  expect_equal(escolhida$fatores, 5)
  expect_equal(escolhida$lambda, 10)
  expect_equal(escolhida$iteracoes, 10)
})
