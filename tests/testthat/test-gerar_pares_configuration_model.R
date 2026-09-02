test_that("grau igual à contagem do outro lado forma o bipartido completo, sem duplicata", {
  set.seed(1)
  ids_c <- paste0("c", 1:5)
  ids_p <- paste0("p", 1:5)

  arestas <- gerar_pares_configuration_model(
    id_consultor = ids_c, grau_consultor = rep(5, 5),
    id_projeto = ids_p, grau_projeto = rep(5, 5),
    grupo_consultor = rep("A", 5), grupo_projeto = rep("A", 5)
  )

  expect_equal(nrow(arestas), 25)
  expect_false(any(duplicated(paste(arestas$consultor_id, arestas$projeto_id))))
  expect_setequal(unique(arestas$consultor_id), ids_c)
  expect_setequal(unique(arestas$projeto_id), ids_p)
})

test_that("prob_vies alto aumenta a taxa de pareamento dentro do mesmo grupo", {
  set.seed(2)
  n <- 40
  ids_c <- paste0("c", seq_len(n))
  ids_p <- paste0("p", seq_len(n))
  grau_c <- rep(3, n)
  grau_p <- rep(3, n)
  grupo_c <- rep(c("A", "B"), each = n / 2)
  grupo_p <- rep(c("A", "B"), each = n / 2)

  arestas_vies <- gerar_pares_configuration_model(ids_c, grau_c, ids_p, grau_p, grupo_c, grupo_p, prob_vies = 1)
  arestas_aleatorio <- gerar_pares_configuration_model(ids_c, grau_c, ids_p, grau_p, grupo_c, grupo_p, prob_vies = 0)

  grupo_do_consultor <- stats::setNames(grupo_c, ids_c)
  grupo_do_projeto <- stats::setNames(grupo_p, ids_p)

  taxa_mesmo_grupo <- function(arestas) {
    mean(grupo_do_consultor[arestas$consultor_id] == grupo_do_projeto[arestas$projeto_id])
  }

  expect_true(taxa_mesmo_grupo(arestas_vies) > taxa_mesmo_grupo(arestas_aleatorio))
})

test_that("grau zero (cold start) nunca aparece nas arestas", {
  set.seed(3)
  n <- 30
  ids_c <- paste0("c", seq_len(n))
  ids_p <- paste0("p", seq_len(n))
  grau_c <- c(rep(0, 10), rep(4, 20))
  grau_p <- c(rep(0, 10), rep(4, 20))

  arestas <- gerar_pares_configuration_model(
    ids_c, grau_c, ids_p, grau_p,
    grupo_consultor = rep("A", n), grupo_projeto = rep("A", n)
  )

  expect_false(any(ids_c[grau_c == 0] %in% arestas$consultor_id))
  expect_false(any(ids_p[grau_p == 0] %in% arestas$projeto_id))
})

test_that("soma de graus diferente entre os lados ainda produz cablagem completa", {
  set.seed(4)
  ids_c <- paste0("c", 1:6)
  ids_p <- paste0("p", 1:4)

  arestas <- gerar_pares_configuration_model(
    id_consultor = ids_c, grau_consultor = rep(2, 6),
    id_projeto = ids_p, grau_projeto = rep(2, 4),
    grupo_consultor = rep("A", 6), grupo_projeto = rep("A", 4)
  )

  expect_false(any(duplicated(paste(arestas$consultor_id, arestas$projeto_id))))
  expect_true(nrow(arestas) > 0)
})
