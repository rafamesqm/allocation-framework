test_that("sem duplicata, a tabela volta inalterada", {
  arestas <- tibble::tibble(
    consultor_id = c("c1", "c2", "c3"),
    projeto_id = c("p1", "p2", "p3")
  )

  resultado <- remover_duplicatas_por_double_edge_swap(arestas)

  expect_equal(nrow(resultado), 3)
  expect_false(any(duplicated(paste(resultado$consultor_id, resultado$projeto_id))))
})

test_that("resolve uma duplicata simples preservando o grau de cada nó", {
  set.seed(1)
  # c1-p1 aparece duas vezes; c2-p2 sobra para trocar ponta com c1-p1
  arestas <- tibble::tibble(
    consultor_id = c("c1", "c1", "c2"),
    projeto_id = c("p1", "p1", "p2")
  )

  resultado <- remover_duplicatas_por_double_edge_swap(arestas)

  expect_false(any(duplicated(paste(resultado$consultor_id, resultado$projeto_id))))
  expect_equal(sort(table(resultado$consultor_id)), sort(table(arestas$consultor_id)))
  expect_equal(sort(table(resultado$projeto_id)), sort(table(arestas$projeto_id)))
})

test_that("grau igual à contagem do outro lado força o bipartido completo, sem duplicata", {
  set.seed(1)
  ids_c <- paste0("c", 1:5)
  ids_p <- paste0("p", 1:5)
  arestas <- tibble::tibble(
    consultor_id = rep(ids_c, each = 5),
    projeto_id = rep(ids_p, times = 5)
  )
  # embaralha para simular a saída bruta do stub-matching antes do reparo
  arestas <- arestas[sample.int(nrow(arestas)), ]
  # descarta metade e duplica o resto para introduzir repetição artificial
  arestas_com_duplicata <- dplyr::bind_rows(arestas[1:15, ], arestas[1:10, ])

  resultado <- remover_duplicatas_por_double_edge_swap(arestas_com_duplicata)

  expect_false(any(duplicated(paste(resultado$consultor_id, resultado$projeto_id))))
})
