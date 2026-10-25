matriz_de_teste <- function() {
  alocacoes <- tibble::tibble(
    consultor_id = c("c1", "c1", "c2", "c3", "c3", "c3"),
    projeto_id = c("p1", "p2", "p2", "p1", "p3", "p4"),
    horas_mes = c(100, 20, 160, 0, 50, 80),
    duracao_dias = c(30, 10, 60, 15, 30, 90)
  )
  consultores <- tibble::tibble(consultor_id = c("c1", "c2", "c3", "c4"))
  projetos <- tibble::tibble(projeto_id = c("p1", "p2", "p3", "p4", "p5"))
  list(
    alocacoes = alocacoes, consultores = consultores, projetos = projetos,
    r = construir_matriz_r(alocacoes, consultores, projetos)
  )
}

perda_als <- function(r, peso, estado, lambda) {
  confianca <- 1 + as.matrix(if (is.null(peso)) 0 * r else peso)
  previsto <- estado$x %*% t(estado$y)
  sum(confianca * (as.matrix(r) - previsto)^2) + lambda * (sum(estado$x^2) + sum(estado$y^2))
}

test_that("com alfa zero, o último passo resolve a ridge de forma fechada", {
  m <- matriz_de_teste()
  estado <- ajustar_als_implicito(m$r, fatores = 2, lambda = 0.5, iteracoes = 15, semente = 1)[["15"]]

  # O último passo atualiza os projetos com os consultores fixos.
  esperado <- as.matrix(Matrix::t(m$r) %*% estado$x) %*% solve(crossprod(estado$x) + diag(0.5, 2))
  expect_equal(unname(estado$y), unname(esperado), tolerance = 1e-10)
})

test_that("com confiança, cada linha resolve as equações normais do artigo", {
  m <- matriz_de_teste()
  peso <- construir_peso_de_confianca(m$alocacoes, m$consultores, m$projetos, alfa = 1)
  estados <- ajustar_als_implicito(m$r, peso, fatores = 2, lambda = 0.5, iteracoes = 8, semente = 1)
  estado <- estados[["8"]]

  # Refaz o passo dos projetos de forma densa, sem o atalho Y'C Y = Y'Y + Y'(C - I)Y.
  confianca <- 1 + as.matrix(peso)
  preferencia <- as.matrix(m$r)
  for (i in seq_len(ncol(m$r))) {
    c_i <- diag(confianca[, i])
    esperado <- solve(
      t(estado$x) %*% c_i %*% estado$x + diag(0.5, 2),
      t(estado$x) %*% c_i %*% preferencia[, i]
    )
    expect_equal(unname(estado$y[i, ]), as.numeric(esperado), tolerance = 1e-10)
  }
})

test_that("a alocação de zero hora conserva a preferência 1", {
  # c3 em p1 tem zero hora: peso zero, que a matriz esparsa pode não guardar.
  m <- matriz_de_teste()
  peso <- construir_peso_de_confianca(m$alocacoes, m$consultores, m$projetos, alfa = 1)
  com_peso <- ajustar_als_implicito(m$r, peso * 0, fatores = 2, lambda = 0.5, iteracoes = 5, semente = 1)
  sem_peso <- ajustar_als_implicito(m$r, NULL, fatores = 2, lambda = 0.5, iteracoes = 5, semente = 1)
  expect_equal(com_peso, sem_peso, tolerance = 1e-10)
})

test_that("quem não tem observação recebe fator nulo", {
  m <- matriz_de_teste()
  estado <- ajustar_als_implicito(m$r, fatores = 2, lambda = 0.5, iteracoes = 5, semente = 1)[["5"]]
  expect_equal(unname(estado$x["c4", ]), c(0, 0))
  expect_equal(unname(estado$y["p5", ]), c(0, 0))
})

test_that("a perda não sobe entre iterações e os estados intermediários são guardados", {
  m <- matriz_de_teste()
  peso <- construir_peso_de_confianca(m$alocacoes, m$consultores, m$projetos, alfa = 1)
  estados <- ajustar_als_implicito(m$r, peso, fatores = 2, lambda = 0.5, iteracoes = 10,
                                   semente = 1, registrar_em = 1:10)
  perdas <- vapply(estados, \(e) perda_als(m$r, peso, e, 0.5), numeric(1))

  expect_named(estados, as.character(1:10))
  expect_true(all(diff(perdas) <= 1e-9))
})

test_that("a mesma semente dá o mesmo ajuste", {
  m <- matriz_de_teste()
  a <- ajustar_als_implicito(m$r, fatores = 3, lambda = 1, iteracoes = 4, semente = 7)
  b <- ajustar_als_implicito(m$r, fatores = 3, lambda = 1, iteracoes = 4, semente = 7)
  expect_identical(a, b)
})
