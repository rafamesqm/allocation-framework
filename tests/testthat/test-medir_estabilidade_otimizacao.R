# O resumo da estabilidade da wave 4 e o certificado em texto, com números
# de brinquedo.

estabilidade_de_brinquedo <- function(semente, deslocamento) {
  custo_otimo <- semente != 3
  tibble::tibble(
    semente = semente,
    solucao = c("realizada", "afinidade", "custo"),
    otima = c(NA, TRUE, custo_otimo),
    segundos = c(NA, 10, 20),
    afinidade = c(100, 200, 150) + deslocamento,
    custo_relativo = c(0, 0.01, -0.1),
    sobrecarga = c(50, 80, 70),
    ocupacao_maxima = c(5, 20, 18)
  )
}

test_that("o resumo da estabilidade dá mediana, intervalo e posição da versionada", {
  realizacoes <- purrr::map(1:5, \(s) estabilidade_de_brinquedo(s, s)) |> purrr::list_rbind()
  versionada <- estabilidade_de_brinquedo(0, 10) |> dplyr::select(-semente)
  resumo <- resumir_estabilidade_da_otimizacao(versionada, realizacoes, nivel = 0.9)
  linha <- \(s, g) resumo[resumo$solucao == s & resumo$grandeza == g, ]

  expect_equal(linha("realizada", "afinidade")$mediana, 103)
  expect_false(linha("realizada", "afinidade")$dentro)
  # O ganho desconta a afinidade da realizada da mesma semente.
  expect_equal(linha("afinidade", "ganho_de_afinidade")$mediana, 100)
  expect_true(linha("afinidade", "ganho_de_afinidade")$dentro)
  # Para a linha ótima, a mediana é a fração das realizações.
  expect_equal(linha("custo", "otima")$mediana, 0.8)
  expect_equal(linha("custo", "otima")$realizacoes, 5)
  expect_false(any(resumo$solucao == "realizada" & resumo$grandeza == "otima"))
})

test_that("o certificado em texto distingue ótimo, inviável, lacuna e falta de solução", {
  expect_equal(
    descrever_certificado(
      c(NA, "Optimal", "Infeasible", "Time limit reached", "Time limit reached"),
      c(NA, 0, NA, Inf, 0.0012)
    ),
    c("--", "ótima", "inviável", "sem solução no tempo-limite", "lacuna 0,0012")
  )
})
