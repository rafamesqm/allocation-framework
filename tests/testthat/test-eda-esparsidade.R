test_that("a densidade usa o grid completo, nao so quem tem interacao", {
  consultores <- tibble::tibble(consultor_id = paste0("c", 1:10))
  projetos <- tibble::tibble(projeto_id = paste0("p", 1:20))
  alocacoes <- tibble::tibble(
    consultor_id = c("c1", "c1", "c2"), projeto_id = c("p1", "p2", "p1")
  )

  esparsidade <- medir_esparsidade(alocacoes, consultores, projetos)

  expect_equal(esparsidade$celulas, 200)
  expect_equal(esparsidade$observacoes, 3)
  expect_equal(esparsidade$densidade, 3 / 200)
})

test_that("o indice de dispersao vale 1 sob Poisson e cresce com sobredispersao", {
  set.seed(1)
  poisson <- stats::rpois(5000, lambda = 4)
  sobredisperso <- stats::rnbinom(5000, mu = 4, size = 0.8)

  expect_equal(ajustar_modelo_de_grau(poisson)$indice_dispersao, 1, tolerance = 0.1)
  expect_gt(ajustar_modelo_de_grau(sobredisperso)$indice_dispersao, 3)
})

test_that("o AIC prefere a familia certa em cada caso", {
  set.seed(2)
  poisson <- stats::rpois(3000, lambda = 5)
  sobredisperso <- stats::rnbinom(3000, mu = 5, size = 0.7)

  # Sob Poisson, a Binomial Negativa gasta um parametro a mais sem ganho.
  expect_lt(ajustar_modelo_de_grau(poisson)$ganho_aic, 5)
  expect_gt(ajustar_modelo_de_grau(sobredisperso)$ganho_aic, 100)
})

test_that("o Gini vale zero na igualdade perfeita e cresce com a concentracao", {
  igual <- rep(5, 100)
  concentrado <- c(rep(1, 99), 1000)

  expect_equal(calcular_lorenz(igual)$gini, 0)
  # Limiar em 0,85 e nao 0,9: o valor exato deste vetor e 0,8999, e testar
  # colado no valor observado troca um teste de substancia por um de digito.
  expect_gt(calcular_lorenz(concentrado)$gini, 0.85)
})

test_that("a curva de Lorenz comeca na origem e termina em um", {
  curva <- calcular_lorenz(c(1, 2, 3, 10))$curva

  expect_equal(curva$fracao_nos[1], 0)
  expect_equal(curva$fracao_interacoes[1], 0)
  expect_equal(dplyr::last(curva$fracao_nos), 1)
  expect_equal(dplyr::last(curva$fracao_interacoes), 1)
})

test_that("a linha de base conta tentativas e acertos corretamente", {
  # Regressao: dentro de tibble(), `tentativas = length(acertos)` lia a coluna
  # `acertos` ja reduzida a um escalar e devolvia 1, com recall absurdo.
  consultores <- tibble::tibble(consultor_id = paste0("c", 1:5))
  alocacoes <- tibble::tibble(
    consultor_id = c("c1", "c1", "c1", "c2", "c2", "c3"),
    projeto_id = paste0("p", 1:6)
  )

  linha_base <- medir_linha_base_popularidade(alocacoes, consultores, tamanhos = c(1, 5))

  expect_equal(linha_base$tentativas, c(6L, 6L))
  expect_true(all(linha_base$recall >= 0 & linha_base$recall <= 1))
  # Lista do tamanho do quadro inteiro acerta tudo.
  expect_equal(linha_base$recall[linha_base$n_recomendados == 5], 1)
})

test_that("o recall da linha de base cresce com o tamanho da lista", {
  dados <- carregar_dados(tabelas = c("consultores", "alocacoes"))

  linha_base <- medir_linha_base_popularidade(dados$alocacoes, dados$consultores)

  expect_true(all(diff(linha_base$recall) > 0))
  expect_true(all(linha_base$tentativas == nrow(dados$alocacoes)))
})

test_that("a popularidade e recalculada sem a alocacao prevista", {
  # Consultor com uma unica alocacao nao pode se beneficiar dela para entrar
  # na lista: sem o deixa-um-de-fora, o acerto sai otimista por construcao.
  consultores <- tibble::tibble(consultor_id = c("a", "b"))
  alocacoes <- tibble::tibble(consultor_id = c("a", "b"), projeto_id = c("p1", "p2"))

  linha_base <- medir_linha_base_popularidade(alocacoes, consultores, tamanhos = 1)

  # Cada um fica com contagem zero ao tirar a propria alocacao, e o outro
  # segue com um, entao nenhum dos dois entra numa lista de tamanho 1.
  expect_equal(linha_base$acertos, 0)
})

test_that("executar_ii1 devolve as quatro partes do bloco", {
  dados <- carregar_dados(tabelas = c("consultores", "projetos", "alocacoes"))

  bloco <- executar_ii1(dados)

  expect_named(bloco, c("objetos", "tabelas", "figuras", "achados"))
  expect_true(all(vapply(bloco$figuras, ggplot2::is_ggplot, logical(1))))
  expect_equal(nrow(bloco$achados), 4)
  expect_true(all(startsWith(bloco$achados$codigo, "II.1.")))
})

test_that("as figuras do bloco II.1 constroem sem erro e carregam o tema", {
  curvas <- tibble::tibble(
    fracao_nos = c(0, 0.5, 1), fracao_interacoes = c(0, 0.2, 1),
    lado = "Consultores"
  )
  linha_base <- tibble::tibble(
    n_recomendados = c(5, 10), acertos = c(10L, 20L),
    tentativas = c(100L, 100L), recall = c(0.1, 0.2)
  )

  figura_a <- figura_lorenz(curvas, "Titulo")
  figura_b <- figura_linha_base_popularidade(linha_base, 285, "Titulo")

  expect_s3_class(figura_a$theme$panel.border, "element_rect")
  expect_s3_class(figura_b$theme$panel.border, "element_rect")
  expect_no_error(ggplot2::ggplot_build(figura_a))
  expect_no_error(ggplot2::ggplot_build(figura_b))
})
