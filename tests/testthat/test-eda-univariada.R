test_that("calcular_assimetria usa a definicao populacional, a mesma do Spark", {
  # Distribuicao simetrica tem assimetria zero.
  expect_equal(calcular_assimetria(c(-2, -1, 0, 1, 2)), 0)

  # Valor conferido a mao: m3 / m2^(3/2) com divisor n, nao a versao
  # amostral corrigida, que daria outro numero para o mesmo vetor.
  x <- c(1, 2, 3, 10)
  desvio <- x - mean(x)
  esperado <- mean(desvio^3) / mean(desvio^2)^(3 / 2)
  expect_equal(calcular_assimetria(x), esperado)
  expect_gt(calcular_assimetria(x), 0)
})

test_that("calcular_assimetria devolve NA onde nao ha o que calcular", {
  expect_true(is.na(calcular_assimetria(c(1, 2))))
  expect_true(is.na(calcular_assimetria(rep(5, 10))))
  expect_equal(calcular_assimetria(c(1, 2, 3, NA, Inf)), calcular_assimetria(c(1, 2, 3)))
})

test_that("resumir_numericas separa quantis incondicionais dos quantis dos positivos", {
  # O ponto central da funcao: com massa em zero as duas versoes medem
  # coisas diferentes, e confundi-las ja produziu conclusao errada aqui.
  tabela <- tibble::tibble(x = c(rep(0, 6), 10, 20, 30, 40))

  resumo <- resumir_numericas(tabela)

  expect_equal(resumo$fracao_zero, 0.6)
  expect_equal(resumo$mediana, 0)
  expect_equal(resumo$mediana_pos, 25)
  expect_equal(resumo$p25_pos, 17.5)
})

test_that("resumir_numericas ignora ausentes e nao finitos na contagem", {
  tabela <- tibble::tibble(x = c(1, 2, 3, NA, Inf))

  resumo <- resumir_numericas(tabela)

  expect_equal(resumo$n, 3)
  expect_equal(resumo$n_ausente, 2)
  expect_equal(resumo$maximo, 3)
})

test_that("resumir_numericas pega so as colunas numericas por padrao", {
  tabela <- tibble::tibble(id = c("a", "b"), valor = c(1, 2), quando = Sys.Date() + 0:1)

  expect_equal(resumir_numericas(tabela)$variavel, "valor")
  expect_equal(resumir_numericas(tabela, "valor")$variavel, "valor")
})

test_that("resumir_categoricas mede equilibrio entre 0 e 1", {
  equilibrada <- tibble::tibble(x = c("a", "b", "c", "d"))
  concentrada <- tibble::tibble(x = c(rep("a", 97), "b", "c", "d"))
  constante <- tibble::tibble(x = rep("a", 10))

  expect_equal(resumir_categoricas(equilibrada)$equilibrio, 1)
  expect_lt(resumir_categoricas(concentrada)$equilibrio, 0.3)
  expect_equal(resumir_categoricas(constante)$equilibrio, 0)
  expect_equal(resumir_categoricas(constante)$n_categorias, 1)
})

test_that("resumir_categoricas identifica moda e fracao da moda", {
  tabela <- tibble::tibble(x = c("a", "a", "a", "b"))

  resumo <- resumir_categoricas(tabela)

  expect_equal(resumo$moda, "a")
  expect_equal(resumo$fracao_moda, 0.75)
})

test_that("referencias_reais tem vocabulario consistente e sem duplicata", {
  referencias <- referencias_reais()

  expect_false(any(duplicated(
    referencias[c("tabela", "variavel", "estatistica", "categoria")]
  )))
  expect_true(all(referencias$estatistica %in% c(
    "media", "desvio", "p25", "mediana", "p75", "maximo", "assimetria",
    "fracao_zero", "p25_pos", "mediana_pos", "p75_pos", "assimetria_pos", "prop"
  )))
  # Proporcao sempre nomeia a categoria; estatistica numerica nunca nomeia.
  proporcoes <- dplyr::filter(referencias, estatistica == "prop")
  numericas <- dplyr::filter(referencias, estatistica != "prop")
  expect_false(any(is.na(proporcoes$categoria)))
  expect_true(all(is.na(numericas$categoria)))
})

test_that("toda referencia aponta para tabela e coluna que existem no contrato", {
  referencias <- referencias_reais()
  contrato <- especificacao_tabelas()
  declaradas <- paste(contrato$tabela, contrato$coluna, sep = ".")

  expect_true(all(paste(referencias$tabela, referencias$variavel, sep = ".") %in% declaradas))
})

test_that("as proporcoes reais de cada variavel somam no maximo 1", {
  soma <- referencias_reais() |>
    dplyr::filter(estatistica == "prop") |>
    dplyr::summarise(total = sum(valor_real), .by = c(tabela, variavel))

  expect_true(all(soma$total <= 1.000001))
})

test_that("confrontar_com_real so devolve linha onde existe ancora registrada", {
  resumo <- resumir_numericas(tibble::tibble(
    duracao_dias = c(1, 2, 3), sem_ancora = c(1, 2, 3)
  ))

  confronto <- confrontar_com_real(resumo, "projetos")

  expect_true(all(confronto$variavel == "duracao_dias"))
  expect_false("sem_ancora" %in% confronto$variavel)
})

test_that("confrontar_com_real calcula a diferenca relativa na direcao certa", {
  referencias <- tibble::tribble(
    ~tabela, ~variavel, ~estatistica, ~categoria, ~valor_real, ~rodada,
    "t", "x", "mediana", NA_character_, 10, "Teste"
  )
  resumo <- resumir_numericas(tibble::tibble(x = c(10, 15, 20)))

  confronto <- confrontar_com_real(resumo, "t", referencias)

  expect_equal(confronto$valor_sintetico, 15)
  expect_equal(confronto$diferenca_relativa, 0.5)
})

test_that("confrontar_proporcoes acusa categoria real que o gerador nunca produziu", {
  referencias <- tibble::tribble(
    ~tabela, ~variavel, ~estatistica, ~categoria, ~valor_real, ~rodada,
    "t", "g", "prop", "presente", 0.5, "Teste",
    "t", "g", "prop", "ausente", 0.5, "Teste"
  )
  tabela <- tibble::tibble(g = rep("presente", 10))

  comparacao <- confrontar_proporcoes(tabela, "t", "g", referencias)

  faltante <- comparacao[comparacao$categoria == "ausente", ]
  expect_equal(faltante$proporcao_sintetica, 0)
  expect_equal(faltante$diferenca, -0.5)
})

test_that("as figuras do bloco I.2 constroem sem erro", {
  tabela <- tibble::tibble(a = c(1, 10, 100, 1000), b = c(0, 0, 5, 50))
  comparacao <- tibble::tibble(
    categoria = c("x", "y"), proporcao_real = c(0.6, 0.4),
    proporcao_sintetica = c(0.55, 0.45), diferenca = c(-0.05, 0.05),
    rodada = "Teste"
  )

  expect_no_error(ggplot2::ggplot_build(
    figura_distribuicoes(tabela, c("a", "b"), "Titulo", escala_log = TRUE)
  ))
  expect_no_error(ggplot2::ggplot_build(
    figura_proporcoes_comparadas(comparacao, "Titulo")
  ))
})

test_that("figura_proporcoes_comparadas respeita a ordem pedida em variavel ordinal", {
  # Ordenar senioridade por frequencia destruiria a ordem da carreira, que e
  # a unica informacao que a variavel carrega.
  comparacao <- tibble::tibble(
    categoria = c("N4", "N3", "N2", "N1", "N0"),
    proporcao_real = c(0.15, 0.38, 0.25, 0.19, 0.03),
    proporcao_sintetica = c(0.14, 0.36, 0.27, 0.20, 0.02),
    diferenca = 0, rodada = "Teste"
  )
  ordem <- c("N0", "N1", "N2", "N3", "N4")

  figura <- figura_proporcoes_comparadas(comparacao, "Titulo", ordem = ordem)

  expect_equal(levels(figura$data$categoria), ordem)
})
