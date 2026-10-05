test_that("a resolucao mede os dois lados do compromisso", {
  # Espaco grosso empata muita gente; espaco fino esvazia as classes. Medir
  # so um dos dois leva a escolha errada de granularidade.
  rotulo <- rep(c("a", "b"), each = 50)

  medida <- medir_resolucao_de_categorias(rotulo, minimo_por_classe = 5)

  expect_equal(medida$n_classes, 2)
  expect_equal(medida$mediana_por_classe, 50)
  expect_equal(medida$classes_ralas, 0)
  # Com duas classes iguais, dois sorteados caem juntos em quase metade.
  expect_equal(medida$empate_medio, 49 / 99, tolerance = 1e-6)
})

test_that("classe rala e contada contra o minimo declarado", {
  rotulo <- c(rep("a", 50), "b", "c")

  medida <- medir_resolucao_de_categorias(rotulo, minimo_por_classe = 5)

  expect_equal(medida$n_classes, 3)
  expect_equal(medida$classes_ralas, 2 / 3)
})

test_that("os perfis de consultor tem norma um", {
  dados <- carregar_dados()

  perfis <- construir_matriz_de_perfis(dados$skills_consultor, dados$espaco_skills)

  expect_equal(nrow(perfis), dplyr::n_distinct(dados$skills_consultor$consultor_id))
  expect_equal(ncol(perfis), nrow(dados$espaco_skills))
  expect_equal(unname(sqrt(rowSums(perfis^2))), rep(1, nrow(perfis)), tolerance = 1e-10)
})

test_that("calcular_auc bate com a definicao em casos conhecidos", {
  # Separacao perfeita, inversao perfeita e empate total.
  expect_equal(calcular_auc(c(3, 2, 1), c(TRUE, FALSE, FALSE)), 1)
  expect_equal(calcular_auc(c(1, 2, 3), c(TRUE, FALSE, FALSE)), 0)
  expect_equal(calcular_auc(c(1, 1, 1), c(TRUE, FALSE, FALSE)), 0.5)
  expect_true(is.na(calcular_auc(c(1, 2), c(TRUE, TRUE))))
})

test_that("o perfil por categoria muda com a ponderacao", {
  # Ponderar por alocacao faz o perfil tender a quem e muito alocado, o que
  # transforma conteudo em popularidade disfarcada.
  dados <- carregar_dados()
  perfis <- construir_matriz_de_perfis(dados$skills_consultor, dados$espaco_skills)

  por_pessoa <- estimar_perfil_por_categoria(
    dados$alocacoes, dados$projetos, perfis, por_pessoa = TRUE
  )
  por_alocacao <- estimar_perfil_por_categoria(
    dados$alocacoes, dados$projetos, perfis, por_pessoa = FALSE
  )

  expect_equal(dim(por_pessoa), dim(por_alocacao))
  expect_false(isTRUE(all.equal(por_pessoa, por_alocacao)))
  expect_equal(unname(sqrt(rowSums(por_pessoa^2))), rep(1, nrow(por_pessoa)), tolerance = 1e-10)
})

test_that("pontuar sem vazamento tira o avaliado do perfil da categoria", {
  perfis <- rbind(a = c(1, 0), b = c(0, 1), c = c(1, 0))
  pertence <- list(
    soma = rbind(setor = c(2, 1)),
    membros = list(setor = c("a", "b", "c")),
    n = c(setor = 3)
  )

  escore <- pontuar_sem_vazamento(c("a", "b"), "setor", perfis, pertence)

  # Sem "a", o perfil vira (1,1) normalizado, e o cosseno de a com ele e
  # 1/sqrt(2). Sem "b", vira (2,0) normalizado, e o cosseno de b e zero.
  expect_equal(unname(escore[1]), 1 / sqrt(2), tolerance = 1e-8)
  expect_equal(unname(escore[2]), 0, tolerance = 1e-8)
})

test_that("a separacao do perfil medio e artefato de vazamento e popularidade", {
  # E o achado central do bloco: removidos os dois vicios, o mecanismo cai
  # para o acaso. Se algum dia subir, alguem reintroduziu um deles.
  dados <- carregar_dados()

  cmp <- comparar_mecanismos_de_p(dados)
  pega <- function(nome) cmp$resultados$auc[cmp$resultados$mecanismo == nome]

  expect_gt(pega("perfil_por_alocacao"), pega("perfil_medio"))
  expect_gt(pega("perfil_medio"), pega("perfil_sem_vazamento"))
  expect_equal(pega("perfil_sem_vazamento"), 0.5, tolerance = 0.06)
})

test_that("o one-hot separa e nao ordena", {
  dados <- carregar_dados()

  cmp <- comparar_mecanismos_de_p(dados)
  one_hot <- cmp$resultados[cmp$resultados$mecanismo == "one_hot", ]

  expect_equal(one_hot$valores_distintos, 2)
  expect_gt(one_hot$auc, 0.6)
})

test_that("a ponte real registra afinidade forte com volume fraco", {
  ponte <- referencias_ponte_frente_offering()

  expect_true(all(ponte$lift > 1))
  # O lift e alto e o share e baixo: e essa combinacao que impede tratar a
  # afinidade como pareamento de um para um.
  expect_gt(max(ponte$lift), 5)
  expect_lt(max(ponte$share_horas), 0.5)
})

test_that("executar_ii6 devolve as quatro partes do bloco", {
  dados <- carregar_dados()

  bloco <- executar_ii6(dados)

  expect_named(bloco, c("objetos", "tabelas", "figuras", "achados"))
  expect_true(all(vapply(bloco$tabelas, is.data.frame, logical(1))))
  expect_true(all(vapply(bloco$figuras, ggplot2::is_ggplot, logical(1))))
  expect_equal(nrow(bloco$achados), 7)
  expect_true(all(startsWith(bloco$achados$codigo, "II.6.")))
  expect_equal(
    bloco$achados$etiqueta[bloco$achados$codigo == "II.6.4"],
    "consequência do gerador"
  )
})

test_that("a figura do bloco II.6 carrega o proprio tema", {
  tema_anterior <- ggplot2::theme_get()
  ggplot2::theme_set(ggplot2::theme_grey())
  on.exit(ggplot2::theme_set(tema_anterior))

  bloco <- executar_ii6(carregar_dados())

  for (figura in bloco$figuras) {
    expect_s3_class(figura$theme$panel.border, "element_rect")
    expect_no_error(ggplot2::ggplot_build(figura))
  }
})

test_that("comparar_mecanismos_de_p nao falha com menos candidatos que negativos pedidos", {
  # Sempre rodou na escala completa, onde o limite nao morde. Dentro das
  # replicas de estabilidade, com populacao pequena, o sorteio sem reposicao
  # quebrava por pedir mais negativos do que existiam. Pedir 500 contra os
  # cerca de 140 candidatos da escala completa exercita o mesmo caminho.
  dados <- carregar_dados()

  resultado <- comparar_mecanismos_de_p(dados, amostra_negativos = 500)

  expect_true(all(resultado$resultados$auc >= 0 & resultado$resultados$auc <= 1))
})

test_that("sem replicas o II.6 nao afirma que a folga do one-hot caiu", {
  # Ausencia de medida nao e evidencia contra. Antes de os estados virarem
  # tres, rodar o bloco sem replicas emitia a ressalva de que a vantagem nao
  # se sustenta, afirmando um resultado que ninguem tinha medido.
  achados <- executar_ii6(carregar_dados(), estabilidade = NULL)$achados
  ii66 <- achados[achados$codigo == "II.6.6", ]
  ii62 <- achados[achados$codigo == "II.6.2", ]

  expect_true(is.na(ii66$limitacao))
  expect_no_match(ii62$enunciado, "não se sustentar")
  expect_match(ii66$enunciado, "não tem intervalo medido")
})

test_that("com a folga medida cruzando o zero o II.6 declara a ressalva", {
  estabilidade <- tibble::tibble(
    medida = c("folga_one_hot_popularidade", "auc_perfil_sem_vazamento"),
    mediana = c(0.03, 0.49), inferior = c(-0.01, 0.46), superior = c(0.065, 0.54)
  )

  achados <- executar_ii6(carregar_dados(), estabilidade = estabilidade)$achados

  expect_match(
    achados$limitacao[achados$codigo == "II.6.6"], "**sem** poder alegar",
    fixed = TRUE
  )
  expect_match(achados$enunciado[achados$codigo == "II.6.6"], "não se sustenta")
})
