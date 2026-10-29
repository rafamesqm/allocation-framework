# O pré-registro antecede qualquer modelo. Estes testes falham se o arquivo
# mudar de valor ou se a pipeline usar valor diferente do registrado. Mudar o
# arquivo depois do primeiro ajuste de modelo invalida a avaliação, e o teste
# existe para que isso não passe em silêncio.

registro <- ler_pre_registro()

test_that("os valores registrados são os combinados antes de qualquer modelo", {
  expect_equal(registro$versao, 1)
  expect_equal(registro$dados$fonte, "synthetic")
  expect_equal(registro$sementes$divisao, 1)
  expect_equal(registro$sementes$desempate, 20261024)
  expect_equal(registro$sementes$fatoracao, 20261024)
  expect_equal(registro$sementes$leitura_qualitativa, 20261022)
  expect_equal(registro$sementes$realizacoes, c(1, 100))

  expect_equal(registro$divisoes$temporal$fracao_teste, 0.3)
  expect_equal(registro$divisoes$temporal$fracao_validacao, 0.3)
  expect_equal(registro$divisoes$por_consultor$grupos, 5)
  expect_equal(registro$metrica$lista_decisoria, 20)
  expect_equal(registro$metrica$listas_descritivas, c(5, 10, 50))

  expect_equal(registro$colaborativo$alfas, c(0, 1))
  expect_equal(registro$colaborativo$grade$fatores, c(2, 5, 10, 20))
  expect_equal(registro$colaborativo$grade$lambda, c(0.1, 1, 10, 100))
  expect_equal(registro$colaborativo$grade$iteracoes, c(10, 20))
  expect_equal(registro$conteudo$ordenacao$grade$lambda_mapeamento, c(0.1, 1, 10, 100))

  expect_equal(registro$teste$nivel, 0.05)
  expect_equal(registro$teste$ganho_minimo$poder, 0.8)
  expect_equal(registro$teste$ganho_minimo$cenarios, c("dominancia", "independencia"))
  expect_equal(registro$segundo_nivel$casos_por_estrato, 2)
  expect_equal(registro$segundo_nivel$capacidade_mensal_horas, 168)
  expect_equal(registro$estabilidade$nivel, 0.9)
})

test_that("a sequência do colaborativo e a exclusão do conteúdo estão registradas", {
  sequencia <- registro$regras_de_adocao$colaborativo$sequencia
  expect_equal(vapply(sequencia, \(h) h$codigo, character(1)), c("H1", "H2"))
  excluidas <- vapply(registro$conteudo$excluidas, \(e) e$alternativa, character(1))
  expect_equal(excluidas, "similaridade de competências")
  expect_match(registro$conteudo$ordenacao$modelo, "Gantner")
  recencia <- registro$conteudo$ordenacao$atributos_excluidos[[1]]
  expect_equal(recencia$atributo, "recência das competências")
  expect_match(recencia$motivo, "I.4.3")
  expect_match(registro$teste$ganho_minimo$calculo, "exato")
  expect_named(
    registro$hibrido$medida_descritiva$piso_composto,
    c("historico_e_formulario", "so_historico", "so_formulario")
  )
  expect_named(
    registro$hibrido$medida_descritiva$por_quadrante,
    c("historico_e_formulario", "so_historico", "so_formulario")
  )
  expect_match(registro$hibrido$medida_descritiva$limitacao, "regra de exposi")
})

test_that("a janela usada pela pipeline é a registrada", {
  expect_equal(janela_de_analise(), janela_do_pre_registro(registro))
})

test_that("as funções do protocolo herdadas da EDA usam os valores registrados", {
  divisao <- formals(dividir_interacoes)
  expect_equal(divisao$fracao_teste, registro$divisoes$temporal$fracao_teste)
  expect_equal(divisao$semente, registro$sementes$divisao)
  expect_equal(divisao$n_grupos, registro$divisoes$por_consultor$grupos)
  expect_equal(
    formals(dividir_por_consultor_em_grupos)$n_grupos, registro$divisoes$por_consultor$grupos
  )

  ganho <- formals(calcular_ganho_minimo_detectavel)
  expect_equal(ganho$alfa, registro$teste$nivel)
  expect_equal(ganho$poder, registro$teste$ganho_minimo$poder)

  sorteio <- formals(sortear_casos_para_leitura)
  expect_equal(sorteio$semente, registro$sementes$leitura_qualitativa)
  expect_equal(sorteio$por_estrato, registro$segundo_nivel$casos_por_estrato)

  expect_equal(
    formals(medir_folga_de_capacidade)$capacidade_mensal_horas,
    registro$segundo_nivel$capacidade_mensal_horas
  )
  expect_equal(listar_vereditos_de_leitura(), registro$segundo_nivel$vereditos)
})

test_that("o protocolo reproduz os valores de verificação na realização versionada", {
  dados <- com_historico_da_janela(carregar_dados(), janela_do_pre_registro(registro))
  verificacao <- registro$verificacao

  temporal <- dividir_interacoes(
    dados$alocacoes, "temporal",
    fracao_teste = registro$divisoes$temporal$fracao_teste,
    semente = registro$sementes$divisao
  )
  expect_equal(
    format(min(temporal$teste$data_inicio)),
    registro$divisoes$temporal$data_corte_verificacao
  )

  poder <- planejar_poder_por_componente(
    dados$alocacoes, dados$consultores, dados$projetos, dados$experiencia_setorial,
    fracao_teste = registro$divisoes$temporal$fracao_teste,
    n_recomendados = registro$metrica$lista_decisoria,
    alfa = registro$teste$nivel, poder = registro$teste$ganho_minimo$poder,
    n_grupos = registro$divisoes$por_consultor$grupos, semente = registro$sementes$divisao
  )
  linha <- \(componente, piso) poder[poder$componente == componente & poder$piso == piso, ]
  colaborativo <- linha("Colaborativo", "Popularidade no treino")
  conteudo <- linha("Conteúdo", "Filtro de setor e acaso")
  acaso <- linha("Conteúdo", "Acaso")

  expect_equal(colaborativo$n_avaliavel, verificacao$colaborativo$pares_avaliaveis)
  expect_equal(colaborativo$valor_piso, verificacao$colaborativo$piso, tolerance = 1e-3)
  expect_equal(colaborativo$ganho_dominancia, verificacao$colaborativo$ganho_minimo_dominancia, tolerance = 1e-3)
  expect_equal(colaborativo$ganho_independencia, verificacao$colaborativo$ganho_minimo_independencia, tolerance = 1e-3)
  expect_equal(conteudo$n_avaliavel, verificacao$conteudo$pares_avaliaveis)
  expect_equal(conteudo$valor_piso, verificacao$conteudo$piso_filtro_acaso, tolerance = 1e-3)
  expect_equal(conteudo$fracao_passa_filtro, verificacao$conteudo$fracao_passa_filtro, tolerance = 1e-3)
  expect_equal(conteudo$ganho_dominancia, verificacao$conteudo$ganho_minimo_dominancia, tolerance = 1e-3)
  expect_equal(conteudo$ganho_independencia, verificacao$conteudo$ganho_minimo_independencia, tolerance = 1e-3)
  expect_equal(acaso$valor_piso, verificacao$conteudo$acaso_puro, tolerance = 1e-3)

  base <- medir_linha_base_sob_protocolo(
    temporal$treino, temporal$teste, dados$consultores, registro$metrica$lista_decisoria
  )
  expect_equal(base$recall, verificacao$popularidade_todos_os_pares_temporais$piso, tolerance = 1e-3)
})

test_that("os intervalos registrados são os das realizações no store", {
  skip_if_not(dir.exists(here::here("_targets")), "store do targets ausente")
  estabilidade <- targets::tar_read(estabilidade, store = here::here("_targets"))
  intervalo <- function(medida) {
    linha <- estabilidade[estabilidade$medida == medida, ]
    c(linha$inferior, linha$superior)
  }
  verificacao <- registro$verificacao

  expect_equal(intervalo("piso_colaborativo"), verificacao$colaborativo$piso_intervalo, tolerance = 1e-3)
  expect_equal(intervalo("piso_conteudo_filtro"), verificacao$conteudo$piso_intervalo, tolerance = 1e-3)
  expect_equal(
    intervalo("recall_protocolo_20"),
    verificacao$popularidade_todos_os_pares_temporais$piso_intervalo,
    tolerance = 1e-3
  )
})

test_that("o alvo da pipeline lê o mesmo pré-registro do arquivo", {
  skip_if_not(dir.exists(here::here("_targets")), "store do targets ausente")
  expect_identical(targets::tar_read(pre_registro, store = here::here("_targets")), registro)
})
