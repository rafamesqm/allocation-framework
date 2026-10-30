# O pré-registro da otimização antecede qualquer solução. Estes testes falham
# se o arquivo mudar de valor ou se a pipeline usar valor diferente do
# registrado, como os de test-pre_registro.R para a wave 3.

registro_otimizacao <- ler_pre_registro_otimizacao()

test_that("os valores registrados são os combinados antes de qualquer solução", {
  expect_equal(registro_otimizacao$versao, 1)
  expect_equal(registro_otimizacao$data_registro, "2026-10-29")
  expect_equal(registro_otimizacao$base, "config/pre_registro.yml")

  expect_equal(registro_otimizacao$carga$medida, "percentual")
  expect_equal(registro_otimizacao$carga$sensibilidade, "horas")
  expect_equal(registro_otimizacao$carga$horas_do_mes_cheio, 168)

  metodo <- registro_otimizacao$metodo
  expect_match(metodo$nome, "Mavrotas, 2009")
  expect_match(metodo$solver, "HiGHS")
  expect_equal(metodo$objetivo_principal, "afinidade")
  expect_equal(metodo$objetivos_restritos, c("custo", "sobrecarga"))
  expect_equal(metodo$intervalos_por_objetivo_restrito, 10)
  expect_equal(metodo$problemas_da_grade, (metodo$intervalos_por_objetivo_restrito + 1)^2)
  expect_equal(metodo$delta, 1e-3)
  expect_equal(metodo$tempo_limite_segundos, 600)
  expect_equal(metodo$mip_rel_gap, 0)
  expect_equal(metodo$regra_de_troca$fracao_minima_da_grade, 0.95)
  expect_equal(metodo$ordem_lexicografica, list(
    afinidade = c("afinidade", "custo", "sobrecarga"),
    custo = c("custo", "afinidade", "sobrecarga"),
    sobrecarga = c("sobrecarga", "afinidade", "custo")
  ))
  expect_equal(metodo$tolerancia_de_fixacao, 1e-6)
  expect_equal(metodo$threads, 1)
  expect_match(metodo$regra_de_troca$alternativa, "Deb et al., 2002")

  objetivos <- registro_otimizacao$objetivos
  expect_equal(
    vapply(objetivos[c("afinidade", "custo", "sobrecarga")], \(o) o$sentido, character(1)),
    c(afinidade = "maximizar", custo = "minimizar", sobrecarga = "minimizar")
  )
  expect_false(objetivos$margem$entra_como_objetivo)

  avaliacao <- registro_otimizacao$avaliacao
  expect_equal(avaliacao$estabilidade$realizacoes, 100)
  expect_equal(avaliacao$estabilidade$nivel, 0.9)
  expect_length(avaliacao$posicao_da_realizada$ganho_maximo, 3)
  expect_equal(registro_otimizacao$achados$arquivo, "outputs/tables/achados_w4.csv")
  expect_length(registro_otimizacao$emendas, 3)
})

test_that("a emenda 1 fixa o NSGA-II antes de rodá-lo", {
  emenda <- registro_otimizacao$emendas[[1]]
  expect_equal(emenda$data, "2026-10-30")
  expect_true(emenda$anterior_a_execucao)
  expect_match(emenda$metodo, "Deb et al., 2002")
  expect_equal(emenda$operadores$probabilidade_de_cruzamento, 0.9)
  expect_equal(emenda$operadores$probabilidade_de_mutacao_por_gene, "1/692")
  expect_equal(emenda$operadores$threads, 1)
  expect_equal(emenda$populacao$tamanho, 200)
  expect_false(emenda$populacao$extremos_exatos_na_populacao)
  expect_equal(emenda$orcamento$geracoes, 2000)
  expect_equal(emenda$sementes$principal, 20261029)
  expect_equal(emenda$sementes$variabilidade, 20261030:20261038)
  expect_equal(
    emenda$objetivos$lista,
    c("afinidade", "custo", "sobrecarga atribuível aos assentos")
  )
  # Os 692 genes são os 692 assentos registrados.
  expect_equal(
    as.integer(sub("1/", "", emenda$operadores$probabilidade_de_mutacao_por_gene)),
    registro_otimizacao$verificacao$assentos
  )
})

test_that("a otimização herda da wave 3 o que não registra de novo", {
  registro <- ler_pre_registro()
  expect_equal(registro_otimizacao$carga$horas_do_mes_cheio, registro$segundo_nivel$capacidade_mensal_horas)
  expect_equal(registro_otimizacao$avaliacao$estabilidade$nivel, registro$estabilidade$nivel)
  expect_equal(
    registro_otimizacao$avaliacao$estabilidade$realizacoes,
    diff(registro$sementes$realizacoes) + 1
  )

  # A data de corte dos assentos é a da wave 3.
  montagem <- montagem_versionada_otimizacao()
  expect_equal(format(montagem$insumos$corte), registro$divisoes$temporal$data_corte_verificacao)
})

test_that("a pipeline monta o problema com a medida e as horas registradas", {
  montagem <- montagem_versionada_otimizacao()
  expect_equal(montagem$problema$medida, registro_otimizacao$carga$medida)
  expect_equal(
    montagem$insumos$capacidade$capacidade_horas,
    registro_otimizacao$carga$horas_do_mes_cheio * montagem$insumos$capacidade$capacidade_pct
  )
})

test_that("a montagem reproduz os valores de verificação na realização versionada", {
  montagem <- montagem_versionada_otimizacao()
  verificacao <- medir_verificacao_da_otimizacao(
    montagem$insumos, montagem$problema, montagem$dados, montagem$registro
  )
  conferencia <- conferir_montagem_da_otimizacao(verificacao, registro_otimizacao)
  expect_true(all(conferencia$confere))
  expect_setequal(conferencia$grandeza, names(unlist(registro_otimizacao$verificacao)))

  # A ocupação da realizada é a da EDA em todo consultor e mês de teste.
  expect_lt(verificacao$maior_diferenca_de_carga_contra_a_eda, 1e-12)
})

test_that("a conferência para a pipeline quando um valor não reproduz", {
  montagem <- montagem_versionada_otimizacao()
  verificacao <- medir_verificacao_da_otimizacao(
    montagem$insumos, montagem$problema, montagem$dados, montagem$registro
  )
  alterado <- registro_otimizacao
  alterado$verificacao$assentos <- alterado$verificacao$assentos + 1
  expect_error(conferir_montagem_da_otimizacao(verificacao, alterado), "Assentos|assentos")
})

test_that("o alvo da pipeline lê o mesmo pré-registro do arquivo", {
  skip_if_not(dir.exists(here::here("_targets")), "store do targets ausente")
  skip_if_not(
    "pre_registro_otimizacao" %in% targets::tar_meta(store = here::here("_targets"))$name,
    "alvo ainda não construído"
  )
  expect_identical(
    targets::tar_read(pre_registro_otimizacao, store = here::here("_targets")),
    registro_otimizacao
  )
})

test_that("a emenda 2 corrige a referência do hipervolume e declara a corrida exploratória", {
  emenda <- localizar_emenda(registro_otimizacao, "emenda 2, referência do hipervolume e corrida exploratória")
  expect_true(emenda$anterior_a_execucao)
  expect_true(emenda$posterior_ao_resultado_da_semente_principal)
  expect_match(emenda$hipervolume$referencia, "alocação realizada")
  expect_match(emenda$hipervolume$normalizacao, "0,433")
  expect_equal(emenda$corrida_exploratoria$semente, registro_otimizacao$emendas[[1]]$sementes$principal)
  expect_equal(emenda$corrida_exploratoria$geracoes, 20000)
  expect_equal(emenda$corrida_exploratoria$registrar_a_cada, 500)
  expect_match(emenda$corrida_exploratoria$funcao, "sem função decisória")
  expect_match(emenda$orcamento, "2.000 gerações registradas")
})

test_that("a emenda 3 registra o limite individual como exploratório", {
  emenda <- localizar_emenda(registro_otimizacao, "emenda 3, limite individual calibrado")
  expect_true(emenda$anterior_a_execucao)
  expect_true(emenda$posterior_ao_resultado)
  expect_match(emenda$funcao, "sem função decisória")
  expect_match(emenda$ocupacao$medida, "mês cheio")
  expect_equal(emenda$limite_individual$valor_de_L, 5.7859)
  expect_equal(emenda$sensibilidade$L, 1.5)
  expect_named(
    emenda$concentracao_simultanea$medidas,
    c("ocupacao_maxima", "meses_acima_de_2", "fracao_no_mais_sobrecarregado")
  )
})

