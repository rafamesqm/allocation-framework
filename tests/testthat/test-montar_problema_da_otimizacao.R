# A montagem do problema da wave 4, sem solver. A instância pequena tem os
# ótimos calculados à mão (ver instancia_pequena_otimizacao()) e serve agora
# para conferir a matriz por força bruta e, quando o solver entrar, para
# conferir o solver.

test_that("a instância pequena reproduz os valores calculados à mão", {
  problema <- montar_problema_da_otimizacao(instancia_pequena_otimizacao(), "percentual")
  solucoes <- enumerar_solucoes(problema)
  viaveis <- solucoes[solucoes$viavel, ]
  de <- \(ocupantes) solucoes[solucoes$ocupantes == ocupantes, ]

  expect_equal(nrow(solucoes), 27)
  expect_equal(nrow(viaveis), 18)
  expect_false(any(solucoes$viavel[substr(solucoes$ocupantes, 1, 1) == substr(solucoes$ocupantes, 2, 2)]))

  expect_equal(max(viaveis$afinidade), 2.5)
  expect_equal(min(viaveis$custo), 41000)
  expect_equal(min(viaveis$sobrecarga), 0)

  expect_equal(unlist(de("ABC")[c("afinidade", "custo", "sobrecarga")]), c(afinidade = 2, custo = 57000, sobrecarga = 0.1))
  expect_equal(unlist(de("BAC")[c("afinidade", "custo", "sobrecarga")]), c(afinidade = 2, custo = 51000, sobrecarga = 0))
  expect_equal(unlist(de("ABB")[c("afinidade", "custo", "sobrecarga")]), c(afinidade = 2.5, custo = 72000, sobrecarga = 0.6))

  maxima_sem_sobrecarga <- viaveis[viaveis$afinidade == 2.5 & viaveis$sobrecarga < 1e-9, ]
  expect_equal(maxima_sem_sobrecarga$ocupantes, "BAB")
  expect_equal(maxima_sem_sobrecarga$custo, 66000)
  expect_setequal(viaveis$ocupantes[viaveis$custo == 41000], c("ACA", "ACC", "CAA", "CAC"))

  # A realizada é viável e dominada.
  realizada <- avaliar_solucao(problema, solucao_realizada(problema))
  expect_true(realizada$viavel)
  domina <- viaveis$afinidade >= realizada$afinidade & viaveis$custo <= realizada$custo &
    viaveis$sobrecarga <= realizada$sobrecarga + 1e-9 &
    (viaveis$afinidade > realizada$afinidade | viaveis$custo < realizada$custo |
      viaveis$sobrecarga < realizada$sobrecarga - 1e-9)
  expect_true("BAC" %in% viaveis$ocupantes[domina])
})

test_that("a sobrecarga da carga fixa sozinha entra como constante e é descontada na atribuível", {
  insumos <- instancia_pequena_otimizacao()
  # D não é candidato a nada e tem carga fixa acima da capacidade no mês 2.
  insumos$capacidade <- dplyr::bind_rows(
    insumos$capacidade,
    tibble::tibble(consultor_id = "D", ano_mes = c("M1", "M2"), capacidade_pct = 1, capacidade_horas = 168)
  )
  insumos$carga_fixa$por_mes <- dplyr::bind_rows(
    insumos$carga_fixa$por_mes,
    tibble::tibble(consultor_id = "D", ano_mes = "M2", carga_pct = 1.25, carga_horas = 210)
  )
  problema <- montar_problema_da_otimizacao(insumos, "percentual")
  realizada <- avaliar_solucao(problema, solucao_realizada(problema))

  expect_equal(problema$constante_sobrecarga, 0.25)
  expect_equal(problema$sobrecarga_da_carga_fixa, 0.25)
  expect_equal(realizada$sobrecarga, 0.35)
  expect_equal(realizada$sobrecarga_atribuivel, 0.1)
})

test_that("a alocação realizada é viável na matriz montada", {
  montagem <- montagem_versionada_otimizacao()
  realizada <- avaliar_solucao(montagem$problema, solucao_realizada(montagem$problema))
  expect_true(realizada$viavel)
  expect_equal(sum(montagem$problema$pares$realizado), nrow(montagem$insumos$assentos))
})

test_that("o nível de todo candidato é o do assento", {
  montagem <- montagem_versionada_otimizacao()
  elegibilidade <- montagem$insumos$elegibilidade
  nivel_do_consultor <- montagem$dados$consultores$nivel_senioridade[
    match(elegibilidade$consultor_id, montagem$dados$consultores$consultor_id)
  ]
  nivel_do_assento <- montagem$insumos$assentos$nivel[
    match(elegibilidade$assento_id, montagem$insumos$assentos$assento_id)
  ]
  expect_equal(nivel_do_consultor, nivel_do_assento)
  expect_equal(elegibilidade$nivel, nivel_do_assento)
})

test_that("nenhuma alocação a partir do corte entra em ŝ", {
  montagem <- montagem_versionada_otimizacao()
  corte <- montagem$insumos$corte
  expect_true(all(montagem$insumos$estimacao$data_inicio < corte))

  # Embaralhar quem ocupou as alocações a partir do corte não muda ŝ.
  dados <- montagem$dados
  depois <- which(dados$alocacoes$data_inicio >= corte)
  dados$alocacoes$consultor_id[depois] <- withr::with_seed(1, sample(dados$alocacoes$consultor_id[depois]))
  embaralhado <- preparar_insumos_da_otimizacao(dados, montagem$registro, montagem$registro_otimizacao)

  expect_equal(embaralhado$corte, corte)
  expect_identical(embaralhado$s_hat, montagem$insumos$s_hat)
})

test_that("ŝ é zero para a zona morta e para o realizado fora da lista", {
  montagem <- montagem_versionada_otimizacao()
  elegibilidade <- montagem$insumos$elegibilidade
  na_lista <- paste(montagem$insumos$s_hat$projeto_id, montagem$insumos$s_hat$consultor_id)
  fora <- elegibilidade[elegibilidade$origem != "lista", ]

  expect_true(all(fora$s_hat == 0))
  expect_false(any(paste(fora$projeto_id, fora$consultor_id) %in% na_lista))
  expect_true(all(fora$realizado[fora$origem == "realizado fora da lista"]))

  # A zona morta não tem histórico na estimação nem setor declarado.
  morta <- unique(fora$consultor_id[fora$origem == "zona morta"])
  expect_false(any(morta %in% montagem$insumos$estimacao$consultor_id))
  expect_false(any(morta %in% montagem$dados$experiencia_setorial$consultor_id))
})

test_that("a variação da margem é o negativo da variação do custo", {
  montagem <- montagem_versionada_otimizacao()
  problema <- montagem$problema
  medir <- function(x) {
    margem <- medir_margem_dos_projetos(problema, x, montagem$insumos, montagem$dados$projetos, montagem$dados$custo_carregado)
    c(margem = sum(margem$margem), custo = avaliar_solucao(problema, x)$custo, custo_por_fora = sum(margem$custo))
  }
  realizada <- medir(solucao_realizada(problema))
  for (semente in 1:3) {
    x <- completar_solucao(problema, sortear_solucao_viavel(problema, semente))
    expect_true(avaliar_solucao(problema, x)$viavel)
    outra <- medir(x)
    expect_equal(outra[["custo_por_fora"]], outra[["custo"]], tolerance = 1e-10)
    expect_equal(outra[["margem"]] - realizada[["margem"]], -(outra[["custo"]] - realizada[["custo"]]), tolerance = 1e-10)
  }
})
