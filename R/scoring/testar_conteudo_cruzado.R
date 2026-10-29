# Emenda 2 do conteúdo: o teste único, o diagnóstico e as realizações com
# as previsões cruzadas de `avaliar_conteudo_cruzado.R`. As funções da
# emenda 1, em `testar_conteudo.R`, ficam intocadas.

#' Mede o R² do mapa dentro e fora da amostra
#'
#' Sobre os respondentes com histórico na estimação, compara o fator
#' ajustado pela fatoração com a previsão do mapa ajustado com toda a
#' estimação, que é a da emenda 1, e com a previsão cruzada da emenda 2. O
#' R² dentro da amostra perto de 1 é o defeito: a previsão reproduz o
#' histórico.
#'
#' @param previsao Lista de `prever_fatores_cruzados()`.
#' @param fatores Fatores ajustados da estimação.
#' @param com_historico Consultores com alocação na estimação.
#' @return Lista com `n`, `r2_dentro` e `r2_fora`.
medir_r2_do_mapa <- function(previsao, fatores, com_historico) {
  ids <- intersect(rownames(previsao$previstos), com_historico)
  observado <- fatores[ids, , drop = FALSE]
  total <- sum(sweep(observado, 2, colMeans(observado))^2)
  r2 <- \(previsto) 1 - sum((previsto[ids, , drop = FALSE] - observado)^2) / total
  list(n = length(ids), r2_dentro = r2(previsao$dentro_da_amostra), r2_fora = r2(previsao$previstos))
}

#' Ajusta o mapeamento escolhido num grupo e avalia o grupo reservado, sob a emenda 2
#'
#' Igual a `testar_conteudo_no_grupo()`, da emenda 1, com a pontuação
#' cruzada e o R² do mapa dentro e fora da amostra.
#'
#' @param contexto Lista de `montar_contexto_do_conteudo()`.
#' @param divisao Lista de `dividir_por_consultor_em_grupos()`.
#' @param grupo Grupo reservado.
#' @param configuracao Linha escolhida.
#' @param registro Lista de `ler_pre_registro()`.
#' @return Lista com `pares`, `entradas` e `r2`.
testar_conteudo_cruzado_no_grupo <- function(contexto, divisao, grupo, configuracao, registro) {
  alocacoes <- contexto$dados$alocacoes
  grupo_da_linha <- divisao$grupo_do_consultor$grupo[
    match(alocacoes$consultor_id, divisao$grupo_do_consultor$consultor_id)
  ]
  treino <- alocacoes[grupo_da_linha != grupo, ]
  teste <- alocacoes[grupo_da_linha == grupo & alocacoes$consultor_id %in% contexto$com_atributo, ]
  com_historico <- unique(treino$consultor_id)

  r <- construir_matriz_r(treino, contexto$dados$consultores, contexto$dados$projetos)
  estado <- ajustar_als_implicito(
    r, peso = NULL, fatores = configuracao$fatores, lambda = configuracao$lambda,
    iteracoes = configuracao$iteracoes, semente = registro$sementes$fatoracao
  )[[as.character(configuracao$iteracoes)]]
  previsao <- prever_fatores_cruzados(
    contexto$atributos, estado$x, com_historico, consultores_da_estimacao(divisao, grupo),
    configuracao$lambda_mapeamento,
    n_subgrupos = emenda_2_do_conteudo(registro)$previsoes_cruzadas$subgrupos,
    semente = registro$sementes$divisao
  )
  escores <- matrix(
    0, length(contexto$consultores), length(contexto$projetos),
    dimnames = list(contexto$consultores, contexto$projetos)
  )
  escores[rownames(previsao$previstos), ] <- previsao$previstos %*% t(estado$y)
  constante <- matrix(rowMeans(escores), nrow(escores), ncol(escores), dimnames = dimnames(escores))

  pares <- teste |>
    dplyr::select(consultor_id, projeto_id) |>
    dplyr::mutate(
      grupo = grupo,
      passa_filtro = contexto$filtro[cbind(
        match(consultor_id, contexto$consultores), match(projeto_id, contexto$projetos)
      )],
      acerto_modelo = acertar_na_lista(teste, contexto, escores),
      acerto_piso = acertar_na_lista(teste, contexto),
      acerto_constante = acertar_na_lista(teste, contexto, constante)
    ) |>
    dplyr::bind_cols(
      acertar_em_listas(teste, contexto, escores, registro$metrica$listas_descritivas, "acerto_modelo"),
      acertar_em_listas(teste, contexto, NULL, registro$metrica$listas_descritivas, "acerto_piso")
    )

  popularidade <- as.numeric(table(factor(treino$consultor_id, levels = contexto$consultores)))
  colunas <- match(unique(teste$projeto_id), contexto$projetos)
  candidatos <- which(contexto$filtro[, colunas, drop = FALSE], arr.ind = TRUE)
  r2 <- medir_r2_do_mapa(previsao, estado$x, com_historico)

  list(
    pares = pares,
    entradas = tibble::tibble(
      grupo = grupo,
      escore = escores[cbind(candidatos[, 1], colunas[candidatos[, 2]])],
      popularidade = popularidade[candidatos[, 1]]
    ),
    r2 = tibble::tibble(grupo = grupo, n = r2$n, r2_dentro = r2$r2_dentro, r2_fora = r2$r2_fora)
  )
}

#' Avalia o componente de conteúdo inteiro sob a emenda 2
#'
#' Mesma estrutura de `avaliar_componente_de_conteudo()`, da emenda 1, para o
#' resumo dela servir às duas, com a pontuação cruzada e o R² do mapa. Nas
#' realizações, a escolha de hiperparâmetros é sempre refeita.
#'
#' @param dados Lista nomeada de tibbles.
#' @param registro Lista de `ler_pre_registro()`.
#' @param selecao Lista de `selecionar_hiperparametros_do_conteudo_cruzado()`,
#'   ou `NULL` para refazê-la.
#' @return Lista como a de `avaliar_componente_de_conteudo()`, com `r2`.
avaliar_componente_de_conteudo_cruzado <- function(dados, registro, selecao = NULL) {
  if (is.null(selecao)) {
    selecao <- selecionar_hiperparametros_do_conteudo_cruzado(dados, registro)
  }
  historico <- com_historico_da_janela(dados, janela_do_pre_registro(registro))
  contexto <- montar_contexto_do_conteudo(historico, registro)
  divisao <- dividir_por_consultor_em_grupos(
    historico$alocacoes, n_grupos = registro$divisoes$por_consultor$grupos,
    semente = registro$sementes$divisao, universo = contexto$consultores
  )

  por_grupo <- purrr::map(selecao$escolhidos$grupo, function(g) {
    testar_conteudo_cruzado_no_grupo(
      contexto, divisao, g, selecao$escolhidos[selecao$escolhidos$grupo == g, ], registro
    )
  })
  pares <- purrr::list_rbind(purrr::map(por_grupo, "pares"))
  entradas <- purrr::list_rbind(purrr::map(por_grupo, "entradas"))

  spearman <- dplyr::bind_rows(
    entradas |>
      dplyr::summarise(rho = stats::cor(escore, popularidade, method = "spearman"), .by = grupo) |>
      dplyr::mutate(grupo = as.character(grupo)),
    tibble::tibble(grupo = "reunida", rho = stats::cor(entradas$escore, entradas$popularidade, method = "spearman"))
  )

  grupo_da_linha <- divisao$grupo_do_consultor$grupo[
    match(historico$alocacoes$consultor_id, divisao$grupo_do_consultor$consultor_id)
  ]
  piso_na_validacao <- purrr::map_dbl(selecao$escolhidos$grupo, function(g) {
    fora <- historico$alocacoes[grupo_da_linha != g & historico$alocacoes$consultor_id %in% contexto$com_atributo, ]
    mean(acertar_na_lista(fora, contexto))
  })

  list(
    selecao = selecao,
    pares = pares,
    entradas = entradas,
    teste = testar_mcnemar_exato(pares$acerto_modelo, pares$acerto_piso, registro$teste$nivel),
    spearman = spearman,
    piso_na_validacao = tibble::tibble(grupo = selecao$escolhidos$grupo, piso = piso_na_validacao),
    r2 = purrr::list_rbind(purrr::map(por_grupo, "r2")),
    lista = contexto$lista,
    n_quadro = length(contexto$consultores)
  )
}

#' Resume uma avaliação sob a emenda 2 numa linha de grandezas
#'
#' O resumo da emenda 1, mais o R² médio do mapa dentro e fora da amostra.
#'
#' @param avaliacao Lista de `avaliar_componente_de_conteudo_cruzado()`.
#' @param registro Lista de `ler_pre_registro()`.
#' @return tibble de uma linha.
resumir_avaliacao_do_conteudo_cruzado <- function(avaliacao, registro) {
  dplyr::bind_cols(
    resumir_avaliacao_do_conteudo(avaliacao, registro),
    tibble::tibble(
      r2_mapa_dentro = mean(avaliacao$r2$r2_dentro),
      r2_mapa_fora = mean(avaliacao$r2$r2_fora)
    )
  )
}

#' Refaz a avaliação sob a emenda 2 sobre realizações do gerador
#'
#' Cada realização refaz o procedimento inteiro, inclusive a escolha de
#' hiperparâmetros por validação interna com as previsões cruzadas.
#'
#' @param sementes Vetor de sementes das realizações.
#' @param registro Lista de `ler_pre_registro()`.
#' @param ... Repassado a `gerar_realizacao()`.
#' @return tibble com uma linha por realização.
simular_avaliacao_do_conteudo_cruzado <- function(sementes, registro, ...) {
  purrr::map(sementes, function(semente) {
    avaliacao <- avaliar_componente_de_conteudo_cruzado(gerar_realizacao(semente, ...), registro)
    dplyr::bind_cols(tibble::tibble(semente = semente), resumir_avaliacao_do_conteudo_cruzado(avaliacao, registro))
  }) |>
    purrr::list_rbind()
}

#' Confere a escolha sob a emenda 2 contra a registrada antes do teste
#'
#' @param selecao Lista de `selecionar_hiperparametros_do_conteudo_cruzado()`.
#' @param registro Lista de `ler_pre_registro()`.
#' @return `selecao`, inalterada. Erro se a escolha divergir.
conferir_escolha_do_conteudo_cruzado <- function(selecao, registro) {
  emenda <- purrr::detect(registro$emendas, \(e) identical(e$nome, "escolha sob a emenda 2 do conteúdo"))
  if (is.null(emenda)) stop("Nenhuma emenda registra a escolha sob a emenda 2.", call. = FALSE)

  registrada <- purrr::map(emenda$hiperparametros_do_conteudo_na_realizacao_versionada, tibble::as_tibble) |>
    purrr::list_rbind() |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric))
  escolhida <- selecao$escolhidos |>
    dplyr::select(grupo, fatores, lambda, iteracoes, lambda_mapeamento) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric))

  if (!isTRUE(all.equal(as.data.frame(escolhida), as.data.frame(registrada), check.attributes = FALSE))) {
    stop("A escolha sob a emenda 2 difere da registrada.", call. = FALSE)
  }
  selecao
}
