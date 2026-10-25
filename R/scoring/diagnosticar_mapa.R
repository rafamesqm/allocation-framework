#' Mede a dispersão dos escores na lista, sob as emendas 1 e 2
#'
#' Medida descritiva única da hipótese de que o defeito da emenda 1 fazia a
#' posição do consultor de teste depender da dispersão das previsões. Para
#' cada grupo e cada emenda, nas listas dos pares de teste que passam o
#' filtro, compara a dispersão dos escores dos candidatos de teste com a dos
#' candidatos da estimação, e mede a distribuição do percentil do consultor
#' de teste na lista. A investigação para aqui, qualquer que seja o
#' resultado.
#'
#' @param dados Lista nomeada de tibbles.
#' @param registro Lista de `ler_pre_registro()`.
#' @param selecao_emenda_1,selecao_emenda_2 Listas das escolhas de
#'   hiperparâmetros das duas emendas.
#' @return tibble com uma linha por emenda e grupo.
medir_dispersao_da_lista <- function(dados, registro, selecao_emenda_1, selecao_emenda_2) {
  historico <- com_historico_da_janela(dados, janela_do_pre_registro(registro))
  contexto <- montar_contexto_do_conteudo(historico, registro)
  divisao <- dividir_por_consultor_em_grupos(
    historico$alocacoes, n_grupos = registro$divisoes$por_consultor$grupos,
    semente = registro$sementes$divisao, universo = contexto$consultores
  )
  grupo_do <- stats::setNames(divisao$grupo_do_consultor$grupo, divisao$grupo_do_consultor$consultor_id)
  grupo_da_linha <- grupo_do[historico$alocacoes$consultor_id]
  escolhas <- list(`emenda 1` = selecao_emenda_1$escolhidos, `emenda 2` = selecao_emenda_2$escolhidos)

  purrr::imap(escolhas, function(escolhidos, emenda) {
    purrr::map(escolhidos$grupo, function(g) {
      cfg <- escolhidos[escolhidos$grupo == g, ]
      treino <- historico$alocacoes[grupo_da_linha != g, ]
      teste <- historico$alocacoes[grupo_da_linha == g & historico$alocacoes$consultor_id %in% contexto$com_atributo, ]
      com_historico <- unique(treino$consultor_id)
      r <- construir_matriz_r(treino, historico$consultores, historico$projetos)
      estado <- ajustar_als_implicito(
        r, peso = NULL, fatores = cfg$fatores, lambda = cfg$lambda,
        iteracoes = cfg$iteracoes, semente = registro$sementes$fatoracao
      )[[as.character(cfg$iteracoes)]]
      escores <- if (emenda == "emenda 1") {
        escorar_mapeamento(estado, contexto, com_historico, cfg$lambda_mapeamento)
      } else {
        escorar_mapeamento_cruzado(
          estado, contexto, com_historico, consultores_da_estimacao(divisao, g), cfg$lambda_mapeamento, registro
        )
      }

      i <- match(teste$consultor_id, contexto$consultores)
      j <- match(teste$projeto_id, contexto$projetos)
      da_estimacao <- grupo_do[contexto$consultores] != g
      por_par <- purrr::map(which(contexto$filtro[cbind(i, j)]), function(k) {
        filtrados <- which(contexto$filtro[, j[k]])
        s <- escores[, j[k]]
        estimacao <- filtrados[da_estimacao[filtrados]]
        de_teste <- filtrados[!da_estimacao[filtrados]]
        ordem <- order(-s[filtrados], contexto$chave[filtrados, j[k]])
        posicao <- match(i[k], filtrados[ordem])
        tibble::tibble(
          dp_estimacao = stats::sd(s[estimacao]),
          dp_teste = if (length(de_teste) > 1) stats::sd(s[de_teste]) else NA_real_,
          z_focal = (s[i[k]] - mean(s[estimacao])) / stats::sd(s[estimacao]),
          percentil = 1 - (posicao - 1) / (length(filtrados) - 1)
        )
      }) |>
        purrr::list_rbind()

      tibble::tibble(
        emenda = emenda, grupo = g, pares = nrow(por_par),
        razao_dispersao = stats::median(por_par$dp_teste, na.rm = TRUE) / stats::median(por_par$dp_estimacao, na.rm = TRUE),
        distancia_focal = stats::median(abs(por_par$z_focal), na.rm = TRUE),
        percentil_mediana = stats::median(por_par$percentil),
        nos_extremos = mean(por_par$percentil > 0.9 | por_par$percentil < 0.1)
      )
    }) |>
      purrr::list_rbind()
  }) |>
    purrr::list_rbind()
}

#' Correlação entre o fator previsto pelo mapa e o fator ajustado, por dimensão
#'
#' Diagnóstico descritivo da emenda 2. Em cada grupo da validação cruzada,
#' refaz a fatoração e as previsões cruzadas com a configuração escolhida e,
#' sobre os respondentes com histórico na estimação, correlaciona cada
#' dimensão do fator previsto fora da amostra com a mesma dimensão do fator
#' ajustado pela fatoração. Complementa o R²: o R² negativo diz que o mapa
#' erra a escala, e a correlação diz se ele acerta ao menos a ordem.
#'
#' @param dados Lista nomeada de tibbles.
#' @param registro Lista de `ler_pre_registro()`.
#' @param selecao Lista de `selecionar_hiperparametros_do_conteudo_cruzado()`.
#' @return tibble com `grupo`, `dimensao`, `n`, `correlacao_fora` e
#'   `correlacao_dentro`.
medir_correlacao_do_mapa <- function(dados, registro, selecao) {
  historico <- com_historico_da_janela(dados, janela_do_pre_registro(registro))
  contexto <- montar_contexto_do_conteudo(historico, registro)
  divisao <- dividir_por_consultor_em_grupos(
    historico$alocacoes, n_grupos = registro$divisoes$por_consultor$grupos,
    semente = registro$sementes$divisao, universo = contexto$consultores
  )
  grupo_da_linha <- divisao$grupo_do_consultor$grupo[
    match(historico$alocacoes$consultor_id, divisao$grupo_do_consultor$consultor_id)
  ]

  purrr::map(selecao$escolhidos$grupo, function(g) {
    configuracao <- selecao$escolhidos[selecao$escolhidos$grupo == g, ]
    treino <- historico$alocacoes[grupo_da_linha != g, ]
    com_historico <- unique(treino$consultor_id)
    r <- construir_matriz_r(treino, historico$consultores, historico$projetos)
    estado <- ajustar_als_implicito(
      r, peso = NULL, fatores = configuracao$fatores, lambda = configuracao$lambda,
      iteracoes = configuracao$iteracoes, semente = registro$sementes$fatoracao
    )[[as.character(configuracao$iteracoes)]]
    previsao <- prever_fatores_cruzados(
      contexto$atributos, estado$x, com_historico, consultores_da_estimacao(divisao, g),
      configuracao$lambda_mapeamento,
      n_subgrupos = emenda_2_do_conteudo(registro)$previsoes_cruzadas$subgrupos,
      semente = registro$sementes$divisao
    )

    ids <- intersect(rownames(previsao$previstos), com_historico)
    correlacionar <- function(previsto, d) {
      ajustado <- estado$x[ids, d]
      if (stats::sd(ajustado) == 0 || stats::sd(previsto[ids, d]) == 0) NA_real_ else stats::cor(previsto[ids, d], ajustado)
    }
    dimensoes <- seq_len(ncol(estado$x))
    tibble::tibble(
      grupo = g,
      dimensao = dimensoes,
      n = length(ids),
      correlacao_fora = purrr::map_dbl(dimensoes, \(d) correlacionar(previsao$previstos, d)),
      correlacao_dentro = purrr::map_dbl(dimensoes, \(d) correlacionar(previsao$dentro_da_amostra, d))
    )
  }) |>
    purrr::list_rbind()
}
