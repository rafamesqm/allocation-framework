# Instâncias da wave 4 compartilhadas pelos testes. Sourceado pelo testthat
# antes dos testes, depois de helper.R.

#' Instância pequena com ótimos calculados à mão
#'
#' Três consultores do mesmo nível (A, B e C), dois projetos, três assentos e
#' dois meses. Custo por hora: A e C valem 100, B vale 200. C tem metade do
#' mês 1 em férias, e A tem carga fixa de 0,5 no mês 1.
#'
#' | assento | projeto | meses | carga | horas | realizado |
#' |---------|---------|-------|-------|-------|-----------|
#' | s1      | P1      | M1    | 0,6   | 100   | A         |
#' | s2      | P1      | M1 M2 | 0,5   | 80+80 | B         |
#' | s3      | P2      | M2    | 1,0   | 150   | C         |
#'
#' ŝ em P1: A 1, B 0,5, C 0. ŝ em P2: A 0, B 1, C 0,5.
#'
#' Ocupantes de (s1, s2, s3), contas à mão:
#'
#' * realizada (A, B, C): afinidade 2,0; custo 57.000; sobrecarga 0,1 (A no
#'   mês 1, 0,5 + 0,6).
#' * (B, A, C): afinidade 2,0; custo 51.000; sobrecarga 0. Domina a realizada.
#' * (B, A, B): afinidade 2,5, a máxima; custo 66.000; sobrecarga 0. É a única
#'   de afinidade máxima sem sobrecarga.
#' * (A, B, B): afinidade 2,5; custo 72.000; sobrecarga 0,6.
#' * custo mínimo 41.000, com A e C em s1 e s2 e qualquer dos dois em s3.
#' * sobrecarga mínima 0.
#'
#' Um consultor não ocupa s1 e s2 juntos (mesmo projeto): das 27 escolhas,
#' 18 são viáveis.
#'
#' @return Lista de insumos no formato de `preparar_insumos_da_otimizacao()`.
instancia_pequena_otimizacao <- function() {
  consultores <- c("A", "B", "C")
  s_hat <- list(P1 = c(A = 1, B = 0.5, C = 0), P2 = c(A = 0, B = 1, C = 0.5))
  custo <- c(A = 100, B = 200, C = 100)
  assentos <- tibble::tibble(
    assento_id = c("s1", "s2", "s3"), projeto_id = c("P1", "P1", "P2"),
    nivel = "N3", consultor_realizado = c("A", "B", "C")
  )
  elegibilidade <- tidyr::expand_grid(assento_id = assentos$assento_id, consultor_id = consultores) |>
    dplyr::left_join(assentos, by = "assento_id") |>
    dplyr::mutate(
      realizado = consultor_id == consultor_realizado,
      origem = "lista",
      s_hat = purrr::map2_dbl(projeto_id, consultor_id, \(p, c) s_hat[[p]][[c]]),
      custo_hora = unname(custo[consultor_id])
    ) |>
    dplyr::select(assento_id, projeto_id, nivel, consultor_id, realizado, origem, s_hat, custo_hora)

  list(
    assentos = assentos,
    meses_dos_assentos = tibble::tibble(
      assento_id = c("s1", "s2", "s2", "s3"), ano_mes = c("M1", "M1", "M2", "M2"),
      carga_pct = c(0.6, 0.5, 0.5, 1.0), carga_horas = c(100, 80, 80, 150)
    ),
    carga_fixa = list(por_mes = tibble::tibble(consultor_id = "A", ano_mes = "M1", carga_pct = 0.5, carga_horas = 84)),
    capacidade = tibble::tibble(
      consultor_id = rep(consultores, each = 2), ano_mes = rep(c("M1", "M2"), 3),
      capacidade_pct = c(1, 1, 1, 1, 0.5, 1)
    ) |>
      dplyr::mutate(capacidade_horas = 168 * capacidade_pct),
    elegibilidade = elegibilidade
  )
}

#' Avalia todas as escolhas de um ocupante por assento, por força bruta
#'
#' @param problema Lista de `montar_problema_da_otimizacao()`.
#' @return tibble com uma linha por escolha: `ocupantes` (texto, na ordem dos
#'   assentos), `viavel`, `afinidade`, `custo` e `sobrecarga`.
enumerar_solucoes <- function(problema) {
  por_assento <- split(problema$pares$coluna, problema$pares$assento_id)
  escolhas <- expand.grid(por_assento)
  purrr::map(seq_len(nrow(escolhas)), function(k) {
    colunas <- unlist(escolhas[k, ])
    avaliacao <- avaliar_solucao(problema, completar_solucao(problema, problema$pares$coluna %in% colunas))
    tibble::tibble(
      ocupantes = paste(problema$pares$consultor_id[colunas], collapse = ""),
      viavel = avaliacao$viavel, afinidade = avaliacao$afinidade,
      custo = avaliacao$custo, sobrecarga = avaliacao$sobrecarga
    )
  }) |>
    purrr::list_rbind()
}

#' Montagem da realização versionada, feita uma vez por sessão de testes
#'
#' @return Lista com `dados`, `registro`, `registro_otimizacao`, `insumos` e
#'   `problema`.
montagem_versionada_otimizacao <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      registro <- ler_pre_registro()
      registro_otimizacao <- ler_pre_registro_otimizacao()
      dados <- carregar_dados()
      insumos <- preparar_insumos_da_otimizacao(dados, registro, registro_otimizacao)
      cache <<- list(
        dados = dados, registro = registro, registro_otimizacao = registro_otimizacao,
        insumos = insumos,
        problema = montar_problema_da_otimizacao(insumos, registro_otimizacao$carga$medida)
      )
    }
    cache
  }
})

#' Escolhe um ocupante ao acaso por assento, respeitando um por projeto
#'
#' @param problema Lista de `montar_problema_da_otimizacao()`.
#' @param semente Inteiro.
#' @return Vetor lógico com um valor por par.
sortear_solucao_viavel <- function(problema, semente) {
  pares <- problema$pares
  escolhidos <- integer(0)
  usados <- character(0)
  withr::with_seed(semente, {
    for (assento in unique(pares$assento_id)) {
      do_assento <- pares[pares$assento_id == assento, ]
      livres <- do_assento[!paste(do_assento$consultor_id, do_assento$projeto_id) %in% usados, ]
      k <- livres$coluna[sample.int(nrow(livres), 1)]
      escolhidos <- c(escolhidos, k)
      usados <- c(usados, paste(pares$consultor_id[k], pares$projeto_id[k]))
    }
  })
  pares$coluna %in% escolhidos
}
