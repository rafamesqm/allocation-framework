#' Acerto em várias listas, com a mesma regra da lista decisória
#'
#' Repete `acertar_na_lista()` para cada tamanho de lista, com o mesmo
#' filtro, os mesmos escores e a mesma chave de desempate. Serve às listas
#' descritivas do pré-registro (`metrica$listas_descritivas`), que não têm
#' teste de decisão.
#'
#' @param pares tibble com `consultor_id` e `projeto_id`.
#' @param contexto Lista com `consultores`, `projetos`, `filtro`, `chave` e
#'   `lista`.
#' @param escores Matriz consultor por projeto, ou `NULL` para o piso.
#' @param tamanhos Tamanhos de lista.
#' @param prefixo Prefixo das colunas, como `"acerto_modelo"`.
#' @return tibble com uma coluna por tamanho, `<prefixo>_<tamanho>`.
acertar_em_listas <- function(pares, contexto, escores, tamanhos, prefixo) {
  colunas <- purrr::map(tamanhos, function(n) {
    contexto$lista <- n
    acertar_na_lista(pares, contexto, escores)
  })
  names(colunas) <- paste0(prefixo, "_", tamanhos)
  tibble::as_tibble(colunas)
}

#' Médias dos acertos nas listas descritivas, para o resumo de uma avaliação
#'
#' @param pares tibble com as colunas de `acertar_em_listas()`.
#' @param prefixos Prefixos das colunas a resumir.
#' @param tamanhos Tamanhos de lista.
#' @return tibble de uma linha, uma coluna por prefixo e tamanho.
resumir_acertos_em_listas <- function(pares, prefixos, tamanhos) {
  nomes <- as.vector(outer(prefixos, tamanhos, paste, sep = "_"))
  tibble::as_tibble(purrr::map(stats::setNames(nomes, paste0("lista_", nomes)), \(n) mean(pares[[n]])))
}
