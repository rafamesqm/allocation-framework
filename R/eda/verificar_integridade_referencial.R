#' Conta chaves órfãs em cada referência declarada no contrato
#'
#' Uma alocação apontando para consultor que não existe, ou uma avaliação
#' apontando para projeto inexistente, envenena silenciosamente qualquer
#' agregação posterior: a linha some no join e a contagem fecha menor sem
#' ninguém notar. Por isso a contagem de órfãos vem antes de qualquer
#' análise, e não depois de um resultado estranho.
#'
#' Reporta também `entidades_referenciadas`, quantas entidades distintas cada
#' referência de fato alcança. É esse número, confrontado com o total da
#' tabela de origem, que mede cobertura (ver `resumir_cobertura()`).
#'
#' @param dados Lista nomeada de tibbles, de `carregar_dados()`.
#' @param especificacao Contrato de esquema, de `especificacao_tabelas()`.
#' @return tibble com `tabela`, `coluna`, `referencia`, `valores_distintos`,
#'   `orfaos` e `integra`, uma linha por referência declarada.
verificar_integridade_referencial <- function(dados, especificacao = especificacao_tabelas()) {
  referencias <- especificacao |>
    dplyr::filter(!is.na(referencia), tabela %in% names(dados))

  if (nrow(referencias) == 0) {
    return(tibble::tibble(
      tabela = character(0), coluna = character(0), referencia = character(0),
      valores_distintos = integer(0), orfaos = integer(0), integra = logical(0)
    ))
  }

  purrr::pmap(
    list(referencias$tabela, referencias$coluna, referencias$referencia),
    function(tabela, coluna, referencia) {
      partes <- strsplit(referencia, ".", fixed = TRUE)[[1]]
      tabela_origem <- partes[1]
      coluna_origem <- partes[2]

      valores <- unique(dados[[tabela]][[coluna]])
      validos <- dados[[tabela_origem]][[coluna_origem]]

      # Contagens resolvidas antes da tibble: `tibble()` deixa cada coluna
      # visível às seguintes, então `length(orfaos)` dentro dela leria a
      # coluna `orfaos`, já reduzida a um inteiro, e não o vetor.
      n_valores <- length(valores)
      n_orfaos <- length(setdiff(valores, validos))

      tibble::tibble(
        tabela = tabela,
        coluna = coluna,
        referencia = referencia,
        valores_distintos = n_valores,
        orfaos = n_orfaos,
        integra = n_orfaos == 0
      )
    }
  ) |>
    purrr::list_rbind()
}
