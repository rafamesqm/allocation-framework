#' Confronta o dado carregado contra o contrato de esquema
#'
#' Responde, com evidência em vez de afirmação, se o sintético honra o
#' `dicionario_de_dados.md`. Reporta quatro formas de divergência: tabela do
#' contrato que não foi carregada, coluna do contrato ausente na tabela,
#' coluna presente no dado e fora do contrato, e coluna cujo tipo não é o
#' declarado.
#'
#' Coluna extra é divergência de pleno direito, não detalhe: campo que
#' ninguém declarou é campo que ninguém decidiu, e no fim vira insumo de
#' modelo sem ter passado por decisão nenhuma.
#'
#' Devolver zero linha é o resultado esperado. A função nunca falha por
#' divergência: quem decide o que fazer com ela é o capítulo, não a
#' verificação.
#'
#' @param dados Lista nomeada de tibbles, de `carregar_dados()`.
#' @param especificacao Contrato de esquema, de `especificacao_tabelas()`.
#' @return tibble com `tabela`, `coluna`, `tipo_divergencia` e `detalhe`,
#'   uma linha por divergência encontrada.
verificar_conformidade <- function(dados, especificacao = especificacao_tabelas()) {
  vazio <- tibble::tibble(
    tabela = character(0), coluna = character(0),
    tipo_divergencia = character(0), detalhe = character(0)
  )

  tabelas_contrato <- unique(especificacao$tabela)

  ausentes <- setdiff(tabelas_contrato, names(dados))
  divergencia_tabela <- if (length(ausentes) == 0) vazio else tibble::tibble(
    tabela = ausentes, coluna = NA_character_,
    tipo_divergencia = "tabela ausente",
    detalhe = "declarada no contrato, não encontrada na fonte"
  )

  observado <- purrr::map(names(dados), function(nome) {
    tibble::tibble(
      tabela = nome,
      coluna = names(dados[[nome]]),
      tipo_observado = vapply(dados[[nome]], classificar_tipo, character(1))
    )
  }) |>
    purrr::list_rbind()

  comparacao <- dplyr::full_join(
    dplyr::select(especificacao, tabela, coluna, tipo_esperado = tipo),
    observado,
    by = c("tabela", "coluna")
  ) |>
    dplyr::filter(tabela %in% names(dados))

  divergencia_coluna <- comparacao |>
    dplyr::filter(is.na(tipo_observado)) |>
    dplyr::transmute(
      tabela, coluna,
      tipo_divergencia = "coluna ausente",
      detalhe = paste0("contrato espera ", tipo_esperado)
    )

  divergencia_extra <- comparacao |>
    dplyr::filter(is.na(tipo_esperado)) |>
    dplyr::transmute(
      tabela, coluna,
      tipo_divergencia = "coluna fora do contrato",
      detalhe = paste0("presente na fonte como ", tipo_observado)
    )

  divergencia_tipo <- comparacao |>
    dplyr::filter(
      !is.na(tipo_esperado), !is.na(tipo_observado), tipo_esperado != tipo_observado
    ) |>
    dplyr::transmute(
      tabela, coluna,
      tipo_divergencia = "tipo divergente",
      detalhe = paste0("contrato espera ", tipo_esperado, ", fonte trouxe ", tipo_observado)
    )

  dplyr::bind_rows(
    divergencia_tabela, divergencia_coluna, divergencia_extra, divergencia_tipo
  ) |>
    dplyr::arrange(tabela, coluna)
}
