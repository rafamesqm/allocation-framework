#' Etiquetas de status epistêmico admitidas para um achado
#'
#' Toda conclusão deste capítulo declara o que ela é capaz de afirmar, porque
#' a EDA roda sobre dado cujo mecanismo gerador é conhecido e foi escrito
#' neste projeto. Sem a etiqueta, achado sobre a Peers e achado sobre o
#' gerador ficam indistinguíveis para quem lê.
#'
#' * `âncora real`: o número é calibrado contra agregado documentado em
#'   `docs/mapeamento_schema.md`. Fala sobre a Peers, com as ressalvas de
#'   cobertura de join já registradas.
#' * `consequência do gerador`: decorre de suposição de modelagem, não de
#'   observação. Fala sobre o instrumento, não sobre a empresa.
#' * `instrumento`: o valor está no diagnóstico ter sido construído e
#'   validado, aplicável ao dado real se o acesso for restabelecido.
#'
#' @return Vetor de texto com as três etiquetas.
etiquetas_achado <- function() {
  c("âncora real", "consequência do gerador", "instrumento")
}

#' Valida e registra os achados numerados de um bloco da EDA
#'
#' Cada bloco declara seus achados como dado, ao lado do código que os
#' produziu, em vez de apenas como prosa no documento. A síntese da Parte III
#' empilha os arquivos em vez de reler catorze documentos e transcrever à
#' mão, transcrição que sairia de sincronia com o bloco de origem na primeira
#' revisão.
#'
#' A numeração é por bloco (`I.1.1`, `II.4.2`), não uma sequência única do
#' capítulo. Com um documento por bloco, numeração global exigiria estado
#' compartilhado entre arquivos e quebraria a cada reordenação.
#'
#' A validação é deliberadamente rígida. Achado sem etiqueta epistêmica ou
#' sem implicação declarada é exatamente o tipo de conclusão solta que este
#' capítulo não deve produzir, e é mais barato falhar aqui do que descobrir
#' na revisão do texto.
#'
#' @param bloco Identificador do bloco, como `"I.1"`.
#' @param achados Data frame com as colunas `codigo`, `etiqueta`,
#'   `enunciado` e `implicacao`.
#' @return O próprio `achados`, validado, com a coluna `bloco` à frente.
registrar_achados <- function(bloco, achados) {
  obrigatorias <- c("codigo", "etiqueta", "enunciado", "implicacao")
  faltantes <- setdiff(obrigatorias, names(achados))
  if (length(faltantes) > 0) {
    stop("Faltam colunas no registro de achados: ", paste(faltantes, collapse = ", "), call. = FALSE)
  }

  invalidas <- setdiff(achados$etiqueta, etiquetas_achado())
  if (length(invalidas) > 0) {
    stop(
      "Etiqueta de achado não reconhecida: ", paste(unique(invalidas), collapse = ", "),
      ". Use uma de: ", paste(etiquetas_achado(), collapse = ", "),
      call. = FALSE
    )
  }

  fora_do_bloco <- achados$codigo[!startsWith(achados$codigo, paste0(bloco, "."))]
  if (length(fora_do_bloco) > 0) {
    stop(
      "Código de achado fora do bloco ", bloco, ": ",
      paste(fora_do_bloco, collapse = ", "),
      call. = FALSE
    )
  }

  if (any(duplicated(achados$codigo))) {
    stop("Código de achado repetido no bloco ", bloco, ".", call. = FALSE)
  }

  vazios <- achados$codigo[!nzchar(trimws(achados$enunciado)) |
    !nzchar(trimws(achados$implicacao))]
  if (length(vazios) > 0) {
    stop(
      "Achado sem enunciado ou sem implicação declarada: ",
      paste(vazios, collapse = ", "),
      call. = FALSE
    )
  }

  tibble::as_tibble(achados) |>
    dplyr::mutate(bloco = bloco, .before = 1) |>
    dplyr::arrange(codigo)
}
