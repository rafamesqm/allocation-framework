#' Resume as variáveis categóricas de uma tabela
#'
#' Contraparte de `resumir_numericas()` para o lado categórico. Além da
#' contagem de categorias e da moda, traz o equilíbrio da distribuição, que é
#' a informação que decide o que fazer com a variável na modelagem: uma
#' categórica muito concentrada carrega pouca informação discriminante, e uma
#' com muitas categorias raras vai precisar de agrupamento antes de virar
#' one-hot.
#'
#' O equilíbrio é a entropia de Shannon normalizada pelo seu máximo
#' (`log(k)`), então vale 1 quando todas as categorias são igualmente
#' frequentes e tende a 0 quando uma só domina. A normalização é o que
#' permite comparar variáveis com números diferentes de categorias, que é
#' exatamente o caso aqui (`fuso` tem 1, `tipo_projeto` tem 12).
#'
#' @param tabela Data frame ou tibble.
#' @param colunas Nomes das colunas a resumir. `NULL` (padrão) usa todas as
#'   de texto, fator ou lógicas.
#' @return tibble com uma linha por variável: `variavel`, `n`, `n_ausente`,
#'   `n_categorias`, `moda`, `fracao_moda` e `equilibrio`.
resumir_categoricas <- function(tabela, colunas = NULL) {
  if (is.null(colunas)) {
    eh_categorica <- vapply(
      tabela,
      function(coluna) is.character(coluna) || is.factor(coluna) || is.logical(coluna),
      logical(1)
    )
    colunas <- names(tabela)[eh_categorica]
  }

  purrr::map(colunas, function(nome_variavel) {
    valores <- tabela[[nome_variavel]]
    presentes <- valores[!is.na(valores)]
    contagem <- sort(table(presentes), decreasing = TRUE)
    proporcao <- contagem / sum(contagem)

    equilibrio <- if (length(contagem) < 2) {
      0
    } else {
      -sum(proporcao * log(proporcao)) / log(length(contagem))
    }

    tibble::tibble(
      variavel = nome_variavel,
      n = length(presentes),
      n_ausente = sum(is.na(valores)),
      n_categorias = length(contagem),
      moda = as.character(names(contagem)[1]),
      fracao_moda = as.numeric(proporcao[1]),
      equilibrio = equilibrio
    )
  }) |>
    purrr::list_rbind()
}
