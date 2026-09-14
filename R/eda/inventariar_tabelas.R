#' Inventaria as tabelas carregadas: porte, chave e unicidade
#'
#' Primeira coisa que o capítulo mostra: o que existe, de que tamanho, e com
#' que granularidade. A coluna `chave_unica` é a que tem conteúdo analítico,
#' não a contagem de linhas: ela confronta a granularidade declarada no
#' contrato com a que o dado de fato tem. Chave declarada que se repete
#' significa que a tabela não é o que o dicionário diz que é, e qualquer
#' contagem feita sobre ela depois estaria errada.
#'
#' @param dados Lista nomeada de tibbles, de `carregar_dados()`.
#' @param especificacao Contrato de esquema, de `especificacao_tabelas()`.
#' @return tibble com `tabela`, `linhas`, `colunas`, `chave` (colunas da
#'   chave, separadas por vírgula), `chave_unica` e `linhas_duplicadas`.
inventariar_tabelas <- function(dados, especificacao = especificacao_tabelas()) {
  chaves <- especificacao |>
    dplyr::filter(papel == "chave") |>
    dplyr::summarise(chave = list(coluna), .by = tabela)

  purrr::map(names(dados), function(nome) {
    # Tudo calculado antes de montar a tibble: `tibble()` avalia os
    # argumentos em sequência e deixa os anteriores visíveis aos seguintes,
    # então uma variável local com o mesmo nome de uma coluna seria
    # sombreada pela coluna, e o resultado sumiria sem erro.
    conteudo <- dados[[nome]]
    n_linhas <- nrow(conteudo)
    n_colunas <- ncol(conteudo)

    colunas_chave <- chaves$chave[chaves$tabela == nome]
    colunas_chave <- if (length(colunas_chave) == 0) character(0) else colunas_chave[[1]]
    colunas_chave <- intersect(colunas_chave, names(conteudo))

    tem_chave <- length(colunas_chave) > 0
    duplicadas <- if (tem_chave) sum(duplicated(conteudo[colunas_chave])) else NA_integer_
    chave_declarada <- if (tem_chave) paste(colunas_chave, collapse = ", ") else NA_character_

    tibble::tibble(
      tabela = nome,
      linhas = n_linhas,
      colunas = n_colunas,
      chave = chave_declarada,
      linhas_duplicadas = duplicadas,
      chave_unica = tem_chave & duplicadas == 0
    )
  }) |>
    purrr::list_rbind() |>
    dplyr::arrange(dplyr::desc(linhas))
}
