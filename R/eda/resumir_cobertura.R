#' Mede a cobertura de cada tabela satélite sobre as entidades
#'
#' Substituto deliberado da análise clássica de dados faltantes. Num dado
#' sintético quase não há `NA`, porque o gerador preenche o que gera, e um
#' mapa de completude cheio de zeros daria aparência de rigor sem dizer nada
#' sobre o problema. O que de fato falta aqui não é célula, é linha: o
#' consultor que não tem nenhuma skill registrada, o projeto que não tem
#' nenhuma alocação.
#'
#' Essa forma de ausência é a que importa para o modelo, é ela que produz
#' cold start, e ao menos uma delas é calibrada contra a realidade (a
#' cobertura de skills, 48,4% dos ativos, ver docs/mapeamento_schema.md).
#'
#' @param dados Lista nomeada de tibbles, de `carregar_dados()`.
#' @param especificacao Contrato de esquema, de `especificacao_tabelas()`.
#' @return tibble com `tabela`, `entidade`, `com_registro`, `total`,
#'   `sem_registro` e `cobertura`, ordenada da menor cobertura para a maior.
resumir_cobertura <- function(dados, especificacao = especificacao_tabelas()) {
  referencias <- especificacao |>
    dplyr::filter(!is.na(referencia), tabela %in% names(dados)) |>
    dplyr::distinct(tabela, coluna, referencia)

  purrr::pmap(
    list(referencias$tabela, referencias$coluna, referencias$referencia),
    function(tabela, coluna, referencia) {
      partes <- strsplit(referencia, ".", fixed = TRUE)[[1]]

      # Derivados resolvidos antes da tibble, pelo mesmo motivo documentado
      # em `verificar_integridade_referencial()`: dentro de `tibble()` cada
      # coluna sombreia a variável local de mesmo nome.
      n_total <- dplyr::n_distinct(dados[[partes[1]]][[partes[2]]])
      n_com_registro <- dplyr::n_distinct(dados[[tabela]][[coluna]])

      tibble::tibble(
        tabela = tabela,
        entidade = referencia,
        coluna = coluna,
        com_registro = n_com_registro,
        total = n_total,
        sem_registro = n_total - n_com_registro,
        cobertura = n_com_registro / n_total
      )
    }
  ) |>
    purrr::list_rbind() |>
    dplyr::arrange(cobertura)
}
