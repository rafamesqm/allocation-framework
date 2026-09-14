#' Constrói `R`, a matriz esparsa consultor × projeto (Camada 2)
#'
#' `R` é o objeto algébrico que o colaborativo consome (ver
#' docs/dicionario_de_dados.md). Uma alocação observada vira uma célula
#' preenchida; todo o resto é estrutural, não é zero de verdade, é par não
#' observado.
#'
#' **A matriz cobre todos os consultores e todos os projetos, não só os que
#' aparecem em `alocacoes`.** Consultor ou projeto em cold start vira linha
#' ou coluna inteiramente vazia, que é justamente o que a wave 2 precisa
#' enxergar e o que o cold start significa. Montar a matriz só sobre quem
#' tem alocação esconderia o problema e ainda deixaria a densidade com o
#' denominador errado.
#'
#' O valor da célula fica em aberto de propósito: a construção do rótulo
#' (desempenho, margem, ou a ocorrência pura da alocação como feedback
#' implícito) é decisão de modelagem que a EDA ainda vai informar, e o
#' dicionário lista quatro candidatos sem escolher. Por isso `valor` é
#' argumento: o default 1 dá a matriz de ocorrência, e passar
#' `alocacoes$pct_alocacao` ou um rótulo derivado dá a matriz ponderada, sem
#' que esta função precise saber qual candidato venceu.
#'
#' @param alocacoes tibble de `gerar_alocacoes()`, com `consultor_id` e
#'   `projeto_id`.
#' @param consultores tibble com a coluna `consultor_id`, que define as
#'   linhas e a ordem delas.
#' @param projetos tibble com a coluna `projeto_id`, que define as colunas.
#' @param valor Vetor numérico do tamanho de `alocacoes`, com o valor de
#'   cada célula observada. `NULL` (padrão) preenche com 1.
#' @return Matriz esparsa `dgCMatrix` de dimensão
#'   `nrow(consultores)` × `nrow(projetos)`, com `dimnames`.
construir_matriz_r <- function(alocacoes, consultores, projetos, valor = NULL) {
  linha <- match(alocacoes$consultor_id, consultores$consultor_id)
  coluna <- match(alocacoes$projeto_id, projetos$projeto_id)

  if (anyNA(linha) || anyNA(coluna)) {
    stop(
      "Há alocação cujo consultor_id ou projeto_id não existe nas tabelas de ",
      "origem. `R` ficaria com célula órfã.",
      call. = FALSE
    )
  }
  if (any(duplicated(cbind(linha, coluna)))) {
    stop(
      "Há par (consultor_id, projeto_id) repetido em `alocacoes`. A matriz ",
      "esparsa somaria as repetições em silêncio, mudando o valor da célula.",
      call. = FALSE
    )
  }

  if (is.null(valor)) {
    valor <- rep(1, nrow(alocacoes))
  }
  stopifnot(length(valor) == nrow(alocacoes))

  Matrix::sparseMatrix(
    i = linha,
    j = coluna,
    x = as.numeric(valor),
    dims = c(nrow(consultores), nrow(projetos)),
    dimnames = list(consultores$consultor_id, projetos$projeto_id)
  )
}
