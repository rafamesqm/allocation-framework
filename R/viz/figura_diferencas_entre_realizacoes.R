#' Figura: diferenças de acerto entre realizações, uma faixa por comparação
#'
#' Cada ponto é uma realização do gerador, com a escolha de hiperparâmetros
#' refeita nela, e o losango é a realização versionada. A linha cheia é o
#' zero, e a tracejada o ganho mínimo detectável no cenário conservador, na
#' mediana das realizações. Serve ao conteúdo, em que as faixas são o
#' mapeamento e a sua versão sem projeto, e ao colaborativo, em que são H1 e
#' H2.
#'
#' @param replicas tibble de realizações, com as colunas de `rotulos` e
#'   `coluna_ganho`.
#' @param versionado tibble de uma linha com as mesmas colunas.
#' @param rotulos Vetor nomeado: coluna de diferença para rótulo da faixa, na
#'   ordem de cima para baixo.
#' @param coluna_ganho Coluna do ganho mínimo conservador.
#' @param titulo,subtitulo,eixo_x Rótulos da figura.
#' @return Objeto ggplot.
figura_diferencas_entre_realizacoes <- function(replicas, versionado, rotulos, coluna_ganho,
                                                titulo, subtitulo = NULL, eixo_x) {
  paleta <- paleta_monografia()
  empilhar <- function(tabela) {
    tabela |>
      dplyr::select(dplyr::all_of(names(rotulos))) |>
      tidyr::pivot_longer(dplyr::everything(), names_to = "serie", values_to = "diferenca") |>
      dplyr::mutate(serie = factor(rotulos[serie], levels = rev(rotulos)))
  }
  ganho <- stats::median(replicas[[coluna_ganho]])

  ggplot2::ggplot(empilhar(replicas), ggplot2::aes(100 * diferenca, serie)) +
    tema_monografia() +
    ggplot2::geom_vline(xintercept = 0, colour = paleta$chrome[["tinta_secundaria"]]) +
    ggplot2::geom_vline(
      xintercept = 100 * ganho, linetype = "dashed", colour = paleta$chrome[["tinta_suave"]]
    ) +
    ggplot2::geom_point(
      position = ggplot2::position_jitter(height = 0.15, width = 0, seed = 1),
      colour = paleta$categorica[[1]], alpha = 0.45, size = 1.6
    ) +
    ggplot2::geom_point(data = empilhar(versionado), colour = paleta$categorica[[2]], shape = 18, size = 4) +
    rotular_figura(titulo = titulo, subtitulo = subtitulo, eixo_x = eixo_x, eixo_y = NULL)
}
