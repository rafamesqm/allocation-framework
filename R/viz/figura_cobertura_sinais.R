#' Figura: cobertura de cada sinal, separada por tipo e por lado da matriz
#'
#' A figura existe para desfazer uma leitura em bloco. Agregar os sinais de
#' conteúdo numa média esconderia que alguns cobrem toda a população e outros
#' menos da metade, e é exatamente essa diferença que decide quais formas de
#' híbrido são viáveis.
#'
#' A cor separa sinal de conteúdo de sinal de interação, que é a distinção
#' que organiza a leitura. São duas categorias apenas, bem dentro do limite
#' seguro da paleta. A faceta separa os dois lados da matriz, que têm
#' denominadores diferentes e não devem ser comparados em contagem, só em
#' fração, e por isso o eixo é fração.
#'
#' @param cobertura tibble de `medir_cobertura_de_sinais()`.
#' @param titulo,subtitulo Rótulos da figura.
#' @return Objeto ggplot.
figura_cobertura_de_sinais <- function(cobertura, titulo, subtitulo = NULL) {
  paleta <- paleta_monografia()

  dados_figura <- cobertura |>
    dplyr::mutate(
      sinal = forcats::fct_reorder(sinal, cobertura),
      tipo = factor(tipo, levels = c("conteúdo", "interação"))
    )

  ggplot2::ggplot(dados_figura, ggplot2::aes(cobertura, sinal, fill = tipo)) +
    tema_monografia() +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::facet_grid(rows = ggplot2::vars(lado), scales = "free_y", space = "free_y") +
    ggplot2::scale_fill_manual(values = unname(paleta$categorica[1:2])) +
    ggplot2::scale_x_continuous(
      labels = function(x) paste0(formatC(100 * x, format = "d"), "%"),
      limits = c(0, 1), expand = ggplot2::expansion(mult = c(0, 0.02))
    ) +
    rotular_figura(
      titulo = titulo,
      subtitulo = subtitulo,
      eixo_x = "Fração das entidades daquele lado com o sinal disponível",
      eixo_y = NULL,
      legenda = "Tipo de sinal"
    )
}
