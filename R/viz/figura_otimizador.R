#' Figura: insumos do otimizador, por existência e escala
#'
#' Cada insumo ocupa uma linha e recebe uma de três situações, que são
#' ordenadas e não categorias soltas: não existe, existe sem escala
#' utilizável, existe com escala. É por isso que a paleta usada é a
#' divergente e não a categórica.
#'
#' A figura é de contagem por situação e não de barra por insumo, porque o
#' que decide a wave 4 é quantos insumos caem em cada estado, e não a ordem
#' entre eles.
#'
#' @param insumos tibble de `registrar_insumos_do_otimizador()`.
#' @param titulo,subtitulo Rótulos da figura.
#' @return Objeto ggplot.
figura_insumos_do_otimizador <- function(insumos, titulo, subtitulo = NULL) {
  paleta <- paleta_monografia()

  dados_figura <- insumos |>
    dplyr::mutate(
      situacao = dplyr::case_when(
        !existe ~ "Não existe",
        !dimensionavel ~ "Existe, sem escala utilizável",
        .default = "Existe, com escala"
      ),
      situacao = factor(
        situacao,
        levels = c("Não existe", "Existe, sem escala utilizável", "Existe, com escala")
      ),
      insumo = forcats::fct_reorder(insumo, as.integer(situacao))
    )

  ggplot2::ggplot(dados_figura, ggplot2::aes(y = insumo, fill = situacao)) +
    tema_monografia() +
    ggplot2::geom_bar(width = 0.7) +
    ggplot2::scale_fill_manual(values = unname(paleta$divergente)) +
    ggplot2::scale_x_continuous(breaks = NULL, expand = ggplot2::expansion(mult = c(0, 0))) +
    rotular_figura(
      titulo = titulo,
      subtitulo = subtitulo,
      eixo_x = NULL,
      eixo_y = NULL,
      legenda = "Situação do insumo"
    )
}
