#' Tema ggplot2 das figuras da monografia
#'
#' Um tema só para o repositório inteiro, para as figuras do capítulo lerem
#' como um sistema e não como uma coleção de gráficos avulsos.
#'
#' Segue a convenção de figura acadêmica impressa, com painel fechado por
#' moldura, grade e subgrade visíveis e marcas de escala nos eixos. A
#' hierarquia entre essas camadas é o que evita que ela vire ruído: a
#' moldura é mais forte que a grade, a grade é mais forte que a subgrade, e
#' as três são mais fracas que qualquer marca de dado. A subgrade existe
#' para permitir leitura aproximada de valor entre duas marcas de escala,
#' então é quase imperceptível de propósito.
#'
#' Nenhuma cor no texto: identidade quem carrega é a marca ao lado, não a
#' letra. Legenda no topo, alinhada à esquerda com o título, para não roubar
#' largura do painel numa figura estreita de página.
#'
#' Não fixa família tipográfica de propósito. A monografia é renderizada por
#' quem audita o repositório, numa máquina que pode não ter a fonte
#' instalada, e uma família ausente quebra a renderização ou troca a fonte
#' em silêncio. O default (`"sans"`) resolve para a sans do sistema em
#' qualquer plataforma.
#'
#' @param tamanho_base Tamanho da fonte base, em pontos. O default 11 é
#'   calibrado para figura de largura de página em corpo de texto.
#' @return Objeto de tema do ggplot2, para somar ao gráfico.
tema_monografia <- function(tamanho_base = 11) {
  cores <- paleta_monografia()$chrome

  ggplot2::theme_minimal(base_size = tamanho_base) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = cores[["fundo"]], colour = NA),
      panel.background = ggplot2::element_rect(fill = cores[["fundo"]], colour = NA),
      panel.border = ggplot2::element_rect(
        fill = NA, colour = cores[["moldura"]], linewidth = 0.4
      ),
      panel.grid.major = ggplot2::element_line(colour = cores[["grade"]], linewidth = 0.3),
      panel.grid.minor = ggplot2::element_line(colour = cores[["subgrade"]], linewidth = 0.2),
      axis.ticks = ggplot2::element_line(colour = cores[["moldura"]], linewidth = 0.3),
      axis.ticks.length = ggplot2::unit(2, "pt"),
      axis.text = ggplot2::element_text(colour = cores[["tinta_suave"]], size = ggplot2::rel(0.9)),
      axis.title = ggplot2::element_text(colour = cores[["tinta_secundaria"]]),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 6)),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 6)),
      plot.title = ggplot2::element_text(
        colour = cores[["tinta_primaria"]], face = "bold", size = ggplot2::rel(1.1)
      ),
      plot.subtitle = ggplot2::element_text(
        colour = cores[["tinta_secundaria"]], margin = ggplot2::margin(t = 2, b = 8)
      ),
      plot.caption = ggplot2::element_text(
        colour = cores[["tinta_suave"]], size = ggplot2::rel(0.8), hjust = 0,
        margin = ggplot2::margin(t = 8)
      ),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.margin = ggplot2::margin(t = 6, r = 8, b = 6, l = 6),
      legend.position = "top",
      legend.justification = "left",
      legend.title = ggplot2::element_text(colour = cores[["tinta_secundaria"]]),
      legend.text = ggplot2::element_text(colour = cores[["tinta_secundaria"]]),
      legend.key = ggplot2::element_blank(),
      legend.margin = ggplot2::margin(b = 4),
      strip.background = ggplot2::element_rect(fill = cores[["subgrade"]], colour = NA),
      strip.text = ggplot2::element_text(
        colour = cores[["tinta_primaria"]], face = "bold",
        margin = ggplot2::margin(t = 3, b = 3, l = 4, r = 4)
      )
    )
}
