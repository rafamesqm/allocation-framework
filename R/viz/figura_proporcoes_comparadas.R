#' Figura: composição sintética contra a proporção real documentada
#'
#' Barras lado a lado, real e sintético, por categoria. Duas séries apenas, o
#' que mantém a figura dentro do limite seguro da paleta mesmo em forma de
#' comparação direta.
#'
#' Barras agrupadas e não empilhadas: empilhar somaria duas proporções que
#' pertencem a distribuições diferentes, o que não tem significado. O leitor
#' precisa comparar altura contra altura dentro de cada categoria, e é isso
#' que o agrupamento permite.
#'
#' Por padrão ordena pela proporção real, decrescente, para a leitura seguir
#' a importância no dado observado e não a ordem alfabética da categoria.
#'
#' **`ordem` existe para as variáveis ordinais, e nelas é obrigatório usá-la.**
#' Ordenar `nivel_senioridade` por frequência produz N3, N2, N1, N4, N0, o que
#' destrói a única informação que a variável carrega, que é a ordem da
#' carreira. Ordenação por frequência só faz sentido em categórica nominal.
#'
#' @param comparacao tibble de `confrontar_proporcoes()`.
#' @param titulo Título da figura.
#' @param subtitulo Subtítulo, opcional.
#' @param ordem Vetor com as categorias na ordem desejada, do início ao fim
#'   do eixo vertical. `NULL` (padrão) ordena pela proporção real.
#' @return Objeto ggplot.
figura_proporcoes_comparadas <- function(comparacao, titulo, subtitulo = NULL,
                                          ordem = NULL) {
  paleta <- paleta_monografia()

  dados_figura <- comparacao |>
    dplyr::mutate(
      categoria = if (is.null(ordem)) {
        forcats::fct_reorder(categoria, proporcao_real)
      } else {
        factor(categoria, levels = ordem)
      }
    ) |>
    tidyr::pivot_longer(
      cols = c(proporcao_real, proporcao_sintetica),
      names_to = "origem", values_to = "proporcao"
    ) |>
    dplyr::mutate(
      origem = dplyr::if_else(origem == "proporcao_real", "Real", "Sintético"),
      origem = factor(origem, levels = c("Real", "Sintético"))
    )

  ggplot2::ggplot(dados_figura, ggplot2::aes(proporcao, categoria, fill = origem)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.75), width = 0.7) +
    ggplot2::scale_fill_manual(values = unname(paleta$categorica[1:2])) +
    ggplot2::scale_x_continuous(
      labels = function(x) paste0(formatC(100 * x, format = "d"), "%"),
      expand = ggplot2::expansion(mult = c(0, 0.05))
    ) +
    rotular_figura(
      titulo = titulo,
      subtitulo = subtitulo,
      eixo_x = "Proporção da categoria",
      eixo_y = NULL,
      legenda = "Origem"
    )
}
