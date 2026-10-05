#' Figura: o que cada forma de divisão deixa para cada componente
#'
#' Duas medidas por forma, lado a lado, porque a decisão está na oposição
#' entre elas: vazamento alto invalida a avaliação de atributo de entidade, e
#' avaliabilidade baixa invalida a do colaborativo. Mostrar só uma levaria a
#' escolher a forma errada.
#'
#' O eixo vai de zero a um nas duas medidas, o que as torna comparáveis na
#' mesma escala sem precisar de segundo eixo, recurso que costuma enganar
#' mais do que informa.
#'
#' @param protocolos tibble de `avaliar_protocolos()`.
#' @param titulo,subtitulo Rótulos da figura.
#' @return Objeto ggplot.
figura_avaliabilidade <- function(protocolos, titulo, subtitulo = NULL) {
  paleta <- paleta_monografia()

  dados_figura <- protocolos |>
    dplyr::select(forma, vazamento_consultor, avaliavel_colaborativo) |>
    tidyr::pivot_longer(
      c(vazamento_consultor, avaliavel_colaborativo),
      names_to = "medida", values_to = "valor"
    ) |>
    dplyr::mutate(
      forma = rotular_forma_de_divisao(forma),
      medida = factor(
        medida,
        levels = c("avaliavel_colaborativo", "vazamento_consultor"),
        labels = c(
          "Pares que o colaborativo pontua",
          "Consultores do teste já vistos no treino"
        )
      )
    )

  ggplot2::ggplot(dados_figura, ggplot2::aes(valor, forma, fill = medida)) +
    tema_monografia() +
    ggplot2::geom_col(width = 0.7, position = ggplot2::position_dodge(width = 0.75)) +
    ggplot2::scale_fill_manual(values = unname(paleta$categorica[1:2])) +
    ggplot2::scale_x_continuous(
      labels = function(x) paste0(formatC(100 * x, format = "d"), "%"),
      limits = c(0, 1), expand = ggplot2::expansion(mult = c(0, 0.02))
    ) +
    rotular_figura(
      titulo = titulo,
      subtitulo = subtitulo,
      eixo_x = "Fração dos pares de teste",
      eixo_y = NULL,
      legenda = "Medida"
    )
}
