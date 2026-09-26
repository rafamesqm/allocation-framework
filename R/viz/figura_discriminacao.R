#' Figura: separação de cada mecanismo, contra o acaso
#'
#' Barras horizontais de AUC com a linha do acaso em 0,5. A linha é
#' referência legítima e não meta imposta: AUC é probabilidade de ordenar
#' certo um par sorteado, e 0,5 é exatamente o valor de quem ordena ao acaso.
#' Sem ela o leitor não tem como saber se 0,49 é pouco.
#'
#' A cor separa o que ordena do que não ordena, que é a distinção que a
#' figura existe para mostrar: o mecanismo de maior AUC é o que empata todo
#' mundo, e ler só o comprimento da barra levaria à escolha errada.
#'
#' @param resultados tibble de `comparar_mecanismos_de_p()`.
#' @param titulo,subtitulo Rótulos da figura.
#' @param limite_empate Acima desta fração empatada o mecanismo é tratado
#'   como incapaz de ordenar. Convenção de leitura declarada.
#' @return Objeto ggplot.
figura_decomposicao_auc <- function(resultados, titulo, subtitulo = NULL,
                                     limite_empate = 0.99) {
  paleta <- paleta_monografia()

  dados_figura <- resultados |>
    dplyr::mutate(
      mecanismo = forcats::fct_reorder(mecanismo, auc),
      ordena = ifelse(
        fracao_empatada >= limite_empate,
        "Empata quase tudo", "Ordena"
      )
    )

  ggplot2::ggplot(dados_figura, ggplot2::aes(auc, mecanismo, fill = ordena)) +
    tema_monografia() +
    ggplot2::geom_col(width = 0.6) +
    ggplot2::geom_vline(
      xintercept = 0.5, colour = paleta$chrome[["moldura"]],
      linetype = "dashed", linewidth = 0.4
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = formatar_decimal(auc, 3)),
      hjust = -0.2, size = 3.2, colour = paleta$chrome[["tinta_suave"]]
    ) +
    ggplot2::scale_fill_manual(values = unname(paleta$categorica[1:2])) +
    ggplot2::scale_x_continuous(
      limits = c(0, 1), expand = ggplot2::expansion(mult = c(0, 0.02))
    ) +
    rotular_figura(
      titulo = titulo,
      subtitulo = subtitulo,
      eixo_x = "Área sob a curva ROC",
      eixo_y = NULL,
      legenda = "Capacidade de ordenar"
    )
}
