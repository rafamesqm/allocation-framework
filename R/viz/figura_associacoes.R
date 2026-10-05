#' Figura: correlações medidas, agrupadas pela procedência declarada
#'
#' Não é um mapa de calor de matriz de correlação, e a escolha é deliberada.
#' O mapa de calor trata todos os pares como se tivessem o mesmo estatuto,
#' que é justamente o que não vale aqui: no sintético, associação calibrada,
#' associação induzida por construção e associação que deveria ser nula são
#' coisas epistemicamente diferentes, e misturá-las num gradiente contínuo
#' apaga a distinção que dá sentido à figura.
#'
#' Cada painel é uma procedência, e a leitura muda de painel para painel. Em
#' `calibrada` o que importa é a distância entre o ponto medido e o alvo
#' real. Em `livre` e `ausente`, o que importa é a distância até o zero, que
#' é a linha de referência. Em `induzida` não há alvo, só a magnitude.
#'
#' A escala horizontal é fixa em toda a faixa possível de correlação, de -1 a
#' 1, e não ajustada aos dados. Ajustá-la por painel faria uma correlação de
#' 0,03 ocupar a mesma largura visual que uma de 0,9, o que é exatamente a
#' leitura errada num gráfico cujo ponto é comparar magnitudes contra zero e
#' contra um alvo.
#'
#' @param confronto tibble de `confrontar_associacoes()`.
#' @param titulo Título da figura.
#' @param subtitulo Subtítulo, opcional.
#' @return Objeto ggplot.
figura_associacoes_por_procedencia <- function(confronto, titulo, subtitulo = NULL) {
  paleta <- paleta_monografia()
  cores <- paleta$chrome

  dados_figura <- confronto |>
    dplyr::mutate(
      par = paste(variavel_a, "x", variavel_b),
      par = forcats::fct_reorder(par, medida)
    )

  alvos <- dplyr::filter(dados_figura, !is.na(valor_real))

  ggplot2::ggplot(dados_figura, ggplot2::aes(medida, par)) +
    tema_monografia() +
    ggplot2::geom_vline(
      xintercept = 0, colour = cores[["moldura"]], linewidth = 0.4
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(x = 0, xend = medida, yend = par),
      colour = cores[["grade"]], linewidth = 0.6
    ) +
    ggplot2::geom_point(
      ggplot2::aes(colour = "Medido no sintético"), size = 2.4
    ) +
    ggplot2::geom_point(
      data = alvos,
      ggplot2::aes(valor_real, par, colour = "Alvo real documentado"),
      size = 2.4, shape = 17
    ) +
    ggplot2::scale_colour_manual(
      values = stats::setNames(
        unname(paleta$categorica[1:2]),
        c("Medido no sintético", "Alvo real documentado")
      )
    ) +
    ggplot2::scale_x_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.5)) +
    ggplot2::facet_wrap(~procedencia, ncol = 1, scales = "free_y") +
    rotular_figura(
      titulo = titulo,
      subtitulo = subtitulo,
      eixo_x = "Correlação de Pearson",
      eixo_y = NULL,
      legenda = "Origem do valor",
      legenda_estetica = "colour"
    )
}
