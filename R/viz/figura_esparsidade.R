#' Figura: curva de Lorenz da concentração de interações
#'
#' Mostra que fração das interações está concentrada em que fração dos nós. A
#' diagonal é a igualdade perfeita, e a distância até ela é o que o Gini
#' resume num número.
#'
#' Duas séries apenas, os dois lados da matriz, o que mantém a figura dentro
#' do limite seguro da paleta e permite comparação direta entre elas.
#'
#' A diagonal de referência é legítima aqui, diferente da linha vertical que
#' foi recusada nas figuras de barras dos blocos I.1 e I.0: ela não é uma
#' meta imposta a categorias, é o próprio significado geométrico da curva,
#' sem a qual o gráfico não se lê.
#'
#' @param curvas tibble com `fracao_nos`, `fracao_interacoes` e `lado`.
#' @param titulo,subtitulo Rótulos da figura.
#' @return Objeto ggplot.
figura_lorenz <- function(curvas, titulo, subtitulo = NULL) {
  paleta <- paleta_monografia()

  ggplot2::ggplot(curvas, ggplot2::aes(fracao_nos, fracao_interacoes, colour = lado)) +
    tema_monografia() +
    ggplot2::geom_abline(
      slope = 1, intercept = 0,
      colour = paleta$chrome[["moldura"]], linetype = "dashed", linewidth = 0.4
    ) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::scale_colour_manual(values = unname(paleta$categorica[1:2])) +
    ggplot2::scale_x_continuous(
      labels = function(x) paste0(formatC(100 * x, format = "d"), "%"), limits = c(0, 1)
    ) +
    ggplot2::scale_y_continuous(
      labels = function(x) paste0(formatC(100 * x, format = "d"), "%"), limits = c(0, 1)
    ) +
    rotular_figura(
      titulo = titulo,
      subtitulo = subtitulo,
      eixo_x = "Fração dos nós, do menos ao mais ativo",
      eixo_y = "Fração acumulada das interações",
      legenda = "Lado da matriz",
      legenda_estetica = "colour"
    )
}

#' Figura: acerto da linha de base de popularidade por tamanho de lista
#'
#' O eixo horizontal é a fração do quadro de consultores que a lista cobre, e
#' não o número absoluto de recomendados. A razão é de leitura: recall de 45%
#' impressiona até se perceber que a lista cobre quase um quinto da
#' população, e é a relação entre os dois que diz se a linha de base é de
#' fato difícil de bater.
#'
#' A diagonal marca o acerto que se obteria recomendando ao acaso uma fração
#' equivalente do quadro. Distância acima dela é o que a popularidade
#' sozinha, sem olhar o projeto, já entrega.
#'
#' @param linha_base tibble de `medir_linha_base_popularidade()`.
#' @param n_consultores Tamanho do quadro, para converter lista em fração.
#' @param titulo,subtitulo Rótulos da figura.
#' @return Objeto ggplot.
figura_linha_base_popularidade <- function(linha_base, n_consultores,
                                            titulo, subtitulo = NULL) {
  paleta <- paleta_monografia()

  dados_figura <- dplyr::mutate(
    linha_base, fracao_do_quadro = n_recomendados / n_consultores
  )

  ggplot2::ggplot(dados_figura, ggplot2::aes(fracao_do_quadro, recall)) +
    tema_monografia() +
    ggplot2::geom_abline(
      slope = 1, intercept = 0,
      colour = paleta$chrome[["moldura"]], linetype = "dashed", linewidth = 0.4
    ) +
    ggplot2::geom_line(linewidth = 0.8, colour = paleta$categorica[1]) +
    ggplot2::geom_point(size = 2.4, colour = paleta$categorica[1]) +
    ggplot2::scale_x_continuous(
      labels = function(x) paste0(formatC(100 * x, format = "d"), "%")
    ) +
    ggplot2::scale_y_continuous(
      labels = function(x) paste0(formatC(100 * x, format = "d"), "%")
    ) +
    rotular_figura(
      titulo = titulo,
      subtitulo = subtitulo,
      eixo_x = "Fração do quadro coberta pela lista",
      eixo_y = "Recall da linha de base"
    )
}
