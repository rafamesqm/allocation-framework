#' Figura: série mensal, com uma ou duas séries
#'
#' Linha e não barra: a leitura aqui é de evolução ao longo do tempo, e o
#' tempo é contínuo. Barra sugeriria categorias independentes lado a lado.
#'
#' Aceita no máximo duas séries de propósito. Série temporal com muitas
#' linhas vira emaranhado justamente onde o leitor precisa seguir uma curva,
#' e duas é o limite em que a comparação direta ainda funciona sem rótulo
#' direto em cada ponto.
#'
#' @param serie tibble em formato longo, com uma coluna de data, uma de
#'   valor e, opcionalmente, uma de série.
#' @param coluna_data,coluna_valor Nomes das colunas.
#' @param coluna_serie Nome da coluna que separa as séries, ou `NULL` para
#'   série única.
#' @param titulo,subtitulo,eixo_y,legenda Rótulos da figura.
#' @return Objeto ggplot.
figura_serie_mensal <- function(serie, coluna_data, coluna_valor,
                                 coluna_serie = NULL,
                                 titulo, subtitulo = NULL,
                                 eixo_y = "Contagem", legenda = NULL) {
  paleta <- paleta_monografia()

  base <- ggplot2::ggplot(
    serie, ggplot2::aes(.data[[coluna_data]], .data[[coluna_valor]])
  ) +
    tema_monografia()

  figura <- if (is.null(coluna_serie)) {
    base + ggplot2::geom_line(linewidth = 0.7, colour = paleta$categorica[1])
  } else {
    base +
      ggplot2::geom_line(
        ggplot2::aes(colour = .data[[coluna_serie]]), linewidth = 0.7
      ) +
      ggplot2::scale_colour_manual(values = unname(paleta$categorica[1:2]))
  }

  figura +
    ggplot2::scale_x_date(date_labels = "%Y-%m") +
    ggplot2::scale_y_continuous(
      labels = scales::label_number(big.mark = ".", decimal.mark = ",")
    ) +
    rotular_figura(
      titulo = titulo,
      subtitulo = subtitulo,
      eixo_x = "Mês",
      eixo_y = eixo_y,
      legenda = legenda,
      legenda_estetica = "colour"
    )
}

#' Figura: contagem observada contra a esperada por mês do ano
#'
#' Apoia a leitura de sazonalidade. As barras são o observado e os pontos são
#' o esperado sob ocorrência uniforme no tempo, corrigido pelo número de dias
#' de cada mês presente na janela (ver `testar_sazonalidade_mensal()`).
#'
#' Sobrepor esperado ao observado, em vez de desenhar só a razão entre os
#' dois, preserva a noção de quantidade: uma razão de 1,3 sobre contagem 10 e
#' sobre contagem 1.000 têm pesos de evidência muito diferentes, e o gráfico
#' de razão sozinho esconderia isso.
#'
#' @param contagens tibble de `testar_sazonalidade_mensal()$contagens`.
#' @param titulo,subtitulo Rótulos da figura.
#' @return Objeto ggplot.
figura_sazonalidade <- function(contagens, titulo, subtitulo = NULL) {
  paleta <- paleta_monografia()

  meses_abreviados <- c(
    "jan", "fev", "mar", "abr", "mai", "jun",
    "jul", "ago", "set", "out", "nov", "dez"
  )

  dados_figura <- contagens |>
    dplyr::mutate(mes_rotulo = factor(meses_abreviados[mes_do_ano], levels = meses_abreviados))

  ggplot2::ggplot(dados_figura, ggplot2::aes(mes_rotulo, observado)) +
    tema_monografia() +
    ggplot2::geom_col(width = 0.7, fill = paleta$categorica[1]) +
    ggplot2::geom_point(
      ggplot2::aes(y = esperado), colour = paleta$categorica[2], size = 2.4
    ) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05))) +
    rotular_figura(
      titulo = titulo,
      subtitulo = subtitulo,
      eixo_x = "Mês do ano",
      eixo_y = "Eventos"
    )
}
