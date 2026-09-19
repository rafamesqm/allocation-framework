#' Figura: evidência por parâmetro contra os conjuntos públicos de referência
#'
#' Eixo vertical em log porque a comparação cobre duas ordens de grandeza, de
#' menos de 0,2 a mais de 20 observações por parâmetro. Em escala linear, a
#' curva deste projeto ficaria colada no eixo e a diferença, que é justamente
#' o ponto da figura, ficaria invisível.
#'
#' A linha horizontal em 1 é legítima e necessária: ela marca a fronteira em
#' que o número de parâmetros livres iguala o de observações, que é um limite
#' do problema e não uma meta imposta às categorias. É diferente da linha
#' vertical recusada nas figuras de barras dos blocos I.0 e I.1.
#'
#' @param comparacao tibble longo com `conjunto`, `fatores` e
#'   `obs_por_parametro`.
#' @param titulo,subtitulo Rótulos da figura.
#' @return Objeto ggplot.
figura_evidencia_por_parametro <- function(comparacao, titulo, subtitulo = NULL) {
  paleta <- paleta_monografia()

  dados_figura <- dplyr::mutate(
    comparacao,
    conjunto = forcats::fct_reorder(conjunto, obs_por_parametro, .fun = max)
  )

  ggplot2::ggplot(
    dados_figura, ggplot2::aes(fatores, obs_por_parametro, colour = conjunto)
  ) +
    tema_monografia() +
    ggplot2::geom_hline(
      yintercept = 1, colour = paleta$chrome[["moldura"]],
      linetype = "dashed", linewidth = 0.4
    ) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_point(size = 2) +
    ggplot2::scale_colour_manual(values = unname(paleta$categorica)) +
    # Legenda em duas linhas: com quatro series e rotulos longos, uma linha
    # so estoura a largura da pagina e corta o ultimo rotulo.
    ggplot2::guides(colour = ggplot2::guide_legend(nrow = 2)) +
    ggplot2::scale_y_log10(labels = scales::label_number(decimal.mark = ",")) +
    ggplot2::scale_x_continuous(breaks = unique(dados_figura$fatores)) +
    rotular_figura(
      titulo = titulo,
      subtitulo = subtitulo,
      eixo_x = "Número de fatores latentes",
      eixo_y = "Observações por parâmetro (log)",
      legenda = "Conjunto",
      legenda_estetica = "colour"
    )
}

#' Figura: fração de entidades sem evidência suficiente por número de fatores
#'
#' Complementa a razão global, que é uma média e por isso esconde o corte por
#' entidade. O vetor latente de um consultor é estimado a partir das
#' interações dele, e quem tem menos interações que fatores não tem vetor
#' determinado pelos dados, por melhor que seja a razão agregada.
#'
#' @param identificabilidade tibble de `medir_identificabilidade()`.
#' @param titulo,subtitulo Rótulos da figura.
#' @return Objeto ggplot.
figura_subdeterminacao <- function(identificabilidade, titulo, subtitulo = NULL) {
  paleta <- paleta_monografia()

  dados_figura <- identificabilidade |>
    tidyr::pivot_longer(
      cols = c(fracao_consultores_subdeterminados, fracao_projetos_subdeterminados),
      names_to = "lado", values_to = "fracao"
    ) |>
    dplyr::mutate(
      lado = dplyr::if_else(
        lado == "fracao_consultores_subdeterminados", "Consultores", "Projetos"
      )
    )

  ggplot2::ggplot(dados_figura, ggplot2::aes(fatores, fracao, colour = lado)) +
    tema_monografia() +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_point(size = 2) +
    ggplot2::scale_colour_manual(values = unname(paleta$categorica[1:2])) +
    ggplot2::scale_x_continuous(breaks = unique(dados_figura$fatores)) +
    ggplot2::scale_y_continuous(
      labels = function(x) paste0(formatC(100 * x, format = "d"), "%"),
      limits = c(0, 1)
    ) +
    rotular_figura(
      titulo = titulo,
      subtitulo = subtitulo,
      eixo_x = "Número de fatores latentes",
      eixo_y = "Fração sem evidência",
      legenda = "Lado da matriz",
      legenda_estetica = "colour"
    )
}
