#' Figura: distribuição da propensão nos pares observados
#'
#' O eixo horizontal é logarítmico porque a propensão varia por três ordens de
#' grandeza, e em escala linear tudo se acumularia contra o zero, escondendo
#' justamente a cauda que decide a variância do estimador reponderado.
#'
#' A linha vertical marca a propensão mediana. É referência legítima e não
#' meta imposta, no mesmo sentido da diagonal da curva de Lorenz do bloco
#' II.1: serve para o leitor situar a cauda esquerda, que é onde os pesos
#' explodem, em relação ao centro da distribuição.
#'
#' A figura mostra só os pares **observados**, porque é sobre eles que o peso
#' `1 / propensão` é de fato aplicado. Os pares de propensão zero não têm
#' lugar num eixo logarítmico e não são observados por definição; sua massa
#' entra no subtítulo, que é onde ela pode ser dita sem distorcer a escala.
#'
#' @param propensao_observada Vetor de propensões nos pares observados.
#' @param mediana Propensão mediana, para a linha de referência.
#' @param titulo,subtitulo Rótulos da figura.
#' @return Objeto ggplot.
figura_distribuicao_propensao <- function(propensao_observada, mediana,
                                           titulo, subtitulo = NULL) {
  paleta <- paleta_monografia()
  dados_figura <- tibble::tibble(propensao = propensao_observada)

  ggplot2::ggplot(dados_figura, ggplot2::aes(propensao)) +
    tema_monografia() +
    ggplot2::geom_histogram(
      bins = 40, fill = unname(paleta$categorica[1]), colour = NA
    ) +
    ggplot2::geom_vline(
      xintercept = mediana, colour = paleta$chrome[["moldura"]],
      linetype = "dashed", linewidth = 0.4
    ) +
    ggplot2::scale_x_log10(labels = scales::label_number(decimal.mark = ",")) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05))) +
    rotular_figura(
      titulo = titulo,
      subtitulo = subtitulo,
      eixo_x = "Propensão estimada do par observado (escala log)",
      eixo_y = "Alocações"
    )
}

#' Figura: o que a propensão por popularidade captura do viés de seleção
#'
#' Três barras, e a distância entre elas é o argumento inteiro do bloco. Se a
#' barra da propensão estivesse perto da observada, reponderar por
#' popularidade corrigiria o viés. Se está perto da aleatória, o modelo é cego
#' ao mecanismo que domina a seleção, e reponderar por ele corrige uma
#' dimensão que não é a que importa.
#'
#' A cor distingue o que é medido no dado do que é previsto pelo modelo, que é
#' a distinção que o leitor precisa fazer para não ler as três barras como
#' três medidas comparáveis.
#'
#' @param comparacao tibble com `cenario`, `taxa` e `origem`.
#' @param titulo,subtitulo Rótulos da figura.
#' @return Objeto ggplot.
figura_cegueira_da_propensao <- function(comparacao, titulo, subtitulo = NULL) {
  paleta <- paleta_monografia()

  dados_figura <- comparacao |>
    dplyr::mutate(
      cenario = forcats::fct_reorder(cenario, taxa),
      origem = factor(origem, levels = c(
        "Medido no dado", "Previsto pelo modelo", "Referência analítica"
      ))
    )

  ggplot2::ggplot(dados_figura, ggplot2::aes(taxa, cenario, fill = origem)) +
    tema_monografia() +
    ggplot2::geom_col(width = 0.6) +
    # Legenda em duas linhas: com rótulos de categoria longos à esquerda, a
    # área de desenho encolhe e uma legenda de uma linha só estoura a
    # largura, cortando o último rótulo sem aviso.
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 2)) +
    ggplot2::geom_text(
      ggplot2::aes(label = formatar_pct(taxa)),
      hjust = -0.2, size = 3.2, colour = paleta$chrome[["tinta_suave"]]
    ) +
    ggplot2::scale_fill_manual(
      values = unname(paleta$categorica[1:3]), drop = FALSE
    ) +
    ggplot2::scale_x_continuous(
      labels = function(x) paste0(formatC(100 * x, format = "d"), "%"),
      limits = c(0, 1), expand = ggplot2::expansion(mult = c(0, 0.02))
    ) +
    rotular_figura(
      titulo = titulo,
      subtitulo = subtitulo,
      eixo_x = "Taxa de pareamento setorial entre consultor e projeto",
      eixo_y = NULL,
      legenda = "Origem do número"
    )
}
