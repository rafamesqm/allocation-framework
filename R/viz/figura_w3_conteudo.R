#' Figura: a diferença para o piso entre realizações, do mapeamento e da variante
#'
#' Cada ponto é uma realização do gerador, com a escolha de hiperparâmetros
#' refeita nela. As duas faixas põem lado a lado o mapeamento e a sua versão
#' sem projeto, de escore constante por consultor: se as nuvens coincidem, o
#' que o mapeamento ordena é o consultor, e não a afinidade com o projeto. A
#' linha cheia é o zero, e a tracejada o ganho mínimo detectável no cenário
#' conservador, na mediana das realizações.
#'
#' @param replicas tibble de `simular_avaliacao_do_conteudo()`.
#' @param versionado tibble de uma linha, de `resumir_avaliacao_do_conteudo()`.
#' @param titulo,subtitulo Rótulos da figura.
#' @return Objeto ggplot.
figura_diferenca_do_conteudo <- function(replicas, versionado, titulo, subtitulo = NULL) {
  paleta <- paleta_monografia()
  rotulos <- c(
    diferenca_mapeamento = "Mapeamento de atributos",
    diferenca_constante = "Variante de escore constante"
  )
  empilhar <- function(tabela, origem) {
    tabela |>
      dplyr::select(dplyr::all_of(names(rotulos))) |>
      tidyr::pivot_longer(dplyr::everything(), names_to = "serie", values_to = "diferenca") |>
      dplyr::mutate(serie = factor(rotulos[serie], levels = rev(rotulos)), origem = origem)
  }
  pontos <- empilhar(replicas, "Realizações")
  versao <- empilhar(versionado, "Realização versionada")
  ganho <- stats::median(replicas$ganho_minimo_independencia)

  ggplot2::ggplot(pontos, ggplot2::aes(100 * diferenca, serie)) +
    tema_monografia() +
    ggplot2::geom_vline(xintercept = 0, colour = paleta$chrome[["tinta_secundaria"]]) +
    ggplot2::geom_vline(
      xintercept = 100 * ganho, linetype = "dashed", colour = paleta$chrome[["tinta_suave"]]
    ) +
    ggplot2::geom_point(
      position = ggplot2::position_jitter(height = 0.15, width = 0, seed = 1),
      colour = paleta$categorica[[1]], alpha = 0.45, size = 1.6
    ) +
    ggplot2::geom_point(data = versao, colour = paleta$categorica[[2]], shape = 18, size = 4) +
    rotular_figura(
      titulo = titulo,
      subtitulo = subtitulo,
      eixo_x = "Diferença de acerto em lista de 20 para o filtro com acaso (pontos percentuais)",
      eixo_y = NULL
    )
}
