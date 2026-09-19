#' Figura: distribuição da nota em cada etapa de avaliação
#'
#' Barras empilhadas em fração, e não em contagem, porque as três etapas têm
#' exatamente o mesmo número de linhas e a comparação que interessa é de
#' composição. Empilhar é legítimo aqui porque as três notas são as partes de
#' um todo que soma um, que é a única situação em que empilhamento não
#' engana.
#'
#' A paleta é a divergente de nota, e não a categórica: ABAIXO, DENTRO e
#' ACIMA não são categorias trocáveis, são um eixo com meio. Usar cores
#' categóricas sugeriria que a distância entre ABAIXO e DENTRO é do mesmo
#' tipo que a distância entre duas categorias sem ordem.
#'
#' @param por_nota tibble com `etapa`, `nota` e `fracao`.
#' @param titulo,subtitulo Rótulos da figura.
#' @return Objeto ggplot.
figura_divergencia_etapas <- function(por_nota, titulo, subtitulo = NULL) {
  paleta <- paleta_monografia()

  dados_figura <- por_nota |>
    dplyr::mutate(
      nota = factor(nota, levels = c("ABAIXO", "DENTRO", "ACIMA")),
      etapa = forcats::fct_reorder(etapa, fracao * (nota == "ACIMA"), .fun = sum)
    )

  ggplot2::ggplot(dados_figura, ggplot2::aes(fracao, etapa, fill = nota)) +
    tema_monografia() +
    # Empilhamento na ordem do fator, e não na inversa que o ggplot2 usa por
    # default em barra horizontal. Num eixo ordinal a ordem visual precisa
    # acompanhar a ordem da escala, senão a barra lê ACIMA à esquerda
    # enquanto a legenda lê ABAIXO à esquerda, e as duas se contradizem.
    ggplot2::geom_col(width = 0.6, position = ggplot2::position_stack(reverse = TRUE)) +
    ggplot2::scale_fill_manual(values = unname(paleta$divergente_nota)) +
    ggplot2::scale_x_continuous(
      labels = function(x) paste0(formatC(100 * x, format = "d"), "%"),
      expand = ggplot2::expansion(mult = c(0, 0))
    ) +
    rotular_figura(
      titulo = titulo,
      subtitulo = subtitulo,
      eixo_x = "Fração das notas atribuídas",
      eixo_y = NULL,
      legenda = "Nota"
    )
}

#' Figura: variância explicada por componente, contra itens independentes
#'
#' A linha de referência é a chave da leitura, e é o mesmo recurso do modelo
#' nulo dos blocos I.5 e II.4. Se os 14 itens fossem independentes entre si,
#' cada componente principal explicaria em média `1/14` da variância, e a
#' curva seria plana sobre a linha. Distância acima dela no primeiro
#' componente é a estrutura comum que faz da média dos itens uma escala.
#'
#' Sem essa referência o leitor não tem como saber se 41% é muito ou pouco,
#' que é exatamente o problema que o bloco II.2 resolveu com os conjuntos
#' públicos.
#'
#' @param variancia Vetor com a fração de variância de cada componente.
#' @param titulo,subtitulo Rótulos da figura.
#' @return Objeto ggplot.
figura_dimensionalidade_itens <- function(variancia, titulo, subtitulo = NULL) {
  paleta <- paleta_monografia()
  n_itens <- length(variancia)

  dados_figura <- tibble::tibble(
    componente = seq_along(variancia),
    variancia = variancia
  )

  ggplot2::ggplot(dados_figura, ggplot2::aes(componente, variancia)) +
    tema_monografia() +
    ggplot2::geom_hline(
      yintercept = 1 / n_itens, colour = paleta$chrome[["moldura"]],
      linetype = "dashed", linewidth = 0.4
    ) +
    ggplot2::geom_line(linewidth = 0.8, colour = unname(paleta$categorica[1])) +
    ggplot2::geom_point(size = 2, colour = unname(paleta$categorica[1])) +
    ggplot2::scale_x_continuous(breaks = seq_along(variancia)) +
    ggplot2::scale_y_continuous(
      labels = function(x) paste0(formatC(100 * x, format = "d"), "%"),
      limits = c(0, NA), expand = ggplot2::expansion(mult = c(0, 0.05))
    ) +
    rotular_figura(
      titulo = titulo,
      subtitulo = subtitulo,
      eixo_x = "Componente principal",
      eixo_y = "Fração da variância explicada"
    )
}
