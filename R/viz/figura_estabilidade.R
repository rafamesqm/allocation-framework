#' Figura: fragilidade de cada grandeza à troca de realização
#'
#' Desenha a amplitude relativa do intervalo de Monte Carlo, ordenada. A
#' escolha da amplitude **relativa** em vez do intervalo bruto é o que torna
#' a figura legível: as grandezas vivem em escalas incompatíveis, de
#' correlação a contagem de vértices, e um gráfico de intervalos brutos
#' compararia coisas não comparáveis, o mesmo erro corrigido no bloco I.1.
#'
#' Grandeza invariante aparece com amplitude zero, e o texto precisa dizer o
#' que isso significa: não é robustez, é parâmetro imposto ao gerador.
#'
#' Sem linha de referência marcando o corte de fragilidade, de propósito.
#' Linha vertical sobre barras categóricas é lida como meta que vale para
#' todas, e aqui o corte é convenção de leitura declarada no texto, não
#' propriedade das grandezas.
#'
#' @param classificado tibble de `executar_i0()`, com `medida`,
#'   `amplitude_relativa` e `classe`.
#' @param titulo,subtitulo Rótulos da figura.
#' @return Objeto ggplot.
figura_estabilidade <- function(classificado, titulo, subtitulo = NULL) {
  paleta <- paleta_monografia()

  dados_figura <- classificado |>
    dplyr::mutate(
      amplitude_relativa = dplyr::coalesce(amplitude_relativa, 0),
      medida = forcats::fct_reorder(medida, amplitude_relativa),
      classe = factor(classe, levels = c("frágil", "estável", "invariante"))
    )

  ggplot2::ggplot(dados_figura, ggplot2::aes(amplitude_relativa, medida, fill = classe)) +
    tema_monografia() +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::scale_fill_manual(
      values = stats::setNames(
        unname(paleta$categorica[c(2, 1, 3)]),
        c("frágil", "estável", "invariante")
      )
    ) +
    ggplot2::scale_x_continuous(
      labels = function(x) paste0(formatC(100 * x, format = "d"), "%"),
      expand = ggplot2::expansion(mult = c(0, 0.05))
    ) +
    rotular_figura(
      titulo = titulo,
      subtitulo = subtitulo,
      eixo_x = "Amplitude do intervalo, em proporção da mediana",
      eixo_y = NULL,
      legenda = "Classe"
    )
}
