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
    # Grandeza invariante tem amplitude zero, e a barra dela não aparece. O
    # ponto no zero é o que faz a cor da legenda corresponder a algo visível.
    ggplot2::geom_point(
      data = dplyr::filter(dados_figura, .data$classe == "invariante"),
      shape = 21, size = 2.2, colour = paleta$chrome[["fundo"]], stroke = 0.4
    ) +
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
    # Sem recorte, para o ponto do invariante, que fica no zero, não sair
    # cortado pela borda do painel.
    ggplot2::coord_cartesian(clip = "off") +
    rotular_figura(
      titulo = titulo,
      subtitulo = subtitulo,
      eixo_x = "Amplitude do intervalo, em proporção da mediana",
      eixo_y = NULL,
      legenda = "Classe"
    )
}

#' Figura: quantas grandezas um sorteio comum deixa fora do intervalo
#'
#' Responde se o conjunto versionado é uma realização típica, e por isso
#' mostra a distribuição de referência e o lugar dele nela, nada além. Cada
#' barra conta as réplicas que, comparadas com o intervalo das demais,
#' deixaram aquele número de grandezas fora. A linha marca o conjunto
#' versionado.
#'
#' As barras ficam em tom recessivo porque são o fundo da comparação, e a
#' linha leva a primeira cor categórica porque é o objeto da pergunta. O
#' rótulo vai direto ao lado dela, em tinta de texto, e não numa legenda:
#' há uma série só.
#'
#' @param referencia Vetor de `contar_fora_por_replica()`.
#' @param n_fora Grandezas fora no conjunto versionado.
#' @param titulo,subtitulo Rótulos da figura.
#' @return Objeto ggplot.
figura_tipicidade_do_versionado <- function(referencia, n_fora, titulo, subtitulo = NULL) {
  paleta <- paleta_monografia()
  contagem <- dplyr::count(tibble::tibble(fora = referencia), .data$fora)

  ggplot2::ggplot(contagem, ggplot2::aes(x = .data$fora, y = .data$n)) +
    tema_monografia() +
    ggplot2::geom_col(width = 0.8, fill = paleta$chrome[["moldura"]]) +
    ggplot2::geom_vline(
      xintercept = n_fora, colour = paleta$categorica[1], linewidth = 0.8
    ) +
    ggplot2::annotate(
      "text", x = n_fora, y = max(contagem$n),
      label = paste0("conjunto versionado: ", n_fora),
      hjust = -0.08, vjust = 1, size = 3.2,
      colour = paleta$chrome[["tinta_primaria"]]
    ) +
    ggplot2::scale_x_continuous(
      breaks = scales::breaks_width(2),
      expand = ggplot2::expansion(mult = c(0.02, 0.15))
    ) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05))) +
    rotular_figura(
      titulo = titulo,
      subtitulo = subtitulo,
      eixo_x = "Grandezas fora do intervalo de 90%",
      eixo_y = "Réplicas"
    )
}

#' Altura da figura de estabilidade, em centímetros
#'
#' A figura tem uma linha por grandeza, e a altura fixa de antes foi pensada
#' para as 38 grandezas da primeira versão. Quando a Parte II passou a ter
#' intervalo, o número subiu e os rótulos começaram a se sobrepor. Derivar a
#' altura do número de linhas impede que isso volte a acontecer a cada
#' grandeza nova.
#'
#' @param n_grandezas Número de grandezas desenhadas.
#' @return Altura em centímetros.
altura_da_figura_estabilidade <- function(n_grandezas) {
  5 + 0.33 * n_grandezas
}
