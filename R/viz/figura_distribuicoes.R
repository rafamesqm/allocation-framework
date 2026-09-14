#' Figura: distribuição das variáveis numéricas de uma camada
#'
#' Pequenos múltiplos, um painel por variável, com escalas livres. Escala
#' livre é necessária aqui e não é comodismo: as variáveis de uma mesma
#' camada diferem em ordens de grandeza (valor de contrato chega a dezenas de
#' milhões, percentual de alocação vive entre 0 e 1), e escala comum
#' esmagaria todas menos uma.
#'
#' O preço da escala livre é que os painéis deixam de ser comparáveis entre
#' si em magnitude, e o leitor precisa saber disso. Por isso a figura serve
#' para ler **forma** (onde está a massa, para que lado é a cauda, se há
#' acúmulo em zero), nunca para comparar tamanho entre painéis. O texto que
#' acompanha carrega os números.
#'
#' `escala_log` existe porque quase toda variável contínua deste projeto é
#' fortemente assimétrica à direita. Em escala linear o histograma vira uma
#' barra no zero e nada mais, que não informa. O zero é deslocado em uma
#' unidade antes do log, para a massa pontual em zero não ser descartada em
#' silêncio, que é o erro clássico ao logaritmar variável com hurdle.
#'
#' @param tabela Data frame ou tibble.
#' @param colunas Nomes das colunas numéricas a desenhar.
#' @param titulo Título da figura.
#' @param subtitulo Subtítulo, opcional.
#' @param escala_log Se `TRUE`, desenha o eixo horizontal em log10 sobre
#'   `valor + 1`.
#' @param n_colunas Número de colunas do painel.
#' @return Objeto ggplot.
figura_distribuicoes <- function(tabela, colunas, titulo,
                                  subtitulo = NULL,
                                  escala_log = FALSE,
                                  n_colunas = 3) {
  dados_longos <- tabela |>
    dplyr::select(dplyr::all_of(colunas)) |>
    tidyr::pivot_longer(
      cols = dplyr::everything(), names_to = "variavel", values_to = "valor"
    )

  if (escala_log) {
    dados_longos <- dplyr::mutate(dados_longos, valor = valor + 1)
  }

  figura <- ggplot2::ggplot(dados_longos, ggplot2::aes(valor)) +
    ggplot2::geom_histogram(
      bins = 40, colour = "white", linewidth = 0.15,
      fill = paleta_monografia()$categorica[1]
    ) +
    ggplot2::facet_wrap(~variavel, scales = "free", ncol = n_colunas) +
    tema_monografia()

  # Em log, rótulo de potência de dez em vez do número por extenso. Com
  # `valor_contrato` cobrindo cinco ordens de grandeza, os números escritos
  # se sobrepõem no eixo e ficam ilegíveis. Potência de dez é a convenção em
  # eixo logarítmico e resolve a colisão sem encurtar o eixo.
  if (escala_log) {
    figura <- figura + ggplot2::scale_x_log10(labels = scales::label_log())
  } else {
    figura <- figura +
      ggplot2::scale_x_continuous(
        labels = scales::label_number(big.mark = ".", decimal.mark = ",")
      )
  }

  figura +
    rotular_figura(
      titulo = titulo,
      subtitulo = subtitulo,
      eixo_x = if (escala_log) "Valor (escala log, deslocada em 1)" else "Valor",
      eixo_y = "Frequência"
    )
}
