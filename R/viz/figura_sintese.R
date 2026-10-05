#' Figura: proveniência das decisões, por bloco
#'
#' A figura responde a uma pergunta de banca e por isso é desenhada para
#' responder a ela e nada mais: onde, no capítulo, as decisões se apoiam no
#' gerador em vez de no dado real.
#'
#' **A paleta é a divergente e não a categórica**, porque as três
#' proveniências não são categorias soltas: são graus de quanto a decisão
#' sobrevive fora deste conjunto de dados. `depende do gerador` no polo
#' desfavorável, `estrutural` no neutro, `fato real` no favorável. É o mesmo
#' raciocínio da figura de insumos do bloco II.8.
#'
#' O eixo é por bloco e não um total só, porque o total esconde justamente o
#' que interessa: se a dependência do gerador está espalhada pelo capítulo ou
#' concentrada em poucos pontos, que é a diferença entre um problema de fundo
#' e uma ressalva localizada.
#'
#' @param achados tibble de `consolidar_achados()`.
#' @param titulo,subtitulo Rótulos da figura.
#' @return Objeto ggplot.
figura_proveniencia_das_decisoes <- function(achados, titulo, subtitulo = NULL) {
  paleta <- paleta_monografia()

  cores <- c(
    "depende do gerador" = unname(paleta$divergente_nota[["ABAIXO"]]),
    "estrutural" = unname(paleta$divergente_nota[["DENTRO"]]),
    "fato real" = unname(paleta$divergente_nota[["ACIMA"]])
  )

  dados_figura <- achados |>
    dplyr::filter(!is.na(.data$decisao)) |>
    dplyr::mutate(
      proveniencia = factor(.data$proveniencia, levels = names(cores)),
      bloco = factor(.data$bloco, levels = rev(unique(.data$bloco)))
    )

  ggplot2::ggplot(dados_figura, ggplot2::aes(y = .data$bloco, fill = .data$proveniencia)) +
    tema_monografia() +
    ggplot2::geom_bar(width = 0.7) +
    ggplot2::scale_fill_manual(values = cores, drop = FALSE) +
    ggplot2::scale_x_continuous(
      breaks = scales::breaks_width(1),
      expand = ggplot2::expansion(mult = c(0, 0.02))
    ) +
    rotular_figura(
      titulo = titulo,
      subtitulo = subtitulo,
      eixo_x = "Decisões que o bloco habilita",
      eixo_y = NULL,
      legenda = "Proveniência",
      # A nota de rodapé padrão diria que a figura sai dos dados sintéticos, e
      # esta não sai: ela conta as declarações que cada bloco registrou. Numa
      # figura cujo assunto é justamente a proveniência, a nota errada seria a
      # contradição mais visível da página.
      fonte = "Fonte: elaboração própria sobre os achados declarados nos blocos do capítulo."
    )
}
