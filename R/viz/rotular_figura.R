#' Rotula uma figura da monografia, exigindo título e eixos
#'
#' Envelope de `ggplot2::labs()` que torna mecânica a convenção de rotulagem
#' do capítulo, em vez de deixá-la por conta da disciplina de quem escreve
#' cada figura: toda figura tem título, e os dois eixos têm nome, com
#' unidade quando houver.
#'
#' Título, `eixo_x` e `eixo_y` são obrigatórios e a função para se algum
#' vier vazio. É deliberado. Figura sem rótulo de eixo é o defeito mais
#' comum em trabalho acadêmico, e um erro na hora de montar a figura custa
#' muito menos que descobrir isso na revisão do texto. Para o caso legítimo
#' de eixo sem rótulo (categoria já óbvia pelos próprios valores), passe
#' `NULL` explicitamente, que é diferente de esquecer.
#'
#' A nota de rodapé traz, por default, a origem dos dados. Isso não é
#' decoração: a wave 2 inteira roda sobre dados sintéticos, e cada figura
#' que sair do capítulo precisa dizer isso por conta própria, porque figura
#' circula solta, fora do texto que a explica.
#'
#' `legenda_estetica` existe porque o título da legenda pertence a uma
#' estética específica. Nomear as duas de uma vez faria o ggplot2 avisar
#' sobre rótulo desconhecido em toda figura que usa só uma delas, e aviso
#' repetido em documento renderizado é ruído que ninguém lê.
#'
#' @param titulo Título da figura. Obrigatório.
#' @param eixo_x,eixo_y Nome de cada eixo, com unidade quando houver.
#'   Obrigatórios; use `NULL` explícito para omitir um deles de propósito.
#' @param subtitulo Subtítulo, opcional. Bom lugar para o recorte dos dados.
#' @param legenda Título da legenda, opcional. Use quando a variável mapeada
#'   à cor não for óbvia pelo título.
#' @param legenda_estetica Estética a que o título de legenda se aplica,
#'   `"fill"` (default, preenchimento de barra e área) ou `"colour"` (linha
#'   e ponto). Ignorado quando `legenda` é `NULL`.
#' @param fonte Nota de rodapé.
#' @return Objeto de rótulos do ggplot2, para somar ao gráfico.
rotular_figura <- function(titulo,
                            eixo_x,
                            eixo_y,
                            subtitulo = NULL,
                            legenda = NULL,
                            legenda_estetica = c("fill", "colour"),
                            fonte = "Fonte: elaboração própria sobre os dados sintéticos do projeto.") {
  if (missing(titulo) || !is.character(titulo) || length(titulo) != 1 || !nzchar(titulo)) {
    stop("Toda figura da monografia precisa de título.", call. = FALSE)
  }
  if (missing(eixo_x) || missing(eixo_y)) {
    stop(
      "Informe eixo_x e eixo_y. Para omitir um de propósito, passe NULL ",
      "explicitamente.",
      call. = FALSE
    )
  }
  legenda_estetica <- match.arg(legenda_estetica)

  rotulos <- list(
    title = titulo,
    subtitle = subtitulo,
    x = eixo_x,
    y = eixo_y,
    caption = fonte
  )
  if (!is.null(legenda)) {
    rotulos[[legenda_estetica]] <- legenda
  }

  do.call(ggplot2::labs, rotulos)
}
