#' Salva uma figura da monografia em `outputs/figures/`
#'
#' Efeito colateral isolado e explícito (ver docs/persona.md), espelho de
#' `salvar_synthetic()` para o lado das figuras: as duas são os únicos
#' pontos de escrita em disco do projeto.
#'
#' Dimensões em centímetros, não em polegadas, porque o alvo é página de
#' documento e é assim que a largura útil de página se mede. O default de
#' 16 cm cabe na área de texto de uma página A4 com margens usuais. 300 dpi
#' é o mínimo para figura impressa não sair serrilhada.
#'
#' O comprimento do caminho é conferido antes de gravar, ver
#' `garantir_caminho_curto()` para o porquê.
#'
#' **Os rótulos são requebrados aqui, contra a largura real do arquivo.** O
#' ggplot2 não quebra texto nem avisa quando ele passa da borda: ele
#' simplesmente some do PNG. `rotular_figura()` já quebra na origem, mas com
#' largura fixa, e largura fixa não serve porque as figuras do projeto vão de
#' 16 a 19 cm e o mesmo subtítulo cabe numa e não cabe na outra. Foi assim que
#' o subtítulo do bloco II.4 saiu cortado mesmo depois da primeira correção.
#'
#' Aqui a largura é conhecida, então o orçamento de caracteres sai dela. As
#' constantes de caracteres por centímetro foram calibradas contra as figuras
#' deste projeto, nas fontes de `tema_monografia()`, e são conservadoras de
#' propósito: uma linha quebrada cedo demais é feia, uma linha cortada é
#' informação perdida sem aviso.
#'
#' @param figura Objeto ggplot.
#' @param nome Nome do arquivo, sem extensão.
#' @param largura_cm,altura_cm Dimensões da figura, em centímetros.
#' @param dpi Resolução, em pontos por polegada.
#' @param diretorio Diretório de destino; por padrão, o de `diretorio_de_saida()`.
#' @return Caminho do arquivo salvo, de forma invisível (para uso com
#'   `tar_target(..., format = "file")`).
salvar_figura <- function(figura, nome,
                           largura_cm = 16, altura_cm = 10,
                           dpi = 300,
                           diretorio = diretorio_de_saida("figures")) {
  caminho <- file.path(diretorio, paste0(nome, ".png"))
  garantir_caminho_curto(caminho)

  figura <- requebrar_rotulos(figura, largura_cm)

  ggplot2::ggsave(
    filename = caminho, plot = figura,
    width = largura_cm, height = altura_cm, units = "cm", dpi = dpi
  )

  invisible(caminho)
}

#' Requebra os rótulos de uma figura contra a largura em que ela será gravada
#'
#' Separada de `salvar_figura()` para poder ser testada sem escrever em disco.
#' `strwrap()` trata quebra existente como espaço e refaz o fluxo, então
#' aplicar de novo sobre texto já quebrado por `rotular_figura()` é correto e
#' não acumula quebras.
#'
#' Título usa orçamento menor que subtítulo porque é negrito e maior (ver
#' `tema_monografia()`); nome de eixo usa menor ainda porque fica centrado
#' sobre a área de desenho, que é mais estreita que a figura por causa dos
#' rótulos de categoria à esquerda.
#'
#' A substituição preserva `NULL`, e isso não é detalhe. `rotular_figura()`
#' exige `eixo_y = NULL` explícito para omitir um eixo de propósito, e em R
#' atribuir `NULL` a um elemento de lista **remove** o elemento em vez de
#' guardar o valor. Removido, o ggplot2 volta ao default e passa a estampar o
#' nome cru da coluna no eixo, que foi o que aconteceu na primeira versão
#' desta função: a figura do bloco II.4 saiu com "cenario" escrito no eixo.
#'
#' @param figura Objeto ggplot.
#' @param largura_cm Largura do arquivo, em centímetros.
#' @param por_cm Caracteres por centímetro de cada tipo de rótulo.
#' @return A figura com os rótulos requebrados.
requebrar_rotulos <- function(figura, largura_cm,
                               por_cm = c(titulo = 3.4, subtitulo = 4.8, eixo = 3.6)) {
  orcamento <- c(
    title = por_cm[["titulo"]], subtitle = por_cm[["subtitulo"]],
    x = por_cm[["eixo"]], y = por_cm[["eixo"]]
  )

  for (rotulo in names(orcamento)) {
    if (!rotulo %in% names(figura$labels)) next

    figura$labels[rotulo] <- list(quebrar_rotulo(
      figura$labels[[rotulo]], round(orcamento[[rotulo]] * largura_cm)
    ))
  }

  figura
}
