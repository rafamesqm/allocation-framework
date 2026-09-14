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
#' @param figura Objeto ggplot.
#' @param nome Nome do arquivo, sem extensão.
#' @param largura_cm,altura_cm Dimensões da figura, em centímetros.
#' @param dpi Resolução, em pontos por polegada.
#' @param diretorio Diretório de destino.
#' @return Caminho do arquivo salvo, de forma invisível (para uso com
#'   `tar_target(..., format = "file")`).
salvar_figura <- function(figura, nome,
                           largura_cm = 16, altura_cm = 10,
                           dpi = 300,
                           diretorio = here::here("outputs", "figures")) {
  caminho <- file.path(diretorio, paste0(nome, ".png"))
  garantir_caminho_curto(caminho)

  ggplot2::ggsave(
    filename = caminho, plot = figura,
    width = largura_cm, height = altura_cm, units = "cm", dpi = dpi
  )

  invisible(caminho)
}
