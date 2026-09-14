#' Recusa caminho que o Windows truncaria em silêncio
#'
#' O Windows corta caminho acima de 260 caracteres **sem erro nenhum**: o
#' arquivo sai com o nome cortado no meio, sem extensão, e a gravação parece
#' ter dado certo. Isso aconteceu de fato neste projeto, ao gerar as figuras
#' de prova do padrão visual, e passou despercebido até alguém listar o
#' diretório.
#'
#' Como o diretório raiz do projeto já é longo, a margem para nome de figura
#' ou de tabela é menor do que parece. Falhar alto é melhor que produzir
#' artefato com nome corrompido, que só apareceria na hora de montar o
#' documento final.
#'
#' Compartilhada por `salvar_figura()` e `salvar_tabela()` para a guarda não
#' divergir entre as duas.
#'
#' @param caminho Caminho completo do arquivo a gravar.
#' @param limite Comprimento máximo tolerado, em caracteres.
#' @return O próprio `caminho`, de forma invisível, quando está dentro do
#'   limite. Para com erro caso contrário.
garantir_caminho_curto <- function(caminho, limite = 255) {
  if (nchar(caminho) > limite) {
    stop(
      "Caminho de ", nchar(caminho), " caracteres, acima do limite de ",
      limite, ". O Windows truncaria o nome sem avisar. Use um nome mais ",
      "curto: ", caminho,
      call. = FALSE
    )
  }
  invisible(caminho)
}
