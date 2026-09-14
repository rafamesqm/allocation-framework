#' Salva uma tabela da monografia em `outputs/tables/`
#'
#' Efeito colateral isolado e explícito (ver docs/persona.md), par de
#' `salvar_figura()`: as duas são os pontos de escrita dos artefatos
#' destinados ao texto, assim como `salvar_synthetic()` é o de dados.
#'
#' Existe porque tabela renderizada apenas dentro do documento não é
#' artefato: não pode ser conferida fora do R, não entra no texto sem ser
#' redigitada, e não sobrevive a uma troca de formato de saída. Figura o
#' projeto já persistia; tabela não, e essa assimetria era um furo.
#'
#' CSV, não `.rds` nem `.xlsx`, pelo mesmo motivo de `salvar_synthetic()`:
#' legível e auditável fora do R. Gravado em UTF-8, que é o que preserva os
#' acentos dos rótulos em pt-BR.
#'
#' **Espera a tabela já formatada para leitura**, com os nomes de coluna em
#' pt-BR e os números no formato do texto, não o objeto intermediário de
#' cálculo. O artefato aqui é o que vai para a monografia; o dado bruto é
#' sempre recomputável pelas funções que o produziram.
#'
#' @param tabela Data frame ou tibble, já formatado para leitura.
#' @param nome Nome do arquivo, sem extensão.
#' @param diretorio Diretório de destino.
#' @return Caminho do arquivo salvo, de forma invisível (para uso com
#'   `tar_target(..., format = "file")`).
salvar_tabela <- function(tabela, nome,
                           diretorio = here::here("outputs", "tables")) {
  caminho <- file.path(diretorio, paste0(nome, ".csv"))
  garantir_caminho_curto(caminho)

  readr::write_csv(tabela, caminho)

  invisible(caminho)
}
