#' Salva uma tabela sintética em `data/synthetic/`, como CSV
#'
#' Efeito colateral isolado e explícito (ver docs/persona.md): é a única
#' função do projeto que escreve dados em disco (`salvar_figura()` é a
#' equivalente para figuras). `data/synthetic/` é a única fonte de dados
#' versionada do repositório (CLAUDE.md, regra 7). CSV, não `.rds`, para o
#' arquivo ficar legível e auditável fora do R.
#'
#' @param tabela tibble a salvar.
#' @param nome Nome do arquivo, sem extensão.
#' @param diretorio Diretório de destino.
#' @return Caminho do arquivo salvo, de forma invisível (para uso com
#'   `tar_target(..., format = "file")`).
salvar_synthetic <- function(tabela, nome, diretorio = "data/synthetic") {
  caminho <- file.path(diretorio, paste0(nome, ".csv"))
  readr::write_csv(tabela, caminho)
  invisible(caminho)
}
