# Sourceado automaticamente pelo testthat antes dos testes (convenção
# helper*.R). Carrega as funções de R/ do mesmo jeito que o _targets.R faz
# com tar_source("R"), para os testes enxergarem as funções sem precisar de
# um pacote formal.
arquivos_r <- list.files(here::here("R"), pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
invisible(lapply(arquivos_r, source))
