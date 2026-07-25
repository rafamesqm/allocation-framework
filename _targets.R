# Pipeline de análise (targets). Roda por padrão sobre data/synthetic/.
# tar_make() deve reconstruir tudo do dado sintético ao resultado.
# Ver docs/dicionario_de_dados.md para o esquema dos dados.

library(targets)

tar_source("R")

tar_option_set(
  packages = c(
    "tidyverse", "data.table", "janitor", "skimr", "DataExplorer", "here", "Matrix"
  )
)

fonte_dados <- "synthetic"

list(
  # Wave 1 (ETL + EDA): targets entram aqui conforme a EDA avança.
  # Ex.: tar_target(consultores_raw, importar_consultores(fonte = fonte_dados))
)
