# Renderiza todos os documentos da modelagem, na ordem das etapas.
#
# Mesmo papel de `outputs/results/eda/_renderizar.R`, e pelo mesmo motivo
# fora da pipeline: renderizar exige o Pandoc, que não vem com o R. Depois de
# `targets::tar_make()`:
#
#     source(here::here("outputs", "results", "scoring", "_renderizar.R"))

documentos <- sort(list.files(
  here::here("outputs", "results", "scoring"),
  pattern = "^w3-.*[.]Rmd$",
  full.names = TRUE
))

for (documento in documentos) {
  message("Renderizando ", basename(documento))
  rmarkdown::render(documento, quiet = TRUE)
}

message("Documentos renderizados: ", length(documentos))
