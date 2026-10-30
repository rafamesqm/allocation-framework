# Renderiza todos os documentos da modelagem, na ordem das etapas.
#
# Mesmo papel de `analysis/_renderizar.R`, e pelo mesmo motivo fora da
# pipeline: renderizar exige o Pandoc, que não vem com o R. Depois de
# `targets::tar_make()`:
#
#     source(here::here("modelling", "_renderizar.R"))

documentos <- sort(list.files(
  here::here("modelling"),
  pattern = "^w[34]-.*[.]Rmd$",
  full.names = TRUE
))

for (documento in documentos) {
  message("Renderizando ", basename(documento))
  rmarkdown::render(documento, quiet = TRUE)
}

message("Documentos renderizados: ", length(documentos))
