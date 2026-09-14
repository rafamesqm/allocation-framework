# Renderiza todos os blocos do capítulo de EDA, na ordem.
#
# Segundo e último passo da reprodução, depois de `targets::tar_make()`:
#
#     targets::tar_make()
#     source(here::here("analysis", "_renderizar.R"))
#
# **Por que a renderização fica fora da pipeline.** Renderizar R Markdown
# exige o Pandoc, que é binário externo e não vem com o R nem com o `renv`.
# Colocar os documentos como alvo faria `tar_make()` falhar em qualquer
# máquina sem Pandoc no PATH, inclusive as que têm só o embutido no RStudio.
# Como o `tar_make()` já reconstrói todas as tabelas e figuras, quem não
# puder renderizar ainda obtém os artefatos da monografia por inteiro.
#
# **Por que este passo é obrigatório depois de mexer no gerador.** Os
# documentos leem os alvos do `targets` e recalculam os números do texto a
# cada renderização, mas o `.html` em si não se atualiza sozinho. Sem rodar
# isto, tabelas e figuras ficam novas e os documentos, velhos.
#
# Coerência numérica não é coerência de conclusão: os números do texto e os
# enunciados dos achados saem do dado a cada render, mas julgamentos em prosa
# (o que é "forte", o que "não é monotônico") continuam sendo responsabilidade
# de quem escreve. Depois de uma mudança de calibração, releia os achados.

blocos <- sort(list.files(
  here::here("analysis"),
  pattern = "^i[0-9]+.*[.]Rmd$",
  full.names = TRUE
))

for (bloco in blocos) {
  message("Renderizando ", basename(bloco))
  rmarkdown::render(bloco, quiet = TRUE)
}

message("Blocos renderizados: ", length(blocos))
