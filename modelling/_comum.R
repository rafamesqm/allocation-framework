# Preâmbulo compartilhado pelos documentos da modelagem, wave 3.
#
# `modelling/` faz para a modelagem o papel que `analysis/` faz para a EDA:
# um documento por etapa, que lê o alvo já construído pelo `targets` e
# narra, sem recalcular nada. O código da modelagem fica em `R/scoring/`,
# como o da EDA fica em `R/eda/`, porque a pipeline e os testes carregam as
# funções de `R/`. Carregue com:
#
#     source(here::here("modelling", "_comum.R"))
#
# Rode `targets::tar_make()` antes de renderizar; ver
# `modelling/_renderizar.R`.

# O preâmbulo da EDA já carrega os pacotes, as funções de R/, o padrão
# gráfico, a guarda contra valor inline vazio e os leitores de alvo e de
# tabela. Reaproveitá-lo evita dois preâmbulos que divergem.
source(here::here("analysis", "_comum.R"))

#' Caminho da leitura qualitativa preenchida pelo autor
#'
#' A pipeline gera só o modelo em branco da rubrica, em
#' `outputs/tables/w3_rubrica_leitura_modelo.csv`. A cópia com os vereditos
#' vive aqui, num arquivo que nenhum alvo escreve, para o `tar_make()` nunca
#' apagar a leitura.
#'
#' @return Caminho do arquivo.
caminho_da_rubrica_preenchida <- function() {
  here::here("modelling", "leitura", "rubrica_preenchida.csv")
}

#' Lê a rubrica preenchida, se ela já existe
#'
#' @return tibble com a rubrica, ou `NULL` enquanto a leitura não foi feita.
ler_rubrica_preenchida <- function() {
  caminho <- caminho_da_rubrica_preenchida()
  if (!file.exists(caminho)) return(NULL)
  readr::read_csv(caminho, show_col_types = FALSE)
}
