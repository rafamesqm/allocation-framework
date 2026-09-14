# Preâmbulo compartilhado por todos os blocos da EDA.
#
# Cada bloco do capítulo é um .Rmd independente, que renderiza sozinho. Este
# arquivo é o que evita que o preâmbulo seja copiado em catorze lugares e
# passe a divergir entre eles. Carregue com:
#
#     source(here::here("analysis", "_comum.R"))
#
# Os documentos não calculam nada: leem o alvo já construído pelo
# `targets` e narram. Análise que roda duas vezes, uma na pipeline e outra no
# texto, é análise que pode divergir de si mesma. Rode `targets::tar_make()`
# antes de renderizar; ver `analysis/_renderizar.R`.

suppressPackageStartupMessages({
  library(here)
  library(tidyverse)
})

# Mesma forma de carregar funções que o _targets.R usa com tar_source("R") e
# que os testes usam no helper: uma fonte só de verdade sobre o que existe.
# Necessário aqui porque o documento formata e desenha com as funções de R/,
# mesmo sem recalcular a análise.
invisible(lapply(
  list.files(here::here("R"), pattern = "[.]R$", recursive = TRUE, full.names = TRUE),
  source
))

aplicar_padrao_monografia()

knitr::opts_chunk$set(
  echo = FALSE,
  message = FALSE,
  warning = FALSE,
  fig.align = "center",
  dpi = 150
)

#' Lê um alvo de bloco da EDA, com mensagem útil se a pipeline não rodou
#'
#' `targets::tar_read()` falha com erro pouco informativo quando o alvo não
#' existe, que é o caso mais provável de quem abre um bloco antes de rodar a
#' pipeline. Trocar por uma instrução direta economiza o diagnóstico.
#'
#' O store é apontado explicitamente para a raiz do projeto. O `knitr`
#' renderiza com o diretório de trabalho em `analysis/`, e sem isso o
#' `targets` procuraria o store em `analysis/_targets/`, que não existe, e
#' concluiria que nenhum alvo foi construído.
#'
#' @param nome Nome do alvo, sem aspas necessárias.
#' @return O valor do alvo.
ler_bloco <- function(nome) {
  store <- here::here("_targets")

  if (!nome %in% targets::tar_objects(store = store)) {
    stop(
      "Alvo '", nome, "' não encontrado. Rode targets::tar_make() antes de ",
      "renderizar os blocos da EDA.",
      call. = FALSE
    )
  }
  targets::tar_read_raw(nome, store = store)
}

#' Renderiza uma tabela do bloco no padrão do capítulo
#'
#' @param tabela Data frame já formatado para leitura.
#' @param legenda Legenda da tabela.
#' @param alinhamento Vetor de alinhamento por coluna, opcional.
#' @return Objeto de tabela do knitr.
mostrar_tabela <- function(tabela, legenda, alinhamento = NULL) {
  knitr::kable(tabela, align = alinhamento, caption = legenda)
}

#' Renderiza os achados de um bloco como texto corrido
#'
#' Os achados vêm do alvo, não do texto do documento, e por isso não podem
#' divergir do que o código concluiu. Enunciado e implicação já embutem os
#' valores calculados.
#'
#' @param achados tibble de `registrar_achados()`.
#' @return Texto em markdown, para uso em chunk com `results = "asis"`.
mostrar_achados <- function(achados) {
  paste0(
    "**Achado ", achados$codigo, "** *(", achados$etiqueta, ")*. ",
    achados$enunciado, " ", achados$implicacao,
    collapse = "\n\n"
  )
}
