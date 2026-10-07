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

# **Valor inline vazio é erro, não texto.** Quando a grandeza que um trecho
# cita deixa de existir, `r ...` devolve vetor vazio ou `NA`, e o knitr
# imprime nada no lugar do número: o documento sai com "()" ou "contra ,"
# e ninguém percebe. Aconteceu no bloco I.2, mais de uma vez, e uma varredura
# do texto renderizado não pegou todos os casos, porque o buraco toma formas
# diferentes conforme a frase. Barrar na origem pega todos: a renderização
# para e aponta as linhas do trecho.
#
# O gancho é o de avaliação, e não o de saída. Para resultado de comprimento
# zero, que é justamente o caso de grandeza que sumiu, o knitr escreve texto
# vazio sem chamar o gancho de saída, e a guarda ficaria cega exatamente onde
# mais importa.
local({
  avaliar_padrao <- knitr::knit_hooks$get("evaluate.inline")
  knitr::knit_hooks$set(evaluate.inline = function(code, envir = knitr::knit_global()) {
    resultado <- avaliar_padrao(code, envir)
    texto <- if (length(resultado) == 0) character(0) else as.character(resultado)
    vazio <- length(texto) == 0 ||
      all(is.na(texto)) ||
      all(!nzchar(trimws(texto))) ||
      all(trimws(texto) %in% c("NA", "n/a", "NaN"))
    if (vazio) {
      stop(
        "Valor inline vazio, NA ou n/a em `r ", code, "`. A grandeza citada ",
        "no texto não existe mais ou não foi medida; corrija o trecho em vez ",
        "de publicá-lo assim.",
        call. = FALSE
      )
    }
    resultado
  })
})

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
  # Coluna numérica crua sai do `kable` no padrão inglês, com ponto decimal,
  # contra a regra de pt-BR em todo texto final. A maior parte das tabelas já
  # chega formatada como texto pelos `formatar_*`; estas opções cobrem as que
  # ainda trazem número, sem depender de cada bloco lembrar de formatar.
  knitr::kable(
    tabela, align = alinhamento, caption = legenda,
    format.args = list(decimal.mark = ",", big.mark = ".")
  )
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
