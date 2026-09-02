#' Gera comentários de texto livre com tamanho variável, sem pretensão de
#' plausibilidade semântica
#'
#' O comprimento real observado varia de poucas palavras a parágrafos longos
#' (100-260 palavras, ver docs/mapeamento_schema.md). Esta função só
#' reproduz essa variação de tamanho, sorteando um número de palavras por
#' comentário e montando o texto a partir de um banco fixo de termos
#' genéricos de consultoria: não modela conteúdo, só extensão, o suficiente
#' para não quebrar nenhuma análise de comprimento de texto na EDA.
#'
#' @param n Quantos comentários gerar.
#' @param min_palavras,max_palavras Faixa de tamanho, em número de palavras.
#' @return Vetor de texto, um comentário por posição.
gerar_comentario_avaliacao <- function(n, min_palavras = 5, max_palavras = 260) {
  banco_de_termos <- c(
    "entrega", "cliente", "projeto", "resultado", "equipe", "prazo",
    "qualidade", "comunicação", "proatividade", "desafio", "solução",
    "processo", "meta", "colaboração", "liderança", "aprendizado",
    "consistente", "dedicado", "impacto", "iniciativa"
  )

  n_palavras <- sample.int(max_palavras - min_palavras + 1, size = n, replace = TRUE) + min_palavras - 1

  vapply(n_palavras, function(tamanho) {
    paste(sample(banco_de_termos, tamanho, replace = TRUE), collapse = " ")
  }, character(1))
}
