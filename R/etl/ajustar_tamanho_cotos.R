#' Ajusta o total de cotos de um lado do configuration model para um
#' tamanho-alvo, adicionando ou removendo cotos
#'
#' Usado para dividir a correção do resíduo entre os dois lados do
#' configuration model (ver `gerar_pares_configuration_model()`), em vez de
#' jogar a diferença inteira num lado só: cada lado é ajustado até o mesmo
#' `tamanho_alvo`, tipicamente a média dos dois totais originais. Achado
#' que motivou essa divisão: jogar toda a diferença num lado só, na escala
#' real do projeto (285 consultores × 952 projetos), quase dobrava a
#' variância de grau do lado menor (ver `validar_utilidade_sintetico()`).
#'
#' Adicionar cotos extras é ponderado pelo grau já sorteado de cada nó
#' (preserva o formato da distribuição, nunca dá aresta extra a quem tem
#' grau zero, o que quebraria o cold start). Remover cotos é uniforme entre
#' as instâncias existentes, o que reduz mais o grau de quem já tinha mais
#' instâncias, proporcionalmente ao grau de cada um.
#'
#' @param cotos,grupo_dos_cotos Vetores paralelos de cotos já expandidos
#'   (`rep(id, grau)`) e o grupo assortativo de cada um.
#' @param ids,grau,grupo Vetores paralelos por nó (não expandidos), usados
#'   só para sortear cotos extras quando é preciso adicionar.
#' @param tamanho_alvo Tamanho final desejado para `cotos`.
#' @return Lista com `cotos` e `grupo_dos_cotos` ajustados para
#'   `tamanho_alvo`.
ajustar_tamanho_cotos <- function(cotos, grupo_dos_cotos, ids, grau, grupo, tamanho_alvo) {
  n_atual <- length(cotos)

  if (n_atual < tamanho_alvo) {
    extra <- sample(seq_along(ids), size = tamanho_alvo - n_atual, replace = TRUE, prob = grau)
    cotos <- c(cotos, ids[extra])
    grupo_dos_cotos <- c(grupo_dos_cotos, grupo[extra])
  } else if (n_atual > tamanho_alvo) {
    manter <- sample.int(n_atual, size = tamanho_alvo)
    cotos <- cotos[manter]
    grupo_dos_cotos <- grupo_dos_cotos[manter]
  }

  list(cotos = cotos, grupo_dos_cotos = grupo_dos_cotos)
}
