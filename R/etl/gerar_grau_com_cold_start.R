#' Sorteia o grau de cada nó combinando cold start estrutural e Binomial
#' Negativa truncada em zero
#'
#' As médias/variâncias reais de grau (ver `gerar_grau_binomial_negativa()`)
#' foram calculadas por `GROUP BY` sobre quem já tem ao menos uma alocação
#' (docs/mapeamento_schema.md, "Distribuição de grau em R": a consulta agrupa
#' por colaborador/código de alocação, o que por construção exclui quem
#' nunca aparece). São, portanto, estatísticas condicionais a grau > 0, não
#' da população inteira.
#'
#' Sortear direto dessa Binomial Negativa sem truncar reintroduziria uma
#' massa de zero que não pertence a uma distribuição condicional: para a
#' calibração do lado do projeto (média 2,51, variância 8,65), essa massa
#' natural chega a ~28%, bem mais que os ~17% de cold start medidos do lado
#' do consultor. Isso infla o cold start além do parâmetro pedido e afasta a
#' variância realizada da variância calibrada, o oposto do pedido explícito
#' do orientador de reproduzir a dispersão real com fidelidade.
#'
#' Por isso a função separa as duas fontes de zero: (1) cold start
#' estrutural, fração `fracao_cold_start` dos nós com grau forçado a zero, e
#' (2) para o resto, a Binomial Negativa truncada em zero por rejeição
#' (redesenha qualquer zero sorteado até sair um valor positivo), respeitando
#' a condicionalidade da calibração real.
#'
#' **Limitação conhecida, documentada, não corrigida:** truncar em zero
#' desloca a média (e, em menor grau, a variância) condicional para cima,
#' porque remove só o menor valor possível da distribuição. Para o grau do
#' lado do projeto (média 2,51, variância 8,65, ~28% de massa natural em
#' zero), a média entre os nós não-cold-start sai por volta de 3,50 em vez
#' de 2,51 (~+39%), a variância sai por volta de 8,57 (~-1%, quase
#' inalterada). Do lado do consultor (média 9,00, variância 44,6, ~3% de
#' massa natural em zero) o efeito é pequeno, a média sai por volta de 9,24
#' (~+3%). Testei corrigir isso ajustando a média pré-truncagem
#' numericamente (`stats::uniroot`): acerta a média mas erra a variância
#' (cai para ~3,7). Testei também ajustar `size` e média pré-truncagem juntos
#' via `stats::optim` para acertar as duas estatísticas truncadas ao mesmo
#' tempo: numericamente instável para esses parâmetros (razão
#' variância/média muito alta), o otimizador converge para uma região
#' degenerada (`size`/`mu` colapsando a ~1e-9), não um ajuste confiável.
#' Decisão (2026-09-01): manter a truncagem simples, sem correção, porque é
#' o único caminho em que `fracao_cold_start` tem o significado direto que
#' se pede (fração exata de grau zero), documentando o desvio de média em
#' vez de esconder ou perseguir uma correção numericamente frágil.
#'
#' @param n Quantos nós.
#' @param media,variancia Média e variância reais do grau, condicionais a
#'   grau > 0 (ver docs/mapeamento_schema.md).
#' @param fracao_cold_start Fração de nós com grau estrutural zero.
#' @return Vetor inteiro de graus, um por nó.
gerar_grau_com_cold_start <- function(n, media, variancia, fracao_cold_start) {
  indices_cold_start <- sample.int(n, size = round(n * fracao_cold_start))
  indices_regulares <- setdiff(seq_len(n), indices_cold_start)

  grau_regular <- gerar_grau_binomial_negativa(length(indices_regulares), media, variancia)
  zeros <- which(grau_regular == 0)
  while (length(zeros) > 0) {
    grau_regular[zeros] <- gerar_grau_binomial_negativa(length(zeros), media, variancia)
    zeros <- which(grau_regular == 0)
  }

  grau <- integer(n)
  grau[indices_regulares] <- grau_regular
  grau
}
