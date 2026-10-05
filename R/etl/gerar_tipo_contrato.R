#' Gera o tipo de contrato, condicionado ao nível de senioridade
#'
#' A versão anterior sorteava o contrato de forma independente do nível, o que
#' produzia estagiários PJ e apagava uma dependência que existe no dado real.
#' O bloco I.3 da EDA mediu isso: V de Cramér corrigido em torno de 0,04 entre
#' `nivel_senioridade` e `tipo_contrato`, ou seja, independência.
#'
#' **O que a evidência real determina, e até onde.** A Sexta rodada de
#' calibração registrou que o campo `Modelo` tem 42 ESTUDANTE ESTAGIARIO
#' entre os 285 ativos, número que coincide exatamente com a contagem de N4
#' na pirâmide. A coincidência exata é forte o bastante para tratar o nível de
#' base como sendo o bloco de estagiários, todos CLT.
#'
#' Acima da base, a evidência **não** determina nada. Os 90 não empregados
#' reais (89 diretores mais 1 autônomo) não cabem em N0 mais N1, que somam
#' apenas 63 pessoas, então não existe de-para limpo entre nível e contrato no
#' topo. Inventar um seria acrescentar suposição sem lastro, justamente o que
#' não se pode fazer com o acesso ao ambiente encerrado.
#'
#' Daí a regra: determinístico onde há âncora exata, marginal calibrada onde
#' não há. A probabilidade aplicada aos demais níveis é escolhida para o total
#' de CLT continuar batendo com a fração real. Com os números reais isso
#' devolve 153 de 243, que é exatamente a contagem real de EMPREGADO GERAL
#' CLT entre os não estagiários: a fórmula reproduz o dado observado em vez de
#' apenas aproximá-lo.
#'
#' @param nivel_senioridade Factor ordenado, do mais júnior ao mais sênior.
#' @param fracao_clt Fração alvo de CLT na população inteira.
#' @param nivel_estagiario Nível tratado como bloco de estagiários, sempre
#'   CLT.
#' @return Vetor de texto com `"CLT"` ou `"PJ"`, um por consultor.
gerar_tipo_contrato <- function(nivel_senioridade, fracao_clt = 0.684,
                                 nivel_estagiario = "N4") {
  n_total <- length(nivel_senioridade)
  eh_estagiario <- as.character(nivel_senioridade) == nivel_estagiario
  n_estagiarios <- sum(eh_estagiario)

  clt_restante <- fracao_clt * n_total - n_estagiarios
  n_restante <- n_total - n_estagiarios

  # Guarda para a população degenerada em que a base sozinha já ultrapassa a
  # fração alvo de CLT: nesse caso ninguém mais fora dela é CLT, em vez de a
  # probabilidade sair negativa.
  prob_clt_restante <- if (n_restante == 0) 0 else max(0, min(1, clt_restante / n_restante))

  tipo <- character(n_total)
  tipo[eh_estagiario] <- "CLT"
  tipo[!eh_estagiario] <- sample(
    c("CLT", "PJ"), size = n_restante, replace = TRUE,
    prob = c(prob_clt_restante, 1 - prob_clt_restante)
  )
  tipo
}
