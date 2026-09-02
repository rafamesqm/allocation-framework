#' Gera o estágio atual de oportunidades no funil de vendas (Camada 3)
#'
#' As contagens reais por situação (ver docs/mapeamento_schema.md, "Quinta
#' rodada") não formam um funil de coorte: não são decrescentes na ordem do
#' funil (Ideia 92, Reunião Inicial 123, Lead 127, Proposta 209, Projeto
#' 952, Recusada/Cancelada 2799), o que é esperado sob fila (a população
#' num estágio reflete taxa de entrada vezes tempo médio de permanência,
#' não quantas oportunidades já passaram por ali). Sem timestamp de mudança
#' de estágio por oportunidade, não dá para calibrar probabilidade de
#' avanço/perda estágio a estágio de forma estatisticamente válida.
#'
#' Por isso este gerador simplifica para um sorteio multinomial do estágio
#' atual por oportunidade, pesado pelo censo real, em vez de simular uma
#' sequência de avanço/perda: é uma simplificação documentada, não uma
#' leitura direta da dinâmica real do funil.
#'
#' @param n_oportunidades Número de oportunidades a gerar.
#' @param semente Inteiro para reprodutibilidade.
#' @return tibble com `oportunidade_id`, `estagio`.
gerar_pipeline_demandas <- function(n_oportunidades, semente = 1) {
  set.seed(semente)

  estagios <- c("Ideia", "Reunião Inicial", "Lead", "Proposta", "Projeto", "Recusada/Cancelada")
  pesos_estagio <- c(92, 123, 127, 209, 952, 2799)

  tibble::tibble(
    oportunidade_id = sprintf("oportunidade_%05d", seq_len(n_oportunidades)),
    estagio = sample(estagios, n_oportunidades, replace = TRUE, prob = pesos_estagio)
  )
}
