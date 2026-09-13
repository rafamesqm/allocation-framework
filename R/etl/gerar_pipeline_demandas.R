#' Gera o estágio atual de oportunidades no funil de vendas (Camada 3)
#'
#' A Quinta rodada de calibração (docs/mapeamento_schema.md) só tinha a
#' contagem por situação num instante (censo transversal, não decrescente na
#' ordem do funil), insuficiente para calibrar avanço estágio a estágio. A
#' Sexta rodada achou as datas de mudança de estágio por oportunidade (`Data
#' Lead`/`Reunião`/`Proposta`/`Projeto`), o que habilita um funil de coorte
#' sequencial real: cada oportunidade tenta avançar Lead → Reunião → Proposta
#' → Projeto, uma porta de cada vez, com a taxa de avanço condicional real
#' (81,3% / 79,7% / 36,3%). `estagio` é o estágio mais avançado que a
#' oportunidade alcançou antes de parar de avançar.
#'
#' Simplificação registrada: o `estagio` aqui é só o marco mais avançado
#' alcançado, não um status de resolução (a fonte real também tem um
#' `Data Rec/Can` separado, indicando perda, e uma oportunidade pode estar
#' presa num estágio por ainda estar em andamento no momento do censo, não
#' necessariamente perdida). Sem essa distinção, quem não chega a "Projeto"
#' fica classificado pelo estágio onde parou, sem afirmar se foi perdida ou
#' segue aberta. Os tempos medianos entre estágios (2/3/16 dias) também
#' foram levantados nesta rodada, mas não entram no esquema desta função
#' (sem campo de data por estágio no retorno), fica para trabalho futuro se
#' a dimensão temporal do pipeline vier a ser necessária.
#'
#' @param n_oportunidades Número de oportunidades a gerar.
#' @param semente Inteiro para reprodutibilidade.
#' @param taxa_lead_reuniao,taxa_reuniao_proposta,taxa_proposta_projeto Taxas
#'   reais de avanço condicional entre estágios consecutivos do funil.
#' @return tibble com `oportunidade_id`, `estagio`.
gerar_pipeline_demandas <- function(n_oportunidades,
                                     semente = 1,
                                     taxa_lead_reuniao = 0.813,
                                     taxa_reuniao_proposta = 0.797,
                                     taxa_proposta_projeto = 0.363) {
  set.seed(semente)

  alcancou_reuniao <- stats::rbinom(n_oportunidades, size = 1, prob = taxa_lead_reuniao) == 1
  alcancou_proposta <- alcancou_reuniao &
    (stats::rbinom(n_oportunidades, size = 1, prob = taxa_reuniao_proposta) == 1)
  alcancou_projeto <- alcancou_proposta &
    (stats::rbinom(n_oportunidades, size = 1, prob = taxa_proposta_projeto) == 1)

  estagio <- dplyr::case_when(
    alcancou_projeto ~ "Projeto",
    alcancou_proposta ~ "Proposta",
    alcancou_reuniao ~ "Reunião",
    TRUE ~ "Lead"
  )

  tibble::tibble(
    oportunidade_id = sprintf("oportunidade_%05d", seq_len(n_oportunidades)),
    estagio = estagio
  )
}
