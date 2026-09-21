#' Gera a receita mensal de cada projeto, decomposta por origem
#'
#' Substitui `gerar_margem_frente_mes()`, e a troca de nome é a correção
#' principal: o nome antigo errava nas **duas** palavras.
#'
#' Não é margem. A segunda consulta às bases comerciais
#' (docs/mapeamento_schema.md, 2026-09-20) provou que
#' `vw_bi_custo_frente_mensal` é a **receita** do projeto distribuída por mês
#' e por origem, e não custo: a soma de `peers_total`, `subcontratados_total`
#' e `actar_total` sobre a receita total do projeto dá 1,000 do décimo ao
#' nonagésimo percentil em 724 versões. Uma razão cravada em 1,000 por toda a
#' faixa é identidade contábil. Não há custo nem margem nessa fonte, e a
#' despesa vem de `pps`.
#'
#' E não é frente. O grão é projeto por mês; o nome "frente" é herdado do
#' SharePoint e nenhuma coluna da fonte nomeia frente. A simplificação que
#' colapsava frente em projeto, antes registrada como dívida, está correta.
#'
#' **O que a versão anterior calculava.** Ela tratava `peers_total_vlr` como
#' receita, `peers_custointerns_vlr` como custo interno, e a diferença como
#' margem. Os dois campos são receita, e a diferença é a receita de nota
#' fiscal. A "margem" que ela emitia não era margem de nada, e o bloco II.5
#' concluiu sobre ela.
#'
#' **Identidade preservada.** `receita_total = receita_nf + receita_interns`
#' vale em 100% das linhas na fonte e vale exatamente aqui, porque a receita
#' de nota fiscal é calculada por diferença.
#'
#' **Ressalva de truncamento.** Os parâmetros de forma mensal e de composição
#' por origem vêm de uma extração limitada a 5.000 linhas, com corte
#' cronológico: não há projeto de 2026 e a offering que mais cresce está
#' subrepresentada. O nível de projeto não sofre disso, porque vem da tabela
#' completa.
#'
#' @param projetos tibble de `gerar_projetos()`, com `data_inicio` e
#'   `data_fim`.
#' @param prob_subcontratados,prob_actar Probabilidade de o mês ter receita
#'   de cada origem.
#' @param participacao_interns Participação mediana da receita de estagiário
#'   nos meses em que ela existe.
#' @param prob_projeto_com_interns Fração de projetos que têm receita de
#'   estagiário em algum mês.
#' @param semente Inteiro para reprodutibilidade.
#' @return tibble com `projeto_id`, `ano_mes`, `receita_total`,
#'   `receita_nf`, `receita_interns`, `receita_subcontratados` e
#'   `receita_actar`, uma linha por projeto e mês ativo.
gerar_receita_projeto_mes <- function(projetos,
                                       prob_subcontratados = 0.065,
                                       prob_actar = 0.05,
                                       participacao_interns = 0.104,
                                       prob_projeto_com_interns = 0.71,
                                       semente = 1) {
  set.seed(semente)

  meses <- expandir_para_meses(projetos$data_inicio, projetos$data_fim)
  n_linhas <- nrow(meses)
  projeto_da_linha <- projetos$projeto_id[meses$linha]

  # Nível mensal calibrado sobre os meses com receita positiva da fonte:
  # mediana 172 mil, quartis 90 mil e 304 mil.
  receita_total <- gerar_variavel_hurdle_lognormal(
    n_linhas,
    prob_zero = 0.159, mediana_positivos = 172000,
    p25_positivos = 90000, p75_positivos = 304000
  )

  # Subcontratados e Actar são componentes de receita, não repasse de custo:
  # os campos de faturamento de cada um igualam o total em 100% das linhas
  # da fonte, o que só faz sentido se forem receita.
  receita_subcontratados <- sortear_componente_de_receita(
    n_linhas, prob_subcontratados,
    mediana = 42000, p10 = 9800, p90 = 204000
  )
  receita_actar <- sortear_componente_de_receita(
    n_linhas, prob_actar,
    mediana = 8000, p10 = 2000, p90 = 46000
  )

  # Os dois componentes não podem engolir a receita do mês. O teto é
  # suposição declarada: a fonte não documenta limite.
  teto <- 0.8 * receita_total
  excesso <- pmax(receita_subcontratados + receita_actar - teto, 0)
  proporcao <- ifelse(
    receita_subcontratados + receita_actar > 0,
    excesso / (receita_subcontratados + receita_actar), 0
  )
  receita_subcontratados <- receita_subcontratados * (1 - proporcao)
  receita_actar <- receita_actar * (1 - proporcao)

  receita_peers <- pmax(receita_total - receita_subcontratados - receita_actar, 0)

  # A presença de receita de estagiário é propriedade do projeto, não do mês:
  # na fonte, 29% dos projetos nunca têm e 47% têm sempre. Sortear por mês
  # produziria uma alternância que a fonte não mostra.
  projetos_com_interns <- sample(
    projetos$projeto_id,
    size = round(nrow(projetos) * prob_projeto_com_interns)
  )
  tem_interns <- projeto_da_linha %in% projetos_com_interns

  participacao <- stats::rlnorm(
    n_linhas, meanlog = log(participacao_interns), sdlog = 0.683
  )
  receita_interns <- ifelse(tem_interns, receita_peers * pmin(participacao, 0.9), 0)

  tibble::tibble(
    projeto_id = projeto_da_linha,
    ano_mes = meses$ano_mes,
    receita_total = receita_peers,
    receita_nf = receita_peers - receita_interns,
    receita_interns = receita_interns,
    receita_subcontratados = receita_subcontratados,
    receita_actar = receita_actar
  )
}

#' Sorteia um componente de receita com presença esporádica
#'
#' Subcontratados e Actar aparecem em poucos meses e, quando aparecem, têm
#' distribuição muito assimétrica. Separado em função própria porque os dois
#' seguem a mesma forma e só mudam os parâmetros, e porque o padrão de
#' Bernoulli seguido de log-normal calibrada por quantis aparece em mais de
#' um lugar do gerador.
#'
#' @param n Número de linhas.
#' @param probabilidade Probabilidade de o componente existir na linha.
#' @param mediana,p10,p90 Quantis do componente quando ele existe.
#' @return Vetor numérico, com zero onde o componente não existe.
sortear_componente_de_receita <- function(n, probabilidade, mediana, p10, p90) {
  presente <- stats::runif(n) < probabilidade
  desvio <- (log(p90) - log(p10)) / (2 * stats::qnorm(0.9))

  valor <- stats::rlnorm(n, meanlog = log(mediana), sdlog = desvio)
  ifelse(presente, valor, 0)
}
