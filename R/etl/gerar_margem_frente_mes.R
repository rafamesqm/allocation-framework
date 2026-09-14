#' Gera receita, custo e margem por frente-mês (candidato (c) da construção
#' do rótulo, Camada 3)
#'
#' Candidato (c) de docs/dicionario_de_dados.md, "Construção do rótulo":
#' trata margem como sinal separado, na granularidade em que de fato existe
#' na Peers (frente/mês), não como parte de `R`. Receita (`peers_total_vlr`)
#' e custo interno (`peers_custointerns_vlr`) reais têm formato hurdle
#' (massa em zero mais cauda assimétrica, ver docs/mapeamento_schema.md,
#' "Quinta rodada"), calibrados por `gerar_variavel_hurdle_lognormal()`.
#'
#' **A margem é ancorada em projeto e mês, não solta.** Uma versão anterior
#' gerava só as três colunas de valor, sem chave nenhuma, o que tornava o
#' candidato (c) impossível de avaliar contra `R`: não dava para saber a que
#' projeto uma margem pertencia. Isso era lacuna do gerador, não do dado
#' real, que tem frente e mês em `vw_bi_custo_frente_mensal`.
#'
#' **Simplificação documentada: frente colapsada em projeto.** A fonte real
#' distingue frente de projeto, mas quantas frentes um projeto tem nunca foi
#' agregado enquanto havia acesso, e o acesso encerrou (ver
#' docs/roadmap.md). Inventar uma distribuição de frentes por projeto seria
#' suposição sem lastro, então cada projeto tem exatamente uma frente, e a
#' linha é projeto-mês. Consequência: o volume de linhas passa a ser
#' determinado pelos projetos e suas durações, não mais fixado nos 5.000
#' registros reais.
#'
#' `receita` e `custo` são sorteados de forma independente entre si: a
#' fonte real provavelmente tem alguma correlação entre os dois (frentes
#' maiores tendem a ter receita e custo maiores juntos), não calibrada nesta
#' rodada. Suposição documentada, não achado.
#'
#' Não modela `actar_receita_vlr`: 95,8% dos registros reais são zero/nulo,
#' não é uma variável primária útil para compor margem nesta fase.
#'
#' @param projetos tibble de `gerar_projetos()`, com `data_inicio` e
#'   `data_fim`.
#' @param semente Inteiro para reprodutibilidade.
#' @return tibble com `projeto_id`, `ano_mes`, `receita`, `custo` e `margem`
#'   (receita menos custo), uma linha por projeto e mês ativo.
gerar_margem_frente_mes <- function(projetos, semente = 1) {
  set.seed(semente)

  meses <- expandir_para_meses(projetos$data_inicio, projetos$data_fim)
  n_frentes_mes <- nrow(meses)

  receita <- gerar_variavel_hurdle_lognormal(
    n_frentes_mes,
    prob_zero = 0.159, mediana_positivos = 136901.46,
    p25_positivos = 46105.5, p75_positivos = 268683.84
  )
  custo <- gerar_variavel_hurdle_lognormal(
    n_frentes_mes,
    prob_zero = 0.447, mediana_positivos = 21000,
    p25_positivos = 16000, p75_positivos = 26250
  )

  tibble::tibble(
    projeto_id = projetos$projeto_id[meses$linha],
    ano_mes = meses$ano_mes,
    receita = receita,
    custo = custo,
    margem = receita - custo
  )
}
