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
#' Não modela `actar_receita_vlr`: 95,8% dos registros reais são zero/nulo,
#' não é uma variável primária útil para compor margem nesta fase.
#'
#' `receita` e `custo` são sorteados de forma independente entre si: a
#' fonte real provavelmente tem alguma correlação entre os dois (frentes
#' maiores tendem a ter receita e custo maiores juntos), não calibrada nesta
#' rodada. Suposição documentada, não achado.
#'
#' @param n_frentes_mes Número de observações de frente-mês a gerar.
#' @param semente Inteiro para reprodutibilidade.
#' @return tibble com `receita`, `custo`, `margem` (receita menos custo).
gerar_margem_frente_mes <- function(n_frentes_mes, semente = 1) {
  set.seed(semente)

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

  tibble::tibble(receita = receita, custo = custo, margem = receita - custo)
}
