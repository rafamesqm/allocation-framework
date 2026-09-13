#' Gera consultores sintéticos (objeto `C`, Camada 1)
#'
#' Pirâmide de senioridade, tempo de casa, localização, disposição a viagem e
#' tipo de contrato calibrados por dado real agregado (ver
#' docs/mapeamento_schema.md, "Calibração agregada" e "Sexta rodada").
#' `tipo_contrato` agrupa CLT + estagiário como "CLT" e diretor não empregado
#' + autônomo como "PJ", proporção real do campo "Modelo" (68,4%/31,6%); o
#' de-para exato entre `tipo_contrato` e `nivel_senioridade` (estagiário
#' coincide com N4) fica para uma revisão futura, não aplicado aqui. `fuso` é
#' derivado de `localizacao`, não amostrado: todos os estados observados na
#' base real estão no fuso America/Sao_Paulo.
#'
#' `skills_hard[]`/`recencia_skill[]` não são gerados aqui: ver
#' `gerar_skills_consultor()`, que produz a tabela longa (consultor × skill).
#'
#' @param n_consultores Número de consultores a gerar. Default 285 (headcount
#'   ativo real).
#' @param semente Inteiro para reprodutibilidade.
#' @return tibble com um consultor por linha: `consultor_id`,
#'   `nivel_senioridade`, `tempo_casa`, `tempo_nivel`, `tipo_contrato`,
#'   `localizacao`, `fuso`, `disposicao_viagem_nacional`,
#'   `disposicao_viagem_internacional`.
gerar_consultores <- function(n_consultores = 285, semente = 1) {
  set.seed(semente)

  niveis_ordenados <- c("N4", "N3", "N2", "N1", "N0")
  pesos_nivel <- c(N4 = 42, N3 = 107, N2 = 71, N1 = 55, N0 = 8)

  estados <- c(
    "SAO PAULO", "MINAS GERAIS", "RIO DE JANEIRO", "RIO GRANDE DO SUL",
    "PERNAMBUCO", "DISTRITO FEDERAL", "SANTA CATARINA", "BAHIA", "PIAUI",
    "PARANA"
  )
  pesos_estado <- c(182, 70, 13, 6, 3, 3, 2, 2, 2, 1)

  prob_viagem_nacional <- 175 / 285
  prob_viagem_internacional <- 185 / 285
  prob_pj <- 0.316 # calibrado por "Modelo" real, Sexta rodada (ver acima)

  nivel_senioridade <- factor(
    sample(niveis_ordenados, size = n_consultores, replace = TRUE, prob = pesos_nivel),
    levels = niveis_ordenados,
    ordered = TRUE
  )

  tempo_casa <- gerar_tempo_casa(nivel_senioridade)
  proporcao_no_nivel_atual <- stats::runif(n_consultores, min = 0.3, max = 1)

  tibble::tibble(
    consultor_id = sprintf("consultor_%04d", seq_len(n_consultores)),
    nivel_senioridade = nivel_senioridade,
    tempo_casa = tempo_casa,
    tempo_nivel = tempo_casa * proporcao_no_nivel_atual,
    tipo_contrato = sample(c("CLT", "PJ"), n_consultores, replace = TRUE, prob = c(1 - prob_pj, prob_pj)),
    localizacao = sample(estados, n_consultores, replace = TRUE, prob = pesos_estado),
    fuso = "America/Sao_Paulo",
    disposicao_viagem_nacional = stats::rbinom(n_consultores, size = 1, prob = prob_viagem_nacional) == 1,
    disposicao_viagem_internacional = stats::rbinom(n_consultores, size = 1, prob = prob_viagem_internacional) == 1
  )
}
