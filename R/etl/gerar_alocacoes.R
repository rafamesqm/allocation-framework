#' Gera a matriz de alocações sintéticas (células de `R`)
#'
#' Sorteia o grau de cada consultor e de cada projeto combinando cold start
#' estrutural e Binomial Negativa truncada em zero (sobredispersão real
#' confirmada, ver `gerar_grau_com_cold_start()` para o porquê da truncagem),
#' cabla os pares por configuration model bipartido com viés de seleção
#' assortativo por setor (ver `gerar_pares_configuration_model()`), e por fim
#' sorteia `pct_alocacao` e `horas_mes` por aresta, condicionados ao
#' `nivel_senioridade` do consultor (correlações reais +0,52 e -0,55, ver
#' docs/mapeamento_schema.md).
#'
#' `papel` não é gerado como categoria própria: a fonte real mostra que esse
#' campo é o `nivel_senioridade`/cargo da pessoa na alocação, não uma
#' taxonomia separada (ver docs/mapeamento_schema.md, Camada 2), por isso é
#' herdado do consultor em vez de fabricado sem lastro.
#'
#' O grupo assortativo do consultor é o setor de maior `intensidade` em
#' `experiencia_setorial` (empate resolvido por ordem arbitrária). Consultor
#' sem nenhuma linha em `experiencia_setorial` cai no grupo `"sem_setor"`,
#' que nunca bate com um setor real de projeto, ficando sujeito só à
#' cablagem aleatória residual.
#'
#' @param consultores tibble de `gerar_consultores()`.
#' @param projetos tibble de `gerar_projetos()`.
#' @param experiencia_setorial tibble de `gerar_experiencia_setorial()`.
#' @param semente Inteiro para reprodutibilidade.
#' @param fracao_cold_start_consultor,fracao_cold_start_projeto Fração de
#'   cada lado com grau forçado a zero.
#' @param prob_vies Probabilidade de a cablagem preferir o mesmo setor (ver
#'   `gerar_pares_configuration_model()`).
#' @return tibble de arestas: `consultor_id`, `projeto_id`, `papel`,
#'   `pct_alocacao`, `horas_mes`.
gerar_alocacoes <- function(consultores, projetos, experiencia_setorial,
                             semente = 1,
                             fracao_cold_start_consultor = 0.17,
                             fracao_cold_start_projeto = 0.17,
                             prob_vies = 0.7) {
  set.seed(semente)

  n_consultores <- nrow(consultores)
  n_projetos <- nrow(projetos)

  grau_consultor <- gerar_grau_com_cold_start(
    n_consultores, media = 9.00, variancia = 44.6,
    fracao_cold_start = fracao_cold_start_consultor
  )
  grau_projeto <- gerar_grau_com_cold_start(
    n_projetos, media = 2.51, variancia = 8.65,
    fracao_cold_start = fracao_cold_start_projeto
  )

  setor_predominante <- experiencia_setorial |>
    dplyr::arrange(dplyr::desc(intensidade)) |>
    dplyr::distinct(consultor_id, .keep_all = TRUE)

  grupo_consultor <- setor_predominante$segmento[match(consultores$consultor_id, setor_predominante$consultor_id)]
  grupo_consultor[is.na(grupo_consultor)] <- "sem_setor"

  arestas <- gerar_pares_configuration_model(
    id_consultor = consultores$consultor_id, grau_consultor = grau_consultor,
    id_projeto = projetos$projeto_id, grau_projeto = grau_projeto,
    grupo_consultor = grupo_consultor, grupo_projeto = projetos$setor,
    prob_vies = prob_vies
  )

  nivel_do_consultor <- consultores$nivel_senioridade[match(arestas$consultor_id, consultores$consultor_id)]

  arestas |>
    dplyr::mutate(
      papel = as.character(nivel_do_consultor),
      pct_alocacao = gerar_variavel_por_nivel(
        nivel_do_consultor,
        correlacao_alvo = 0.52, media = 0.7, desvio_variavel = 0.2,
        minimo = 0.05, maximo = 1
      ),
      horas_mes = gerar_variavel_por_nivel(
        nivel_do_consultor,
        correlacao_alvo = -0.55, media = 120, desvio_variavel = 66.90,
        minimo = 0
      )
    )
}
