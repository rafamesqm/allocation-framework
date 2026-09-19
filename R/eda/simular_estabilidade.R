#' Mede, numa realização do gerador, as grandezas que sustentam os achados
#'
#' Uma realização é um sorteio, não uma propriedade do modelo. Esta função
#' existe para que a Parte I possa dizer quanto de cada número reportado é
#' estrutura e quanto é a semente que calhou de ser usada.
#'
#' **A realização versionada não é a réplica de semente 1.** O `targets`
#' atribui a cada alvo uma semente própria e determinística, derivada do nome
#' do alvo, enquanto aqui os geradores correm em sequência num mesmo fluxo
#' aleatório. As duas coisas são reprodutíveis, mas não coincidem. A leitura
#' correta é que o conjunto versionado é um sorteio da mesma distribuição que
#' estas réplicas caracterizam, e não um elemento desta amostra.
#'
#' `avaliacoes_desempenho` fica de fora por não alimentar nenhum achado da
#' Parte I e responder sozinha por quase todo o tempo de geração.
#'
#' @param semente Inteiro que identifica a realização.
#' @param n_consultores,n_projetos Escala da população.
#' @param data_referencia Data de referência da geração.
#' @return tibble de uma linha, uma coluna por grandeza medida.
medir_realizacao <- function(semente,
                              n_consultores = 285,
                              n_projetos = 952,
                              data_referencia = as.Date("2026-09-01")) {
  consultores <- gerar_consultores(n_consultores = n_consultores, semente = semente)
  projetos <- gerar_projetos(
    n_projetos = n_projetos, semente = semente, data_referencia = data_referencia
  )
  # `experiencia` é o atributo latente e alimenta a alocação, que responde à
  # experiência real da pessoa. `experiencia_observada` é o que o formulário
  # capturou, e é sobre ela que a cobertura de conteúdo é medida. Misturar as
  # duas aqui reintroduziria o artefato que a Parte II encontrou.
  respondentes <- sortear_respondentes_formulario(consultores)
  experiencia <- gerar_experiencia_setorial(consultores)
  experiencia_observada <- restringir_a_respondentes(experiencia, respondentes)
  alocacoes <- gerar_alocacoes(consultores, projetos, experiencia, semente = semente)
  custo <- gerar_custo_carregado(consultores, semente = semente)
  skills <- gerar_skills_consultor(
    consultores, gerar_espaco_skills(), respondentes = respondentes
  )
  grafo <- gerar_grafo_coalocacao(alocacoes)
  ocupacao <- agregar_ocupacao_consultor(alocacoes)

  nivel_num <- senioridade_numerica(consultores$nivel_senioridade)
  nivel_alocacao <- nivel_num[match(alocacoes$consultor_id, consultores$consultor_id)]

  atributos <- tibble::tibble(consultor_id = consultores$consultor_id, nivel_num = nivel_num)
  g <- construir_grafo_coalocacao(grafo, consultores, atributos)

  setor_predominante <- experiencia |>
    dplyr::arrange(dplyr::desc(intensidade)) |>
    dplyr::distinct(consultor_id, .keep_all = TRUE)
  grupo_consultor <- setor_predominante$segmento[
    match(alocacoes$consultor_id, setor_predominante$consultor_id)
  ]
  setor_projeto <- projetos$setor[match(alocacoes$projeto_id, projetos$projeto_id)]

  cobertura_skills <- dplyr::n_distinct(skills$consultor_id) / nrow(consultores)

  grau_consultor <- dplyr::count(alocacoes, consultor_id)$n
  grau_projeto <- dplyr::count(alocacoes, projeto_id)$n
  linha_base <- medir_linha_base_popularidade(alocacoes, consultores, tamanhos = 20)
  identificabilidade_k5 <- medir_identificabilidade(alocacoes, consultores, projetos, fatores = 5)
  sem_interacao <- !consultores$consultor_id %in% alocacoes$consultor_id
  sem_skills <- !consultores$consultor_id %in% skills$consultor_id
  sem_setor_observado <- !consultores$consultor_id %in% experiencia_observada$consultor_id

  tibble::tibble(
    semente = semente,
    densidade_R = nrow(alocacoes) / (nrow(consultores) * nrow(projetos)),
    cold_start_consultor = 1 - dplyr::n_distinct(alocacoes$consultor_id) / nrow(consultores),
    cold_start_projeto = 1 - dplyr::n_distinct(alocacoes$projeto_id) / nrow(projetos),
    cobertura_skills = cobertura_skills,
    fracao_sem_skills = 1 - cobertura_skills,
    cor_nivel_custo = stats::cor(nivel_num, custo$custo_carregado),
    cor_nivel_tempo_casa = stats::cor(nivel_num, consultores$tempo_casa),
    cor_nivel_pct_alocacao = stats::cor(nivel_alocacao, alocacoes$pct_alocacao),
    cor_nivel_horas_mes = stats::cor(nivel_alocacao, alocacoes$horas_mes),
    v_contrato_nivel = calcular_v_cramer(
      consultores$nivel_senioridade, consultores$tipo_contrato
    ),
    n_localizacoes = dplyr::n_distinct(consultores$localizacao),
    agrupamento_grafo = igraph::transitivity(g, type = "global"),
    isolados_grafo = sum(igraph::degree(g) == 0),
    acima_capacidade = mean(ocupacao$pct_ocupacao > 1),
    grau_medio_consultor = nrow(alocacoes) / dplyr::n_distinct(alocacoes$consultor_id),
    taxa_mesmo_setor = mean(grupo_consultor == setor_projeto, na.rm = TRUE),
    gini_grau_consultor = calcular_lorenz(grau_consultor)$gini,
    gini_grau_projeto = calcular_lorenz(grau_projeto)$gini,
    dispersao_grau_consultor = stats::var(grau_consultor) / mean(grau_consultor),
    dispersao_grau_projeto = stats::var(grau_projeto) / mean(grau_projeto),
    recall_popularidade_20 = linha_base$recall,
    projetos_subdeterminados_k5 = identificabilidade_k5$fracao_projetos_subdeterminados,
    consultores_subdeterminados_k5 = identificabilidade_k5$fracao_consultores_subdeterminados,
    zona_morta_skills = mean(sem_interacao & sem_skills),
    cobertura_setor_observada = mean(!sem_setor_observado),
    zona_morta_setor = mean(sem_interacao & sem_setor_observado),
    massa_pct_alocacao_em_1 = mean(alocacoes$pct_alocacao == 1),
    massa_horas_mes_em_0 = mean(alocacoes$horas_mes == 0)
  )
}

#' Repete a geração sobre várias sementes
#'
#' @param sementes Vetor de sementes.
#' @param ... Repassado a `medir_realizacao()`.
#' @return tibble com uma linha por realização.
simular_estabilidade <- function(sementes = 1:40, ...) {
  purrr::map(sementes, medir_realizacao, ...) |>
    purrr::list_rbind()
}

#' Resume a variabilidade de Monte Carlo de cada grandeza
#'
#' **O que este intervalo é, e o que não é.** Ele mede quanto cada número se
#' moveria se a população sintética fosse sorteada de novo. Não é incerteza
#' sobre a Peers: o dado real é o que é, e a incerteza sobre ele vem das
#' ressalvas de cobertura de join registradas em `mapeamento_schema.md`, não
#' daqui. Confundir os dois transformaria variabilidade de código em margem
#' de erro sobre a empresa, que seria exatamente o tipo de afirmação que este
#' capítulo existe para evitar.
#'
#' `amplitude_relativa` é a largura do intervalo dividida pela mediana, e
#' serve para ordenar as grandezas por fragilidade: é ela que diz em quais
#' achados a semente decide o número.
#'
#' @param replicas tibble de `simular_estabilidade()`.
#' @param nivel Massa de probabilidade coberta pelo intervalo.
#' @return tibble com `medida`, `mediana`, `desvio_mc`, `inferior`,
#'   `superior`, `amplitude_relativa` e `n_replicas`.
resumir_estabilidade <- function(replicas, nivel = 0.90) {
  margem <- (1 - nivel) / 2
  n_replicas <- nrow(replicas)

  replicas |>
    dplyr::select(-semente) |>
    tidyr::pivot_longer(
      dplyr::everything(), names_to = "medida", values_to = "valor"
    ) |>
    dplyr::summarise(
      mediana = stats::median(valor),
      desvio_mc = stats::sd(valor),
      inferior = unname(stats::quantile(valor, margem)),
      superior = unname(stats::quantile(valor, 1 - margem)),
      .by = medida
    ) |>
    dplyr::mutate(
      amplitude_relativa = dplyr::if_else(
        mediana == 0, NA_real_, (superior - inferior) / abs(mediana)
      ),
      n_replicas = n_replicas
    ) |>
    dplyr::arrange(dplyr::desc(amplitude_relativa))
}

#' Extrai o intervalo de uma grandeza, formatado para o texto
#'
#' Evita que cada bloco reescreva a formatação do intervalo e divirja dos
#' demais.
#'
#' @param estabilidade tibble de `resumir_estabilidade()`.
#' @param nome_medida Nome da grandeza.
#' @param formatador Função de formatação aplicada aos três números.
#' @return Texto no formato `"mediana (inferior a superior)"`, ou `NA` se a
#'   grandeza não foi medida.
formatar_intervalo <- function(estabilidade, nome_medida, formatador = formatar_pct) {
  linha <- estabilidade[estabilidade$medida == nome_medida, ]
  if (nrow(linha) == 0) return(NA_character_)

  paste0(
    formatador(linha$mediana), " (", formatador(linha$inferior),
    " a ", formatador(linha$superior), ")"
  )
}
