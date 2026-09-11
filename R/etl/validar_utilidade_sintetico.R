#' Compara estatística agregada do dataset sintético completo contra os
#' números reais documentados (validação de utilidade)
#'
#' Gera o dataset sintético completo na escala real (parâmetros default de
#' cada peça) e compara, lado a lado, contra os números reais já
#' documentados em docs/mapeamento_schema.md: utilidade geral, estatística
#' agregada de várias variáveis (Snoke, Raab, Nowok, Dibben & Slavkovic,
#' 2018, "General and Specific Utility Measures for Synthetic Data"), e
#' utilidade específica, se os achados-alvo da wave 2 (esparsidade, cold
#' start, viés de seleção) aparecem de forma parecida no sintético.
#'
#' Algumas diferenças são esperadas e já documentadas nos módulos que as
#' produzem, não indicam defeito: o grau médio dos dois lados sai acima do
#' real, mais no lado do projeto que no do consultor, por dois mecanismos
#' conhecidos que se somam (ver `gerar_grau_com_cold_start()`, truncagem em
#' zero, e `ajustar_tamanho_cotos()`, ajuste dos dois lados para uma média
#' de compromisso). A taxa de pareamento por mesmo setor não tem um valor
#' real para comparar, só confirma que o viés de seleção plantado supera a
#' taxa esperada ao acaso.
#'
#' @param semente Inteiro para reprodutibilidade de toda a geração.
#' @return tibble com `categoria` (geral/especifica), `medida`, `real`,
#'   `sintetico`, `diferenca_relativa`.
validar_utilidade_sintetico <- function(semente = 1) {
  consultores <- gerar_consultores(semente = semente)
  projetos <- gerar_projetos(semente = semente, data_referencia = as.Date("2026-01-01"))
  experiencia <- gerar_experiencia_setorial(consultores)
  alocacoes <- gerar_alocacoes(consultores, projetos, experiencia, semente = semente)
  custo_carregado <- gerar_custo_carregado(consultores, semente = semente)

  nivel_numerico <- as.numeric(consultores$nivel_senioridade)
  custo_por_consultor <- dplyr::left_join(consultores, custo_carregado, by = "consultor_id")

  grau_consultor <- dplyr::count(alocacoes, consultor_id)$n
  grau_projeto <- dplyr::count(alocacoes, projeto_id)$n

  nivel_numerico_alocacao <- as.numeric(
    consultores$nivel_senioridade[match(alocacoes$consultor_id, consultores$consultor_id)]
  )

  setor_predominante <- experiencia |>
    dplyr::arrange(dplyr::desc(intensidade)) |>
    dplyr::distinct(consultor_id, .keep_all = TRUE)
  grupo_do_consultor <- setor_predominante$segmento[match(alocacoes$consultor_id, setor_predominante$consultor_id)]
  setor_do_projeto <- projetos$setor[match(alocacoes$projeto_id, projetos$projeto_id)]
  taxa_mesmo_setor <- mean(grupo_do_consultor == setor_do_projeto, na.rm = TRUE)
  taxa_mesmo_setor_ao_acaso <- 1 / dplyr::n_distinct(projetos$setor)

  fracao_cold_start_consultor <- 1 - dplyr::n_distinct(alocacoes$consultor_id) / nrow(consultores)
  densidade_r <- nrow(alocacoes) / (nrow(consultores) * nrow(projetos))

  geral <- tibble::tribble(
    ~medida, ~real, ~sintetico,
    "correlação nível x custo_carregado", 0.90, stats::cor(nivel_numerico, custo_por_consultor$custo_carregado),
    "correlação nível x tempo_casa", 0.34, stats::cor(nivel_numerico, consultores$tempo_casa),
    "correlação nível x pct_alocacao", 0.52, stats::cor(nivel_numerico_alocacao, alocacoes$pct_alocacao),
    "correlação nível x horas_mes", -0.55, stats::cor(nivel_numerico_alocacao, alocacoes$horas_mes),
    "grau_consultor: média", 9.00, mean(grau_consultor),
    "grau_consultor: variância", 44.6, stats::var(grau_consultor),
    "grau_projeto: média", 2.51, mean(grau_projeto),
    "grau_projeto: variância", 8.65, stats::var(grau_projeto),
  )

  especifica <- tibble::tribble(
    ~medida, ~real, ~sintetico,
    "densidade de R", 0.005, densidade_r,
    "fração de cold start (consultor)", 0.17, fracao_cold_start_consultor,
    "taxa de pareamento por mesmo setor", taxa_mesmo_setor_ao_acaso, taxa_mesmo_setor,
  )

  dplyr::bind_rows(
    dplyr::mutate(geral, categoria = "geral", .before = 1),
    dplyr::mutate(especifica, categoria = "especifica", .before = 1)
  ) |>
    dplyr::mutate(diferenca_relativa = (sintetico - real) / real)
}
