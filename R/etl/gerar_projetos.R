#' Gera a tabela de projetos sintéticos (linhas de `P`)
#'
#' Por padrão gera só os projetos em execução (952, ver
#' docs/mapeamento_schema.md, "Projetos por situação"), não o funil de
#' propostas/leads/recusados: isso vira `pipeline_demandas` (Camada 3) à
#' parte.
#'
#' `setor` reaproveita as categorias e pesos reais de `Segmento`, e
#' `tipo_projeto` os de `Offering` (mesmas listas usadas em
#' `gerar_experiencia_setorial()`/`gerar_expertise_funcional()`), para os
#' dois lados do content-based (`C` e `P`) viverem no mesmo espaço de
#' categorias. Isso é o proxy de `P` decidido diante da ausência de
#' `skills_requeridas[]` como processo real (ver dicionario_de_dados.md).
#'
#' `data_inicio`/`duracao_dias` são suposição documentada: a duração média
#' real (177,6 dias) tem confiança baixa, porque a fonte é um campo de
#' auditoria (`dt_alteracao`), não uma data de fim confirmada. Serve aqui só
#' de teto grosseiro para uma distribuição plausível, não como parâmetro
#' calibrado de verdade.
#'
#' @param n_projetos Número de projetos a gerar. Default 952 (projetos em
#'   execução reais).
#' @param semente Inteiro para reprodutibilidade.
#' @param data_referencia Data a partir da qual a janela de `data_inicio` é
#'   sorteada para trás. Default a data corrente; fixar para reprodutibilidade
#'   exata entre execuções em dias diferentes.
#' @param janela_inicio_dias Amplitude, em dias antes de `data_referencia`,
#'   da janela de onde `data_inicio` é sorteada.
#' @param duracao_media_dias Duração média do projeto, em dias (suposição
#'   documentada, ver detalhe acima).
#' @return tibble com um projeto por linha: `projeto_id`, `setor`,
#'   `tipo_projeto`, `valor_contrato`, `esforco_estimado`, `data_inicio`,
#'   `duracao_dias`, `data_fim`.
gerar_projetos <- function(n_projetos = 952,
                            semente = 1,
                            data_referencia = Sys.Date(),
                            janela_inicio_dias = 730,
                            duracao_media_dias = 177.6) {
  set.seed(semente)

  segmentos <- c(
    "Varejo", "Serviços", "Indústria", "Banking", "Educação", "Health",
    "Insurance", "Agronegócio", "Alimentos e Bebidas", "Private Equity"
  )
  pesos_segmento <- c(1097, 773, 510, 384, 348, 315, 240, 182, 165, 72)

  offerings <- c(
    "Experience & Efficiency", "Strategy and M&A",
    "Data Analytics & Artificial Intelligence", "Supply Chain",
    "Tech Consulting", "Finance", "Talent & Organization",
    "Digital Products & Platforms", "Cyber Security", "Digital Factory",
    "Peers.AI", "Educação Pública"
  )
  pesos_offering <- c(991, 642, 530, 524, 427, 424, 192, 155, 119, 114, 49, 47)

  duracao_dias <- round(stats::rexp(n_projetos, rate = 1 / duracao_media_dias))
  data_inicio <- data_referencia - sample.int(janela_inicio_dias, n_projetos, replace = TRUE)

  tibble::tibble(
    projeto_id = sprintf("projeto_%04d", seq_len(n_projetos)),
    setor = sample(segmentos, n_projetos, replace = TRUE, prob = pesos_segmento),
    tipo_projeto = sample(offerings, n_projetos, replace = TRUE, prob = pesos_offering),
    valor_contrato = gerar_variavel_hurdle_lognormal(
      n_projetos,
      prob_zero = 0.147, mediana_positivos = 108300,
      p25_positivos = 34814, p75_positivos = 375054
    ),
    esforco_estimado = gerar_esforco_estimado(n_projetos),
    data_inicio = data_inicio,
    duracao_dias = duracao_dias,
    data_fim = data_inicio + duracao_dias
  )
}
