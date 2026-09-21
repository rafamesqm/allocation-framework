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
#' `data_inicio`/`duracao_dias`: a duração é calibrada pela Sexta rodada
#' (docs/mapeamento_schema.md), a partir das datas reais de início/fim do
#' span de alocações por projeto (n=1834): mediana 18 dias, P25 4, P75 81,
#' assimetria forte (3,6). Substitui a estimativa anterior (Exponencial de
#' média 177,6 dias), que vinha de um campo de auditoria
#' (`dt_alteracao`) sem confirmação como data de fim real. Log-normal
#' calibrada por casamento de quantis (`calibrar_lognormal_por_quantis()`),
#' sem massa em zero.
#'
#' **Piso de 1 dia na duração.** O arredondamento da log-normal produzia
#' duração exata de zero em cerca de 6% dos projetos (`data_fim` igual a
#' `data_inicio`), artefato de `round()` sobre a cauda inferior, não
#' calibração: um projeto de duração zero não existe, e deixava sem janela
#' as alocações que precisam caber dentro dele
#' (`gerar_periodo_alocacao()`).
#'
#' **Decisão de modelagem: teto em `duracao_teto_dias`.** Os três quantis
#' reais (4/18/81 dias), batidos exatamente por uma log-normal, implicam uma
#' cauda bem mais pesada do que a assimetria real reportada (3,6) sugere: em
#' escala real (952 projetos), cerca de 3% saíam com duração acima do máximo
#' real observado (1.184 dias), incluindo casos de dezenas de anos,
#' fisicamente implausíveis para um projeto de consultoria (diferente de
#' `valor_contrato`, onde uma cauda pesada extrema ainda é economicamente
#' plausível). O teto de 2.368 dias (2x o máximo real observado) corta essa
#' cauda absurda preservando o essencial do casamento de quantis para a
#' maioria dos casos: só afeta a franja superior da distribuição.
#'
#' @param n_projetos Número de projetos a gerar. Default 952 (projetos em
#'   execução reais).
#' @param semente Inteiro para reprodutibilidade.
#' @param data_referencia Data a partir da qual a janela de `data_inicio` é
#'   sorteada para trás. Default a data corrente; fixar para reprodutibilidade
#'   exata entre execuções em dias diferentes.
#' @param janela_inicio_dias Amplitude, em dias antes de `data_referencia`,
#'   da janela de onde `data_inicio` é sorteada.
#' @param duracao_mediana_dias,duracao_p25_dias,duracao_p75_dias Mediana e
#'   quartis reais da duração de projeto, em dias (ver detalhe acima),
#'   usados para calibrar a log-normal por casamento de quantis.
#' @param duracao_teto_dias Teto de duração, em dias (ver decisão de
#'   modelagem acima): 2x o máximo real observado (1.184 dias).
#' @return tibble com um projeto por linha: `projeto_id`, `setor`,
#'   `tipo_projeto`, `valor_contrato`, `esforco_estimado`, `data_inicio`,
#'   `duracao_dias`, `data_fim`.
gerar_projetos <- function(n_projetos = 952,
                            semente = 1,
                            data_referencia = Sys.Date(),
                            janela_inicio_dias = 730,
                            duracao_mediana_dias = 81,
                            duracao_p25_dias = 42,
                            duracao_p75_dias = 156,
                            duracao_teto_dias = 2368) {
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

  parametros_duracao <- calibrar_lognormal_por_quantis(
    duracao_mediana_dias, duracao_p25_dias, duracao_p75_dias
  )

  duracao_sorteada <- round(stats::rlnorm(
    n_projetos,
    meanlog = parametros_duracao$meanlog, sdlog = parametros_duracao$sdlog
  ))
  duracao_dias <- pmin(pmax(duracao_sorteada, 1), duracao_teto_dias)
  data_inicio <- data_referencia - sample.int(janela_inicio_dias, n_projetos, replace = TRUE)

  tibble::tibble(
    projeto_id = sprintf("projeto_%04d", seq_len(n_projetos)),
    setor = sample(segmentos, n_projetos, replace = TRUE, prob = pesos_segmento),
    tipo_projeto = sample(offerings, n_projetos, replace = TRUE, prob = pesos_offering),
    # Recalibrado pela segunda consulta comercial: `silver.pps`, última
    # versão por CRM, 556 projetos, tabela completa e sem teto de extração.
    # A calibração anterior vinha de `actarnet_alocacao_pessoas` e dava
    # mediana de 108 mil, quatro vezes menor. As duas medem valor de
    # contrato, mas só a de `pps` é a mesma fonte da âncora de margem, e
    # misturar fontes era o que fazia a margem estrutural não fechar.
    valor_contrato = gerar_variavel_hurdle_lognormal(
      n_projetos,
      prob_zero = 0.147, mediana_positivos = 457000,
      p25_positivos = 190000, p75_positivos = 1100000
    ),
    esforco_estimado = gerar_esforco_estimado(n_projetos),
    data_inicio = data_inicio,
    duracao_dias = duracao_dias,
    data_fim = data_inicio + duracao_dias
  )
}
