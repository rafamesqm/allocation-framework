#' Gera experiência setorial de cada consultor (parte de `C`)
#'
#' Usa as categorias reais de `Segmento` (ver docs/mapeamento_schema.md,
#' distribuição de `vw_crm_oportunidades_latest`) como espaço setorial. Cada
#' consultor acumula experiência em 1 a `max_setores` segmentos. Só o espaço
#' de categorias e seus pesos relativos são reais; a intensidade por pessoa
#' não tem calibração agregada nesta rodada, é suposição documentada.
#'
#' @param consultores tibble de `gerar_consultores()`.
#' @param max_setores Número máximo de segmentos distintos por consultor.
#' @return tibble longo: `consultor_id`, `segmento`, `intensidade` (0 a 1).
gerar_experiencia_setorial <- function(consultores, max_setores = 3) {
  segmentos <- c(
    "Varejo", "Serviços", "Indústria", "Banking", "Educação", "Health",
    "Insurance", "Agronegócio", "Alimentos e Bebidas", "Private Equity"
  )
  pesos_segmento <- c(1097, 773, 510, 384, 348, 315, 240, 182, 165, 72)

  purrr::map_dfr(consultores$consultor_id, function(id_consultor) {
    n_setores <- sample.int(max_setores, size = 1)
    setores_do_consultor <- sample(segmentos, size = n_setores, prob = pesos_segmento)

    tibble::tibble(
      consultor_id = id_consultor,
      segmento = setores_do_consultor,
      intensidade = stats::runif(n_setores)
    )
  })
}
