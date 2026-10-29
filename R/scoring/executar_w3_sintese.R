#' Executa a síntese da wave 3, que consolida os achados das seis etapas
#'
#' Mesma lógica da Parte III da EDA: a síntese não recalcula nada, empilha os
#' achados que cada etapa declarou ao lado do código que os produziu, e tira
#' deles as tabelas de decisões e de limitações. Usa as funções de
#' `R/eda/analisar_sintese.R`, que conferem código repetido e a ordem de
#' leitura.
#'
#' @param achados_por_etapa Lista de tibbles de `registrar_achados()`.
#' @return Lista com `objetos` (o registro consolidado) e `tabelas`.
executar_w3_sintese <- function(achados_por_etapa) {
  achados <- consolidar_achados(achados_por_etapa)
  sintese <- tabular_sintese(achados)

  list(
    objetos = list(achados = achados, sintese = sintese),
    tabelas = list(
      achados_w3 = achados,
      w3_decisoes = sintese |>
        dplyr::filter(!is.na(decisao)) |>
        dplyr::transmute(
          Achado = codigo,
          `Decisão que habilita` = decisao,
          `Ressalva da própria decisão` = dplyr::coalesce(limitacao, "--"),
          `Proveniência` = proveniencia
        ),
      w3_limitacoes = sintese |>
        dplyr::filter(!is.na(limitacao), is.na(decisao)) |>
        dplyr::transmute(
          Achado = codigo,
          `Limitação que impõe` = limitacao,
          `Proveniência` = proveniencia
        )
    )
  )
}
