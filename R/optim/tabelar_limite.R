#' Certificado de uma solução exata, em texto
#'
#' @param status Status do HiGHS (`NA` para a realizada).
#' @param mip_gap Lacuna relativa.
#' @return Vetor de texto.
descrever_certificado <- function(status, mip_gap) {
  dplyr::case_when(
    is.na(status) ~ "--",
    status == "Optimal" ~ "ótima",
    status == "Infeasible" ~ "inviável",
    is.na(mip_gap) | is.infinite(mip_gap) ~ "sem solução no tempo-limite",
    .default = paste("lacuna", formatar_decimal(mip_gap, 4))
  )
}

#' Nomes das soluções nas tabelas da wave 4
#'
#' @param solucao Vetor de nomes internos.
#' @return Vetor de texto.
nomear_solucoes_da_otimizacao <- function(solucao) {
  nomes <- c(
    realizada = "Alocação realizada",
    "payoff, afinidade" = "Payoff, afinidade máxima", "payoff, custo" = "Payoff, custo mínimo",
    "payoff, sobrecarga" = "Payoff, sobrecarga mínima (2 de 3 passos)",
    "item 10.c, afinidade" = "Item 10.c, afinidade máxima presa na realizada",
    "item 10.c, custo" = "Item 10.c, custo mínimo preso na realizada",
    "item 10.c, sobrecarga" = "Item 10.c, sobrecarga mínima presa na realizada",
    afinidade = "Linha de afinidade máxima", custo = "Linha de custo mínimo"
  )
  unname(nomes[solucao])
}

#' Concentração simultânea nas soluções da emenda 2
#'
#' As soluções exatas e a realizada uma a uma, e a fronteira registrada pela
#' mediana e pelos extremos.
#'
#' @param concentracao Lista de `medir_concentracao_da_avaliacao()`.
#' @return tibble formatada para `salvar_tabela()`.
tabelar_concentracao_simultanea <- function(concentracao) {
  referencias <- concentracao$referencias |>
    dplyr::transmute(
      `Solução` = nomear_solucoes_da_otimizacao(solucao),
      `Ocupação máxima num mês` = formatar_decimal(ocupacao_maxima, 2),
      `Meses de consultor acima de 2` = formatar_inteiro(meses_acima_de_2),
      `Sobrecarga atribuível no consultor mais sobrecarregado` = formatar_pct(fracao_no_mais_sobrecarregado)
    )
  pontos <- concentracao$pontos
  faixa <- function(x, formatar) paste0(formatar(stats::median(x)), " (", formatar(min(x)), " a ", formatar(max(x)), ")")
  fronteira <- tibble::tibble(
    `Solução` = paste0("Fronteira registrada, ", nrow(pontos), " pontos: mediana (mínimo a máximo)"),
    `Ocupação máxima num mês` = faixa(pontos$ocupacao_maxima, \(x) formatar_decimal(x, 2)),
    `Meses de consultor acima de 2` = faixa(pontos$meses_acima_de_2, formatar_inteiro),
    `Sobrecarga atribuível no consultor mais sobrecarregado` = faixa(pontos$fracao_no_mais_sobrecarregado, formatar_pct)
  )
  dplyr::bind_rows(referencias, fronteira)
}

#' Os problemas da emenda 3 e a sensibilidade
#'
#' @param descricao tibble de `descrever_problemas_limitados()`.
#' @return tibble formatada para `salvar_tabela()`.
tabelar_problemas_limitados <- function(descricao) {
  custo_da_realizada <- descricao$custo[descricao$solucao == "realizada"][1]
  descricao |>
    dplyr::transmute(
      `Solução` = nomear_solucoes_da_otimizacao(solucao),
      `Passos com solução` = ifelse(is.na(passos_com_solucao), "--", paste(passos_com_solucao, "de 3")),
      L = formatar_decimal(limite, 2),
      Certificado = descrever_certificado(status, mip_gap),
      Segundos = ifelse(is.na(segundos), "--", formatar_decimal(segundos, 0)),
      Afinidade = formatar_decimal(afinidade, 1),
      `Custo em relação à realizada` = formatar_pct(custo / custo_da_realizada - 1),
      `Sobrecarga atribuível` = formatar_decimal(sobrecarga, 1),
      `Ocupação máxima num mês` = formatar_decimal(ocupacao_maxima, 2),
      `Meses de consultor acima de 2` = formatar_inteiro(meses_acima_de_2),
      `Sobrecarga atribuível no consultor mais sobrecarregado` = formatar_pct(fracao_no_mais_sobrecarregado),
      `Maior número de assentos de um consultor` = formatar_inteiro(maior_numero_de_assentos),
      `Assentos nos 10 mais populares` = formatar_pct(fracao_nos_10_mais_populares)
    )
}

#' A estabilidade entre realizações
#'
#' @param resumo tibble de `resumir_estabilidade_da_otimizacao()`.
#' @return tibble formatada para `salvar_tabela()`.
tabelar_estabilidade_da_otimizacao <- function(resumo) {
  nomes <- c(
    afinidade = "Afinidade", custo_relativo = "Custo em relação à realizada",
    sobrecarga = "Sobrecarga atribuível", ocupacao_maxima = "Ocupação máxima num mês",
    ganho_de_afinidade = "Ganho de afinidade sobre a realizada",
    acrescimo_de_sobrecarga = "Acréscimo de sobrecarga atribuível sobre a realizada",
    otima = "Fração das realizações com a linha ótima", segundos = "Segundos somados dos três passos"
  )
  formatar <- function(valor, grandeza) {
    dplyr::case_when(
      grandeza %in% c("custo_relativo", "otima") ~ formatar_pct(valor),
      grandeza == "ocupacao_maxima" ~ formatar_decimal(valor, 2),
      grandeza == "segundos" ~ formatar_decimal(valor, 0),
      .default = formatar_decimal(valor, 1)
    )
  }
  resumo |>
    dplyr::transmute(
      `Solução` = nomear_solucoes_da_otimizacao(solucao),
      Grandeza = unname(nomes[grandeza]),
      `Realização versionada` = formatar(versionada, grandeza),
      Mediana = formatar(mediana, grandeza),
      `Intervalo de 90%` = paste(formatar(inferior, grandeza), "a", formatar(superior, grandeza)),
      `Versionada no intervalo` = ifelse(dentro, "sim", "não"),
      `Realizações` = formatar_inteiro(realizacoes)
    )
}
