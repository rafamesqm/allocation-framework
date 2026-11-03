#' Tabela da comparação dos extremos com a tabela de payoff exata
#'
#' O custo sai em relação ao custo da alocação realizada, pela regra de
#' redação do pré-registro da otimização.
#'
#' @param comparacao Lista de `comparar_fronteira_com_exato()`.
#' @param payoff Lista de `resolver_tabela_de_payoff()`.
#' @param verificacao Lista de `medir_verificacao_da_otimizacao()`.
#' @return tibble formatada para `salvar_tabela()`.
tabelar_extremos_do_nsga2 <- function(comparacao, payoff, verificacao) {
  nomes <- c(afinidade = "Afinidade máxima", custo = "Custo mínimo", sobrecarga = "Sobrecarga mínima")
  lacuna <- payoff$subproblemas |>
    dplyr::summarise(lacuna = max(mip_gap), .by = linha)
  relativo <- \(x) formatar_pct(x / verificacao$custo_realizado - 1)

  comparacao$extremos |>
    dplyr::left_join(lacuna, by = c(objetivo = "linha")) |>
    dplyr::transmute(
      Extremo = unname(nomes[objetivo]),
      `Linha exata` = dplyr::case_when(
        !completa ~ "incompleta",
        lacuna > 0 ~ paste("lacuna de até", formatar_decimal(lacuna, 4)),
        .default = "ótima"
      ),
      `Distância normalizada` = formatar_decimal(distancia, 3),
      `Afinidade, evolutiva` = formatar_decimal(afinidade_evolutiva, 1),
      `Afinidade, exata` = formatar_decimal(afinidade_exata, 1),
      `Custo, evolutivo` = relativo(custo_evolutivo),
      `Custo, exato` = relativo(custo_exato),
      `Sobrecarga atribuível, evolutiva` = formatar_decimal(sobrecarga_evolutiva, 1),
      `Sobrecarga atribuível, exata` = formatar_decimal(sobrecarga_exata, 1)
    )
}

#' Resumo da execução do NSGA-II e da comparação com o exato
#'
#' @param execucao Lista de `executar_nsga2_registrado()`.
#' @param fronteira Lista de `extrair_fronteira()`.
#' @param comparacao Lista de `comparar_fronteira_com_exato()`.
#' @return tibble formatada para `salvar_tabela()`.
tabelar_resumo_do_nsga2 <- function(execucao, fronteira, comparacao) {
  tibble::tibble(
    Medida = c(
      "Semente", "Minutos de execução", "Pontos distintos na fronteira",
      "Fração da fronteira dominada por solução exata",
      "Hipervolume normalizado da fronteira", "Hipervolume normalizado da tabela de payoff"
    ),
    Valor = c(
      as.character(execucao$semente), formatar_decimal(execucao$segundos / 60, 1),
      formatar_inteiro(nrow(fronteira$objetivos)), formatar_pct(comparacao$fracao_dominada),
      formatar_decimal(comparacao$hipervolume, 4), formatar_decimal(comparacao$hipervolume_do_payoff, 4)
    )
  )
}

#' Tabela da convergência: hipervolume da primeira fronteira por geração
#'
#' @param convergencia tibble de `medir_convergencia()`.
#' @return tibble formatada para `salvar_tabela()`.
tabelar_convergencia_do_nsga2 <- function(convergencia) {
  convergencia |>
    dplyr::transmute(
      `Geração` = formatar_inteiro(geracao),
      `Pontos distintos` = formatar_inteiro(pontos),
      `Hipervolume normalizado` = formatar_decimal(hipervolume, 4)
    )
}
