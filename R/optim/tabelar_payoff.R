#' Tabela de payoff para o texto, com a alocação realizada ao lado
#'
#' O custo sai só como diferença relativa ao custo da realizada, pela regra de
#' redação do pré-registro da otimização (valores monetários só como
#' diferenças relativas).
#'
#' @param payoff Lista de `resolver_tabela_de_payoff()`.
#' @param descricao tibble de `descrever_solucoes_do_payoff()`.
#' @param verificacao Lista de `medir_verificacao_da_otimizacao()`.
#' @return tibble formatada para `salvar_tabela()`.
tabelar_payoff <- function(payoff, descricao, verificacao) {
  rotulos <- c(
    afinidade = "Afinidade máxima", custo = "Custo mínimo",
    sobrecarga = "Sobrecarga mínima", realizada = "Alocação realizada"
  )
  linhas <- dplyr::bind_rows(
    payoff$payoff |> dplyr::filter(passos_com_solucao > 0),
    tibble::tibble(
      linha = "realizada", passos_com_solucao = 3L, afinidade = verificacao$afinidade_realizada,
      custo = verificacao$custo_realizado, sobrecarga = verificacao$sobrecarga_realizada_percentual,
      sobrecarga_atribuivel = verificacao$sobrecarga_atribuivel_realizada
    )
  ) |>
    dplyr::left_join(descricao, by = "linha")

  linhas |>
    dplyr::transmute(
      `Solução` = unname(rotulos[linha]),
      `Passos lexicográficos com solução` = dplyr::if_else(linha == "realizada", "--", paste(passos_com_solucao, "de 3")),
      Afinidade = formatar_decimal(afinidade, 1),
      `Afinidade nos assentos com realizado na lista` = formatar_decimal(afinidade_nos_assentos_da_lista, 1),
      `Custo em relação à realizada` = formatar_pct(custo / verificacao$custo_realizado - 1),
      Sobrecarga = formatar_decimal(sobrecarga, 1),
      `Sobrecarga atribuível aos assentos` = formatar_decimal(sobrecarga_atribuivel, 1),
      `Assentos da zona morta` = formatar_inteiro(assentos_da_zona_morta),
      `Assentos com o realizado` = formatar_inteiro(assentos_com_realizado)
    )
}

#' Amplitude de cada objetivo na tabela de payoff
#'
#' É a amplitude que normaliza as folgas do AUGMECON e define a grade. O custo
#' sai em relação ao custo da realizada.
#'
#' @param payoff Lista de `resolver_tabela_de_payoff()`.
#' @param verificacao Lista de `medir_verificacao_da_otimizacao()`.
#' @return tibble formatada para `salvar_tabela()`.
tabelar_amplitudes_do_payoff <- function(payoff, verificacao) {
  tabela <- payoff$payoff |> dplyr::filter(passos_com_solucao > 0)
  amplitude <- \(x) c(min(x), max(x), max(x) - min(x))
  afinidade <- amplitude(tabela$afinidade)
  custo <- amplitude(tabela$custo) / verificacao$custo_realizado
  sobrecarga <- amplitude(tabela$sobrecarga)
  tibble::tibble(
    Objetivo = c("Afinidade", "Custo, em relação à realizada", "Sobrecarga"),
    `Mínimo` = c(formatar_decimal(afinidade[1], 1), formatar_pct(custo[1] - 1), formatar_decimal(sobrecarga[1], 1)),
    `Máximo` = c(formatar_decimal(afinidade[2], 1), formatar_pct(custo[2] - 1), formatar_decimal(sobrecarga[2], 1)),
    Amplitude = c(formatar_decimal(afinidade[3], 1), formatar_pct(custo[3]), formatar_decimal(sobrecarga[3], 1))
  )
}

#' Tratabilidade de cada subproblema da tabela de payoff
#'
#' @param payoff Lista de `resolver_tabela_de_payoff()`.
#' @return tibble formatada para `salvar_tabela()`.
tabelar_tratabilidade_do_payoff <- function(payoff) {
  nomes <- c(afinidade = "afinidade", custo = "custo", sobrecarga = "sobrecarga")
  payoff$subproblemas |>
    dplyr::transmute(
      `Linha da tabela` = unname(nomes[linha]),
      Passo = passo,
      `Objetivo otimizado` = unname(nomes[objetivo]),
      Status = dplyr::case_match(status, "Optimal" ~ "ótimo", "Time limit reached" ~ "tempo-limite", .default = status),
      Segundos = formatar_decimal(segundos, 1),
      # Lacuna infinita: o tempo acabou sem nenhuma solução viável.
      `Lacuna relativa` = dplyr::if_else(is.finite(mip_gap), formatar_decimal(mip_gap, 6), "sem solução"),
      `Nós` = formatar_inteiro(nos)
    )
}
