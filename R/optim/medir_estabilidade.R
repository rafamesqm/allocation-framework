#' Descreve x̄ e as linhas de afinidade e de custo da tabela de payoff
#'
#' As grandezas da estabilidade (emendas 1 a 3): os três objetivos, o custo
#' em relação ao de x̄, a ocupação máxima, se a linha chegou ao ótimo e o
#' tempo somado dos passos.
#'
#' @param problema Lista de `montar_problema_da_otimizacao()`.
#' @param insumos Lista de `preparar_insumos_da_otimizacao()`.
#' @param payoff Lista de `resolver_tabela_de_payoff()` com ao menos as
#'   linhas de afinidade e de custo.
#' @return tibble com uma linha por solução: `realizada`, `afinidade` e
#'   `custo`.
descrever_estabilidade_da_otimizacao <- function(problema, insumos, payoff) {
  linhas <- c("afinidade", "custo")
  solucoes <- c(list(realizada = solucao_realizada(problema)), payoff$solucoes[linhas])
  certificados <- dplyr::bind_rows(
    tibble::tibble(solucao = "realizada", status = NA_character_, segundos = NA_real_, mip_gap = NA_real_),
    certificar_linhas_do_payoff(payoff) |>
      dplyr::mutate(solucao = sub("payoff, ", "", solucao)) |>
      dplyr::filter(solucao %in% linhas)
  )
  descricao <- descrever_solucoes_exatas(solucoes, certificados[match(names(solucoes), certificados$solucao), ], problema, insumos)
  custo_da_realizada <- descricao$custo[descricao$solucao == "realizada"]
  descricao |>
    dplyr::transmute(
      solucao, otima = status == "Optimal", segundos,
      afinidade, custo_relativo = custo / custo_da_realizada - 1, sobrecarga, ocupacao_maxima
    )
}

#' Uma realização da estabilidade da otimização
#'
#' Gera os dados com férias, refaz insumos, ŝ, assentos, carga e o problema,
#' e resolve só as linhas de afinidade e de custo da tabela de payoff.
#'
#' @param semente Semente da realização.
#' @param registro Lista de `ler_pre_registro()`.
#' @param desenho Pré-registro da otimização sem as emendas.
#' @param ... Repassado a `gerar_realizacao_com_ferias()`.
#' @return tibble de `descrever_estabilidade_da_otimizacao()` com `semente`.
medir_realizacao_da_otimizacao <- function(semente, registro, desenho, ...) {
  dados <- gerar_realizacao_com_ferias(semente, ...)
  insumos <- preparar_insumos_da_otimizacao(dados, registro, desenho)
  problema <- montar_problema_da_otimizacao(insumos, desenho$carga$medida)
  duas_linhas <- desenho
  duas_linhas$metodo$ordem_lexicografica <- desenho$metodo$ordem_lexicografica[c("afinidade", "custo")]
  payoff <- resolver_tabela_de_payoff(problema, duas_linhas)
  dplyr::bind_cols(
    tibble::tibble(semente = semente),
    descrever_estabilidade_da_otimizacao(problema, insumos, payoff)
  )
}

#' Resume a estabilidade: mediana e intervalo entre realizações
#'
#' Cada grandeza tem a mediana e o intervalo de 90% entre realizações, e a
#' posição da realização versionada nele. Ponto fora não é defeito: um
#' sorteio em cada dez cai fora por definição.
#'
#' @param versionada tibble de `descrever_estabilidade_da_otimizacao()`.
#' @param realizacoes tibble de `medir_realizacao_da_otimizacao()`, uma
#'   linha por semente e solução.
#' @param nivel Nível do intervalo.
#' @return tibble com `solucao`, `grandeza`, `versionada`, `mediana`,
#'   `inferior`, `superior`, `dentro` e `realizacoes`.
resumir_estabilidade_da_otimizacao <- function(versionada, realizacoes, nivel) {
  derivar <- function(tabela) {
    realizada <- tabela[tabela$solucao == "realizada", ]
    tabela |>
      dplyr::mutate(
        ganho_de_afinidade = afinidade - realizada$afinidade,
        acrescimo_de_sobrecarga = sobrecarga - realizada$sobrecarga
      )
  }
  grandezas <- c(
    "afinidade", "custo_relativo", "sobrecarga", "ocupacao_maxima", "ganho_de_afinidade",
    "acrescimo_de_sobrecarga", "otima", "segundos"
  )
  alongar <- function(tabela) {
    tabela |>
      dplyr::mutate(otima = as.numeric(otima)) |>
      tidyr::pivot_longer(dplyr::all_of(grandezas), names_to = "grandeza", values_to = "valor") |>
      dplyr::filter(!(solucao == "realizada" & grandeza %in% c("otima", "segundos", "ganho_de_afinidade", "acrescimo_de_sobrecarga", "custo_relativo")))
  }
  das_realizacoes <- realizacoes |>
    dplyr::group_split(semente) |>
    purrr::map(derivar) |>
    purrr::list_rbind() |>
    alongar()
  cauda <- (1 - nivel) / 2

  das_realizacoes |>
    dplyr::summarise(
      # Para a linha ótima, a "mediana" é a fração das realizações.
      mediana = if (dplyr::cur_group()$grandeza == "otima") mean(valor) else stats::median(valor),
      inferior = stats::quantile(valor, cauda, names = FALSE),
      superior = stats::quantile(valor, 1 - cauda, names = FALSE),
      realizacoes = dplyr::n(),
      .by = c(solucao, grandeza)
    ) |>
    dplyr::inner_join(
      alongar(derivar(versionada)) |> dplyr::select(solucao, grandeza, versionada = valor),
      by = c("solucao", "grandeza")
    ) |>
    dplyr::mutate(dentro = versionada >= inferior & versionada <= superior) |>
    dplyr::relocate(versionada, .after = grandeza)
}
