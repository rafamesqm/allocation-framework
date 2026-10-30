#' Tabela das execuções do NSGA-II por semente
#'
#' @param principal tibble de `resumir_execucao_do_nsga2()` da semente principal.
#' @param variabilidade tibble das nove sementes restantes.
#' @param realizada Vetor de `objetivos_da_realizada()`.
#' @return tibble formatada para `salvar_tabela()`.
tabelar_sementes_do_nsga2 <- function(principal, variabilidade, realizada) {
  dplyr::bind_rows(principal, variabilidade) |>
    dplyr::transmute(
      Semente = as.character(semente),
      Minutos = formatar_decimal(segundos / 60, 1),
      `Pontos distintos` = formatar_inteiro(pontos),
      `Afinidade máxima` = formatar_decimal(afinidade_maxima, 1),
      `Custo mínimo em relação à realizada` = formatar_pct(custo_minimo / realizada[["custo"]] - 1),
      `Sobrecarga atribuível mínima` = formatar_decimal(sobrecarga_minima, 1),
      `Hipervolume na caixa da realizada` = formatar_decimal(hipervolume_da_realizada, 4),
      `Hipervolume da emenda 1` = formatar_decimal(hipervolume_da_emenda_1, 4),
      `Fração dominada por solução exata` = formatar_pct(fracao_dominada)
    )
}

#' Tabela da trajetória dos extremos e do hipervolume
#'
#' @param trajetoria tibble de `medir_trajetoria()`.
#' @param realizada Vetor de `objetivos_da_realizada()`.
#' @return tibble formatada para `salvar_tabela()`.
tabelar_trajetoria <- function(trajetoria, realizada) {
  trajetoria |>
    dplyr::transmute(
      `Geração` = formatar_inteiro(geracao),
      `Afinidade máxima` = formatar_decimal(afinidade_maxima, 1),
      `Custo mínimo em relação à realizada` = formatar_pct(custo_minimo / realizada[["custo"]] - 1),
      `Sobrecarga atribuível mínima` = formatar_decimal(sobrecarga_minima, 1),
      `Hipervolume na caixa da realizada` = formatar_decimal(hipervolume, 4)
    )
}

#' Tabela das soluções de referência: a realizada e as exatas
#'
#' @param avaliacao Lista de `avaliar_fronteira_registrada()`.
#' @param exatos_da_realizada Lista de `resolver_problemas_da_realizada()`.
#' @param payoff Lista de `resolver_tabela_de_payoff()`.
#' @param realizada Vetor de `objetivos_da_realizada()`.
#' @return tibble formatada para `salvar_tabela()`.
tabelar_referencias_da_avaliacao <- function(avaliacao, exatos_da_realizada, payoff, realizada) {
  certificado <- function(solucao) {
    origem <- sub(",.*", "", solucao)
    objetivo <- sub(".*, ", "", solucao)
    lacuna <- if (origem == "payoff") {
      max(payoff$subproblemas$mip_gap[payoff$subproblemas$linha == objetivo])
    } else if (origem == "item 10.c") {
      exatos_da_realizada$problemas$mip_gap[exatos_da_realizada$problemas$objetivo == objetivo]
    } else {
      NA_real_
    }
    dplyr::case_when(
      is.na(lacuna) ~ "--",
      is.infinite(lacuna) ~ "último passo sem solução",
      lacuna == 0 ~ "ótima",
      .default = paste("lacuna", formatar_decimal(lacuna, 4))
    )
  }
  nomes <- c(
    realizada = "Alocação realizada",
    "payoff, afinidade" = "Payoff, afinidade máxima", "payoff, custo" = "Payoff, custo mínimo",
    "payoff, sobrecarga" = "Payoff, sobrecarga mínima (2 de 3 passos)",
    "item 10.c, afinidade" = "Item 10.c, afinidade máxima presa na realizada",
    "item 10.c, custo" = "Item 10.c, custo mínimo preso na realizada",
    "item 10.c, sobrecarga" = "Item 10.c, sobrecarga mínima presa na realizada"
  )
  avaliacao$referencias |>
    dplyr::transmute(
      `Solução` = unname(nomes[solucao]),
      Certificado = purrr::map_chr(solucao, certificado),
      Afinidade = formatar_decimal(afinidade, 1),
      `Custo em relação à realizada` = formatar_pct(custo / realizada[["custo"]] - 1),
      `Sobrecarga atribuível` = formatar_decimal(sobrecarga, 1),
      `Sobrecarga atribuível em horas` = formatar_decimal(sobrecarga_em_horas, 0),
      `Assentos da zona morta` = formatar_inteiro(assentos_da_zona_morta),
      `Assentos nos 10 mais populares` = formatar_pct(fracao_nos_10_mais_populares),
      `Maior número de assentos de um consultor` = formatar_inteiro(maior_numero_de_assentos)
    )
}

#' Tabela de pontos selecionados da fronteira registrada
#'
#' Os três extremos e os dois tratamentos da capacidade que são pontos da
#' fronteira; o limite rígido é inviável, certificado na etapa 2.
#'
#' @param avaliacao Lista de `avaliar_fronteira_registrada()`.
#' @param realizada Vetor de `objetivos_da_realizada()`.
#' @return tibble formatada para `salvar_tabela()`.
tabelar_pontos_da_fronteira <- function(avaliacao, realizada) {
  pontos <- avaliacao$pontos
  selecionados <- dplyr::bind_rows(
    pontos[which.max(pontos$afinidade), ] |> dplyr::mutate(ponto = "Extremo de afinidade"),
    pontos[which.min(pontos$custo), ] |> dplyr::mutate(ponto = "Extremo de custo"),
    pontos[which.min(pontos$sobrecarga), ] |> dplyr::mutate(ponto = "Extremo de sobrecarga"),
    avaliacao$capacidade |> dplyr::mutate(ponto = paste("Tratamento da capacidade:", tratamento))
  )
  selecionados |>
    dplyr::transmute(
      Ponto = ponto,
      Afinidade = formatar_decimal(afinidade, 1),
      `Custo em relação à realizada` = formatar_pct(custo / realizada[["custo"]] - 1),
      `Sobrecarga atribuível` = formatar_decimal(sobrecarga, 1),
      `Sobrecarga atribuível em horas` = formatar_decimal(sobrecarga_em_horas, 0),
      `Assentos da zona morta` = formatar_inteiro(assentos_da_zona_morta),
      `Assentos nos 10 mais populares` = formatar_pct(fracao_nos_10_mais_populares),
      `Maior número de assentos de um consultor` = formatar_inteiro(maior_numero_de_assentos)
    )
}

#' Resumo dos itens 10.c a 10.f
#'
#' @param avaliacao Lista de `avaliar_fronteira_registrada()`.
#' @return tibble formatada para `salvar_tabela()`.
tabelar_resumo_da_avaliacao <- function(avaliacao) {
  posicao <- avaliacao$posicao
  pontos <- avaliacao$pontos
  zona_morta <- pontos$assentos_da_zona_morta
  tibble::tibble(
    Item = c(
      "Posição da realizada", "Posição da realizada", "Posição da realizada", "Posição da realizada",
      "Posição da realizada",
      "Conflito", "Conflito",
      "Capacidade",
      "Zona morta", "Zona morta",
      "Horas", "Horas"
    ),
    Medida = c(
      "Pontos da fronteira que dominam a realizada",
      "Maior ganho de afinidade entre eles",
      "Maior redução de custo entre eles",
      "Maior redução de sobrecarga atribuível entre eles",
      "Dominada pela solução exata de sobrecarga mínima",
      "Sobrecarga por unidade de afinidade entre os extremos exatos",
      "Mediana da mesma taxa no caminho da fronteira",
      "Limite rígido (épsilon zero na sobrecarga atribuível)",
      "Assentos da zona morta por ponto: mediana",
      "Assentos da zona morta por ponto: mínimo e máximo",
      "Pontos que dominam a realizada também com a carga em horas",
      "Correlação de Spearman entre a sobrecarga em percentual e em horas"
    ),
    Valor = c(
      paste(formatar_inteiro(posicao$pontos_que_dominam), "de", formatar_inteiro(posicao$pontos)),
      formatar_decimal(posicao$ganho_de_afinidade, 1),
      formatar_pct(posicao$reducao_de_custo),
      formatar_decimal(posicao$reducao_de_sobrecarga, 1),
      ifelse(posicao$dominada_pela_exata_de_sobrecarga_minima, "sim", "não"),
      formatar_decimal(avaliacao$conflito$taxa_entre_extremos_exatos, 2),
      formatar_decimal(avaliacao$conflito$taxa_mediana_na_fronteira, 2),
      "inviável, certificado pelo limite inferior do solver na etapa 2",
      formatar_decimal(stats::median(zona_morta), 0),
      paste(formatar_inteiro(min(zona_morta)), "e", formatar_inteiro(max(zona_morta))),
      formatar_pct(avaliacao$horas$fracao_que_domina_em_horas),
      formatar_decimal(avaliacao$horas$correlacao_de_spearman, 2)
    )
  )
}
