#' Resume a janela temporal coberta por cada campo de data
#'
#' Descobre os campos de data pelo próprio contrato de esquema, em vez de
#' listá-los à mão: campo de data novo passa a aparecer aqui sem ninguém
#' precisar lembrar de atualizar a análise.
#'
#' A amplitude importa mais que os extremos. Janela muito mais longa que o
#' horizonte de negócio é sinal de cauda implausível, e neste projeto há um
#' caso conhecido: a duração de projeto tem teto de 2.368 dias, o que empurra
#' datas de fim para muito além da data de referência da geração.
#'
#' @param dados Lista nomeada de tibbles, de `carregar_dados()`.
#' @param especificacao Contrato de esquema, de `especificacao_tabelas()`.
#' @return tibble com `tabela`, `coluna`, `inicio`, `fim`, `amplitude_dias` e
#'   `n`, uma linha por campo de data.
resumir_janela_temporal <- function(dados, especificacao = especificacao_tabelas()) {
  campos <- especificacao |>
    dplyr::filter(tipo == "data", tabela %in% names(dados))

  purrr::pmap(
    list(campos$tabela, campos$coluna),
    function(nome_tabela, nome_coluna) {
      valores <- dados[[nome_tabela]][[nome_coluna]]
      valores <- valores[!is.na(valores)]

      tibble::tibble(
        tabela = nome_tabela,
        coluna = nome_coluna,
        inicio = min(valores),
        fim = max(valores),
        amplitude_dias = as.integer(max(valores) - min(valores)),
        n = length(valores)
      )
    }
  ) |>
    purrr::list_rbind() |>
    dplyr::arrange(inicio)
}

#' Conta eventos por mês-calendário a partir de uma data de ocorrência
#'
#' Série de contagem para eventos pontuais, como o início de um projeto.
#' Meses sem nenhum evento viram linha com zero, e não desaparecem: buraco na
#' série é informação, e uma série que pula meses vazios desenha uma
#' continuidade que não existe.
#'
#' @param datas Vetor de `Date`.
#' @return tibble com `mes` (primeiro dia do mês) e `n`.
serie_mensal_eventos <- function(datas) {
  datas <- datas[!is.na(datas)]
  meses <- lubridate::floor_date(datas, "month")

  todos_os_meses <- seq(min(meses), max(meses), by = "month")

  tibble::tibble(mes = meses) |>
    dplyr::count(mes) |>
    dplyr::right_join(tibble::tibble(mes = todos_os_meses), by = "mes") |>
    dplyr::mutate(n = dplyr::coalesce(n, 0L)) |>
    dplyr::arrange(mes)
}

#' Conta quantos intervalos estão vigentes em cada mês-calendário
#'
#' Diferente de `serie_mensal_eventos()`, que conta ocorrências pontuais.
#' Uma alocação de quatro meses é um evento só na data de início, mas está
#' vigente em quatro meses distintos, e é a vigência que descreve carga de
#' trabalho ao longo do tempo.
#'
#' @param data_inicio,data_fim Vetores de `Date`, de mesmo comprimento.
#' @return tibble com `mes` (primeiro dia do mês) e `n_vigentes`.
serie_vigencias_mensais <- function(data_inicio, data_fim) {
  meses <- expandir_para_meses(data_inicio, data_fim)

  meses |>
    dplyr::count(ano_mes, name = "n_vigentes") |>
    dplyr::mutate(mes = as.Date(paste0(ano_mes, "-01"))) |>
    dplyr::select(mes, n_vigentes) |>
    dplyr::arrange(mes)
}

#' Testa se a ocorrência de eventos varia por mês do ano
#'
#' Qui-quadrado de aderência das contagens por mês do ano contra o esperado
#' sob ocorrência uniforme no tempo.
#'
#' **O esperado não é contagem igual entre os doze meses.** Meses têm
#' comprimentos diferentes, e a janela observada quase nunca cobre um número
#' inteiro de anos, então alguns meses do ano aparecem mais vezes que outros
#' dentro dela. Ignorar isso e testar contra doze proporções iguais acusaria
#' sazonalidade onde só há calendário. O esperado aqui é proporcional ao
#' número de dias de cada mês do ano efetivamente presentes na janela.
#'
#' **O que um resultado não significativo permite concluir.** Não permite
#' concluir que não há sazonalidade, apenas que estes dados não dão evidência
#' dela. A distinção importa neste projeto: sabe-se por construção que o
#' gerador sorteia datas de início de forma uniforme, então a ausência de
#' sazonalidade é premissa do código, não achado sobre a Peers. O teste vale
#' como instrumento pronto para o dado real, não como conclusão.
#'
#' @param datas Vetor de `Date`.
#' @return Lista com `contagens` (tibble por mês do ano, observado e
#'   esperado), `qui_quadrado`, `gl` e `valor_p`.
testar_sazonalidade_mensal <- function(datas) {
  datas <- datas[!is.na(datas)]

  dias_da_janela <- seq(min(datas), max(datas), by = "day")
  dias_por_mes <- table(factor(lubridate::month(dias_da_janela), levels = 1:12))
  esperado <- as.numeric(dias_por_mes) / sum(dias_por_mes)

  observado <- as.numeric(table(factor(lubridate::month(datas), levels = 1:12)))

  teste <- stats::chisq.test(observado, p = esperado)

  # Derivados resolvidos antes da tibble. `tibble()` deixa cada coluna
  # visível às seguintes, então `esperado` dentro dela leria a coluna já
  # escalada e dividiria por `sum(observado)` duas vezes.
  esperado_absoluto <- esperado * sum(observado)
  razao_observado_esperado <- observado / esperado_absoluto

  list(
    contagens = tibble::tibble(
      mes_do_ano = 1:12,
      observado = observado,
      esperado = esperado_absoluto,
      razao = razao_observado_esperado
    ),
    qui_quadrado = unname(teste$statistic),
    gl = unname(teste$parameter),
    valor_p = teste$p.value
  )
}
