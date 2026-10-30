#' Mede os valores de verificação da montagem do problema
#'
#' São os números que o pré-registro da otimização grava antes de qualquer
#' solução, e que a pipeline confere a cada corrida. A sobrecarga da alocação
#' realizada é medida por dois caminhos independentes: pela matriz montada e
#' pela ocupação da EDA (`agregar_ocupacao_consultor()` sobre o histórico da
#' janela) nos mesmos meses, contra a mesma capacidade.
#'
#' @param insumos Lista de `preparar_insumos_da_otimizacao()`.
#' @param problema Lista de `montar_problema_da_otimizacao()`, na medida
#'   percentual.
#' @param dados Lista nomeada de tibbles, de `carregar_dados()`.
#' @param registro Lista de `ler_pre_registro()`.
#' @return Lista nomeada de valores, com os nomes de `verificacao` no
#'   pré-registro da otimização, mais as medidas descritivas da montagem.
medir_verificacao_da_otimizacao <- function(insumos, problema, dados, registro) {
  stopifnot(problema$medida == "percentual")
  elegibilidade <- insumos$elegibilidade
  realizados <- elegibilidade[elegibilidade$realizado, ]

  historico <- com_historico_da_janela(dados, janela_do_pre_registro(registro))
  ocupacao_eda <- insumos$capacidade |>
    dplyr::left_join(
      historico$ocupacao_consultor |> dplyr::select(consultor_id, ano_mes, pct_ocupacao),
      by = c("consultor_id", "ano_mes")
    ) |>
    dplyr::mutate(pct_ocupacao = dplyr::coalesce(pct_ocupacao, 0))

  carga_realizada <- dplyr::bind_rows(
    insumos$carga_fixa$por_mes |> dplyr::select(consultor_id, ano_mes, carga_pct),
    insumos$meses_dos_assentos |>
      dplyr::mutate(consultor_id = insumos$assentos$consultor_realizado[match(assento_id, insumos$assentos$assento_id)]) |>
      dplyr::select(consultor_id, ano_mes, carga_pct)
  ) |>
    dplyr::summarise(carga_pct = sum(carga_pct), .by = c(consultor_id, ano_mes))
  comparacao <- ocupacao_eda |>
    dplyr::left_join(carga_realizada, by = c("consultor_id", "ano_mes")) |>
    dplyr::mutate(carga_pct = dplyr::coalesce(carga_pct, 0))

  realizada <- avaliar_solucao(problema, solucao_realizada(problema))
  por_nivel <- table(factor(elegibilidade$nivel, levels = c("N0", "N1", "N2", "N3", "N4")))
  candidatos_por_assento <- table(elegibilidade$assento_id)

  # Meses de consultor sem capacidade (férias no mês inteiro) e os assentos
  # realizados ativos neles.
  sem_capacidade <- insumos$capacidade[insumos$capacidade$capacidade_pct == 0, ]
  realizados_sem_capacidade <- insumos$meses_dos_assentos |>
    dplyr::mutate(consultor_id = insumos$assentos$consultor_realizado[match(assento_id, insumos$assentos$assento_id)]) |>
    dplyr::semi_join(sem_capacidade, by = c("consultor_id", "ano_mes"))

  # Admissão: a data de referência menos o tempo de casa. Limitação, não
  # restrição; o gerador não a respeita.
  admissao <- calcular_data_de_admissao(insumos$consultores, insumos$janela[["fim"]])
  inicio_do_assento <- insumos$assentos$data_inicio[match(elegibilidade$assento_id, insumos$assentos$assento_id)]
  antes_da_admissao <- inicio_do_assento < admissao[elegibilidade$consultor_id]

  list(
    assentos = nrow(insumos$assentos),
    projetos = dplyr::n_distinct(insumos$assentos$projeto_id),
    alocacoes_fixas_ativas_apos_o_corte = sum(insumos$carga_fixa$alocacoes$ativa_apos_o_corte),
    alocacoes_fixas_so_no_mes_do_corte = sum(!insumos$carga_fixa$alocacoes$ativa_apos_o_corte),
    pares_removidos_por_carga_fixa_no_projeto = insumos$pares_removidos_por_carga_fixa_no_projeto,
    binarias_por_nivel = as.list(stats::setNames(as.integer(por_nivel), names(por_nivel))),
    binarias = nrow(elegibilidade),
    fracao_realizados_fora_do_filtro = mean(realizados$origem == "realizado fora da lista"),
    pares_zona_morta = sum(elegibilidade$origem == "zona morta"),
    fracao_pares_zona_morta = mean(elegibilidade$origem == "zona morta"),
    meses_de_consultor_sem_capacidade = nrow(sem_capacidade),
    assentos_realizados_em_mes_sem_capacidade = dplyr::n_distinct(realizados_sem_capacidade$assento_id),
    assentos_censurados_no_fim_da_janela = sum(insumos$assentos$data_fim == insumos$janela[["fim"]] - 1),
    assentos_realizados_antes_da_admissao = sum(antes_da_admissao & elegibilidade$realizado),
    pares_antes_da_admissao = sum(antes_da_admissao),
    sobrecarga_realizada_percentual = realizada$sobrecarga,
    # Descritivas da montagem, fora da conferência.
    afinidade_realizada_nos_assentos_da_lista = sum(realizados$s_hat[realizados$origem == "lista"]),
    assentos_com_realizado_na_lista = sum(realizados$origem == "lista"),
    amplitude_de_s_hat_por_nivel = medir_amplitude_de_s_hat(elegibilidade),
    sobrecarga_realizada_pela_ocupacao_da_eda = sum(pmax(comparacao$pct_ocupacao - comparacao$capacidade_pct, 0)),
    maior_diferenca_de_carga_contra_a_eda = max(abs(comparacao$carga_pct - comparacao$pct_ocupacao)),
    realizada_viavel = realizada$viavel,
    afinidade_realizada = realizada$afinidade,
    custo_realizado = realizada$custo,
    sobrecarga_atribuivel_realizada = realizada$sobrecarga_atribuivel,
    sobrecarga_da_carga_fixa = problema$sobrecarga_da_carga_fixa,
    realizados_zona_morta = sum(realizados$origem == "zona morta"),
    realizados_fora_da_lista = sum(realizados$origem == "realizado fora da lista"),
    assentos_so_com_o_realizado = sum(candidatos_por_assento == 1),
    menor_numero_de_candidatos = min(candidatos_por_assento),
    variaveis_continuas = nrow(problema$sobrecarga),
    restricoes = nrow(problema$restricoes),
    nao_nulos = Matrix::nnzero(problema$restricoes),
    restricoes_por_tipo = as.list(table(problema$tipo_da_restricao))
  )
}

#' Data de admissão de cada consultor, pelo tempo de casa
#'
#' O tempo de casa é medido na data de referência da geração, que é o fim da
#' janela.
#'
#' @param consultores tibble com `consultor_id` e `tempo_casa` em anos.
#' @param referencia Data em que o tempo de casa é medido.
#' @return Vetor `Date` nomeado pelo `consultor_id`.
calcular_data_de_admissao <- function(consultores, referencia) {
  stats::setNames(referencia - round(consultores$tempo_casa * 365.25), consultores$consultor_id)
}

#' Amplitude de `ŝ` entre os candidatos de cada assento, por nível
#'
#' Sem renormalização por assento, cada assento pesa na afinidade conforme a
#' distância entre o maior e o menor `ŝ` dos seus candidatos. Descritivo.
#'
#' @param elegibilidade tibble de `montar_elegibilidade()`.
#' @return tibble com `nivel`, `assentos` e a mediana, o mínimo e o máximo da
#'   amplitude.
medir_amplitude_de_s_hat <- function(elegibilidade) {
  elegibilidade |>
    dplyr::summarise(amplitude = max(s_hat) - min(s_hat), .by = c(nivel, assento_id)) |>
    dplyr::summarise(
      assentos = dplyr::n(), mediana = stats::median(amplitude),
      minimo = min(amplitude), maximo = max(amplitude), .by = nivel
    ) |>
    dplyr::arrange(nivel)
}

#' Confere a montagem contra os valores de verificação do pré-registro
#'
#' Para a pipeline se algum valor não reproduzir: resolver um problema
#' diferente do registrado seria responder a outra pergunta.
#'
#' Duas checagens não dependem de valor registrado e também param a pipeline:
#' a alocação realizada tem de ser viável, e a sobrecarga dela pela matriz tem
#' de coincidir com a sobrecarga pela ocupação da EDA.
#'
#' @param verificacao Lista de `medir_verificacao_da_otimizacao()`.
#' @param registro_otimizacao Lista de `ler_pre_registro_otimizacao()`.
#' @param tolerancia Diferença absoluta admitida nos valores contínuos, pelo
#'   arredondamento do registro a quatro casas.
#' @return tibble com `grandeza`, `medido`, `registrado` e `confere`.
conferir_montagem_da_otimizacao <- function(verificacao, registro_otimizacao, tolerancia = 1e-4) {
  if (!isTRUE(verificacao$realizada_viavel)) {
    stop("A alocação realizada não é viável na matriz montada.", call. = FALSE)
  }
  if (abs(verificacao$sobrecarga_realizada_percentual - verificacao$sobrecarga_realizada_pela_ocupacao_da_eda) > 1e-8) {
    stop("A sobrecarga da alocação realizada não reproduz a da ocupação da EDA.", call. = FALSE)
  }

  registrado <- unlist(registro_otimizacao$verificacao)
  medido <- unlist(verificacao[names(registro_otimizacao$verificacao)])
  faltantes <- setdiff(names(registrado), names(medido))
  if (length(faltantes) > 0) {
    stop("Valor de verificação sem medida: ", paste(faltantes, collapse = ", "), call. = FALSE)
  }

  conferencia <- tibble::tibble(
    grandeza = names(registrado),
    medido = unname(medido[names(registrado)]),
    registrado = unname(registrado)
  ) |>
    dplyr::mutate(confere = abs(medido - registrado) <= tolerancia + 5e-5)

  if (!all(conferencia$confere)) {
    stop(
      "Montagem fora do pré-registrado: ",
      paste(conferencia$grandeza[!conferencia$confere], collapse = "; "),
      ". A wave 4 não resolve um problema diferente do registrado.",
      call. = FALSE
    )
  }
  conferencia
}

#' Tabela do porte do problema, para o texto
#'
#' @param verificacao Lista de `medir_verificacao_da_otimizacao()`.
#' @return tibble formatada para `salvar_tabela()`.
tabelar_porte_da_otimizacao <- function(verificacao) {
  por_nivel <- verificacao$binarias_por_nivel
  restricoes <- verificacao$restricoes_por_tipo
  tibble::tibble(
    Medida = c(
      paste("Variáveis binárias, nível", names(por_nivel)),
      "Variáveis binárias, total",
      "Variáveis contínuas de sobrecarga",
      paste("Restrições,", names(restricoes)),
      "Restrições, total",
      "Coeficientes não nulos"
    ),
    Valor = formatar_inteiro(c(
      unlist(por_nivel), verificacao$binarias, verificacao$variaveis_continuas,
      unlist(restricoes), verificacao$restricoes, verificacao$nao_nulos
    ))
  )
}

#' Tabela da conferência da montagem, para o texto
#'
#' @param conferencia tibble de `conferir_montagem_da_otimizacao()`.
#' @return tibble formatada para `salvar_tabela()`.
tabelar_conferencia_da_otimizacao <- function(conferencia) {
  nomes <- c(
    assentos = "Assentos",
    projetos = "Projetos com assento",
    alocacoes_fixas_ativas_apos_o_corte = "Alocações fixas ativas a partir do corte",
    alocacoes_fixas_so_no_mes_do_corte = "Alocações fixas só no mês do corte, antes dele",
    pares_removidos_por_carga_fixa_no_projeto = "Pares removidos por carga fixa no projeto",
    binarias = "Variáveis binárias",
    fracao_realizados_fora_do_filtro = "Fração dos realizados fora do filtro",
    pares_zona_morta = "Pares da zona morta",
    fracao_pares_zona_morta = "Fração das binárias na zona morta",
    meses_de_consultor_sem_capacidade = "Meses de consultor sem capacidade",
    assentos_realizados_em_mes_sem_capacidade = "Assentos realizados em mês sem capacidade",
    assentos_censurados_no_fim_da_janela = "Assentos censurados no fim da janela",
    assentos_realizados_antes_da_admissao = "Assentos realizados antes da admissão",
    pares_antes_da_admissao = "Pares candidatos antes da admissão",
    sobrecarga_realizada_percentual = "Sobrecarga da alocação realizada, em percentual"
  )
  rotulo <- dplyr::coalesce(
    nomes[conferencia$grandeza],
    sub("^binarias_por_nivel[.]", "Variáveis binárias, nível ", conferencia$grandeza)
  )
  # Contagem sai inteira; fração e sobrecarga, com as quatro casas do registro.
  formatar <- \(x) ifelse(x == round(x), formatar_inteiro(x), formatar_decimal(x, 4))
  tibble::tibble(
    Grandeza = unname(rotulo),
    Medido = formatar(conferencia$medido),
    Registrado = formatar(conferencia$registrado),
    Confere = ifelse(conferencia$confere, "sim", "não")
  )
}
