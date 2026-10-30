#' Reúne os insumos do problema de otimização sobre o período de teste
#'
#' O exercício é retrospectivo (achado II.8.4): o otimizador redistribui as
#' alocações do período de teste da divisão temporal da wave 3, e a pergunta é
#' se a alocação realizada é dominada. Tudo o que define o problema sai daqui,
#' com a janela, a divisão e a semente de desempate do pré-registro da wave 3
#' e as regras do pré-registro da otimização.
#'
#' @param dados Lista nomeada de tibbles, de `carregar_dados()`, com
#'   `ferias_consultor`.
#' @param registro Lista de `ler_pre_registro()`.
#' @param registro_otimizacao Lista de `ler_pre_registro_otimizacao()`.
#' @return Lista com `corte`, `meses`, `estimacao`, `assentos`,
#'   `meses_dos_assentos`, `carga_fixa`, `capacidade`, `s_hat`,
#'   `elegibilidade`, `pares_removidos_por_carga_fixa_no_projeto`,
#'   `consultores` e `janela`.
preparar_insumos_da_otimizacao <- function(dados, registro, registro_otimizacao) {
  historico <- com_historico_da_janela(dados, janela_do_pre_registro(registro))
  divisao <- dividir_interacoes(
    historico$alocacoes, "temporal",
    fracao_teste = registro$divisoes$temporal$fracao_teste, semente = registro$sementes$divisao
  )
  corte <- min(divisao$teste$data_inicio)
  meses <- listar_meses_de_teste(corte, janela_do_pre_registro(registro))

  assentos <- montar_assentos(divisao$teste, historico$projetos)
  carga_fixa <- montar_carga_fixa(divisao$treino, corte, meses, registro_otimizacao)
  capacidade <- calcular_capacidade(
    historico$consultores, dados$ferias_consultor, meses,
    horas_do_mes = registro_otimizacao$carga$horas_do_mes_cheio
  )
  s_hat <- calcular_s_hat_na_estimacao(historico, divisao$treino, unique(assentos$projeto_id), registro)
  elegibilidade <- montar_elegibilidade(assentos, historico, divisao$treino, s_hat, carga_fixa$alocacoes)
  # A mesma montagem sem a regra da carga fixa no projeto, só para contar os
  # pares que ela remove.
  sem_a_regra <- montar_elegibilidade(assentos, historico, divisao$treino, s_hat, carga_fixa$alocacoes[0, ])

  list(
    corte = corte,
    meses = meses,
    estimacao = divisao$treino,
    assentos = assentos,
    meses_dos_assentos = expandir_assentos_em_meses(assentos, meses),
    carga_fixa = carga_fixa,
    capacidade = capacidade,
    s_hat = s_hat,
    elegibilidade = elegibilidade,
    pares_removidos_por_carga_fixa_no_projeto = nrow(sem_a_regra) - nrow(elegibilidade),
    consultores = historico$consultores,
    janela = janela_do_pre_registro(registro)
  )
}

#' Meses de teste: do mês do corte ao último mês da janela
#'
#' O mês do corte entra inteiro, porque é por mês que a capacidade se mede: um
#' assento que começa no dia 13 divide o mês com o que terminou antes do corte.
#'
#' @param corte Data de corte da divisão temporal.
#' @param janela Vetor de `janela_do_pre_registro()`.
#' @return Vetor de texto `"AAAA-MM"`, em ordem.
listar_meses_de_teste <- function(corte, janela) {
  meses <- seq(lubridate::floor_date(corte, "month"), janela[["fim"]] - 1, by = "month")
  format(lubridate::floor_date(meses, "month"), "%Y-%m")
}

#' Converte cada alocação do período de teste num assento
#'
#' O assento guarda o projeto, as datas censuradas na janela, a carga mensal
#' e o nível do realizado, que é o papel da alocação. O otimizador decide quem
#' o ocupa; as datas e a carga não mudam.
#'
#' @param teste tibble das alocações de teste, já censuradas na janela.
#' @param projetos tibble com `projeto_id` e `setor`.
#' @return tibble com `assento_id`, `projeto_id`, `setor`, `nivel`,
#'   `consultor_realizado`, `data_inicio`, `data_fim`, `pct_alocacao` e
#'   `horas_mes`.
montar_assentos <- function(teste, projetos) {
  teste |>
    dplyr::arrange(projeto_id, data_inicio, consultor_id) |>
    dplyr::transmute(
      assento_id = sprintf("assento_%04d", dplyr::row_number()),
      projeto_id,
      setor = projetos$setor[match(projeto_id, projetos$projeto_id)],
      nivel = papel,
      consultor_realizado = consultor_id,
      data_inicio, data_fim, pct_alocacao, horas_mes
    )
}

#' Expande intervalos em meses e pondera a carga pelos dias ativos
#'
#' É a mesma expansão de `agregar_ocupacao_consultor()`, que gerou a ocupação
#' da EDA: o mês parcial entra na fração de dias cobertos. Usar outra expansão
#' faria a ocupação da alocação realizada divergir da do Capítulo 4.
#'
#' @param tabela tibble com `data_inicio`, `data_fim`, `pct_alocacao` e
#'   `horas_mes`.
#' @param meses Meses a manter.
#' @return tibble com `linha`, `ano_mes`, `carga_pct` e `carga_horas`.
expandir_carga_em_meses <- function(tabela, meses) {
  expandir_para_meses(tabela$data_inicio, tabela$data_fim) |>
    dplyr::filter(ano_mes %in% meses) |>
    dplyr::transmute(
      linha, ano_mes,
      carga_pct = tabela$pct_alocacao[linha] * dias_ativos / dias_no_mes,
      carga_horas = tabela$horas_mes[linha] * dias_ativos / dias_no_mes
    )
}

#' Carga de cada assento em cada mês de teste
#'
#' @param assentos tibble de `montar_assentos()`.
#' @param meses Meses de teste.
#' @return tibble com `assento_id`, `ano_mes`, `carga_pct` e `carga_horas`.
expandir_assentos_em_meses <- function(assentos, meses) {
  carga <- expandir_carga_em_meses(assentos, meses)
  carga |>
    dplyr::transmute(assento_id = assentos$assento_id[linha], ano_mes, carga_pct, carga_horas)
}

#' Carga fixa: alocações iniciadas antes do corte que ocupam os meses de teste
#'
#' Entram como carga, e não como assento: o otimizador não as redistribui.
#' Ficam as que têm algum dia nos meses de teste, inclusive as que terminam no
#' mês do corte antes dele, porque a ocupação da EDA as conta nesse mês.
#'
#' @param estimacao tibble das alocações do período de estimação.
#' @param corte Data de corte.
#' @param meses Meses de teste.
#' @param registro_otimizacao Lista de `ler_pre_registro_otimizacao()`.
#' @return Lista com `alocacoes` (as alocações fixas, com
#'   `ativa_apos_o_corte`) e `por_mes` (carga por consultor e mês).
montar_carga_fixa <- function(estimacao, corte, meses, registro_otimizacao) {
  inicio_dos_meses <- as.Date(paste0(meses[1], "-01"))
  fixas <- estimacao |>
    dplyr::filter(data_fim >= inicio_dos_meses) |>
    dplyr::mutate(ativa_apos_o_corte = data_fim >= corte)

  carga <- expandir_carga_em_meses(fixas, meses)
  por_mes <- carga |>
    dplyr::mutate(consultor_id = fixas$consultor_id[linha]) |>
    dplyr::summarise(
      carga_pct = sum(carga_pct), carga_horas = sum(carga_horas), .by = c(consultor_id, ano_mes)
    )

  list(alocacoes = fixas, por_mes = por_mes)
}

#' Capacidade de cada consultor em cada mês de teste
#'
#' Um menos a fração do mês em férias, com a mesma expansão das férias que a
#' folga do segundo nível da wave 3 usa (`medir_folga_de_capacidade()`). Em
#' horas, a mesma fração do mês cheio.
#'
#' @param consultores tibble com `consultor_id`.
#' @param ferias tibble com `consultor_id`, `inicio_ferias` e `fim_ferias`.
#' @param meses Meses de teste.
#' @param horas_do_mes Horas de um mês cheio.
#' @return tibble com `consultor_id`, `ano_mes`, `capacidade_pct` e
#'   `capacidade_horas`, uma linha por consultor e mês.
calcular_capacidade <- function(consultores, ferias, meses, horas_do_mes) {
  de_ferias <- expandir_para_meses(ferias$inicio_ferias, ferias$fim_ferias) |>
    dplyr::transmute(consultor_id = ferias$consultor_id[linha], ano_mes, fracao = dias_ativos / dias_no_mes) |>
    dplyr::summarise(fracao = min(sum(fracao), 1), .by = c(consultor_id, ano_mes))

  tidyr::expand_grid(consultor_id = consultores$consultor_id, ano_mes = meses) |>
    dplyr::left_join(de_ferias, by = c("consultor_id", "ano_mes")) |>
    dplyr::transmute(
      consultor_id, ano_mes,
      capacidade_pct = 1 - dplyr::coalesce(fracao, 0),
      capacidade_horas = horas_do_mes * capacidade_pct
    )
}

#' Calcula `ŝ` dos projetos de teste só com o período de estimação
#'
#' Mesma montagem do híbrido da wave 3 (`montar_listas_do_hibrido()`): quem
#' tem histórico é ordenado pela popularidade, quem só respondeu aos
#' formulários fica depois, ao acaso dentro do filtro pela chave de desempate,
#' e `ŝ` é o percentil na lista do projeto. A diferença é a contagem: só as
#' alocações da estimação, que é também o que define o quadrante. Nenhuma
#' alocação a partir do corte entra, e um teste confere isso.
#'
#' @param historico Lista com o histórico da janela.
#' @param estimacao tibble das alocações do período de estimação.
#' @param projetos Vetor de `projeto_id` a pontuar.
#' @param registro Lista de `ler_pre_registro()`.
#' @return tibble de `montar_listas_do_hibrido()` com `s_hat`.
calcular_s_hat_na_estimacao <- function(historico, estimacao, projetos, registro) {
  contexto <- montar_contexto_do_hibrido(historico, registro)
  contagem <- as.numeric(table(factor(estimacao$consultor_id, levels = contexto$consultores)))
  popularidade <- matrix(
    contagem, length(contexto$consultores), length(contexto$projetos),
    dimnames = list(contexto$consultores, contexto$projetos)
  )
  montar_listas_do_hibrido(projetos, contexto, popularidade) |>
    dplyr::mutate(s_hat = calcular_percentil_na_lista(posicao, n_candidatos))
}

#' Os candidatos de cada assento, com `ŝ` e o custo por hora
#'
#' Um consultor é candidato a um assento quando é do nível do assento e:
#'
#' * passa pela regra da comutação da wave 3: declarou o setor do projeto, ou
#'   não declarou setor e tem histórico na estimação;
#' * ou é da zona morta (sem histórico na estimação e sem setor declarado),
#'   que entra com `ŝ = 0` por regra de exposição do autor;
#' * ou é o realizado daquele assento, sempre elegível para ele, para a
#'   alocação realizada ser viável.
#'
#' Quem tem alocação fixa no projeto não é candidato aos assentos dele, pela
#' regra de um assento por consultor e projeto, exceto se for o realizado.
#' Fora da lista do modelo, `ŝ` é zero.
#'
#' @param assentos tibble de `montar_assentos()`.
#' @param historico Lista com o histórico da janela.
#' @param estimacao tibble das alocações do período de estimação.
#' @param s_hat tibble de `calcular_s_hat_na_estimacao()`.
#' @param alocacoes_fixas tibble das alocações fixas.
#' @return tibble com `assento_id`, `projeto_id`, `nivel`, `consultor_id`,
#'   `realizado`, `origem` (`lista`, `zona morta` ou `realizado fora da
#'   lista`), `s_hat` e `custo_hora`.
montar_elegibilidade <- function(assentos, historico, estimacao, s_hat, alocacoes_fixas) {
  setor <- setor_predominante_observado(historico$experiencia_setorial)
  consultores <- historico$consultores |>
    dplyr::transmute(
      consultor_id, nivel = nivel_senioridade,
      setor_declarado = setor$segmento[match(consultor_id, setor$consultor_id)],
      tem_historico = consultor_id %in% estimacao$consultor_id,
      custo_hora = historico$custo_carregado$custo_carregado[
        match(consultor_id, historico$custo_carregado$consultor_id)
      ]
    )
  ja_no_projeto <- paste(alocacoes_fixas$consultor_id, alocacoes_fixas$projeto_id)

  assentos |>
    dplyr::select(assento_id, projeto_id, setor, nivel, consultor_realizado) |>
    dplyr::inner_join(consultores, by = "nivel", relationship = "many-to-many") |>
    dplyr::mutate(
      realizado = consultor_id == consultor_realizado,
      zona_morta = is.na(setor_declarado) & !tem_historico,
      passa_filtro = is.na(setor_declarado) | setor_declarado == setor
    ) |>
    dplyr::filter(realizado | (passa_filtro & !paste(consultor_id, projeto_id) %in% ja_no_projeto)) |>
    dplyr::left_join(s_hat |> dplyr::select(projeto_id, consultor_id, s_hat), by = c("projeto_id", "consultor_id")) |>
    dplyr::mutate(
      origem = dplyr::case_when(
        !is.na(s_hat) ~ "lista",
        zona_morta ~ "zona morta",
        .default = "realizado fora da lista"
      ),
      s_hat = dplyr::coalesce(s_hat, 0)
    ) |>
    dplyr::select(assento_id, projeto_id, nivel, consultor_id, realizado, origem, s_hat, custo_hora) |>
    dplyr::arrange(assento_id, consultor_id)
}
