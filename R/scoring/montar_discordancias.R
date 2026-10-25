#' Ordena a lista de recomendação de um projeto
#'
#' Mesma regra de `acertar_na_lista()`, escrita como ordem explícita para o
#' segundo nível, que precisa saber quem foi recomendado e não só se o
#' realizado entrou: primeiro o conjunto filtrado, pelo escore e, nos
#' empates, pela chave de desempate; depois o resto do quadro, só pela chave.
#'
#' @param projeto `projeto_id`.
#' @param contexto Lista com `consultores`, `projetos`, `filtro`, `chave` e
#'   `lista`.
#' @param escores Matriz consultor por projeto, ou `NULL` para ordem só pela
#'   chave dentro do filtro.
#' @return Vetor com os `lista` primeiros `consultor_id`, em ordem.
ordenar_recomendados <- function(projeto, contexto, escores = NULL) {
  j <- match(projeto, contexto$projetos)
  escore <- if (is.null(escores)) rep(0, length(contexto$consultores)) else escores[, j]
  ordem <- order(!contexto$filtro[, j], -escore, contexto$chave[, j])
  utils::head(contexto$consultores[ordem], contexto$lista)
}

#' Monta as discordâncias de um componente a partir dos pares do teste dele
#'
#' Discordância é o par em que o consultor realizado ficou fora da lista. A
#' data é a de início da alocação realizada, que fixa o mês da comparação.
#'
#' @param pares tibble com `consultor_id`, `projeto_id` e `acerto`.
#' @param alocacoes Alocações do histórico da janela, de onde sai a data.
#' @param contexto,escores Repassados a `ordenar_recomendados()`.
#' @param ramo `"conteúdo"` ou `"colaborativo"`.
#' @param descrever_filtro Função de `projeto_id` para o texto do filtro
#'   aplicado, que entra na rubrica como explicação da arquitetura.
#' @return tibble no formato de `R/eda/analisar_plausibilidade.R`, com a
#'   lista de recomendados em `recomendados`.
montar_discordancias_do_componente <- function(pares, alocacoes, contexto, escores, ramo,
                                               descrever_filtro) {
  inicio <- alocacoes |>
    dplyr::summarise(data_inicio = min(data_inicio), .by = c(consultor_id, projeto_id))

  pares |>
    dplyr::filter(acerto == 0) |>
    dplyr::left_join(inicio, by = c("consultor_id", "projeto_id")) |>
    dplyr::transmute(
      projeto_id, data_inicio,
      consultor_realizado = consultor_id,
      ramo = ramo,
      recomendados = purrr::map(projeto_id, \(p) ordenar_recomendados(p, contexto, escores)),
      filtro = purrr::map_chr(projeto_id, descrever_filtro)
    )
}

#' Monta e classifica as discordâncias dos dois componentes
#'
#' É o segundo nível do protocolo (achado II.7.8), aplicado com as funções
#' do bloco II.7 e com a ordenação que cada componente adotou:
#'
#' * **conteúdo**: os pares de teste da validação cruzada por consultor. A
#'   ordem dentro do filtro ficou ao acaso (W3.2.7), então **o primeiro
#'   recomendado é um sorteio dentro do filtro**, com a chave de desempate;
#' * **colaborativo**: os pares avaliáveis do teste temporal, com a lista
#'   ordenada pela popularidade na estimação (W3.3.2) e quem não tem fator no
#'   fim.
#'
#' A folga de capacidade usa as férias sintéticas, que só cobrem 2026: nos
#' meses da janela anteriores a 2026 a folga é calculada sem férias.
#'
#' @param dados Lista nomeada de tibbles.
#' @param registro Lista de `ler_pre_registro()`.
#' @param avaliacao_conteudo Lista de `avaliar_componente_de_conteudo()`.
#' @param avaliacao_colaborativo Lista de `avaliar_componente_colaborativo()`.
#' @param adocao Lista de `consolidar_adocao()`.
#' @return Lista com `discordancias` (todas, com o recomendado comparado),
#'   `classificadas`, `sem_comparavel` (por ramo), `desconhecidos`,
#'   `sorteio` e `rubrica`.
avaliar_plausibilidade_das_discordancias <- function(dados, registro, avaliacao_conteudo,
                                                     avaliacao_colaborativo, adocao) {
  stopifnot(adocao$conteudo == "acaso", adocao$colaborativo == "popularidade")
  historico <- com_historico_da_janela(dados, janela_do_pre_registro(registro))
  setor_do_projeto <- \(p) historico$projetos$setor[match(p, historico$projetos$projeto_id)]

  contexto_conteudo <- montar_contexto_do_conteudo(historico, registro)
  conteudo <- montar_discordancias_do_componente(
    dplyr::rename(avaliacao_conteudo$pares, acerto = acerto_piso),
    historico$alocacoes, contexto_conteudo, NULL, "conteúdo",
    \(p) paste0("setor ", setor_do_projeto(p), ", ordem sorteada dentro do filtro")
  )

  temporal <- dividir_interacoes(
    historico$alocacoes, "temporal",
    fracao_teste = registro$divisoes$temporal$fracao_teste, semente = registro$sementes$divisao
  )
  contexto_colaborativo <- restringir_lista_a_quem_tem_fator(
    montar_contexto_do_colaborativo(historico, registro), temporal$treino
  )
  colaborativo <- montar_discordancias_do_componente(
    dplyr::rename(avaliacao_colaborativo$pares, acerto = acerto_popularidade),
    historico$alocacoes, contexto_colaborativo,
    escorar_popularidade(temporal$treino, contexto_colaborativo), "colaborativo",
    \(p) "sem filtro, popularidade na estimação"
  )

  sem_historico <- !historico$consultores$consultor_id %in% temporal$treino$consultor_id
  sem_formulario <- !historico$consultores$consultor_id %in% historico$experiencia_setorial$consultor_id
  zona_morta <- historico$consultores$consultor_id[sem_historico & sem_formulario]

  discordancias <- dplyr::bind_rows(conteudo, colaborativo) |>
    dplyr::mutate(
      discordancia_id = sprintf("d%04d", dplyr::row_number()),
      consultor_recomendado = selecionar_recomendado_comparado(
        recomendados, projeto_id, format(data_inicio, "%Y-%m"), historico$alocacoes
      ),
      posicao_popularidade = purrr::map2_int(recomendados, consultor_recomendado, \(lista, r) {
        if (is.na(r)) NA_integer_ else match(r, lista)
      }),
      na_zona_morta = consultor_realizado %in% zona_morta,
      .before = 1
    )

  separadas <- separar_discordancias_comparaveis(discordancias)
  sem_comparavel <- discordancias |>
    dplyr::summarise(
      discordancias = dplyr::n(), sem_comparavel = sum(is.na(consultor_recomendado)), .by = ramo
    )

  classificadas <- separadas$comparaveis |>
    dplyr::select(-recomendados) |>
    medir_criterios_da_discordancia(
      historico, capacidade_mensal_horas = registro$segundo_nivel$capacidade_mensal_horas
    ) |>
    classificar_discordancia_por_dominancia() |>
    dplyr::mutate(antes_das_ferias = format(data_inicio, "%Y") < "2026")

  sorteio <- sortear_casos_para_leitura(
    classificadas,
    por_estrato = registro$segundo_nivel$casos_por_estrato,
    semente = registro$sementes$leitura_qualitativa
  )

  list(
    discordancias = discordancias,
    classificadas = classificadas,
    sem_comparavel = sem_comparavel,
    desconhecidos = classificadas |>
      dplyr::group_by(ramo) |>
      dplyr::group_modify(\(tabela, chave) contar_criterios_desconhecidos(tabela)) |>
      dplyr::ungroup(),
    sorteio = sorteio,
    rubrica = montar_rubrica_de_leitura(sorteio$casos)
  )
}
