#' Semente das férias de uma realização, derivada da semente dela
#'
#' Fica longe das sementes das realizações, de 1 a 100, para as férias nunca
#' reaproveitarem o fluxo aleatório de outra realização.
#'
#' @param semente Semente da realização.
#' @return Inteiro.
derivar_semente_de_ferias <- function(semente) {
  1000000L + as.integer(semente)
}

#' Gera uma realização com férias, sem mexer no resto do sorteio
#'
#' `gerar_realizacao()` não gera férias, e a folga de capacidade do segundo
#' nível precisa delas. As férias saem depois de tudo, com semente própria
#' derivada da semente da realização e com o estado aleatório preservado:
#' as demais tabelas são idênticas às de `gerar_realizacao()` com a mesma
#' semente, o que um teste confere.
#'
#' @param semente Semente da realização.
#' @param dias_ferias,ano_ferias Repassados a `gerar_ferias_consultor()`.
#' @param ... Repassado a `gerar_realizacao()`.
#' @return Lista de `gerar_realizacao()` com `ferias_consultor`.
gerar_realizacao_com_ferias <- function(semente, dias_ferias = 30, ano_ferias = 2026, ...) {
  dados <- gerar_realizacao(semente, ...)
  dados$ferias_consultor <- withr::with_preserve_seed(gerar_ferias_consultor(
    dados$consultores, dias_ferias = dias_ferias, ano_referencia = ano_ferias,
    semente = derivar_semente_de_ferias(semente)
  ))
  dados
}

#' Mede o segundo nível e o híbrido de uma realização, com a adoção fixada
#'
#' Com a popularidade no colaborativo e o acaso no conteúdo, as listas não
#' dependem de nenhum modelo ajustado: o conteúdo ordena o filtro pela chave
#' de desempate, e o colaborativo pela contagem na estimação. Por isso os
#' acertos que definem as discordâncias saem direto das divisões, sem refazer
#' a escolha de hiperparâmetros. A adoção é a da realização versionada, e
#' não é decidida de novo em cada realização: o que aqui varia é a medida do
#' modelo adotado, e a estabilidade da decisão está nas frequências de
#' rejeição das Etapas 2 e 3.
#'
#' @param dados Lista nomeada de tibbles, com `ferias_consultor`.
#' @param registro Lista de `ler_pre_registro()`.
#' @param adocao Lista de `consolidar_adocao()`.
#' @return tibble de uma linha com as proporções de dominância por ramo, a
#'   concentração do recomendado, a folga e o híbrido por quadrante.
medir_segundo_nivel <- function(dados, registro, adocao) {
  historico <- com_historico_da_janela(dados, janela_do_pre_registro(registro))

  contexto_conteudo <- montar_contexto_do_conteudo(historico, registro)
  grupos <- dividir_por_consultor_em_grupos(
    historico$alocacoes, n_grupos = registro$divisoes$por_consultor$grupos,
    semente = registro$sementes$divisao, universo = contexto_conteudo$consultores
  )
  pares_conteudo <- grupos$teste |>
    dplyr::filter(consultor_id %in% contexto_conteudo$com_atributo) |>
    dplyr::select(consultor_id, projeto_id)
  pares_conteudo$acerto_piso <- acertar_na_lista(pares_conteudo, contexto_conteudo)

  temporal <- dividir_interacoes(
    historico$alocacoes, "temporal",
    fracao_teste = registro$divisoes$temporal$fracao_teste, semente = registro$sementes$divisao
  )
  contexto_colaborativo <- restringir_lista_a_quem_tem_fator(
    montar_contexto_do_colaborativo(historico, registro), temporal$treino
  )
  pares_colaborativo <- selecionar_pares_avaliaveis(temporal$treino, temporal$teste) |>
    dplyr::select(consultor_id, projeto_id)
  pares_colaborativo$acerto_popularidade <- acertar_na_lista(
    pares_colaborativo, contexto_colaborativo, escorar_popularidade(temporal$treino, contexto_colaborativo)
  )

  plausibilidade <- avaliar_plausibilidade_das_discordancias(
    dados, registro, list(pares = pares_conteudo), list(pares = pares_colaborativo), adocao
  )
  pares_do_hibrido <- avaliar_hibrido(dados, registro, adocao)$pares
  hibrido <- resumir_hibrido_por_quadrante(pares_do_hibrido)

  classificadas <- plausibilidade$classificadas
  fracao_da_classe <- function(ramo, classe) {
    do_ramo <- classificadas$classe[classificadas$ramo == ramo]
    if (length(do_ramo) == 0) NA_real_ else mean(do_ramo == classe)
  }
  colaborativo <- classificadas[classificadas$ramo == "colaborativo", ]
  hibrido_de <- \(q, coluna) hibrido[[coluna]][hibrido$quadrante == q]

  tibble::tibble(
    discordancias_conteudo = sum(classificadas$ramo == "conteúdo"),
    discordancias_colaborativo = nrow(colaborativo),
    conteudo_recomendado_melhor = fracao_da_classe("conteúdo", "recomendado melhor"),
    conteudo_recomendado_pior = fracao_da_classe("conteúdo", "recomendado pior"),
    conteudo_incomparaveis = fracao_da_classe("conteúdo", "incomparáveis"),
    colaborativo_recomendado_melhor = fracao_da_classe("colaborativo", "recomendado melhor"),
    colaborativo_recomendado_pior = fracao_da_classe("colaborativo", "recomendado pior"),
    colaborativo_incomparaveis = fracao_da_classe("colaborativo", "incomparáveis"),
    sem_comparavel = sum(plausibilidade$sem_comparavel$sem_comparavel),
    desempates_por_tempo_de_casa = sum(classificadas$desempate_por_tempo_de_casa),
    colaborativo_setor_desconhecido = mean(is.na(colaborativo$setor_recomendado) |
      colaborativo$setor_recomendado == "desconhecido" | colaborativo$setor_realizado == "desconhecido"),
    colaborativo_recomendado_mais_frequente = max(table(colaborativo$consultor_recomendado)) / nrow(colaborativo),
    colaborativo_realizado_com_mais_folga = mean(colaborativo$folga_realizado > colaborativo$folga_recomendado),
    hibrido_acerto = hibrido_de("todos", "acerto_hibrido"),
    hibrido_menos_piso_composto = hibrido_de("todos", "acerto_hibrido") - hibrido_de("todos", "acerto_piso"),
    hibrido_acerto_historico_e_formulario = hibrido_de("histórico e formulário", "acerto_hibrido"),
    hibrido_acerto_so_historico = hibrido_de("só histórico", "acerto_hibrido"),
    hibrido_fracao_so_formulario = hibrido_de("só formulário", "fracao"),
    hibrido_fracao_zona_morta = hibrido_de("zona morta", "fracao")
  ) |>
    dplyr::bind_cols(resumir_acertos_em_listas(
      pares_do_hibrido, c("acerto_hibrido", "acerto_piso_composto"), registro$metrica$listas_descritivas
    ))
}

#' Refaz o segundo nível e o híbrido sobre realizações do gerador
#'
#' @param sementes Vetor de sementes das realizações.
#' @param registro Lista de `ler_pre_registro()`.
#' @param adocao Lista de `consolidar_adocao()`.
#' @param ... Repassado a `gerar_realizacao_com_ferias()`.
#' @return tibble com uma linha por realização.
simular_segundo_nivel <- function(sementes, registro, adocao, ...) {
  purrr::map(sementes, function(semente) {
    dplyr::bind_cols(
      tibble::tibble(semente = semente),
      medir_segundo_nivel(gerar_realizacao_com_ferias(semente, ...), registro, adocao)
    )
  }) |>
    purrr::list_rbind()
}
