# Emenda 2 do conteúdo: previsões cruzadas do mapa de atributos para fatores.
#
# Sob a emenda 1, os candidatos do filtro com histórico na estimação eram
# pontuados pela previsão do mapa dentro da amostra, e o consultor de teste
# pela previsão fora dela. Com pouca regularização do mapa, a previsão dentro
# da amostra quase reproduz o fator ajustado pelo histórico, e os candidatos
# não ficam na mesma escala. Aqui, todo candidato é pontuado fora da amostra:
# quem está na estimação, por um mapa ajustado sem o seu subgrupo; quem não
# está, pelo mapa ajustado com toda a estimação.
#
# As funções da emenda 1 ficam intocadas em `avaliar_conteudo.R` e
# `testar_conteudo.R`, para o resultado dela ser preservado e reportado.

#' Divide um conjunto de consultores em subgrupos fixos
#'
#' Mesma regra dos grupos da validação cruzada por consultor
#' (`dividir_por_consultor_em_grupos()`): identificadores em ordem e uma
#' sequência `1, 2, ..., n, 1, 2, ...` embaralhada pela semente. A atribuição
#' não depende da ordem das linhas de nenhuma tabela.
#'
#' @param ids Vetor de `consultor_id`.
#' @param n_subgrupos Número de subgrupos.
#' @param semente Semente de divisão.
#' @return Vetor inteiro com o subgrupo de cada `consultor_id`, nomeado por
#'   ele, na ordem dos identificadores.
atribuir_subgrupos <- function(ids, n_subgrupos, semente) {
  ids <- sort(unique(ids))
  subgrupo <- withr::with_seed(semente, sample(rep_len(seq_len(n_subgrupos), length(ids))))
  stats::setNames(subgrupo, ids)
}

#' Prevê os fatores de todos os respondentes por previsões cruzadas
#'
#' Quem está na estimação recebe a previsão de um mapa ajustado sem o seu
#' subgrupo. Quem não está, como o consultor de teste, recebe a previsão do
#' mapa ajustado com toda a estimação. Nenhuma previsão sai de um mapa que
#' viu o fator do próprio consultor.
#'
#' @param atributos Matriz de `construir_atributos_declarados()`.
#' @param fatores Fatores dos consultores, da fatoração da estimação.
#' @param com_historico Consultores com alocação na estimação.
#' @param universo_estimacao Consultores da estimação, isto é, do quadro fora
#'   dos grupos reservados.
#' @param lambda Regularização do mapa.
#' @param n_subgrupos,semente Repassados a `atribuir_subgrupos()`.
#' @return Lista com `previstos` (matriz de fatores previstos fora da
#'   amostra, uma linha por respondente) e `dentro_da_amostra` (a previsão do
#'   mapa com toda a estimação, para o diagnóstico do R²).
prever_fatores_cruzados <- function(atributos, fatores, com_historico, universo_estimacao,
                                    lambda, n_subgrupos, semente) {
  completo <- prever_fatores(ajustar_mapeamento(atributos, fatores, com_historico, lambda), atributos)
  previstos <- completo

  subgrupos <- atribuir_subgrupos(universo_estimacao, n_subgrupos, semente)
  for (s in seq_len(n_subgrupos)) {
    deste <- names(subgrupos)[subgrupos == s]
    alvo <- intersect(rownames(atributos), deste)
    if (length(alvo) == 0) next
    mapa <- ajustar_mapeamento(atributos, fatores, setdiff(com_historico, deste), lambda)
    previstos[alvo, ] <- prever_fatores(mapa, atributos[alvo, , drop = FALSE])
  }

  list(previstos = previstos, dentro_da_amostra = completo)
}

#' Pontua todos os respondentes por previsões cruzadas
#'
#' @param estado Elemento de `ajustar_als_implicito()`.
#' @param contexto Lista de `montar_contexto_do_conteudo()`.
#' @param com_historico,universo_estimacao,lambda_mapeamento Repassados a
#'   `prever_fatores_cruzados()`.
#' @param registro Lista de `ler_pre_registro()`.
#' @return Matriz consultor por projeto, com as dimensões do contexto.
escorar_mapeamento_cruzado <- function(estado, contexto, com_historico, universo_estimacao,
                                       lambda_mapeamento, registro) {
  previsao <- prever_fatores_cruzados(
    contexto$atributos, estado$x, com_historico, universo_estimacao, lambda_mapeamento,
    n_subgrupos = emenda_2_do_conteudo(registro)$previsoes_cruzadas$subgrupos,
    semente = registro$sementes$divisao
  )
  escores <- matrix(
    0, length(contexto$consultores), length(contexto$projetos),
    dimnames = list(contexto$consultores, contexto$projetos)
  )
  escores[rownames(previsao$previstos), ] <- previsao$previstos %*% t(estado$y)
  escores
}

#' Devolve a emenda 2 do conteúdo, achada pelo nome
#'
#' @param registro Lista de `ler_pre_registro()`.
#' @return A emenda. Erro se ela não existir.
emenda_2_do_conteudo <- function(registro) {
  emenda <- purrr::detect(registro$emendas, \(e) identical(e$nome, "emenda 2 do conteúdo"))
  if (is.null(emenda)) stop("A emenda 2 do conteúdo não está no pré-registro.", call. = FALSE)
  emenda
}

#' Consultores da estimação: o quadro fora dos grupos reservados
#'
#' @param divisao Lista de `dividir_por_consultor_em_grupos()`.
#' @param reservados Grupos fora da estimação.
#' @return Vetor de `consultor_id`.
consultores_da_estimacao <- function(divisao, reservados) {
  grupos <- divisao$grupo_do_consultor
  grupos$consultor_id[!grupos$grupo %in% reservados]
}

#' Mede o acerto de toda configuração da grade, com previsões cruzadas
#'
#' Igual a `medir_grade_do_mapeamento()`, da emenda 1, com a pontuação
#' trocada pela cruzada.
#'
#' @param treino Alocações de estimação.
#' @param validacao Pares a avaliar.
#' @param contexto Lista de `montar_contexto_do_conteudo()`.
#' @param universo_estimacao Consultores da estimação.
#' @param registro Lista de `ler_pre_registro()`.
#' @return tibble com uma linha por configuração, com `acertos` e `pares`.
medir_grade_do_mapeamento_cruzado <- function(treino, validacao, contexto, universo_estimacao, registro) {
  grade <- registro$conteudo$ordenacao$grade
  dados <- contexto$dados
  r <- construir_matriz_r(treino, dados$consultores, dados$projetos)
  com_historico <- unique(treino$consultor_id)

  linhas <- list()
  for (k in grade$fatores) {
    for (lambda in grade$lambda) {
      estados <- ajustar_als_implicito(
        r, peso = NULL, fatores = k, lambda = lambda,
        iteracoes = max(grade$iteracoes), semente = registro$sementes$fatoracao,
        registrar_em = grade$iteracoes
      )
      for (iteracoes in grade$iteracoes) {
        for (lambda_mapa in grade$lambda_mapeamento) {
          escores <- escorar_mapeamento_cruzado(
            estados[[as.character(iteracoes)]], contexto, com_historico,
            universo_estimacao, lambda_mapa, registro
          )
          linhas[[length(linhas) + 1]] <- tibble::tibble(
            fatores = k, lambda = lambda, iteracoes = iteracoes,
            lambda_mapeamento = lambda_mapa,
            acertos = sum(acertar_na_lista(validacao, contexto, escores)),
            pares = nrow(validacao)
          )
        }
      }
    }
  }
  purrr::list_rbind(linhas)
}

#' Escolhe os hiperparâmetros do conteúdo sob a emenda 2
#'
#' Mesma validação interna da emenda 1, em rodízio dos quatro grupos de
#' estimação, com a pontuação cruzada: a validação interna tinha o mesmo
#' defeito. Os grupos reservados ao teste não são tocados.
#'
#' @param dados Lista nomeada de tibbles.
#' @param registro Lista de `ler_pre_registro()`.
#' @return Lista com `grade` e `escolhidos`, como em
#'   `selecionar_hiperparametros_do_conteudo()`.
selecionar_hiperparametros_do_conteudo_cruzado <- function(dados, registro) {
  historico <- com_historico_da_janela(dados, janela_do_pre_registro(registro))
  contexto <- montar_contexto_do_conteudo(historico, registro)
  divisao <- dividir_por_consultor_em_grupos(
    historico$alocacoes, n_grupos = registro$divisoes$por_consultor$grupos,
    semente = registro$sementes$divisao, universo = contexto$consultores
  )
  alocacoes <- historico$alocacoes
  grupo_da_linha <- divisao$grupo_do_consultor$grupo[
    match(alocacoes$consultor_id, divisao$grupo_do_consultor$consultor_id)
  ]
  grupos <- sort(unique(divisao$grupo_do_consultor$grupo))

  grade <- purrr::map(grupos, function(g) {
    purrr::map(setdiff(grupos, g), function(h) {
      treino <- alocacoes[!grupo_da_linha %in% c(g, h), ]
      validacao <- alocacoes[grupo_da_linha == h & alocacoes$consultor_id %in% contexto$com_atributo, ]
      medir_grade_do_mapeamento_cruzado(
        treino, validacao, contexto, consultores_da_estimacao(divisao, c(g, h)), registro
      )
    }) |>
      purrr::list_rbind() |>
      dplyr::summarise(
        acertos = sum(acertos), pares = sum(pares),
        .by = c(fatores, lambda, iteracoes, lambda_mapeamento)
      ) |>
      dplyr::mutate(acerto = acertos / pares, grupo = g, .before = 1)
  }) |>
    purrr::list_rbind()

  list(
    grade = grade,
    escolhidos = grade |>
      dplyr::group_by(grupo) |>
      dplyr::group_modify(\(validacao, chave) escolher_configuracao_do_conteudo(validacao)) |>
      dplyr::ungroup()
  )
}
