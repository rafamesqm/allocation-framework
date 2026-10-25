#' Teste de McNemar exato e bilateral, sobre acertos pareados
#'
#' É o teste pré-registrado. Só os pares discordantes informam: `b` é o
#' número em que o modelo acerta e o piso erra, `c` o inverso. Sob a
#' hipótese nula, `b` segue uma binomial com `b + c` tentativas e
#' probabilidade 1/2, e o valor-p é o de `binom.test()`. Rejeitar só conta
#' como superar o piso se `b` for maior que `c`.
#'
#' @param acerto_modelo,acerto_piso Vetores de zeros e uns, pareados.
#' @param alfa Nível do teste.
#' @return Lista com `b`, `c`, `valor_p` e `supera`.
testar_mcnemar_exato <- function(acerto_modelo, acerto_piso, alfa = 0.05) {
  stopifnot(length(acerto_modelo) == length(acerto_piso))
  b <- sum(acerto_modelo == 1 & acerto_piso == 0)
  c <- sum(acerto_modelo == 0 & acerto_piso == 1)
  valor_p <- if (b + c == 0) 1 else stats::binom.test(b, b + c, 0.5)$p.value
  list(b = b, c = c, valor_p = valor_p, supera = valor_p <= alfa && b > c)
}

#' Ajusta o mapeamento escolhido num grupo e avalia o grupo reservado
#'
#' A fatoração e o mapa são ajustados só nos outros quatro grupos, com a
#' configuração que a validação interna escolheu para este grupo. Cada par
#' de teste ganha três acertos, todos sob a mesma chave de desempate: o do
#' mapeamento, o do piso de filtro com acaso e o da variante de escore
#' constante do diagnóstico. A variante dá a cada consultor, em todo
#' projeto, a média do seu escore sobre os projetos, e por isso só carrega o
#' que o mapeamento diz do consultor, e nada do projeto.
#'
#' @param contexto Lista de `montar_contexto_do_conteudo()`.
#' @param divisao Lista de `dividir_por_consultor_em_grupos()`.
#' @param grupo Grupo reservado.
#' @param configuracao Linha escolhida, com `fatores`, `lambda`, `iteracoes`
#'   e `lambda_mapeamento`.
#' @param semente Semente da fatoração.
#' @return Lista com `pares` (os pares de teste e os três acertos) e
#'   `entradas` (escore e popularidade de cada candidato filtrado dos
#'   projetos de teste, para a correlação de Spearman).
testar_conteudo_no_grupo <- function(contexto, divisao, grupo, configuracao, semente) {
  alocacoes <- contexto$dados$alocacoes
  grupo_da_linha <- divisao$grupo_do_consultor$grupo[
    match(alocacoes$consultor_id, divisao$grupo_do_consultor$consultor_id)
  ]
  treino <- alocacoes[grupo_da_linha != grupo, ]
  teste <- alocacoes[grupo_da_linha == grupo & alocacoes$consultor_id %in% contexto$com_atributo, ]

  r <- construir_matriz_r(treino, contexto$dados$consultores, contexto$dados$projetos)
  estado <- ajustar_als_implicito(
    r, peso = NULL, fatores = configuracao$fatores, lambda = configuracao$lambda,
    iteracoes = configuracao$iteracoes, semente = semente
  )[[as.character(configuracao$iteracoes)]]
  escores <- escorar_mapeamento(
    estado, contexto, unique(treino$consultor_id), configuracao$lambda_mapeamento
  )
  constante <- matrix(rowMeans(escores), nrow(escores), ncol(escores), dimnames = dimnames(escores))

  pares <- teste |>
    dplyr::select(consultor_id, projeto_id) |>
    dplyr::mutate(
      grupo = grupo,
      passa_filtro = contexto$filtro[cbind(
        match(consultor_id, contexto$consultores), match(projeto_id, contexto$projetos)
      )],
      acerto_modelo = acertar_na_lista(teste, contexto, escores),
      acerto_piso = acertar_na_lista(teste, contexto),
      acerto_constante = acertar_na_lista(teste, contexto, constante)
    )

  popularidade <- as.numeric(table(factor(treino$consultor_id, levels = contexto$consultores)))
  colunas <- match(unique(teste$projeto_id), contexto$projetos)
  candidatos <- which(contexto$filtro[, colunas, drop = FALSE], arr.ind = TRUE)
  entradas <- tibble::tibble(
    grupo = grupo,
    escore = escores[cbind(candidatos[, 1], colunas[candidatos[, 2]])],
    popularidade = popularidade[candidatos[, 1]]
  )

  list(pares = pares, entradas = entradas)
}

#' Avalia o componente de conteúdo inteiro numa realização
#'
#' Escolhe os hiperparâmetros na validação interna, a menos que a escolha
#' venha pronta, testa cada grupo reservado com a sua escolha, reúne os
#' cinco grupos e roda o McNemar exato do mapeamento contra o filtro com
#' acaso. Nas realizações a escolha é sempre refeita: nenhum valor da
#' realização versionada é reaproveitado.
#'
#' @param dados Lista nomeada de tibbles.
#' @param registro Lista de `ler_pre_registro()`.
#' @param selecao Lista de `selecionar_hiperparametros_do_conteudo()`, ou
#'   `NULL` para refazê-la.
#' @return Lista com `selecao`, `pares`, `entradas`, `teste`, `spearman`
#'   (por grupo e reunida) e `piso_na_validacao`.
avaliar_componente_de_conteudo <- function(dados, registro, selecao = NULL) {
  if (is.null(selecao)) {
    selecao <- selecionar_hiperparametros_do_conteudo(dados, registro)
  }
  historico <- com_historico_da_janela(dados, janela_do_pre_registro(registro))
  contexto <- montar_contexto_do_conteudo(historico, registro)
  divisao <- dividir_por_consultor_em_grupos(
    historico$alocacoes, n_grupos = registro$divisoes$por_consultor$grupos,
    semente = registro$sementes$divisao, universo = contexto$consultores
  )

  por_grupo <- purrr::map(selecao$escolhidos$grupo, function(g) {
    testar_conteudo_no_grupo(
      contexto, divisao, g, selecao$escolhidos[selecao$escolhidos$grupo == g, ],
      registro$sementes$fatoracao
    )
  })
  pares <- purrr::list_rbind(purrr::map(por_grupo, "pares"))
  entradas <- purrr::list_rbind(purrr::map(por_grupo, "entradas"))

  spearman <- dplyr::bind_rows(
    entradas |>
      dplyr::summarise(rho = stats::cor(escore, popularidade, method = "spearman"), .by = grupo) |>
      dplyr::mutate(grupo = as.character(grupo)),
    tibble::tibble(grupo = "reunida", rho = stats::cor(entradas$escore, entradas$popularidade, method = "spearman"))
  )

  # O piso nos pares de validação de cada grupo, que são os de fora dele, só
  # para ler o quanto a melhor configuração da grade passa do piso dentro da
  # estimação.
  grupo_da_linha <- divisao$grupo_do_consultor$grupo[
    match(historico$alocacoes$consultor_id, divisao$grupo_do_consultor$consultor_id)
  ]
  piso_na_validacao <- purrr::map_dbl(selecao$escolhidos$grupo, function(g) {
    fora <- historico$alocacoes[grupo_da_linha != g & historico$alocacoes$consultor_id %in% contexto$com_atributo, ]
    mean(acertar_na_lista(fora, contexto))
  })

  list(
    selecao = selecao,
    pares = pares,
    entradas = entradas,
    teste = testar_mcnemar_exato(pares$acerto_modelo, pares$acerto_piso, registro$teste$nivel),
    spearman = spearman,
    piso_na_validacao = tibble::tibble(grupo = selecao$escolhidos$grupo, piso = piso_na_validacao),
    lista = contexto$lista,
    n_quadro = length(contexto$consultores)
  )
}

#' Resume uma avaliação do conteúdo numa linha de grandezas
#'
#' A mesma função resume a realização versionada e cada réplica, para ponto
#' e intervalo saírem da mesma definição, como no bloco I.0.
#'
#' @param avaliacao Lista de `avaliar_componente_de_conteudo()`.
#' @param registro Lista de `ler_pre_registro()`.
#' @return tibble de uma linha.
resumir_avaliacao_do_conteudo <- function(avaliacao, registro) {
  pares <- avaliacao$pares
  grade <- avaliacao$selecao$grade
  escolhidos <- avaliacao$selecao$escolhidos
  piso <- mean(pares$acerto_piso)
  n <- nrow(pares)
  ganho <- function(cenario) {
    calcular_ganho_minimo_detectavel(
      n, piso, cenario, alfa = registro$teste$nivel, poder = registro$teste$ganho_minimo$poder
    )
  }

  superficie <- grade |>
    dplyr::summarise(
      maximo = max(acerto), mediana = stats::median(acerto),
      empatadas = sum(acerto == max(acerto)), .by = grupo
    ) |>
    dplyr::left_join(avaliacao$piso_na_validacao, by = "grupo")

  tibble::tibble(
    n_pares_conteudo = n,
    acerto_mapeamento = mean(pares$acerto_modelo),
    acerto_piso_filtro = piso,
    diferenca_mapeamento = mean(pares$acerto_modelo) - piso,
    discordantes_a_favor = avaliacao$teste$b,
    discordantes_contra = avaliacao$teste$c,
    valor_p_mcnemar = avaliacao$teste$valor_p,
    supera_piso = as.numeric(avaliacao$teste$supera),
    ganho_minimo_dominancia = ganho("dominancia"),
    ganho_minimo_independencia = ganho("independencia"),
    acerto_constante = mean(pares$acerto_constante),
    diferenca_constante = mean(pares$acerto_constante) - piso,
    spearman_reunida = avaliacao$spearman$rho[avaliacao$spearman$grupo == "reunida"],
    acaso_puro = avaliacao$lista / avaliacao$n_quadro,
    fracao_passa_filtro = mean(pares$passa_filtro),
    grade_maximo_menos_mediana = mean(superficie$maximo - superficie$mediana),
    grade_maximo_menos_piso = mean(superficie$maximo - superficie$piso),
    grade_mediana_menos_piso = mean(superficie$mediana - superficie$piso),
    grade_empatadas_no_maximo = mean(superficie$empatadas),
    escolha_fatores_distintos = dplyr::n_distinct(escolhidos$fatores),
    escolha_lambda_mapa_distintos = dplyr::n_distinct(escolhidos$lambda_mapeamento),
    escolha_configuracoes_distintas = nrow(dplyr::distinct(
      escolhidos, fatores, lambda, iteracoes, lambda_mapeamento
    ))
  )
}

#' Refaz a avaliação do conteúdo inteira sobre realizações do gerador
#'
#' Cada realização é gerada do zero e passa pelo mesmo procedimento da
#' versionada, inclusive a escolha de hiperparâmetros por validação interna.
#'
#' @param sementes Vetor de sementes das realizações.
#' @param registro Lista de `ler_pre_registro()`.
#' @param ... Repassado a `gerar_realizacao()`.
#' @return tibble com uma linha por realização.
simular_avaliacao_do_conteudo <- function(sementes, registro, ...) {
  purrr::map(sementes, function(semente) {
    avaliacao <- avaliar_componente_de_conteudo(gerar_realizacao(semente, ...), registro)
    dplyr::bind_cols(tibble::tibble(semente = semente), resumir_avaliacao_do_conteudo(avaliacao, registro))
  }) |>
    purrr::list_rbind()
}

#' Confere a escolha da pipeline contra a registrada na emenda
#'
#' O teste só roda com os hiperparâmetros que a emenda de 2026-10-24
#' registrou antes dele. Se a escolha mudar, a pipeline para.
#'
#' @param selecao Lista de `selecionar_hiperparametros_do_conteudo()`.
#' @param registro Lista de `ler_pre_registro()`.
#' @return `selecao`, inalterada. Erro se a escolha divergir.
conferir_escolha_registrada <- function(selecao, registro) {
  # A emenda é achada pela chave, e não pela posição na lista: uma emenda
  # nova inserida antes não pode desarmar esta trava em silêncio.
  chave <- "hiperparametros_do_conteudo_na_realizacao_versionada"
  emenda <- purrr::detect(registro$emendas, \(e) !is.null(e[[chave]]))
  if (is.null(emenda)) stop("Nenhuma emenda registra a escolha do conteúdo.", call. = FALSE)
  registrada <- purrr::map(emenda[[chave]], tibble::as_tibble) |>
    purrr::list_rbind() |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric))
  escolhida <- selecao$escolhidos |>
    dplyr::select(grupo, fatores, lambda, iteracoes, lambda_mapeamento) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric))

  if (!isTRUE(all.equal(as.data.frame(escolhida), as.data.frame(registrada), check.attributes = FALSE))) {
    stop("A escolha de hiperparâmetros difere da registrada na emenda.", call. = FALSE)
  }
  selecao
}
