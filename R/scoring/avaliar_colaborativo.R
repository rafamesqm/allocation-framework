#' Reúne o que toda avaliação do colaborativo usa e não muda entre ajustes
#'
#' O colaborativo é avaliado sem filtro de setor, como o piso do bloco II.7:
#' a lista é o quadro inteiro, ordenado pelo escore e, nos empates, pela
#' chave de desempate. Por isso o filtro aqui deixa passar todo mundo, e a
#' mesma `acertar_na_lista()` do conteúdo serve sem mudança. A chave é a
#' mesma do conteúdo, sorteada com a semente de desempate do pré-registro.
#'
#' @param dados Lista com o histórico da janela, de `com_historico_da_janela()`.
#' @param registro Lista de `ler_pre_registro()`.
#' @return Lista com `dados`, `consultores`, `projetos`, `filtro`, `chave` e
#'   `lista`.
montar_contexto_do_colaborativo <- function(dados, registro) {
  consultores <- dados$consultores$consultor_id
  projetos <- dados$projetos$projeto_id
  dimensoes <- list(consultores, projetos)

  list(
    dados = dados,
    consultores = consultores,
    projetos = projetos,
    filtro = matrix(TRUE, length(consultores), length(projetos), dimnames = dimensoes),
    chave = withr::with_seed(registro$sementes$desempate, matrix(
      stats::runif(length(consultores) * length(projetos)),
      length(consultores), length(projetos), dimnames = dimensoes
    )),
    lista = registro$metrica$lista_decisoria
  )
}

#' Manda ao fim da lista quem não tem fator na estimação
#'
#' Emenda de 2026-10-24: consultor sem alocação na estimação tem fator nulo
#' e escore zero, que ficaria à frente de quem tem escore negativo só por
#' esse artefato. Ele passa a ir para o fim da lista, em ordem aleatória pela
#' chave de desempate, como no piso de popularidade, em que a contagem zero
#' já o põe lá. O mecanismo é o do filtro: quem fica fora dele entra depois
#' dos demais, ordenado só pela chave.
#'
#' @param contexto Lista de `montar_contexto_do_colaborativo()`.
#' @param treino Alocações de estimação.
#' @return `contexto` com o filtro fechado para quem não tem fator.
restringir_lista_a_quem_tem_fator <- function(contexto, treino) {
  contexto$filtro[!contexto$consultores %in% treino$consultor_id, ] <- FALSE
  contexto
}

#' Separa os pares de teste que o colaborativo consegue pontuar
#'
#' O colaborativo só tem vetor para quem apareceu na estimação: consultor e
#' projeto precisam estar nela. Os demais pares não são avaliáveis por ele, e
#' a fração deles é reportada à parte.
#'
#' @param treino,teste Alocações de estimação e de teste.
#' @return `teste` restrito aos pares avaliáveis.
selecionar_pares_avaliaveis <- function(treino, teste) {
  teste[teste$consultor_id %in% treino$consultor_id & teste$projeto_id %in% treino$projeto_id, ]
}

#' Escores de popularidade na estimação, iguais em todo projeto
#'
#' @param treino Alocações de estimação.
#' @param contexto Lista de `montar_contexto_do_colaborativo()`.
#' @return Matriz consultor por projeto com o número de alocações de cada
#'   consultor na estimação.
escorar_popularidade <- function(treino, contexto) {
  contagem <- as.numeric(table(factor(treino$consultor_id, levels = contexto$consultores)))
  matrix(
    contagem, length(contexto$consultores), length(contexto$projetos),
    dimnames = list(contexto$consultores, contexto$projetos)
  )
}

#' Mede o acerto de toda configuração da fatoração sobre treino e validação
#'
#' A fatoração é ajustada uma vez por número de fatores e regularização, e
#' passa pelo estado de 10 iterações a caminho do de 20.
#'
#' @param treino Alocações de estimação.
#' @param validacao Pares avaliáveis a pontuar.
#' @param contexto Lista de `montar_contexto_do_colaborativo()`.
#' @param grade Lista com `fatores`, `lambda` e `iteracoes`.
#' @param alfa Peso do logaritmo das horas na confiança.
#' @param semente Semente da fatoração.
#' @return tibble com uma linha por configuração, com `acertos` e `pares`.
medir_grade_do_colaborativo <- function(treino, validacao, contexto, grade, alfa, semente) {
  dados <- contexto$dados
  r <- construir_matriz_r(
    dplyr::distinct(treino, consultor_id, projeto_id), dados$consultores, dados$projetos
  )
  peso <- construir_peso_de_confianca(treino, dados$consultores, dados$projetos, alfa)
  contexto <- restringir_lista_a_quem_tem_fator(contexto, treino)

  linhas <- list()
  for (k in grade$fatores) {
    for (lambda in grade$lambda) {
      estados <- ajustar_als_implicito(
        r, peso = peso, fatores = k, lambda = lambda,
        iteracoes = max(grade$iteracoes), semente = semente, registrar_em = grade$iteracoes
      )
      for (iteracoes in grade$iteracoes) {
        estado <- estados[[as.character(iteracoes)]]
        linhas[[length(linhas) + 1]] <- tibble::tibble(
          alfa = alfa, fatores = k, lambda = lambda, iteracoes = iteracoes,
          acertos = sum(acertar_na_lista(validacao, contexto, estado$x %*% t(estado$y))),
          pares = nrow(validacao)
        )
      }
    }
  }
  purrr::list_rbind(linhas)
}

#' Escolhe a configuração de maior acerto, com o desempate pré-registrado
#'
#' Empate no acerto vai para o modelo mais parcimonioso: menos fatores, maior
#' regularização, menos iterações.
#'
#' @param validacao tibble de `medir_grade_do_colaborativo()`.
#' @return A linha escolhida.
escolher_configuracao_do_colaborativo <- function(validacao) {
  validacao |>
    dplyr::mutate(acerto = acertos / pares) |>
    dplyr::arrange(dplyr::desc(acerto), fatores, dplyr::desc(lambda), iteracoes) |>
    dplyr::slice(1)
}

#' Escolhe os hiperparâmetros do colaborativo na validação temporal interna
#'
#' A estimação é o período anterior ao corte da separação temporal. Dentro
#' dela, a mesma regra de corte separa uma validação, e a escolha é feita nos
#' pares avaliáveis dessa validação, para cada alfa em separado. O teste fica
#' intocado.
#'
#' @param dados Lista nomeada de tibbles.
#' @param registro Lista de `ler_pre_registro()`.
#' @return Lista com `grade`, `escolhidos` (uma linha por alfa) e `validacao`
#'   (pares, avaliáveis e o piso de popularidade na validação).
selecionar_hiperparametros_do_colaborativo <- function(dados, registro) {
  historico <- com_historico_da_janela(dados, janela_do_pre_registro(registro))
  contexto <- montar_contexto_do_colaborativo(historico, registro)
  externa <- dividir_interacoes(
    historico$alocacoes, "temporal",
    fracao_teste = registro$divisoes$temporal$fracao_teste, semente = registro$sementes$divisao
  )
  interna <- dividir_interacoes(
    externa$treino, "temporal",
    fracao_teste = registro$divisoes$temporal$fracao_validacao, semente = registro$sementes$divisao
  )
  validacao <- selecionar_pares_avaliaveis(interna$treino, interna$teste)

  grade <- purrr::map(registro$colaborativo$alfas, function(alfa) {
    medir_grade_do_colaborativo(
      interna$treino, validacao, contexto, registro$colaborativo$grade,
      alfa, registro$sementes$fatoracao
    )
  }) |>
    purrr::list_rbind() |>
    dplyr::mutate(acerto = acertos / pares)

  list(
    grade = grade,
    escolhidos = grade |>
      dplyr::group_by(alfa) |>
      dplyr::group_modify(\(validacao, chave) escolher_configuracao_do_colaborativo(validacao)) |>
      dplyr::ungroup(),
    validacao = tibble::tibble(
      pares = nrow(interna$teste),
      avaliaveis = nrow(validacao),
      corte = min(interna$teste$data_inicio),
      piso_popularidade = mean(acertar_na_lista(
        validacao, restringir_lista_a_quem_tem_fator(contexto, interna$treino),
        escorar_popularidade(interna$treino, contexto)
      ))
    )
  )
}

#' Monta a tabela dos hiperparâmetros escolhidos para cada alfa
#'
#' @param selecao Lista de `selecionar_hiperparametros_do_colaborativo()`.
#' @return tibble formatada para `salvar_tabela()`.
tabelar_hiperparametros_do_colaborativo <- function(selecao) {
  empatadas <- selecao$grade |>
    dplyr::summarise(
      empatadas = sum(acerto == max(acerto)), configuracoes = dplyr::n(),
      mediana = stats::median(acerto), .by = alfa
    )

  selecao$escolhidos |>
    dplyr::left_join(empatadas, by = "alfa") |>
    dplyr::transmute(
      Alfa = alfa,
      Fatores = fatores,
      Lambda = formatar_decimal(lambda, 1),
      `Iterações` = iteracoes,
      `Acerto na validação interna` = formatar_pct(acerto),
      `Mediana da grade` = formatar_pct(mediana),
      `Piso de popularidade na validação` = formatar_pct(selecao$validacao$piso_popularidade),
      `Pares avaliáveis de validação` = formatar_inteiro(pares),
      `Configurações empatadas no máximo` = paste0(empatadas, " de ", configuracoes)
    )
}

#' Confere a escolha do colaborativo contra a registrada na emenda
#'
#' O teste só roda com os hiperparâmetros que a emenda registrou antes dele.
#'
#' @param selecao Lista de `selecionar_hiperparametros_do_colaborativo()`.
#' @param registro Lista de `ler_pre_registro()`.
#' @return `selecao`, inalterada. Erro se a escolha divergir.
conferir_escolha_do_colaborativo <- function(selecao, registro) {
  chave <- "hiperparametros_do_colaborativo_na_realizacao_versionada"
  emenda <- purrr::detect(registro$emendas, \(e) !is.null(e[[chave]]))
  if (is.null(emenda)) stop("Nenhuma emenda registra a escolha do colaborativo.", call. = FALSE)

  registrada <- purrr::map(emenda[[chave]], tibble::as_tibble) |>
    purrr::list_rbind() |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric))
  escolhida <- selecao$escolhidos |>
    dplyr::select(alfa, fatores, lambda, iteracoes) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric))

  if (!isTRUE(all.equal(as.data.frame(escolhida), as.data.frame(registrada), check.attributes = FALSE))) {
    stop("A escolha de hiperparâmetros do colaborativo difere da registrada na emenda.", call. = FALSE)
  }
  selecao
}

#' Avalia o componente colaborativo inteiro numa realização
#'
#' A fatoração de cada alfa é ajustada em toda a estimação com a escolha da
#' validação interna, e os pares avaliáveis do teste temporal recebem três
#' acertos sob a mesma chave de desempate: popularidade, fatoração binária e
#' fatoração com confiança. A sequência pré-registrada decide: H1, binária
#' contra popularidade; H2, confiança contra binária, só se H1 rejeitar. A
#' comparação de H2 é calculada sempre, e fica descritiva quando H1 não
#' rejeita.
#'
#' @param dados Lista nomeada de tibbles.
#' @param registro Lista de `ler_pre_registro()`.
#' @param selecao Lista de `selecionar_hiperparametros_do_colaborativo()`, ou
#'   `NULL` para refazê-la.
#' @return Lista com `selecao`, `pares`, `h1`, `h2`, `adotado`, `n_teste` e
#'   `n_avaliavel`.
avaliar_componente_colaborativo <- function(dados, registro, selecao = NULL) {
  if (is.null(selecao)) {
    selecao <- selecionar_hiperparametros_do_colaborativo(dados, registro)
  }
  historico <- com_historico_da_janela(dados, janela_do_pre_registro(registro))
  divisao <- dividir_interacoes(
    historico$alocacoes, "temporal",
    fracao_teste = registro$divisoes$temporal$fracao_teste, semente = registro$sementes$divisao
  )
  contexto <- restringir_lista_a_quem_tem_fator(
    montar_contexto_do_colaborativo(historico, registro), divisao$treino
  )
  avaliaveis <- selecionar_pares_avaliaveis(divisao$treino, divisao$teste)
  r <- construir_matriz_r(
    dplyr::distinct(divisao$treino, consultor_id, projeto_id), historico$consultores, historico$projetos
  )

  escores_da_fatoracao <- function(alfa) {
    escolha <- selecao$escolhidos[selecao$escolhidos$alfa == alfa, ]
    peso <- construir_peso_de_confianca(divisao$treino, historico$consultores, historico$projetos, alfa)
    estado <- ajustar_als_implicito(
      r, peso = peso, fatores = escolha$fatores, lambda = escolha$lambda,
      iteracoes = escolha$iteracoes, semente = registro$sementes$fatoracao
    )[[as.character(escolha$iteracoes)]]
    estado$x %*% t(estado$y)
  }

  popularidade <- escorar_popularidade(divisao$treino, contexto)
  binario <- escores_da_fatoracao(0)
  pares <- avaliaveis |>
    dplyr::select(consultor_id, projeto_id) |>
    dplyr::mutate(
      acerto_popularidade = acertar_na_lista(avaliaveis, contexto, popularidade),
      acerto_binario = acertar_na_lista(avaliaveis, contexto, binario),
      acerto_confianca = acertar_na_lista(avaliaveis, contexto, escores_da_fatoracao(1))
    )

  nivel <- registro$teste$nivel
  h1 <- testar_mcnemar_exato(pares$acerto_binario, pares$acerto_popularidade, nivel)
  h2 <- testar_mcnemar_exato(pares$acerto_confianca, pares$acerto_binario, nivel)
  adotado <- if (!h1$supera) {
    "popularidade, com sinal binário"
  } else if (!h2$supera) {
    "fatoração com sinal binário"
  } else {
    "fatoração com confiança por horas"
  }

  # O piso no acerto esperado, a fração do bloco de empatados que cabe na
  # lista, é o que o pré-registro fixou; o binário acima desfaz os empates
  # por sorteio, como o McNemar exige. Os dois são reportados lado a lado.
  piso_esperado <- medir_linha_base_sob_protocolo(
    divisao$treino, avaliaveis, historico$consultores, registro$metrica$lista_decisoria
  )$recall

  # Diagnóstico de convergência, descritivo: escore da fatoração binária
  # contra a popularidade, sobre os consultores com fator e os projetos dos
  # pares avaliáveis.
  com_fator <- which(contexto$consultores %in% divisao$treino$consultor_id)
  colunas <- match(unique(avaliaveis$projeto_id), contexto$projetos)
  spearman <- stats::cor(
    as.numeric(binario[com_fator, colunas]), as.numeric(popularidade[com_fator, colunas]),
    method = "spearman"
  )

  list(
    selecao = selecao, pares = pares, h1 = h1, h2 = h2, h2_decisorio = h1$supera,
    adotado = adotado, n_teste = nrow(divisao$teste), n_avaliavel = nrow(avaliaveis),
    piso_esperado = piso_esperado, spearman_binario_popularidade = spearman
  )
}

#' Resume uma avaliação do colaborativo numa linha de grandezas
#'
#' @param avaliacao Lista de `avaliar_componente_colaborativo()`.
#' @param registro Lista de `ler_pre_registro()`.
#' @return tibble de uma linha.
resumir_avaliacao_do_colaborativo <- function(avaliacao, registro) {
  pares <- avaliacao$pares
  n <- nrow(pares)
  ganho <- function(piso, cenario) {
    calcular_ganho_minimo_detectavel(
      n, piso, cenario, alfa = registro$teste$nivel, poder = registro$teste$ganho_minimo$poder
    )
  }
  piso <- mean(pares$acerto_popularidade)
  binario <- mean(pares$acerto_binario)
  confianca <- mean(pares$acerto_confianca)
  grade <- avaliacao$selecao$grade |>
    dplyr::summarise(maximo = max(acerto), mediana = stats::median(acerto), .by = alfa)
  piso_validacao <- avaliacao$selecao$validacao$piso_popularidade

  tibble::tibble(
    n_teste_temporal = avaliacao$n_teste,
    n_avaliavel = n,
    fracao_avaliavel = n / avaliacao$n_teste,
    acerto_popularidade = piso,
    acerto_popularidade_esperado = avaliacao$piso_esperado,
    acerto_binario = binario,
    acerto_confianca = confianca,
    diferenca_h1 = binario - piso,
    discordantes_h1_a_favor = avaliacao$h1$b,
    discordantes_h1_contra = avaliacao$h1$c,
    valor_p_h1 = avaliacao$h1$valor_p,
    supera_h1 = as.numeric(avaliacao$h1$supera),
    diferenca_h2 = confianca - binario,
    discordantes_h2_a_favor = avaliacao$h2$b,
    discordantes_h2_contra = avaliacao$h2$c,
    valor_p_h2 = avaliacao$h2$valor_p,
    supera_h2 = as.numeric(avaliacao$h2$supera),
    ganho_minimo_h1_dominancia = ganho(piso, "dominancia"),
    ganho_minimo_h1_independencia = ganho(piso, "independencia"),
    ganho_minimo_h2_independencia = ganho(binario, "independencia"),
    concordancia_binario_popularidade = mean(pares$acerto_binario == pares$acerto_popularidade),
    spearman_binario_popularidade = avaliacao$spearman_binario_popularidade,
    validacao_avaliaveis = avaliacao$selecao$validacao$avaliaveis,
    validacao_binario_menos_piso = grade$maximo[grade$alfa == 0] - piso_validacao,
    validacao_mediana_binario_menos_piso = grade$mediana[grade$alfa == 0] - piso_validacao
  )
}

#' Refaz a avaliação do colaborativo inteira sobre realizações do gerador
#'
#' @param sementes Vetor de sementes das realizações.
#' @param registro Lista de `ler_pre_registro()`.
#' @param ... Repassado a `gerar_realizacao()`.
#' @return tibble com uma linha por realização.
simular_avaliacao_do_colaborativo <- function(sementes, registro, ...) {
  purrr::map(sementes, function(semente) {
    avaliacao <- avaliar_componente_colaborativo(gerar_realizacao(semente, ...), registro)
    dplyr::bind_cols(tibble::tibble(semente = semente), resumir_avaliacao_do_colaborativo(avaliacao, registro))
  }) |>
    purrr::list_rbind()
}
