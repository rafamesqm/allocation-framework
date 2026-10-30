#' Resolve o problema montado para um objetivo, pelo HiGHS
#'
#' As binárias entram como inteiras entre zero e um; a sobrecarga, como
#' contínua não negativa. Restrições extras (a fixação lexicográfica, e mais
#' adiante as restrições épsilon) entram como linhas acrescentadas à matriz.
#'
#' @param problema Lista de `montar_problema_da_otimizacao()`.
#' @param objetivo Nome do objetivo em `problema$objetivos`.
#' @param extras Lista de restrições extras, cada uma com `objetivo`,
#'   `minimo` e `maximo` sobre o valor desse objetivo (sem a constante da
#'   sobrecarga).
#' @param controle Lista de `controlar_solver()`.
#' @return Lista com `status`, `segundos`, `mip_gap`, `nos`, `valor` (do
#'   objetivo, sem constante) e `x` (a solução completada, ou `NULL` sem
#'   solução).
resolver_objetivo <- function(problema, objetivo, extras = list(), controle) {
  linhas_extras <- do.call(rbind, lapply(extras, \(e) problema$objetivos[[e$objetivo]]))
  restricoes <- if (length(extras) == 0) {
    problema$restricoes
  } else {
    rbind(problema$restricoes, Matrix::Matrix(linhas_extras, sparse = TRUE))
  }
  igualdade <- problema$sentido == "=="

  inicio <- proc.time()[["elapsed"]]
  resultado <- highs::highs_solve(
    L = problema$objetivos[[objetivo]],
    lower = rep(0, ncol(problema$restricoes)),
    upper = ifelse(problema$binaria, 1, Inf),
    A = restricoes,
    lhs = c(ifelse(igualdade, problema$lado_direito, -Inf), vapply(extras, \(e) e$minimo, numeric(1))),
    rhs = c(problema$lado_direito, vapply(extras, \(e) e$maximo, numeric(1))),
    types = ifelse(problema$binaria, "I", "C"),
    maximum = problema$sentido_dos_objetivos[[objetivo]] == "max",
    control = controle
  )
  segundos <- proc.time()[["elapsed"]] - inicio

  # Sem solução viável, o HiGHS ainda devolve um vetor primal (de zeros) ao
  # bater o tempo-limite; só o status primal diz se há solução.
  tem_solucao <- identical(resultado$info$primal_solution_status, "Feasible")
  x <- if (tem_solucao) {
    completar_solucao(problema, round(resultado$primal_solution[problema$binaria]))
  } else {
    NULL
  }
  list(
    status = resultado$status_message,
    segundos = segundos,
    mip_gap = resultado$info$mip_gap %||% NA_real_,
    nos = resultado$info$mip_node_count %||% NA_real_,
    valor = if (is.null(x)) NA_real_ else sum(problema$objetivos[[objetivo]] * x),
    x = x
  )
}

#' Opções do HiGHS registradas no pré-registro da otimização
#'
#' @param registro_otimizacao Lista de `ler_pre_registro_otimizacao()`.
#' @return Lista de `highs::highs_control()`.
controlar_solver <- function(registro_otimizacao) {
  metodo <- registro_otimizacao$metodo
  highs::highs_control(
    threads = as.integer(metodo$threads),
    # O YAML lê 600 e 0 como inteiros, e o highs exige double.
    time_limit = as.double(metodo$tempo_limite_segundos),
    mip_rel_gap = as.double(metodo$mip_rel_gap),
    log_to_console = FALSE
  )
}

#' Tabela de payoff por otimização lexicográfica
#'
#' Para cada objetivo, otimiza-o; depois, com ele preso ao ótimo pela
#' tolerância relativa registrada, otimiza o segundo da ordem lexicográfica;
#' depois, com os dois presos, o terceiro. A linha da tabela são os três
#' objetivos da solução do último passo. São nove problemas inteiros, e cada
#' um fica registrado com status e tempo, que é o que decide a regra de troca
#' de método.
#'
#' @param problema Lista de `montar_problema_da_otimizacao()`.
#' @param registro_otimizacao Lista de `ler_pre_registro_otimizacao()`.
#' @return Lista com `payoff` (uma linha por objetivo otimizado, com os três
#'   valores), `subproblemas` (um por passo, com status, tempo, lacuna, nós e
#'   valor) e `solucoes` (o vetor solução de cada linha).
resolver_tabela_de_payoff <- function(problema, registro_otimizacao) {
  metodo <- registro_otimizacao$metodo
  controle <- controlar_solver(registro_otimizacao)
  tolerancia <- metodo$tolerancia_de_fixacao

  linhas <- purrr::imap(metodo$ordem_lexicografica, function(ordem, linha) {
    extras <- list()
    passos <- list()
    x <- NULL
    for (passo in seq_along(ordem)) {
      objetivo <- ordem[[passo]]
      resolvido <- resolver_objetivo(problema, objetivo, extras, controle)
      passos[[passo]] <- tibble::tibble(
        linha = linha, passo = passo, objetivo = objetivo, status = resolvido$status,
        segundos = resolvido$segundos, mip_gap = resolvido$mip_gap, nos = resolvido$nos,
        valor = resolvido$valor
      )
      if (is.null(resolvido$x)) break
      x <- resolvido$x
      folga <- tolerancia * max(1, abs(resolvido$valor))
      extras[[passo]] <- if (problema$sentido_dos_objetivos[[objetivo]] == "max") {
        list(objetivo = objetivo, minimo = resolvido$valor - folga, maximo = Inf)
      } else {
        list(objetivo = objetivo, minimo = -Inf, maximo = resolvido$valor + folga)
      }
    }
    list(passos = purrr::list_rbind(passos), x = x, passos_com_solucao = length(extras))
  })

  subproblemas <- purrr::map(linhas, "passos") |> purrr::list_rbind()
  solucoes <- purrr::map(linhas, "x")
  # Linha incompleta: algum passo terminou sem solução viável. Os valores são
  # os da solução do último passo que teve uma, e a linha fica marcada.
  payoff <- purrr::imap(linhas, function(resultado, linha) {
    completa <- resultado$passos_com_solucao == length(metodo$ordem_lexicografica[[linha]])
    if (is.null(resultado$x)) {
      return(tibble::tibble(linha = linha, completa = completa, passos_com_solucao = 0L))
    }
    avaliacao <- avaliar_solucao(problema, resultado$x, tolerancia = 1e-6)
    tibble::tibble(
      linha = linha, completa = completa, passos_com_solucao = resultado$passos_com_solucao,
      viavel = avaliacao$viavel,
      afinidade = avaliacao$afinidade, custo = avaliacao$custo,
      sobrecarga = avaliacao$sobrecarga, sobrecarga_atribuivel = avaliacao$sobrecarga_atribuivel
    )
  }) |>
    purrr::list_rbind()

  list(payoff = payoff, subproblemas = subproblemas, solucoes = solucoes)
}

#' Aplica a regra de troca de método à parte que a tabela de payoff decide
#'
#' A regra registrada pede a tabela de payoff inteira até a otimalidade e ao
#' menos a fração mínima da grade. Sem a grade, só a primeira metade pode ser
#' decidida aqui; se ela falhar, o método exato já está descartado.
#'
#' @param payoff Lista de `resolver_tabela_de_payoff()`.
#' @return Lista com `payoff_otima` (lógico) e `subproblemas_otimos`.
avaliar_tratabilidade_do_payoff <- function(payoff) {
  otimos <- payoff$subproblemas$status == "Optimal"
  list(
    payoff_otima = all(otimos) && length(otimos) == 9 && all(payoff$payoff$completa),
    subproblemas_otimos = sum(otimos)
  )
}

#' Descreve as soluções da tabela de payoff
#'
#' Quantos assentos a zona morta ocupa em cada solução, a afinidade só nos
#' assentos cujo realizado está na lista do modelo, e quantos consultores
#' distintos ocupam assentos.
#'
#' @param payoff Lista de `resolver_tabela_de_payoff()`.
#' @param problema Lista de `montar_problema_da_otimizacao()`.
#' @return tibble com uma linha por solução, mais a da alocação realizada.
descrever_solucoes_do_payoff <- function(payoff, problema) {
  pares <- problema$pares
  assentos_da_lista <- pares$assento_id[pares$realizado & pares$origem == "lista"]
  descrever <- function(x, linha) {
    ocupados <- pares[x[seq_len(nrow(pares))] > 0.5, ]
    tibble::tibble(
      linha = linha,
      assentos_da_zona_morta = sum(ocupados$origem == "zona morta"),
      assentos_com_realizado = sum(ocupados$realizado),
      afinidade_nos_assentos_da_lista = sum(ocupados$s_hat[ocupados$assento_id %in% assentos_da_lista]),
      consultores_distintos = dplyr::n_distinct(ocupados$consultor_id)
    )
  }
  com_solucao <- !purrr::map_lgl(payoff$solucoes, is.null)
  dplyr::bind_rows(
    purrr::imap(payoff$solucoes[com_solucao], descrever) |> purrr::list_rbind(),
    descrever(solucao_realizada(problema), "realizada")
  )
}
