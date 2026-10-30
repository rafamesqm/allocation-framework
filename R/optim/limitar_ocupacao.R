#' Carga mensal de cada consultor nas soluções dadas
#'
#' A carga é a fixa mais a dos assentos, em fração de um mês cheio, sem
#' descontar férias, como a ocupação da EDA (emenda 3). Entram todos os
#' consultores e meses de teste, inclusive os que só têm carga fixa.
#'
#' @param problema Lista de `montar_problema_da_otimizacao()` na medida
#'   percentual.
#' @param insumos Lista de `preparar_insumos_da_otimizacao()`.
#' @param colunas Matriz de colunas, uma solução por linha.
#' @return Lista com `meses` (tibble com `consultor_id`, `ano_mes`,
#'   `capacidade` e `fixa`, uma linha por consultor e mês) e `carga` (matriz,
#'   um consultor e mês por linha e uma solução por coluna).
medir_carga_mensal <- function(problema, insumos, colunas) {
  stopifnot(problema$medida == "percentual")
  meses <- insumos$capacidade |>
    dplyr::select(consultor_id, ano_mes, capacidade = capacidade_pct) |>
    dplyr::left_join(
      insumos$carga_fixa$por_mes |> dplyr::select(consultor_id, ano_mes, fixa = carga_pct),
      by = c("consultor_id", "ano_mes")
    ) |>
    dplyr::mutate(fixa = dplyr::coalesce(fixa, 0))

  # Carga dos assentos nos consultores e meses com linha de capacidade.
  n_pares <- nrow(problema$pares)
  escolhas <- Matrix::sparseMatrix(
    i = as.vector(t(colunas)), j = rep(seq_len(nrow(colunas)), each = ncol(colunas)),
    x = 1, dims = c(n_pares, nrow(colunas))
  )
  linhas <- which(problema$tipo_da_restricao == "capacidade por consultor e mês")
  dos_assentos <- as.matrix(problema$restricoes[linhas, seq_len(n_pares), drop = FALSE] %*% escolhas)

  carga <- matrix(meses$fixa, nrow(meses), nrow(colunas))
  posicao <- match(
    paste(problema$sobrecarga$consultor_id, problema$sobrecarga$ano_mes),
    paste(meses$consultor_id, meses$ano_mes)
  )
  carga[posicao, ] <- carga[posicao, ] + dos_assentos
  list(meses = meses, carga = carga)
}

#' Concentração simultânea da carga (emenda 3)
#'
#' A ocupação máxima de um consultor num mês, quantos meses de consultor
#' passam de 2 e a fração da sobrecarga atribuível no consultor de maior
#' sobrecarga. A sobrecarga atribuível usa a capacidade com férias, como o
#' objetivo.
#'
#' @inheritParams medir_carga_mensal
#' @return tibble com `ocupacao_maxima`, `meses_acima_de_2` e
#'   `fracao_no_mais_sobrecarregado`, uma linha por solução.
medir_concentracao_simultanea <- function(problema, insumos, colunas) {
  mensal <- medir_carga_mensal(problema, insumos, colunas)
  meses <- mensal$meses
  da_fixa <- pmax(meses$fixa - meses$capacidade, 0)
  purrr::map(seq_len(ncol(mensal$carga)), function(k) {
    carga <- mensal$carga[, k]
    atribuivel <- pmax(carga - meses$capacidade, 0) - da_fixa
    por_consultor <- tapply(atribuivel, meses$consultor_id, sum)
    tibble::tibble(
      ocupacao_maxima = max(carga),
      meses_acima_de_2 = sum(carga > 2),
      fracao_no_mais_sobrecarregado = if (sum(por_consultor) > 0) max(por_consultor) / sum(por_consultor) else NA_real_
    )
  }) |>
    purrr::list_rbind()
}

#' O L da emenda 3: a maior ocupação mensal da alocação realizada
#'
#' @inheritParams medir_carga_mensal
#' @return Número.
calcular_limite_individual <- function(problema, insumos) {
  realizada <- colunas_das_solucoes(problema, list(solucao_realizada(problema)))
  max(medir_carga_mensal(problema, insumos, realizada)$carga)
}

#' Confere o L medido contra o registrado na emenda 3
#'
#' @param limite Número de `calcular_limite_individual()`.
#' @param emenda Emenda 3.
#' @return `limite`, se conferir; senão para.
conferir_limite_individual <- function(limite, emenda) {
  registrado <- emenda$limite_individual
  if (abs(limite - registrado$valor_de_L) > registrado$tolerancia_de_L) {
    stop(
      "O L medido (", format(limite, digits = 7), ") não confere com o registrado na emenda 3 (",
      registrado$valor_de_L, ").",
      call. = FALSE
    )
  }
  limite
}

#' Acrescenta o limite individual ao problema
#'
#' Para cada consultor e mês com linha de capacidade, a carga dos assentos
#' fica no máximo `max(L, fixa) - fixa`. Nos demais, a carga é só a fixa e o
#' limite vale por construção.
#'
#' @param problema Lista de `montar_problema_da_otimizacao()` na medida
#'   percentual.
#' @param limite O L, em fração de um mês cheio.
#' @return O problema com as linhas do limite acrescentadas.
limitar_ocupacao <- function(problema, limite) {
  stopifnot(problema$medida == "percentual")
  n_pares <- nrow(problema$pares)
  linhas <- which(problema$tipo_da_restricao == "capacidade por consultor e mês")
  dos_assentos <- problema$restricoes[linhas, , drop = FALSE]
  dos_assentos[, -seq_len(n_pares)] <- 0
  fixa <- problema$sobrecarga$fixa

  problema$restricoes <- rbind(problema$restricoes, Matrix::drop0(dos_assentos))
  problema$tipo_da_restricao <- c(problema$tipo_da_restricao, rep("limite individual por consultor e mês", length(linhas)))
  problema$sentido <- c(problema$sentido, rep("<=", length(linhas)))
  problema$lado_direito <- c(problema$lado_direito, pmax(limite, fixa) - fixa)
  problema$limite_individual <- limite
  problema
}

#' Os problemas exatos da emenda 3, com o limite individual
#'
#' As linhas de afinidade e de custo da tabela de payoff, pela ordem
#' lexicográfica registrada, e os três problemas do item 10.c.
#'
#' @param problema_limitado Lista de `limitar_ocupacao()`.
#' @param desenho Pré-registro da otimização sem as emendas.
#' @return Lista com `payoff` (de `resolver_tabela_de_payoff()`, só as duas
#'   linhas) e `realizada` (de `resolver_problemas_da_realizada()`).
resolver_problemas_limitados <- function(problema_limitado, desenho) {
  duas_linhas <- desenho
  duas_linhas$metodo$ordem_lexicografica <- desenho$metodo$ordem_lexicografica[c("afinidade", "custo")]
  list(
    payoff = resolver_tabela_de_payoff(problema_limitado, duas_linhas),
    realizada = resolver_problemas_da_realizada(problema_limitado, desenho)
  )
}

#' Sensibilidade da emenda 3: afinidade máxima presa em x̄ com outro L
#'
#' @param problema Lista de `montar_problema_da_otimizacao()`, sem limite.
#' @param limite O L da sensibilidade.
#' @param desenho Pré-registro da otimização sem as emendas.
#' @return Lista de `resolver_objetivo()`, com `limite`.
resolver_afinidade_com_limite <- function(problema, limite, desenho) {
  x_realizado <- solucao_realizada(problema)
  presos <- list(
    list(objetivo = "custo", minimo = -Inf, maximo = sum(problema$objetivos$custo * x_realizado)),
    list(objetivo = "sobrecarga", minimo = -Inf, maximo = sum(problema$objetivos$sobrecarga * x_realizado))
  )
  resolvido <- resolver_objetivo(limitar_ocupacao(problema, limite), "afinidade", presos, controlar_solver(desenho))
  resolvido$limite <- limite
  resolvido
}

#' Descreve as soluções exatas: objetivos, certificado e concentração
#'
#' @param solucoes Lista nomeada de vetores solução (`NULL` sem solução).
#' @param certificados tibble com `solucao`, `status`, `segundos` e
#'   `mip_gap`, na ordem de `solucoes`.
#' @param problema Problema em que as soluções são avaliadas.
#' @param insumos Lista de `preparar_insumos_da_otimizacao()`.
#' @return tibble com uma linha por solução.
descrever_solucoes_exatas <- function(solucoes, certificados, problema, insumos) {
  com_solucao <- !purrr::map_lgl(solucoes, is.null)
  objetivos <- purrr::map(solucoes, function(x) {
    if (is.null(x)) {
      return(tibble::tibble(viavel = NA, afinidade = NA_real_, custo = NA_real_, sobrecarga = NA_real_))
    }
    avaliacao <- avaliar_solucao(problema, x, tolerancia = 1e-6)
    tibble::tibble(
      viavel = avaliacao$viavel, afinidade = avaliacao$afinidade, custo = avaliacao$custo,
      sobrecarga = avaliacao$sobrecarga_atribuivel
    )
  }) |>
    purrr::list_rbind()

  concentracao <- tibble::tibble(
    assentos_da_zona_morta = NA_real_, fracao_nos_10_mais_populares = NA_real_,
    maior_numero_de_assentos = NA_real_, ocupacao_maxima = NA_real_, meses_acima_de_2 = NA_real_,
    fracao_no_mais_sobrecarregado = NA_real_
  )[rep(1, length(solucoes)), ]
  if (any(com_solucao)) {
    colunas <- colunas_das_solucoes(problema, solucoes[com_solucao])
    concentracao[com_solucao, ] <- dplyr::bind_cols(
      descrever_ocupacao(problema, colunas, insumos$estimacao),
      medir_concentracao_simultanea(problema, insumos, colunas)
    )
  }
  dplyr::bind_cols(certificados, objetivos, concentracao)
}

#' Certificado de cada linha da tabela de payoff
#'
#' A linha é ótima quando todos os passos são; senão, vale o status do
#' primeiro passo que não foi. O tempo soma os passos e a lacuna é a maior.
#'
#' @param payoff Lista de `resolver_tabela_de_payoff()`.
#' @return tibble com `solucao`, `status`, `segundos` e `mip_gap`.
certificar_linhas_do_payoff <- function(payoff) {
  payoff$subproblemas |>
    dplyr::summarise(
      status = if (all(status == "Optimal")) "Optimal" else status[status != "Optimal"][1],
      segundos = sum(segundos),
      mip_gap = max(mip_gap),
      .by = linha
    ) |>
    dplyr::transmute(solucao = paste("payoff,", linha), status, segundos, mip_gap)
}

#' Certificado das linhas da tabela de payoff que podem parar no meio
#'
#' A solução de uma linha incompleta é a do último passo com solução, e o
#' certificado olha só esses passos: a linha é ótima quando todos eles são;
#' senão, vale o status do primeiro que não foi, com a maior lacuna entre
#' eles. O tempo soma todos os passos, inclusive o que terminou sem solução.
#'
#' @param payoff Lista de `resolver_tabela_de_payoff()`.
#' @return tibble com `solucao`, `status`, `segundos`, `mip_gap` e
#'   `passos_com_solucao`.
certificar_passos_do_payoff <- function(payoff) {
  payoff$subproblemas |>
    dplyr::summarise(
      segundos = sum(segundos),
      passos_com_solucao = sum(!is.na(valor)),
      status = if (all(status[!is.na(valor)] == "Optimal")) "Optimal" else status[!is.na(valor) & status != "Optimal"][1],
      mip_gap = max(mip_gap[!is.na(valor)]),
      .by = linha
    ) |>
    dplyr::transmute(solucao = paste("payoff,", linha), status, segundos, mip_gap, passos_com_solucao)
}

#' Certificado dos problemas do item 10.c
#'
#' @param exatos Lista de `resolver_problemas_da_realizada()`.
#' @return tibble com `solucao`, `status`, `segundos` e `mip_gap`.
certificar_problemas_da_realizada <- function(exatos) {
  exatos$problemas |>
    dplyr::transmute(solucao = paste("item 10.c,", objetivo), status, segundos, mip_gap)
}

#' Concentração simultânea nas soluções da emenda 2 (emenda 3)
#'
#' A realizada, as soluções da tabela de payoff, as do item 10.c e os pontos
#' da fronteira registrada da semente principal. Nada é resolvido de novo.
#'
#' @param execucao Lista de `executar_nsga2_registrado()` da semente principal.
#' @param problema Lista de `montar_problema_da_otimizacao()`.
#' @param insumos Lista de `preparar_insumos_da_otimizacao()`.
#' @param payoff Lista de `resolver_tabela_de_payoff()`.
#' @param exatos_da_realizada Lista de `resolver_problemas_da_realizada()`.
#' @return Lista com `referencias` (as exatas e a realizada) e `pontos` (a
#'   fronteira, com os três objetivos).
medir_concentracao_da_avaliacao <- function(execucao, problema, insumos, payoff, exatos_da_realizada) {
  solucoes <- c(
    list(realizada = solucao_realizada(problema)),
    stats::setNames(payoff$solucoes, paste("payoff,", names(payoff$solucoes))),
    stats::setNames(exatos_da_realizada$solucoes, paste("item 10.c,", names(exatos_da_realizada$solucoes)))
  )
  certificados <- dplyr::bind_rows(
    tibble::tibble(solucao = "realizada", status = NA_character_, segundos = NA_real_, mip_gap = NA_real_),
    certificar_linhas_do_payoff(payoff),
    certificar_problemas_da_realizada(exatos_da_realizada)
  )
  referencias <- descrever_solucoes_exatas(solucoes, certificados[match(names(solucoes), certificados$solucao), ], problema, insumos)

  fronteira <- extrair_fronteira(execucao)
  colunas <- colunas_dos_genes(preparar_codificacao(problema), fronteira$genes)
  pontos <- dplyr::bind_cols(fronteira$objetivos, medir_concentracao_simultanea(problema, insumos, colunas))
  list(referencias = referencias, pontos = pontos)
}

#' Descreve os problemas da emenda 3 e a sensibilidade
#'
#' Cada solução é avaliada no problema com o limite com que foi resolvida, e a
#' realizada entra primeiro, avaliada com o L registrado.
#'
#' @param limitados Lista de `resolver_problemas_limitados()`.
#' @param sensibilidade Lista de `resolver_afinidade_com_limite()`.
#' @param problema Lista de `montar_problema_da_otimizacao()`, sem limite.
#' @param limite O L registrado.
#' @param insumos Lista de `preparar_insumos_da_otimizacao()`.
#' @return tibble com uma linha por solução, com `limite`.
descrever_problemas_limitados <- function(limitados, sensibilidade, problema, limite, insumos) {
  limitado <- limitar_ocupacao(problema, limite)
  solucoes <- c(
    list(realizada = solucao_realizada(limitado)),
    stats::setNames(limitados$payoff$solucoes, paste("payoff,", names(limitados$payoff$solucoes))),
    stats::setNames(limitados$realizada$solucoes, paste("item 10.c,", names(limitados$realizada$solucoes)))
  )
  certificados <- dplyr::bind_rows(
    tibble::tibble(solucao = "realizada", status = NA_character_, segundos = NA_real_, mip_gap = NA_real_),
    certificar_passos_do_payoff(limitados$payoff),
    certificar_problemas_da_realizada(limitados$realizada)
  )
  com_l <- descrever_solucoes_exatas(solucoes, certificados[match(names(solucoes), certificados$solucao), ], limitado, insumos) |>
    dplyr::mutate(limite = limite)

  com_sensibilidade <- descrever_solucoes_exatas(
    list("item 10.c, afinidade" = sensibilidade$x),
    tibble::tibble(
      solucao = "item 10.c, afinidade", status = sensibilidade$status, segundos = sensibilidade$segundos,
      mip_gap = sensibilidade$mip_gap
    ),
    limitar_ocupacao(problema, sensibilidade$limite), insumos
  ) |>
    dplyr::mutate(limite = sensibilidade$limite)
  dplyr::bind_rows(com_l, com_sensibilidade)
}
