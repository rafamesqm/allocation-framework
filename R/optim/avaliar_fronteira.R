#' Ponto ideal certificado pela tabela de payoff exata
#'
#' Afinidade e custo vêm das linhas resolvidas até a otimalidade. A
#' sobrecarga atribuível vem do limite inferior do solver no primeiro passo da
#' linha da sobrecarga, que parou no tempo-limite: o valor da melhor solução
#' vezes um menos a lacuna relativa, convertido para a atribuível.
#'
#' @param payoff Lista de `resolver_tabela_de_payoff()`.
#' @param problema Lista de `montar_problema_da_otimizacao()`.
#' @return Vetor nomeado com `afinidade`, `custo` e `sobrecarga`.
calcular_ideal_certificado <- function(payoff, problema) {
  primeiro <- \(linha) payoff$subproblemas[payoff$subproblemas$linha == linha & payoff$subproblemas$passo == 1, ]
  if (!all(c(primeiro("afinidade")$status, primeiro("custo")$status) == "Optimal")) {
    stop("O ideal certificado exige as linhas de afinidade e de custo ótimas.", call. = FALSE)
  }
  sobrecarga <- primeiro("sobrecarga")
  limite <- sobrecarga$valor * (1 - sobrecarga$mip_gap)
  c(
    afinidade = primeiro("afinidade")$valor,
    custo = primeiro("custo")$valor,
    sobrecarga = limite + problema$constante_sobrecarga - problema$sobrecarga_da_carga_fixa
  )
}

#' Hipervolume na caixa entre a alocação realizada e o ideal (emenda 2)
#'
#' Cada objetivo vai a zero no ideal e a um na realizada, todos a minimizar; a
#' referência é `(1, 1, 1)`. O resultado é a fração da região que domina a
#' realizada alcançada pelos pontos.
#'
#' @param objetivos Matriz com `afinidade`, `custo` e `sobrecarga`
#'   (atribuível).
#' @param ideal Vetor de `calcular_ideal_certificado()`.
#' @param realizada Vetor com os três objetivos da realizada.
#' @return Número entre zero e um.
calcular_hipervolume_da_realizada <- function(objetivos, ideal, realizada) {
  calcular_hipervolume(normalizar_na_caixa_da_realizada(objetivos, ideal, realizada))
}

#' Normaliza os objetivos na caixa entre o ideal e a alocação realizada
#'
#' @inheritParams calcular_hipervolume_da_realizada
#' @return Matriz a minimizar, zero no ideal e um na realizada.
normalizar_na_caixa_da_realizada <- function(objetivos, ideal, realizada) {
  minimizar <- para_minimizacao(objetivos)
  melhor <- c(-ideal[["afinidade"]], ideal[["custo"]], ideal[["sobrecarga"]])
  pior <- c(-realizada[["afinidade"]], realizada[["custo"]], realizada[["sobrecarga"]])
  sweep(sweep(minimizar, 2, melhor), 2, pior - melhor, "/")
}

#' Objetivos da alocação realizada, na escala do NSGA-II
#'
#' @param verificacao Lista de `medir_verificacao_da_otimizacao()`.
#' @return Vetor nomeado com `afinidade`, `custo` e `sobrecarga` (atribuível).
objetivos_da_realizada <- function(verificacao) {
  c(
    afinidade = verificacao$afinidade_realizada, custo = verificacao$custo_realizado,
    sobrecarga = verificacao$sobrecarga_atribuivel_realizada
  )
}

#' Resume uma execução do NSGA-II numa linha
#'
#' @param execucao Lista de `executar_nsga2_registrado()`.
#' @param payoff Lista de `resolver_tabela_de_payoff()`.
#' @param ideal Vetor de `calcular_ideal_certificado()`.
#' @param realizada Vetor de `objetivos_da_realizada()`.
#' @param ordem_lexicografica Lista do pré-registro.
#' @return tibble de uma linha.
resumir_execucao_do_nsga2 <- function(execucao, payoff, ideal, realizada, ordem_lexicografica) {
  fronteira <- extrair_fronteira(execucao)$objetivos
  comparacao <- comparar_fronteira_com_exato(fronteira, payoff, ordem_lexicografica)
  matriz <- as.matrix(fronteira)
  tibble::tibble(
    semente = execucao$semente,
    segundos = execucao$segundos,
    pontos = nrow(matriz),
    afinidade_maxima = max(matriz[, "afinidade"]),
    custo_minimo = min(matriz[, "custo"]),
    sobrecarga_minima = min(matriz[, "sobrecarga"]),
    hipervolume_da_realizada = calcular_hipervolume_da_realizada(matriz, ideal, realizada),
    hipervolume_da_emenda_1 = comparacao$hipervolume,
    fracao_dominada = comparacao$fracao_dominada,
    distancia_afinidade = comparacao$extremos$distancia[comparacao$extremos$objetivo == "afinidade"],
    distancia_custo = comparacao$extremos$distancia[comparacao$extremos$objetivo == "custo"],
    distancia_sobrecarga = comparacao$extremos$distancia[comparacao$extremos$objetivo == "sobrecarga"]
  )
}

#' Corrida exploratória da emenda 2: a semente principal com mais gerações
#'
#' @param problema Lista de `montar_problema_da_otimizacao()`.
#' @param emenda_nsga2 Emenda 1.
#' @param emenda_exploratoria Emenda 2.
#' @return Lista de `executar_nsga2()` com `semente`.
executar_nsga2_exploratorio <- function(problema, emenda_nsga2, emenda_exploratoria) {
  corrida <- emenda_exploratoria$corrida_exploratoria
  n_assentos <- sum(problema$pares$realizado)
  execucao <- executar_nsga2(
    problema,
    tamanho = emenda_nsga2$populacao$tamanho,
    geracoes = corrida$geracoes,
    probabilidade_de_cruzamento = emenda_nsga2$operadores$probabilidade_de_cruzamento,
    probabilidade_de_mutacao = 1 / n_assentos,
    semente = corrida$semente,
    registrar_a_cada = corrida$registrar_a_cada
  )
  execucao$semente <- corrida$semente
  execucao
}

#' Trajetória dos extremos e do hipervolume ao longo das gerações
#'
#' @param execucao Lista de `executar_nsga2()`.
#' @param ideal Vetor de `calcular_ideal_certificado()`.
#' @param realizada Vetor de `objetivos_da_realizada()`.
#' @return tibble com `geracao`, os três extremos e `hipervolume`.
medir_trajetoria <- function(execucao, ideal, realizada) {
  purrr::map(execucao$historico, function(registro) {
    objetivos <- registro$objetivos
    tibble::tibble(
      geracao = registro$geracao,
      afinidade_maxima = max(objetivos[, "afinidade"]),
      custo_minimo = min(objetivos[, "custo"]),
      sobrecarga_minima = min(objetivos[, "sobrecarga"]),
      hipervolume = calcular_hipervolume_da_realizada(objetivos, ideal, realizada)
    )
  }) |>
    purrr::list_rbind()
}

#' Os três problemas exatos do item 10.c, presos na alocação realizada
#'
#' Cada objetivo otimizado com os outros dois no máximo iguais aos da
#' realizada (afinidade pelo menos a dela; custo e sobrecarga no máximo os
#' dela). Um problema de um objetivo cada, pelo HiGHS, com o controle
#' registrado.
#'
#' @param problema Lista de `montar_problema_da_otimizacao()`.
#' @param desenho Pré-registro da otimização sem as emendas.
#' @return Lista com `problemas` (tibble com status, tempo, lacuna e os três
#'   objetivos de cada solução) e `solucoes`.
resolver_problemas_da_realizada <- function(problema, desenho) {
  controle <- controlar_solver(desenho)
  x_realizado <- solucao_realizada(problema)
  valor_realizado <- purrr::map_dbl(problema$objetivos, \(c) sum(c * x_realizado))
  prender <- function(objetivo) {
    if (problema$sentido_dos_objetivos[[objetivo]] == "max") {
      list(objetivo = objetivo, minimo = valor_realizado[[objetivo]], maximo = Inf)
    } else {
      list(objetivo = objetivo, minimo = -Inf, maximo = valor_realizado[[objetivo]])
    }
  }

  resolvidos <- purrr::map(names(problema$objetivos), function(objetivo) {
    outros <- setdiff(names(problema$objetivos), objetivo)
    resolver_objetivo(problema, objetivo, purrr::map(outros, prender), controle)
  }) |>
    stats::setNames(names(problema$objetivos))

  problemas <- purrr::imap(resolvidos, function(resolvido, objetivo) {
    avaliacao <- if (is.null(resolvido$x)) NULL else avaliar_solucao(problema, resolvido$x, tolerancia = 1e-6)
    tibble::tibble(
      objetivo = objetivo, status = resolvido$status, segundos = resolvido$segundos,
      mip_gap = resolvido$mip_gap,
      afinidade = avaliacao$afinidade %||% NA_real_, custo = avaliacao$custo %||% NA_real_,
      sobrecarga = avaliacao$sobrecarga_atribuivel %||% NA_real_
    )
  }) |>
    purrr::list_rbind()

  list(problemas = problemas, solucoes = purrr::map(resolvidos, "x"))
}

#' Colunas escolhidas por soluções completas do problema
#'
#' @param problema Lista de `montar_problema_da_otimizacao()`.
#' @param solucoes Lista de vetores de `completar_solucao()`.
#' @return Matriz, uma solução por linha, a coluna do par de cada assento.
colunas_das_solucoes <- function(problema, solucoes) {
  n_pares <- nrow(problema$pares)
  do.call(rbind, purrr::map(solucoes, \(x) which(x[seq_len(n_pares)] > 0.5)))
}

#' Descreve a ocupação de cada solução: zona morta e concentração
#'
#' A concentração é a fração dos assentos ocupados pelos 10 consultores mais
#' populares na estimação, e o maior número de assentos de um mesmo consultor
#' (emenda 2, detalhes de implementação).
#'
#' @param problema Lista de `montar_problema_da_otimizacao()`.
#' @param colunas Matriz de colunas, uma solução por linha.
#' @param estimacao tibble das alocações do período de estimação.
#' @return tibble com `assentos_da_zona_morta`, `fracao_nos_10_mais_populares`
#'   e `maior_numero_de_assentos`, uma linha por solução.
descrever_ocupacao <- function(problema, colunas, estimacao) {
  contagem <- dplyr::count(estimacao, consultor_id) |> dplyr::arrange(dplyr::desc(n), consultor_id)
  populares <- utils::head(contagem$consultor_id, 10)
  purrr::map(seq_len(nrow(colunas)), function(i) {
    ocupados <- problema$pares[colunas[i, ], ]
    tibble::tibble(
      assentos_da_zona_morta = sum(ocupados$origem == "zona morta"),
      fracao_nos_10_mais_populares = mean(ocupados$consultor_id %in% populares),
      maior_numero_de_assentos = max(table(ocupados$consultor_id))
    )
  }) |>
    purrr::list_rbind()
}

#' Sobrecarga atribuível em horas das mesmas soluções
#'
#' @param problema_horas Lista de `montar_problema_da_otimizacao()` na medida
#'   em horas, com os mesmos pares.
#' @param colunas Matriz de colunas, uma solução por linha.
#' @return Vetor numérico.
medir_sobrecarga_em_horas <- function(problema_horas, colunas) {
  codificacao <- preparar_codificacao(problema_horas)
  genes <- colunas - matrix(codificacao$inicio - 1L, nrow(colunas), ncol(colunas), byrow = TRUE)
  avaliar_populacao(codificacao, genes)$objetivos[, "sobrecarga"]
}

#' Itens 10.c a 10.f sobre a fronteira registrada da semente principal
#'
#' Posição da realizada, conflito entre afinidade e sobrecarga, tratamentos
#' da capacidade, zona morta e sensibilidade em horas, com as referências
#' exatas ao lado. Tudo descritivo.
#'
#' @param execucao Lista de `executar_nsga2_registrado()` da semente principal.
#' @param problema,problema_horas Problemas nas duas medidas de carga.
#' @param insumos Lista de `preparar_insumos_da_otimizacao()`.
#' @param payoff Lista de `resolver_tabela_de_payoff()`.
#' @param exatos_da_realizada Lista de `resolver_problemas_da_realizada()`.
#' @param ideal Vetor de `calcular_ideal_certificado()`.
#' @param realizada Vetor de `objetivos_da_realizada()`.
#' @return Lista com `pontos` (a fronteira com a ocupação e as horas),
#'   `referencias` (a realizada e as soluções exatas, nas mesmas colunas),
#'   `posicao`, `conflito`, `capacidade` e `horas`.
avaliar_fronteira_registrada <- function(execucao, problema, problema_horas, insumos, payoff,
                                         exatos_da_realizada, ideal, realizada) {
  fronteira <- extrair_fronteira(execucao)
  codificacao <- preparar_codificacao(problema)
  colunas <- colunas_dos_genes(codificacao, fronteira$genes)
  pontos <- dplyr::bind_cols(
    fronteira$objetivos,
    descrever_ocupacao(problema, colunas, insumos$estimacao),
    tibble::tibble(sobrecarga_em_horas = medir_sobrecarga_em_horas(problema_horas, colunas))
  )

  # A realizada e as soluções exatas, com as mesmas medidas.
  exatas <- c(
    stats::setNames(payoff$solucoes, paste("payoff,", names(payoff$solucoes))),
    stats::setNames(exatos_da_realizada$solucoes, paste("item 10.c,", names(exatos_da_realizada$solucoes)))
  )
  exatas <- c(list(realizada = solucao_realizada(problema)), purrr::compact(exatas))
  colunas_exatas <- colunas_das_solucoes(problema, exatas)
  referencias <- purrr::imap(exatas, function(x, nome) {
    avaliacao <- avaliar_solucao(problema, x, tolerancia = 1e-6)
    tibble::tibble(
      solucao = nome, afinidade = avaliacao$afinidade, custo = avaliacao$custo,
      sobrecarga = avaliacao$sobrecarga_atribuivel
    )
  }) |>
    purrr::list_rbind() |>
    dplyr::bind_cols(
      descrever_ocupacao(problema, colunas_exatas, insumos$estimacao),
      tibble::tibble(sobrecarga_em_horas = medir_sobrecarga_em_horas(problema_horas, colunas_exatas))
    )
  horas_da_realizada <- referencias$sobrecarga_em_horas[referencias$solucao == "realizada"]

  # Posição da realizada.
  domina <- pontos$afinidade >= realizada[["afinidade"]] & pontos$custo <= realizada[["custo"]] &
    pontos$sobrecarga <= realizada[["sobrecarga"]] &
    (pontos$afinidade > realizada[["afinidade"]] | pontos$custo < realizada[["custo"]] |
      pontos$sobrecarga < realizada[["sobrecarga"]])
  sobrecarga_minima <- referencias[referencias$solucao == "payoff, sobrecarga", ]
  posicao <- list(
    pontos_que_dominam = sum(domina),
    pontos = nrow(pontos),
    ganho_de_afinidade = max(pontos$afinidade[domina]) - realizada[["afinidade"]],
    reducao_de_custo = 1 - min(pontos$custo[domina]) / realizada[["custo"]],
    reducao_de_sobrecarga = realizada[["sobrecarga"]] - min(pontos$sobrecarga[domina]),
    realizada_na_populacao_inicial = TRUE,
    dominada_pela_exata_de_sobrecarga_minima = sobrecarga_minima$afinidade > realizada[["afinidade"]] &&
      sobrecarga_minima$custo < realizada[["custo"]] && sobrecarga_minima$sobrecarga < realizada[["sobrecarga"]]
  )

  # Conflito: extremos exatos primeiro, a fronteira como ilustração.
  maxima <- referencias[referencias$solucao == "payoff, afinidade", ]
  plano <- as.matrix(pontos[, c("afinidade", "sobrecarga")])
  no_plano <- ordenar_nao_dominados(calcular_dominacao(cbind(-plano[, 1], plano[, 2]), rep(0, nrow(plano)))) == 1
  caminho <- pontos[no_plano, ] |> dplyr::arrange(afinidade)
  conflito <- list(
    taxa_entre_extremos_exatos = (maxima$sobrecarga - sobrecarga_minima$sobrecarga) /
      (maxima$afinidade - sobrecarga_minima$afinidade),
    taxa_mediana_na_fronteira = stats::median(diff(caminho$sobrecarga) / diff(caminho$afinidade)),
    pontos_no_caminho = nrow(caminho)
  )

  # Tratamentos da capacidade como pontos da fronteira.
  normal <- normalizar_na_caixa_da_realizada(as.matrix(pontos[, c("afinidade", "custo", "sobrecarga")]), ideal, realizada)
  calibrados <- which(pontos$sobrecarga <= realizada[["sobrecarga"]])
  capacidade <- dplyr::bind_rows(
    pontos[calibrados[which.max(pontos$afinidade[calibrados])], ] |> dplyr::mutate(tratamento = "limite calibrado"),
    pontos[which.min(rowSums(normal)), ] |> dplyr::mutate(tratamento = "penalidade, pesos iguais")
  )

  horas <- list(
    realizada = horas_da_realizada,
    fracao_que_domina_em_horas = mean(
      pontos$afinidade >= realizada[["afinidade"]] & pontos$custo <= realizada[["custo"]] &
        pontos$sobrecarga_em_horas <= horas_da_realizada
    ),
    correlacao_de_spearman = stats::cor(pontos$sobrecarga, pontos$sobrecarga_em_horas, method = "spearman")
  )

  list(pontos = pontos, referencias = referencias, posicao = posicao, conflito = conflito,
       capacidade = capacidade, horas = horas)
}
