#' Objetivos exatos da tabela de payoff, na escala da fronteira evolutiva
#'
#' O NSGA-II otimiza a sobrecarga atribuível aos assentos; a tabela de payoff
#' guarda as duas, e é a atribuível que entra na comparação.
#'
#' @param payoff Lista de `resolver_tabela_de_payoff()`.
#' @return Matriz com `afinidade`, `custo` e `sobrecarga`, uma linha por linha
#'   da tabela com solução, nomeadas pelo objetivo da linha.
objetivos_do_payoff <- function(payoff) {
  linhas <- payoff$payoff[payoff$payoff$passos_com_solucao > 0, ]
  matrix(
    c(linhas$afinidade, linhas$custo, linhas$sobrecarga_atribuivel), nrow(linhas),
    dimnames = list(linhas$linha, c("afinidade", "custo", "sobrecarga"))
  )
}

#' Normaliza os objetivos pelas amplitudes da tabela de payoff
#'
#' Zero é o melhor valor de cada objetivo na tabela e um, o pior; a afinidade
#' é invertida para que todos fiquem a minimizar. O ponto de referência do
#' hipervolume é então `(1, 1, 1)`.
#'
#' @param objetivos Matriz com `afinidade`, `custo` e `sobrecarga`.
#' @param exatos Matriz de `objetivos_do_payoff()`.
#' @return Matriz normalizada, a minimizar.
normalizar_pelo_payoff <- function(objetivos, exatos) {
  minimizar <- para_minimizacao(objetivos)
  referencia <- para_minimizacao(exatos)
  melhor <- apply(referencia, 2, min)
  pior <- apply(referencia, 2, max)
  sweep(sweep(minimizar, 2, melhor), 2, pior - melhor, "/")
}

#' Hipervolume exato em três objetivos a minimizar
#'
#' Fatias ao longo do terceiro objetivo: entre dois valores consecutivos, a
#' área dominada no plano dos dois primeiros pelos pontos já alcançados. Só
#' contam os pontos que dominam a referência.
#'
#' @param pontos Matriz com três colunas, a minimizar.
#' @param referencia Ponto de referência.
#' @return Número.
calcular_hipervolume <- function(pontos, referencia = c(1, 1, 1)) {
  dentro <- pontos[apply(pontos, 1, \(p) all(p < referencia)), , drop = FALSE]
  if (nrow(dentro) == 0) return(0)
  dentro <- dentro[order(dentro[, 3]), , drop = FALSE]
  alturas <- diff(c(dentro[, 3], referencia[3]))

  area_2d <- function(p) {
    p <- p[order(p[, 1], p[, 2]), , drop = FALSE]
    menor_y <- cummin(p[, 2])
    larguras <- diff(c(p[, 1], referencia[1]))
    sum(larguras * (referencia[2] - menor_y))
  }
  sum(vapply(seq_len(nrow(dentro)), \(k) alturas[k] * area_2d(dentro[seq_len(k), 1:2, drop = FALSE]), numeric(1)))
}

#' Compara a fronteira evolutiva com a tabela de payoff exata
#'
#' Três medidas descritivas da emenda 1: a distância de cada extremo da
#' fronteira ao extremo exato correspondente, na escala normalizada; a fração
#' dos pontos da fronteira dominados por alguma solução exata; e o
#' hipervolume normalizado.
#'
#' O extremo da fronteira num objetivo é o ponto de melhor valor nele, com os
#' empates desfeitos pela ordem lexicográfica registrada para aquela linha.
#'
#' @param fronteira tibble com `afinidade`, `custo` e `sobrecarga`.
#' @param payoff Lista de `resolver_tabela_de_payoff()`.
#' @param ordem_lexicografica Lista do pré-registro, `metodo$ordem_lexicografica`.
#' @return Lista com `extremos` (tibble), `fracao_dominada`, `hipervolume` e
#'   `hipervolume_do_payoff`.
comparar_fronteira_com_exato <- function(fronteira, payoff, ordem_lexicografica) {
  exatos <- objetivos_do_payoff(payoff)
  pontos <- as.matrix(fronteira[, c("afinidade", "custo", "sobrecarga")])
  normal <- normalizar_pelo_payoff(pontos, exatos)
  normal_exatos <- normalizar_pelo_payoff(exatos, exatos)
  minimizar <- para_minimizacao(pontos)

  extremos <- purrr::map(rownames(exatos), function(objetivo) {
    ordem <- ordem_lexicografica[[objetivo]]
    melhor <- do.call(order, unname(as.data.frame(minimizar[, ordem, drop = FALSE])))[1]
    linha <- payoff$payoff[payoff$payoff$linha == objetivo, ]
    tibble::tibble(
      objetivo = objetivo,
      completa = linha$completa,
      distancia = sqrt(sum((normal[melhor, ] - normal_exatos[objetivo, ])^2)),
      afinidade_evolutiva = pontos[melhor, "afinidade"], afinidade_exata = exatos[objetivo, "afinidade"],
      custo_evolutivo = pontos[melhor, "custo"], custo_exato = exatos[objetivo, "custo"],
      sobrecarga_evolutiva = pontos[melhor, "sobrecarga"], sobrecarga_exata = exatos[objetivo, "sobrecarga"]
    )
  }) |>
    purrr::list_rbind()

  minimizar_exatos <- para_minimizacao(exatos)
  dominado <- apply(minimizar, 1, function(p) {
    any(apply(minimizar_exatos, 1, \(e) all(e <= p) && any(e < p)))
  })

  list(
    extremos = extremos,
    fracao_dominada = mean(dominado),
    hipervolume = calcular_hipervolume(normal),
    hipervolume_do_payoff = calcular_hipervolume(normal_exatos)
  )
}

#' Hipervolume normalizado da primeira fronteira ao longo das gerações
#'
#' @param execucao Lista de `executar_nsga2()`.
#' @param payoff Lista de `resolver_tabela_de_payoff()`.
#' @return tibble com `geracao`, `pontos` e `hipervolume`.
medir_convergencia <- function(execucao, payoff) {
  exatos <- objetivos_do_payoff(payoff)
  purrr::map(execucao$historico, function(registro) {
    tibble::tibble(
      geracao = registro$geracao,
      pontos = nrow(unique(round(registro$objetivos, 9))),
      hipervolume = calcular_hipervolume(normalizar_pelo_payoff(registro$objetivos, exatos))
    )
  }) |>
    purrr::list_rbind()
}
