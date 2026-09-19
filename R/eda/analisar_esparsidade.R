#' Mede a esparsidade da matriz de interações
#'
#' Densidade sobre o grid completo, e não sobre quem tem alguma interação:
#' consultor e projeto sem alocação fazem parte do denominador, porque é
#' sobre o grid inteiro que um recomendador precisa decidir. Mesmo princípio
#' de `construir_matriz_r()`.
#'
#' @param alocacoes tibble de alocações.
#' @param consultores,projetos tibbles que definem o grid.
#' @return tibble de uma linha com `observacoes`, `celulas`, `densidade`,
#'   `consultores`, `projetos`.
medir_esparsidade <- function(alocacoes, consultores, projetos) {
  n_consultores <- nrow(consultores)
  n_projetos <- nrow(projetos)
  n_celulas <- n_consultores * n_projetos
  n_observacoes <- nrow(alocacoes)

  tibble::tibble(
    observacoes = n_observacoes,
    celulas = n_celulas,
    densidade = n_observacoes / n_celulas,
    consultores = n_consultores,
    projetos = n_projetos
  )
}

#' Compara Poisson e Binomial Negativa como modelos da sequência de grau
#'
#' A escolha entre as duas não é detalhe de ajuste: Poisson impõe variância
#' igual à média, e sequência de grau com variância muito maior que a média
#' significa que poucos nós concentram muita interação. É essa concentração
#' que degrada método de vizinhança, porque a vizinhança dos nós de grau
#' baixo fica vazia ou dominada pelos poucos nós populares.
#'
#' O índice de dispersão (variância sobre média) é o diagnóstico direto:
#' vale 1 sob Poisson e cresce com a sobredispersão. O ajuste por máxima
#' verossimilhança das duas famílias, comparado por AIC, formaliza a leitura.
#'
#' @param graus Vetor inteiro de graus observados.
#' @return tibble de uma linha com `media`, `variancia`, `indice_dispersao`,
#'   `aic_poisson`, `aic_binomial_negativa` e `ganho_aic`.
ajustar_modelo_de_grau <- function(graus) {
  graus <- graus[is.finite(graus)]
  media <- mean(graus)
  variancia <- stats::var(graus)

  log_veros_poisson <- sum(stats::dpois(graus, lambda = media, log = TRUE))
  aic_poisson <- 2 * 1 - 2 * log_veros_poisson

  # A Binomial Negativa pode não convergir em amostra degenerada; nesse caso
  # a comparação simplesmente não é reportada, em vez de a função quebrar.
  ajuste_bn <- try(
    suppressWarnings(MASS::fitdistr(graus, densfun = "negative binomial")),
    silent = TRUE
  )
  aic_bn <- if (inherits(ajuste_bn, "try-error")) {
    NA_real_
  } else {
    2 * 2 - 2 * ajuste_bn$loglik
  }

  tibble::tibble(
    n = length(graus),
    media = media,
    variancia = variancia,
    indice_dispersao = variancia / media,
    aic_poisson = aic_poisson,
    aic_binomial_negativa = aic_bn,
    ganho_aic = aic_poisson - aic_bn
  )
}

#' Calcula a curva de Lorenz e o Gini de uma sequência de grau
#'
#' Quantifica concentração: quanto da interação total está nos nós mais
#' ativos. É a leitura que sustenta a discussão de cauda longa e de viés de
#' popularidade.
#'
#' @param graus Vetor numérico não negativo.
#' @return Lista com `curva` (tibble de `fracao_nos` e `fracao_interacoes`) e
#'   `gini`.
calcular_lorenz <- function(graus) {
  ordenados <- sort(graus)
  n <- length(ordenados)
  total <- sum(ordenados)

  gini <- sum((2 * seq_len(n) - n - 1) * ordenados) / (n * total)

  list(
    curva = tibble::tibble(
      fracao_nos = c(0, seq_len(n) / n),
      fracao_interacoes = c(0, cumsum(ordenados) / total)
    ),
    gini = gini
  )
}

#' Mede o acerto de um recomendador que só olha popularidade
#'
#' Cremonesi, Koren e Turrin (2010) mostram que recomendar simplesmente os
#' itens mais populares é uma linha de base surpreendentemente forte, e que
#' modelo avaliado sem ela pode parecer bom sem ser. Este é o número que o
#' recomendador da wave 3 precisa bater para justificar sua existência.
#'
#' A popularidade de cada consultor é recalculada **excluindo a alocação
#' sendo prevista**. Sem isso a alocação que se quer prever entra na própria
#' contagem que a prevê, e o acerto sai otimista por construção, sobretudo
#' para quem tem grau baixo.
#'
#' Ressalva de escopo: isto é um deixa-um-de-fora sobre o conjunto inteiro,
#' não uma avaliação sob protocolo. A divisão treino e teste, a métrica
#' definitiva e a amostragem de negativos são decisão do bloco II.7, e o
#' número aqui deve ser lido como ordem de grandeza da linha de base, não
#' como resultado de avaliação.
#'
#' @param alocacoes tibble com `consultor_id` e `projeto_id`.
#' @param consultores tibble com `consultor_id`, que define o universo.
#' @param tamanhos Vetor de tamanhos de lista a avaliar.
#' @return tibble com `n_recomendados`, `acertos`, `tentativas` e `recall`.
medir_linha_base_popularidade <- function(alocacoes, consultores, tamanhos = c(5, 10, 20, 50)) {
  popularidade <- as.integer(table(
    factor(alocacoes$consultor_id, levels = consultores$consultor_id)
  ))
  indice <- match(alocacoes$consultor_id, consultores$consultor_id)
  contagem_focal <- popularidade[indice]

  # Quantos consultores ficam estritamente acima do focal depois de tirar a
  # alocação que se quer prever. Como as contagens são inteiras, tirar uma
  # unidade do focal equivale a perguntar quantos outros têm contagem maior
  # ou igual à dele. Forma fechada, sem ordenar 285 elementos por alocação.
  maiores_ou_iguais <- vapply(
    contagem_focal, function(alvo) sum(popularidade >= alvo), integer(1)
  )
  posicao <- maiores_ou_iguais - 1L

  purrr::map(tamanhos, function(n_recomendados) {
    acertos <- posicao < n_recomendados
    n_acertos <- sum(acertos)
    n_tentativas <- length(acertos)

    tibble::tibble(
      n_recomendados = n_recomendados,
      acertos = n_acertos,
      tentativas = n_tentativas,
      recall = n_acertos / n_tentativas
    )
  }) |>
    purrr::list_rbind()
}
