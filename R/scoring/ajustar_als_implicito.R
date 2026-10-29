#' Ajusta a fatoração para feedback implícito por mínimos quadrados alternados
#'
#' Hu, Koren e Volinsky (2008). Toda célula de `R` entra na perda: a
#' preferência é 1 onde houve alocação e 0 no resto, e a confiança é
#' `1 + peso`, com `peso = alfa * log(1 + horas)` nas células observadas e
#' zero nas demais. A perda é
#'
#' `soma_ui c_ui (p_ui - x_u' y_i)^2 + lambda (soma_u |x_u|^2 + soma_i |y_i|^2)`,
#'
#' e cada passo resolve um lado com o outro fixo, em forma fechada.
#'
#' **Com `alfa = 0` a confiança é 1 em toda célula**, e o passo vira uma
#' regressão ridge comum, `X = R Y (Y'Y + lambda I)^-1`, resolvida de uma vez
#' para todas as linhas. Com confiança, cada linha tem o seu sistema, montado
#' com o atalho do artigo: `Y'C_u Y = Y'Y + Y'(C_u - I)Y`, em que o segundo
#' termo só envolve as células observadas.
#'
#' Linha ou coluna sem nenhuma observação recebe fator nulo, porque o lado
#' direito do sistema é zero. É o que torna o consultor sem histórico, e o
#' projeto que não aparece na estimação, impossíveis de pontuar por aqui.
#'
#' @param r Matriz esparsa de ocorrência, consultores nas linhas e projetos
#'   nas colunas, de `construir_matriz_r()`.
#' @param peso `NULL` para confiança 1 em toda célula, ou matriz esparsa com
#'   o mesmo padrão de `r` e o valor `alfa * log(1 + horas)` em cada célula
#'   observada.
#' @param fatores Número de fatores latentes.
#' @param lambda Regularização.
#' @param iteracoes Número de passos alternados, cada um atualizando os dois
#'   lados.
#' @param semente Semente da inicialização dos fatores dos projetos.
#' @param registrar_em Iterações em que o estado é guardado. Permite avaliar
#'   10 e 20 iterações a partir da mesma corrida, que passa pelo estado de 10
#'   a caminho do de 20.
#' @return Lista nomeada pelas iterações de `registrar_em`, cada elemento com
#'   `x` (consultores por fatores) e `y` (projetos por fatores).
ajustar_als_implicito <- function(r, peso = NULL, fatores, lambda, iteracoes,
                                  semente, registrar_em = iteracoes) {
  stopifnot(all(registrar_em <= iteracoes), fatores >= 1, lambda > 0)
  identidade <- diag(lambda, fatores)

  y <- withr::with_seed(semente, matrix(
    stats::rnorm(ncol(r) * fatores, sd = 0.01), ncol(r), fatores
  ))
  rownames(y) <- colnames(r)

  estados <- list()
  for (passo in seq_len(iteracoes)) {
    x <- resolver_lado_als(r, peso, y, identidade)
    y <- resolver_lado_als(Matrix::t(r), if (is.null(peso)) NULL else Matrix::t(peso), x, identidade)
    if (passo %in% registrar_em) {
      estados[[as.character(passo)]] <- list(x = x, y = y)
    }
  }
  estados
}

#' Resolve um lado da fatoração com o outro fixo
#'
#' @param r Matriz esparsa de ocorrência, com as linhas do lado a resolver.
#' @param peso `NULL` ou matriz esparsa de confiança menos um.
#' @param fixo Fatores do outro lado, uma linha por coluna de `r`.
#' @param identidade Matriz `lambda * I`.
#' @return Matriz de fatores, uma linha por linha de `r`.
resolver_lado_als <- function(r, peso, fixo, identidade) {
  gram <- crossprod(fixo) + identidade

  if (is.null(peso)) {
    resolvido <- as.matrix(r %*% fixo) %*% solve(gram)
  } else {
    # As células vêm do padrão de `r`, e não do de `peso`: uma alocação com
    # zero hora tem peso zero, pode não ficar guardada na matriz de peso, e
    # ainda assim tem preferência 1.
    celulas <- Matrix::summary(methods::as(r, "TsparseMatrix"))
    peso_da_celula <- as.numeric(peso[cbind(celulas$i, celulas$j)])
    resolvido <- matrix(0, nrow(r), ncol(fixo))
    for (u in unique(celulas$i)) {
      da_linha <- celulas$i == u
      lado_fixo <- fixo[celulas$j[da_linha], , drop = FALSE]
      w <- peso_da_celula[da_linha]
      sistema <- gram + crossprod(lado_fixo, w * lado_fixo)
      resolvido[u, ] <- solve(sistema, crossprod(lado_fixo, 1 + w))
    }
  }
  rownames(resolvido) <- rownames(r)
  resolvido
}

#' Monta a matriz de confiança menos um para a fatoração
#'
#' @param alocacoes tibble de alocações, com `horas_mes` e `duracao_dias`.
#' @param consultores,projetos Tabelas que definem as linhas e colunas.
#' @param alfa Peso do logaritmo das horas.
#' @return `NULL` quando `alfa` é zero; senão matriz esparsa com
#'   `alfa * log(1 + horas)` em cada célula observada.
construir_peso_de_confianca <- function(alocacoes, consultores, projetos, alfa) {
  if (alfa == 0) return(NULL)
  sinal <- construir_sinal_implicito(alocacoes, alfa = alfa)
  construir_matriz_r(sinal, consultores, projetos, valor = sinal$confianca - 1)
}
