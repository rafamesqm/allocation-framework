#' McNemar para pares agrupados, de Durkalski et al. (2003)
#'
#' O McNemar comum supõe pares independentes. No conteúdo, os pares de teste
#' se agrupam por consultor: o mesmo consultor aparece em vários projetos, e
#' os acertos dele tendem a andar juntos. A estatística de Durkalski et al.
#' (2003, Statistics in Medicine, 22(15), 2417-2428) soma, por grupo, a
#' diferença entre os pares que favorecem o modelo e os que favorecem o piso,
#' `d_k = b_k - c_k`, e compara `(soma de d_k)^2 / soma de d_k^2` a uma
#' qui-quadrado com um grau de liberdade. É análise descritiva, posterior ao
#' resultado, e não substitui o teste pré-registrado.
#'
#' @param acerto_modelo,acerto_piso Vetores de zeros e uns, pareados.
#' @param grupo Vetor com o grupo de cada par, o consultor de teste.
#' @param alfa Nível do teste.
#' @return Lista com `estatistica`, `valor_p`, `a_favor` e `contra`, as duas
#'   últimas lógicas: rejeição na direção do modelo ou na do piso.
testar_mcnemar_agrupado <- function(acerto_modelo, acerto_piso, grupo, alfa = 0.05) {
  stopifnot(length(acerto_modelo) == length(acerto_piso), length(grupo) == length(acerto_modelo))
  d <- tapply(acerto_modelo - acerto_piso, grupo, sum)
  soma <- sum(d)
  denominador <- sum(d^2)
  estatistica <- if (denominador == 0) 0 else soma^2 / denominador
  valor_p <- stats::pchisq(estatistica, df = 1, lower.tail = FALSE)
  list(
    estatistica = estatistica, valor_p = valor_p,
    a_favor = valor_p <= alfa && soma > 0, contra = valor_p <= alfa && soma < 0
  )
}

#' Estatística padronizada do McNemar comum
#'
#' `(b - c) / sqrt(b + c)`, que sob pares independentes e sem efeito tem
#' variância perto de 1. A variância dela entre realizações acima de 1 é o
#' sinal de que os pares não são independentes.
#'
#' @param b,c Pares discordantes a favor do modelo e do piso.
#' @return A estatística, ou zero sem discordantes.
padronizar_mcnemar <- function(b, c) {
  ifelse(b + c > 0, (b - c) / sqrt(b + c), 0)
}

#' Resume a dependência entre pares de uma avaliação
#'
#' O McNemar comum, nas duas direções, e o agrupado por consultor, também
#' nas duas direções, mais a estatística padronizada do comum.
#'
#' @param acerto_modelo,acerto_piso Vetores de zeros e uns, pareados.
#' @param consultor Consultor de cada par.
#' @param alfa Nível do teste.
#' @return tibble de uma linha.
resumir_dependencia_dos_pares <- function(acerto_modelo, acerto_piso, consultor, alfa = 0.05) {
  comum <- testar_mcnemar_exato(acerto_modelo, acerto_piso, alfa)
  agrupado <- testar_mcnemar_agrupado(acerto_modelo, acerto_piso, consultor, alfa)
  tibble::tibble(
    mcnemar_padronizado = padronizar_mcnemar(comum$b, comum$c),
    comum_rejeita_a_favor = as.numeric(comum$valor_p <= alfa && comum$b > comum$c),
    comum_rejeita_contra = as.numeric(comum$valor_p <= alfa && comum$b < comum$c),
    agrupado_valor_p = agrupado$valor_p,
    agrupado_rejeita_a_favor = as.numeric(agrupado$a_favor),
    agrupado_rejeita_contra = as.numeric(agrupado$contra),
    consultores_de_teste = dplyr::n_distinct(consultor)
  )
}
