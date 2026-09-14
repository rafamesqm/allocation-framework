#' Calcula a assimetria populacional de um vetor
#'
#' Usa a definição populacional, `m3 / m2^(3/2)` com divisor `n`, e não a
#' amostral corrigida. Não é indiferente: é essa a definição que a função
#' `skewness()` do Spark usa, e é dela que vêm todos os valores reais
#' registrados em `docs/mapeamento_schema.md`. Calcular a versão corrigida
#' aqui tornaria a comparação com o real inválida sem que nada indicasse o
#' problema, porque as duas só divergem visivelmente em amostra pequena.
#'
#' @param x Vetor numérico. `NA` e valores não finitos são descartados.
#' @return Assimetria, ou `NA_real_` se houver menos de três observações
#'   finitas ou variância nula.
calcular_assimetria <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 3) return(NA_real_)

  desvio <- x - mean(x)
  m2 <- mean(desvio^2)
  m3 <- mean(desvio^3)
  if (m2 == 0) return(NA_real_)

  m3 / m2^(3 / 2)
}

#' Resume as variáveis numéricas de uma tabela
#'
#' Quadro univariado padrão da frente não direcionada. Traz quantis e
#' assimetria ao lado de média e desvio de propósito: quase toda variável
#' contínua deste projeto é assimétrica à direita, e nessas a média e o
#' desvio são dominados pela cauda e dizem pouco sobre onde a massa está.
#' Foi por isso que o próprio gerador foi calibrado por casamento de
#' quantis, e o resumo tem que falar a mesma língua da calibração.
#'
#' `fracao_zero` não é detalhe de formatação: várias variáveis aqui têm
#' massa pontual em zero (`valor_contrato`, receita e custo por frente-mês),
#' e essa massa é o que distingue um modelo hurdle de uma log-normal comum.
#' Uma leitura que só olhe a mediana não a enxerga.
#'
#' **Os quantis vêm em duas versões, incondicional e só dos positivos.** Não
#' é redundância. Em variável com massa em zero as duas medem coisas
#' diferentes, e confundi-las produz conclusão errada com aparência de
#' rigor: em `custo` da margem por frente-mês, com 45% de zeros, o P25
#' incondicional é exatamente zero, enquanto o P25 dos positivos é 16 mil.
#' Os agregados reais deste projeto foram levantados nas duas formas,
#' dependendo do campo (ver `docs/mapeamento_schema.md`), e a comparação só
#' vale entre iguais. Em variável sem zeros as duas coincidem.
#'
#' @param tabela Data frame ou tibble.
#' @param colunas Nomes das colunas a resumir. `NULL` (padrão) usa todas as
#'   numéricas.
#' @return tibble com uma linha por variável: `variavel`, `n`, `n_ausente`,
#'   `fracao_zero`, `minimo`, `p25`, `mediana`, `p75`, `maximo`, `media`,
#'   `desvio`, `assimetria`, e as versões só dos positivos `p25_pos`,
#'   `mediana_pos`, `p75_pos` e `assimetria_pos`.
resumir_numericas <- function(tabela, colunas = NULL) {
  if (is.null(colunas)) {
    colunas <- names(tabela)[vapply(tabela, is.numeric, logical(1))]
  }

  purrr::map(colunas, function(nome_variavel) {
    valores <- tabela[[nome_variavel]]
    finitos <- valores[is.finite(valores)]
    positivos <- finitos[finitos > 0]
    quantis <- stats::quantile(finitos, c(0.25, 0.5, 0.75), names = FALSE)

    quantis_pos <- if (length(positivos) == 0) {
      rep(NA_real_, 3)
    } else {
      stats::quantile(positivos, c(0.25, 0.5, 0.75), names = FALSE)
    }

    tibble::tibble(
      variavel = nome_variavel,
      n = length(finitos),
      n_ausente = sum(!is.finite(valores)),
      fracao_zero = mean(finitos == 0),
      minimo = min(finitos),
      p25 = quantis[1],
      mediana = quantis[2],
      p75 = quantis[3],
      maximo = max(finitos),
      media = mean(finitos),
      desvio = stats::sd(finitos),
      assimetria = calcular_assimetria(finitos),
      p25_pos = quantis_pos[1],
      mediana_pos = quantis_pos[2],
      p75_pos = quantis_pos[3],
      assimetria_pos = calcular_assimetria(positivos)
    )
  }) |>
    purrr::list_rbind()
}
