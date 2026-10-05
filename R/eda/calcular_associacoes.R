#' Calcula a correlação entre todos os pares de variáveis numéricas
#'
#' Spearman por padrão, e não Pearson, porque quase toda contínua deste
#' projeto é fortemente assimétrica à direita (ver bloco I.2). Pearson mede
#' associação linear e é dominado pelos mesmos valores extremos que já
#' tornam média e desvio pouco informativos aqui; Spearman mede associação
#' monotônica sobre postos e é insensível a isso.
#'
#' **Ressalva de comparabilidade:** os alvos reais registrados em
#' `docs/mapeamento_schema.md` vieram de `CORR()` no Spark, que é Pearson.
#' Confrontar Spearman com eles compara coisas diferentes. Por isso a função
#' devolve as duas, e o confronto com o real usa Pearson enquanto a leitura
#' da estrutura usa Spearman.
#'
#' Devolve formato longo, um par por linha, em vez de matriz: é o formato que
#' junta com o registro de procedência e que alimenta a figura sem precisar
#' derreter a matriz depois.
#'
#' @param tabela Data frame ou tibble.
#' @param colunas Colunas numéricas a cruzar. `NULL` usa todas as numéricas.
#' @return tibble com `variavel_a`, `variavel_b`, `spearman`, `pearson` e
#'   `n`, uma linha por par não ordenado, sem a diagonal.
calcular_correlacoes <- function(tabela, colunas = NULL) {
  if (is.null(colunas)) {
    colunas <- names(tabela)[vapply(tabela, is.numeric, logical(1))]
  }

  pares <- utils::combn(colunas, 2, simplify = FALSE)

  purrr::map(pares, function(par) {
    a <- tabela[[par[1]]]
    b <- tabela[[par[2]]]
    completos <- is.finite(a) & is.finite(b)

    tibble::tibble(
      variavel_a = par[1],
      variavel_b = par[2],
      spearman = stats::cor(a[completos], b[completos], method = "spearman"),
      pearson = stats::cor(a[completos], b[completos], method = "pearson"),
      n = sum(completos)
    )
  }) |>
    purrr::list_rbind()
}

#' Calcula o V de Cramér com correção de viés entre duas categóricas
#'
#' Medida de associação entre categóricas, normalizada entre 0 e 1, o que
#' permite comparar pares com números diferentes de categorias. Zero
#' significa independência; um, dependência completa.
#'
#' **A correção de viés não é refinamento opcional, é o que torna a medida
#' interpretável aqui.** O V clássico não é centrado em zero sob
#' independência: por ser uma raiz de soma de quadrados, seu valor esperado é
#' estritamente positivo e cresce com o número de categorias e diminui com o
#' tamanho da amostra, aproximadamente `sqrt(max(r-1, c-1) / n)`. Neste
#' projeto isso é severo. Com 285 consultores, `nivel_senioridade` (5
#' categorias) contra `localizacao` (10) tem V esperado em torno de 0,18
#' **mesmo se as duas forem sorteadas de forma completamente independente**,
#' que é exatamente o caso do gerador. Medido sem correção, o valor observado
#' foi 0,183, e lê-lo como associação real produziria um achado falso.
#'
#' A correção é a de Bergsma (2013, *A bias correction for Cramér's V and
#' Tschuprow's T*): desconta do qui-quadrado a parcela esperada sob
#' independência e ajusta a contagem de categorias na mesma direção. Pares
#' independentes passam a medir perto de zero, e o limiar de leitura volta a
#' ter significado.
#'
#' A estatística qui-quadrado é calculada diretamente da tabela de
#' contingência, em vez de por `stats::chisq.test()`, para evitar o aviso de
#' aproximação incorreta que aquela função emite sempre que alguma célula
#' esperada é pequena. Aqui o aviso seria ruído: o V é medida descritiva de
#' força de associação, não teste de hipótese, e nenhum valor-p é reportado a
#' partir dele.
#'
#' @param x,y Vetores categóricos de mesmo comprimento.
#' @param corrigir Se `TRUE` (padrão), aplica a correção de Bergsma.
#' @return V de Cramér, entre 0 e 1, ou `NA_real_` se alguma das variáveis
#'   for constante.
calcular_v_cramer <- function(x, y, corrigir = TRUE) {
  contingencia <- table(x, y)
  n_linhas <- nrow(contingencia)
  n_colunas <- ncol(contingencia)
  if (n_linhas < 2 || n_colunas < 2) return(NA_real_)

  n <- sum(contingencia)
  esperado <- outer(rowSums(contingencia), colSums(contingencia)) / n
  qui_quadrado <- sum((contingencia - esperado)^2 / esperado)
  phi2 <- qui_quadrado / n

  if (!corrigir) {
    return(sqrt(phi2 / min(n_linhas - 1, n_colunas - 1)))
  }

  phi2_corrigido <- max(0, phi2 - (n_linhas - 1) * (n_colunas - 1) / (n - 1))
  linhas_corrigidas <- n_linhas - (n_linhas - 1)^2 / (n - 1)
  colunas_corrigidas <- n_colunas - (n_colunas - 1)^2 / (n - 1)

  sqrt(phi2_corrigido / min(linhas_corrigidas - 1, colunas_corrigidas - 1))
}

#' Calcula o V de Cramér entre todos os pares de variáveis categóricas
#'
#' @param tabela Data frame ou tibble.
#' @param colunas Colunas categóricas a cruzar.
#' @return tibble com `variavel_a`, `variavel_b`, `v_cramer` e `n`.
calcular_associacoes_categoricas <- function(tabela, colunas) {
  pares <- utils::combn(colunas, 2, simplify = FALSE)

  purrr::map(pares, function(par) {
    tibble::tibble(
      variavel_a = par[1],
      variavel_b = par[2],
      v_cramer = calcular_v_cramer(tabela[[par[1]]], tabela[[par[2]]]),
      n = nrow(tabela)
    )
  }) |>
    purrr::list_rbind()
}
