#' Compara os dados e as tabelas regenerados com os versionados
#'
#' É a conferência da reconstrução completa. Exige igualdade para os dados
#' sintéticos, para as tabelas da EDA e da wave 3, para a montagem da wave 4 e
#' para as linhas da wave 4 que vêm de problemas resolvidos até o ótimo. Os
#' valores de problemas que param no tempo-limite, e tudo o que deriva deles,
#' não se reproduzem entre máquinas e entram só como relato. Os tempos de
#' execução também.
#'
#' Os dados sintéticos guardam números com todos os dígitos, e a última casa
#' de ponto flutuante pode variar entre sistemas operacionais; por isso são
#' comparados com tolerância relativa. As tabelas são formatadas com poucas
#' casas e são comparadas linha a linha.
#'
#' @param referencia Revisão do Git com os versionados.
#' @param raiz Raiz do repositório.
#' @param tolerancia Tolerância relativa para os dados sintéticos.
#' @return tibble com `arquivo`, `regra`, `igual` e `detalhe`, uma linha por
#'   arquivo.
comparar_com_versionado <- function(referencia = "HEAD", raiz = here::here(), tolerancia = 1e-9) {
  versionados <- system2("git", c("-C", shQuote(raiz), "ls-tree", "-r", "--name-only", referencia), stdout = TRUE)
  regenerados <- unlist(purrr::map(c("data/synthetic", "outputs/tables"), function(pasta) {
    file.path(pasta, list.files(file.path(raiz, pasta), pattern = "[.]csv$"))
  }))
  arquivos <- union(grep("^(data/synthetic|outputs/tables)/[^/]+[.]csv$", versionados, value = TRUE), regenerados)
  purrr::map(sort(arquivos), function(arquivo) {
    regra <- regra_de_comparacao(arquivo)
    antes <- if (arquivo %in% versionados) {
      system2("git", c("-C", shQuote(raiz), "show", paste0(referencia, ":", arquivo)), stdout = TRUE)
    }
    caminho <- file.path(raiz, arquivo)
    depois <- if (file.exists(caminho)) readLines(caminho, encoding = "UTF-8", warn = FALSE)
    resultado <- if (is.null(antes) || is.null(depois)) {
      list(igual = FALSE, detalhe = if (is.null(antes)) "não versionado" else "não regenerado")
    } else {
      comparar_conteudo(antes, depois, regra, tolerancia)
    }
    tibble::tibble(arquivo = arquivo, regra = regra$tipo, igual = resultado$igual, detalhe = resultado$detalhe)
  }) |>
    purrr::list_rbind()
}

#' Regra de comparação de cada arquivo
#'
#' As linhas exatas da wave 4 são as das linhas de afinidade máxima e de custo
#' mínimo da tabela de payoff, que chegam ao ótimo, as da alocação realizada e
#' a do problema de custo do item 10.c; na estabilidade, todas as grandezas
#' menos o tempo, porque as linhas resolvidas nas realizações chegam ao ótimo.
#'
#' @param arquivo Caminho relativo à raiz.
#' @return Lista com `tipo` ("numérica", "exata", "linhas" ou "relato") e,
#'   para "linhas", a função que escolhe as linhas comparadas.
regra_de_comparacao <- function(arquivo) {
  nome <- basename(arquivo)
  primeira_coluna_em <- function(rotulos) function(tabela) tabela[[1]] %in% rotulos
  linhas_exatas <- list(
    "w4_payoff.csv" = primeira_coluna_em(c("Afinidade máxima", "Custo mínimo", "Alocação realizada")),
    "w4_avaliacao_referencias.csv" = primeira_coluna_em(c(
      "Alocação realizada", "Payoff, afinidade máxima", "Payoff, custo mínimo",
      "Item 10.c, custo mínimo preso na realizada"
    )),
    "w4_estabilidade.csv" = function(tabela) tabela[[2]] != "Segundos somados dos três passos"
  )
  if (startsWith(arquivo, "data/synthetic/")) {
    return(list(tipo = "numérica"))
  }
  if (nome %in% c("w4_conferencia.csv", "w4_porte.csv") || !grepl("^(w4_|achados_w4)", nome)) {
    return(list(tipo = "exata"))
  }
  if (nome %in% names(linhas_exatas)) {
    return(list(tipo = "linhas", escolher = linhas_exatas[[nome]]))
  }
  list(tipo = "relato")
}

#' Compara o conteúdo de um arquivo pela regra dele
#'
#' @param antes,depois Linhas do arquivo versionado e do regenerado.
#' @param regra Lista de `regra_de_comparacao()`.
#' @param tolerancia Tolerância relativa da regra "numérica".
#' @return Lista com `igual` e `detalhe`.
comparar_conteudo <- function(antes, depois, regra, tolerancia) {
  antes <- sub("\r$", "", antes)
  depois <- sub("\r$", "", depois)
  ler <- function(linhas) utils::read.csv(text = linhas, colClasses = "character", check.names = FALSE, encoding = "UTF-8")
  switch(regra$tipo,
    exata = ,
    relato = {
      diferentes <- if (length(antes) == length(depois)) sum(antes != depois) else NA
      list(
        igual = identical(antes, depois),
        detalhe = if (identical(antes, depois)) "" else if (is.na(diferentes)) "número de linhas diferente" else paste(diferentes, "linhas diferentes")
      )
    },
    linhas = {
      a <- ler(antes)
      d <- ler(depois)
      a <- a[regra$escolher(a), , drop = FALSE]
      d <- d[regra$escolher(d), , drop = FALSE]
      igual <- identical(names(a), names(d)) && identical(unname(as.list(a)), unname(as.list(d)))
      list(igual = igual, detalhe = if (igual) paste(nrow(a), "linhas exatas iguais") else "linhas exatas diferentes")
    },
    numérica = comparar_numericamente(ler(antes), ler(depois), tolerancia)
  )
}

#' Compara duas tabelas de dados com tolerância nas colunas numéricas
#'
#' @param a,d Tabelas lidas como texto.
#' @param tolerancia Tolerância relativa.
#' @return Lista com `igual` e `detalhe`.
comparar_numericamente <- function(a, d, tolerancia) {
  if (!identical(names(a), names(d)) || nrow(a) != nrow(d)) {
    return(list(igual = FALSE, detalhe = "estrutura diferente"))
  }
  maior <- 0
  for (coluna in names(a)) {
    if (identical(a[[coluna]], d[[coluna]])) next
    na <- suppressWarnings(as.numeric(a[[coluna]]))
    nd <- suppressWarnings(as.numeric(d[[coluna]]))
    numerica <- !is.na(na) & !is.na(nd)
    if (any(a[[coluna]][!numerica] != d[[coluna]][!numerica], na.rm = TRUE) ||
      any(is.na(a[[coluna]]) != is.na(d[[coluna]]))) {
      return(list(igual = FALSE, detalhe = paste("texto diferente na coluna", coluna)))
    }
    relativa <- abs(na[numerica] - nd[numerica]) / pmax(abs(na[numerica]), abs(nd[numerica]), .Machine$double.xmin)
    maior <- max(maior, relativa)
  }
  list(
    igual = maior <= tolerancia,
    detalhe = if (maior == 0) "" else paste("maior diferença relativa", format(maior, digits = 3))
  )
}
