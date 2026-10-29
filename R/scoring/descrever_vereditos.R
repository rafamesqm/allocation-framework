#' Lê a rubrica preenchida pelo autor, se ela já existe
#'
#' A pipeline gera só o modelo em branco da rubrica. A cópia com os vereditos
#' vive em `modelling/leitura/`, que nenhum alvo escreve; aqui ela só é lida,
#' para a contagem dos vereditos entrar no achado W3.5.4.
#'
#' @param caminho Caminho da rubrica preenchida.
#' @return tibble com a rubrica, ou `NULL` enquanto a leitura não foi feita.
ler_vereditos <- function(caminho = here::here("modelling", "leitura", "rubrica_preenchida.csv")) {
  if (!file.exists(caminho)) return(NULL)
  readr::read_csv(caminho, show_col_types = FALSE)
}

#' Escreve por extenso um número pequeno de casos
#'
#' @param n Inteiro entre 1 e 10.
#' @return Texto, como "três".
escrever_por_extenso <- function(n) {
  extenso <- c("um", "dois", "três", "quatro", "cinco", "seis", "sete", "oito", "nove", "dez")
  if (n >= 1 && n <= length(extenso)) extenso[n] else formatar_inteiro(n)
}

#' Põe um veredito da rubrica no plural
#'
#' @param veredito Texto da escala da rubrica.
#' @return Texto no plural, como "plausíveis com ressalva".
pluralizar_veredito <- function(veredito) {
  sub("^(\\S*?)l\\b", "\\1is", veredito, perl = TRUE)
}

#' Descreve os vereditos de um ramo numa frase
#'
#' "Os quatro casos do colaborativo foram julgados implausíveis" quando todos
#' têm o mesmo veredito, e a contagem de cada um quando não têm.
#'
#' @param vereditos tibble da rubrica preenchida, com `Ramo` e `Veredito`.
#' @param ramo Ramo a descrever.
#' @return Texto.
descrever_vereditos_do_ramo <- function(vereditos, ramo) {
  escala <- c("plausível", "plausível com ressalva", "implausível", "indeterminável")
  do_ramo <- vereditos$Veredito[vereditos$Ramo == ramo]
  contagem <- table(factor(do_ramo, levels = escala))
  contagem <- contagem[contagem > 0]
  total <- escrever_por_extenso(length(do_ramo))

  if (length(contagem) == 1) {
    rotulo <- if (length(do_ramo) == 1) names(contagem) else pluralizar_veredito(names(contagem))
    return(paste0("os ", total, " casos do ", ramo, " foram julgados ", rotulo))
  }
  partes <- purrr::map2_chr(names(contagem), as.integer(contagem), \(v, n) {
    paste(escrever_por_extenso(n), if (n == 1) v else pluralizar_veredito(v))
  })
  paste0(
    "os ", total, " do ", ramo, ", ",
    paste(utils::head(partes, -1), collapse = ", "), " e ", utils::tail(partes, 1)
  )
}
