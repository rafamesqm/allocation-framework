#' Número de realizações da wave 3 nesta corrida
#'
#' Lido da variável de ambiente `W3_REALIZACOES`. Sem ela, vale o número
#' registrado no pré-registro (`sementes$realizacoes`), que é o da corrida
#' completa. A integração contínua usa um número menor no push, para caber no
#' tempo do job.
#'
#' @return Inteiro entre 1 e o número registrado.
realizacoes_w3 <- function() {
  registradas <- realizacoes_registradas_w3()
  valor <- Sys.getenv("W3_REALIZACOES", as.character(registradas))
  n <- suppressWarnings(as.integer(valor))
  if (is.na(n) || n < 1 || n > registradas) {
    stop(
      "W3_REALIZACOES deve ser um inteiro entre 1 e ", registradas, "; veio '", valor, "'.",
      call. = FALSE
    )
  }
  n
}

#' Número de realizações registrado no pré-registro da wave 3
#'
#' @return Inteiro.
realizacoes_registradas_w3 <- function() {
  sementes <- yaml::read_yaml(here::here("config", "pre_registro.yml"))$sementes$realizacoes
  as.integer(sementes[2] - sementes[1] + 1)
}

#' Diretório onde a pipeline grava tabelas ou figuras
#'
#' **Trava contra corrida reduzida em `outputs/`.** Com menos realizações que
#' as registradas, as tabelas e figuras da wave 3 sairiam diferentes das
#' versionadas, e gravá-las em `outputs/` misturaria uma corrida parcial com a
#' completa. Nesse caso a pipeline inteira grava numa pasta temporária, dada
#' por `W3_SAIDA_REDUZIDA` ou, sem ela, dentro de `tempdir()`.
#'
#' @param tipo "tables" ou "figures".
#' @param realizacoes Número de realizações da corrida.
#' @return Caminho do diretório, que existe ao sair.
diretorio_de_saida <- function(tipo = c("tables", "figures"), realizacoes = realizacoes_w3()) {
  tipo <- match.arg(tipo)
  if (realizacoes == realizacoes_registradas_w3()) {
    return(here::here("outputs", tipo))
  }
  raiz <- Sys.getenv("W3_SAIDA_REDUZIDA", file.path(tempdir(), "saida_reduzida"))
  destino <- file.path(raiz, tipo)
  if (normalizePath(destino, mustWork = FALSE) == normalizePath(here::here("outputs", tipo), mustWork = FALSE)) {
    stop("Corrida reduzida não grava em outputs/; aponte W3_SAIDA_REDUZIDA para outra pasta.", call. = FALSE)
  }
  dir.create(destino, recursive = TRUE, showWarnings = FALSE)
  destino
}
