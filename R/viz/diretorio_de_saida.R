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

#' Corrida da wave 4: completa ou reduzida
#'
#' Lida da variável de ambiente `W4_CORRIDA`, "completa" (o padrão) ou
#' "reduzida". A reduzida existe para a integração contínua no push caber no
#' tempo do job: cada problema do HiGHS para em 60 segundos em vez dos 600
#' registrados, só duas das nove sementes de variabilidade do NSGA-II rodam,
#' e a corrida exploratória tem 2.000 gerações em vez de 20.000. Nada disso
#' vale como resultado; a completa é a registrada.
#'
#' @return Lista com `reduzida` e, se for, os parâmetros reduzidos.
parametros_da_corrida_w4 <- function() {
  modo <- Sys.getenv("W4_CORRIDA", "completa")
  if (!modo %in% c("completa", "reduzida")) {
    stop("W4_CORRIDA deve ser 'completa' ou 'reduzida'; veio '", modo, "'.", call. = FALSE)
  }
  if (modo == "completa") {
    return(list(reduzida = FALSE))
  }
  list(reduzida = TRUE, tempo_limite_segundos = 60, sementes_de_variabilidade = 2, geracoes_exploratorias = 2000)
}

#' Aplica a corrida reduzida da wave 4 ao desenho e às emendas
#'
#' Na corrida completa devolve o objeto intacto, e os alvos que dependem dele
#' não refazem.
#'
#' @param objeto O desenho registrado, a emenda 1 ou a emenda 2.
#' @param corrida Lista de `parametros_da_corrida_w4()`.
#' @return O objeto, com o tempo-limite, as sementes de variabilidade ou as
#'   gerações exploratórias reduzidos quando a corrida é reduzida.
reduzir_corrida_w4 <- function(objeto, corrida) {
  if (!corrida$reduzida) {
    return(objeto)
  }
  # pluck(), e não $, porque em algumas emendas `metodo` é texto.
  if (!is.null(purrr::pluck(objeto, "metodo", "tempo_limite_segundos"))) {
    objeto$metodo$tempo_limite_segundos <- corrida$tempo_limite_segundos
  }
  if (!is.null(purrr::pluck(objeto, "sementes", "variabilidade"))) {
    objeto$sementes$variabilidade <- utils::head(objeto$sementes$variabilidade, corrida$sementes_de_variabilidade)
  }
  if (!is.null(purrr::pluck(objeto, "corrida_exploratoria", "geracoes"))) {
    objeto$corrida_exploratoria$geracoes <- corrida$geracoes_exploratorias
  }
  objeto
}

#' Diretório onde a pipeline grava tabelas ou figuras
#'
#' **Trava contra corrida reduzida em `outputs/`.** Com menos realizações que
#' as registradas, ou com a wave 4 reduzida, as tabelas e figuras sairiam
#' diferentes das versionadas, e gravá-las em `outputs/` misturaria uma
#' corrida parcial com a completa. Nesse caso a pipeline inteira grava numa
#' pasta temporária, dada por `W3_SAIDA_REDUZIDA` ou, sem ela, dentro de
#' `tempdir()`.
#'
#' @param tipo "tables" ou "figures".
#' @param realizacoes Número de realizações da corrida.
#' @param corrida_w4 Lista de `parametros_da_corrida_w4()`.
#' @return Caminho do diretório, que existe ao sair.
diretorio_de_saida <- function(tipo = c("tables", "figures"), realizacoes = realizacoes_w3(),
                               corrida_w4 = parametros_da_corrida_w4()) {
  tipo <- match.arg(tipo)
  if (realizacoes == realizacoes_registradas_w3() && !corrida_w4$reduzida) {
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
