#' Lê o pré-registro da avaliação da wave 3
#'
#' O pré-registro fixa sementes, divisões, listas, grades de hiperparâmetros,
#' o teste e as regras de adoção antes de qualquer modelo ser ajustado (ver
#' `config/pre_registro.yml`). Toda função da wave 3 recebe esses valores
#' daqui, pelo alvo `pre_registro` da pipeline, e nunca de default próprio:
#' é o que permite aos testes acusar valor fora do registrado.
#'
#' @param caminho Caminho do arquivo YAML.
#' @return Lista aninhada com o conteúdo do arquivo.
ler_pre_registro <- function(caminho = here::here("config", "pre_registro.yml")) {
  if (!file.exists(caminho)) {
    stop("Pré-registro não encontrado em '", caminho, "'.", call. = FALSE)
  }
  registro <- yaml::read_yaml(caminho)

  obrigatorios <- c(
    "dados", "sementes", "divisoes", "metrica", "colaborativo", "conteudo",
    "teste", "regras_de_adocao", "hibrido", "segundo_nivel", "estabilidade",
    "verificacao"
  )
  faltantes <- setdiff(obrigatorios, names(registro))
  if (length(faltantes) > 0) {
    stop("Faltam seções no pré-registro: ", paste(faltantes, collapse = ", "), call. = FALSE)
  }
  registro
}

#' Converte a janela do pré-registro no formato de `janela_de_analise()`
#'
#' @param registro Lista de `ler_pre_registro()`.
#' @return Vetor `Date` nomeado com `inicio` e `fim`.
janela_do_pre_registro <- function(registro) {
  c(
    inicio = as.Date(registro$dados$janela$inicio),
    fim = as.Date(registro$dados$janela$fim)
  )
}
