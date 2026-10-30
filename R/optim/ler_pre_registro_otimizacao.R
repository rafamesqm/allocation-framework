#' Lê o pré-registro da otimização da wave 4
#'
#' Mesma disciplina do pré-registro da wave 3: o arquivo fixa as decisões do
#' autor antes de qualquer problema ser resolvido, e toda função da wave 4
#' recebe os valores daqui, pelo alvo `pre_registro_otimizacao`. O que vem da
#' wave 3 (janela, divisão temporal, semente de desempate) continua sendo lido
#' de `config/pre_registro.yml`, e não é repetido neste arquivo.
#'
#' @param caminho Caminho do arquivo YAML.
#' @return Lista aninhada com o conteúdo do arquivo.
ler_pre_registro_otimizacao <- function(caminho = here::here("config", "pre_registro_otimizacao.yml")) {
  if (!file.exists(caminho)) {
    stop("Pré-registro da otimização não encontrado em '", caminho, "'.", call. = FALSE)
  }
  registro <- yaml::read_yaml(caminho)

  obrigatorios <- c(
    "assentos", "nivel", "elegibilidade", "escore", "zona_morta", "carga",
    "objetivos", "capacidade_como_objetivo", "metodo", "avaliacao", "achados",
    "verificacao"
  )
  faltantes <- setdiff(obrigatorios, names(registro))
  if (length(faltantes) > 0) {
    stop("Faltam seções no pré-registro da otimização: ", paste(faltantes, collapse = ", "), call. = FALSE)
  }
  registro
}

#' O desenho registrado, sem as emendas
#'
#' A montagem e a tabela de payoff dependem só do desenho. Separá-lo das
#' emendas evita que uma emenda nova as refaça na pipeline.
#'
#' @param registro Lista de `ler_pre_registro_otimizacao()`.
#' @return `registro` sem o campo `emendas`.
separar_desenho_registrado <- function(registro) {
  registro[setdiff(names(registro), "emendas")]
}

#' Localiza uma emenda pelo nome
#'
#' @param registro Lista de `ler_pre_registro_otimizacao()`.
#' @param nome Valor do campo `nome` da emenda.
#' @return A emenda. Erro se não houver exatamente uma com esse nome.
localizar_emenda <- function(registro, nome) {
  nomes <- vapply(registro$emendas, \(e) e$nome %||% NA_character_, character(1))
  encontradas <- which(nomes == nome)
  if (length(encontradas) != 1) {
    stop("Emenda '", nome, "' não encontrada uma única vez no pré-registro.", call. = FALSE)
  }
  registro$emendas[[encontradas]]
}
