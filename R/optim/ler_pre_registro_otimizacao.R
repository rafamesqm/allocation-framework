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
