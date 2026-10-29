#' Carrega as tabelas do projeto, parametrizado por fonte
#'
#' Ponto de entrada único de dados (ver docs/predefinicoes.md). A lógica de
#' negócio que consome o resultado é idêntica nas duas fontes, só a origem
#' muda (CLAUDE.md, regra 7): quem chama recebe a mesma lista de tabelas,
#' com os mesmos nomes e as mesmas colunas, sem saber de onde vieram.
#'
#' `synthetic` é o padrão, lê de `data/synthetic/`, a única fonte de dados
#' versionada do repositório, e é o que a pipeline roda e o que o auditor
#' executa. Espelha `salvar_synthetic()`, que é quem escreve esses arquivos.
#'
#' **`databricks` não está implementada como código, de propósito.** Não é
#' pendência nem TODO, é uma fronteira do projeto, por dois motivos que se
#' somam. Primeiro, a regra 6 do `CLAUDE.md` proíbe persistir, cachear ou
#' transformar dado real dentro de pipeline versionada: uma implementação
#' que valesse a pena versionar teria que fazer exatamente isso. Segundo, o
#' acesso ao Unity Catalog encerrou em 2026-09-09 (ver docs/roadmap.md), e
#' nenhuma implementação poderia ser testada nem executada hoje. O parâmetro
#' existe porque a arquitetura de acesso parametrizado é uma decisão de
#' projeto registrada, e chamar com essa fonte para de forma explícita, em
#' vez de silenciosamente devolver sintético como se fosse real.
#'
#' Use `tabelas` para ler só o que a análise precisa.
#'
#' @param fonte `"synthetic"` (padrão) ou `"databricks"`.
#' @param tabelas Vetor de nomes de tabela, sem extensão. `NULL` (padrão)
#'   carrega todas as disponíveis na fonte.
#' @param diretorio Diretório de leitura da fonte `synthetic`.
#' @return Lista nomeada de tibbles, um elemento por tabela, ordenada pelo
#'   nome da tabela.
carregar_dados <- function(fonte = c("synthetic", "databricks"),
                            tabelas = NULL,
                            diretorio = here::here("data", "synthetic")) {
  fonte <- match.arg(fonte)

  if (fonte == "databricks") {
    stop(
      "A fonte 'databricks' não está implementada como código, por decisão ",
      "de projeto: a regra 6 do CLAUDE.md proíbe transformar dado real em ",
      "pipeline versionada, e o acesso ao Unity Catalog encerrou em ",
      "2026-09-09. Use fonte = 'synthetic'.",
      call. = FALSE
    )
  }

  disponiveis <- sort(tools::file_path_sans_ext(
    list.files(diretorio, pattern = "\\.csv$")
  ))
  if (length(disponiveis) == 0) {
    stop("Nenhum CSV encontrado em '", diretorio, "'. Rode targets::tar_make().", call. = FALSE)
  }

  if (is.null(tabelas)) {
    tabelas <- disponiveis
  } else {
    ausentes <- setdiff(tabelas, disponiveis)
    if (length(ausentes) > 0) {
      stop("Tabela não encontrada na fonte: ", paste(ausentes, collapse = ", "), call. = FALSE)
    }
  }

  caminhos <- file.path(diretorio, paste0(tabelas, ".csv"))
  dados <- stats::setNames(
    lapply(caminhos, readr::read_csv, show_col_types = FALSE, progress = FALSE),
    tabelas
  )
  if ("alocacoes" %in% names(dados)) conferir_pares_unicos(dados$alocacoes)
  dados
}

#' Para a leitura se algum par consultor e projeto aparecer duas vezes
#'
#' A popularidade conta projetos distintos por consultor (ver
#' docs/dicionario_de_dados.md). Com pares únicos, isso coincide com contar
#' alocações; esta verificação garante que a coincidência valha na base lida.
#'
#' @param alocacoes tibble de alocações, com `consultor_id` e `projeto_id`.
#' @return `alocacoes`, invisível, se não houver par repetido.
conferir_pares_unicos <- function(alocacoes) {
  repetidos <- sum(duplicated(alocacoes[c("consultor_id", "projeto_id")]))
  if (repetidos > 0) {
    stop(
      "Há ", repetidos, " par(es) consultor e projeto repetido(s) nas alocações; ",
      "a popularidade por projetos distintos deixaria de coincidir com a contagem de alocações.",
      call. = FALSE
    )
  }
  invisible(alocacoes)
}
