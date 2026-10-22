#' Ordem de leitura dos códigos de achado
#'
#' A ordenação alfabética que `registrar_achados()` aplica dentro de cada
#' bloco erra a partir do décimo achado: `II.4.10` viria antes de `II.4.2`,
#' porque compara texto e não número. Dentro de um bloco isso passa quase
#' despercebido; numa tabela que empilha os catorze blocos, fica visível e
#' atrapalha a conferência contra o documento de origem.
#'
#' A parte romana é resolvida por correspondência numa lista fixa, e não por
#' conversão de numeral romano, porque só existem três partes e inventar um
#' conversor geral aqui seria resolver um problema que o projeto não tem.
#'
#' @param codigo Vetor de códigos na forma `"II.5.10"`.
#' @return Vetor de índices que ordena `codigo` na ordem de leitura.
ordem_de_leitura <- function(codigo) {
  partes <- strsplit(codigo, ".", fixed = TRUE)

  parte <- vapply(partes, `[`, character(1), 1)
  bloco <- as.integer(vapply(partes, `[`, character(1), 2))
  achado <- as.integer(vapply(partes, `[`, character(1), 3))

  desconhecidas <- setdiff(parte, c("I", "II", "III"))
  if (length(desconhecidas) > 0) {
    stop(
      "Parte de código de achado não reconhecida: ",
      paste(unique(desconhecidas), collapse = ", "),
      call. = FALSE
    )
  }

  order(match(parte, c("I", "II", "III")), bloco, achado)
}

#' Empilha os achados dos catorze blocos num registro único
#'
#' A Parte III não relê os documentos: ela empilha o dado que cada bloco
#' declarou ao lado do código que o produziu. É o que o docstring de
#' `registrar_achados()` antecipa, e o que impede a síntese de sair de
#' sincronia com os blocos na primeira revisão de texto.
#'
#' A verificação de código repetido entre blocos é o guarda que falta em
#' `registrar_achados()`, que só enxerga um bloco por vez e portanto não
#' detectaria dois blocos reivindicando o mesmo código.
#'
#' @param achados_por_bloco Lista de tibbles de `registrar_achados()`.
#' @return tibble único, na ordem de leitura.
consolidar_achados <- function(achados_por_bloco) {
  todos <- dplyr::bind_rows(achados_por_bloco)

  repetidos <- todos$codigo[duplicated(todos$codigo)]
  if (length(repetidos) > 0) {
    stop(
      "Código de achado reivindicado por mais de um bloco: ",
      paste(unique(repetidos), collapse = ", "),
      call. = FALSE
    )
  }

  todos[ordem_de_leitura(todos$codigo), ]
}

#' Monta a tabela de síntese da Parte III
#'
#' A tabela que o roadmap pede: achado numerado, decisão que ele habilita,
#' limitação que ele impõe. A coluna de proveniência é o acréscimo, e existe
#' por uma pergunta concreta de banca: se o modelo é menor do que a ambição
#' inicial, isso é limitação do dado real ou defeito da sintetização.
#'
#' Só entram os achados que carregam decisão ou limitação. O filtro quase não
#' filtra, e é isso que ele informa: praticamente todo achado do capítulo tem
#' consequência declarada, e os poucos que não têm são verificações de
#' conformidade e guardas de regressão.
#'
#' @param achados tibble de `consolidar_achados()`.
#' @return tibble com uma linha por achado que decide ou limita.
tabular_sintese <- function(achados) {
  achados |>
    dplyr::filter(!is.na(.data$decisao) | !is.na(.data$limitacao)) |>
    dplyr::select(
      "bloco", "codigo", "etiqueta", "decisao", "limitacao", "proveniencia"
    )
}

#' Conta a proveniência das decisões e das limitações
#'
#' Separa os dois porque a pergunta é diferente em cada um. Sobre a decisão,
#' interessa se ela cai se o gerador mudar. Sobre a limitação, se ela é do
#' dado real e portanto permanente, ou do instrumento e portanto removível.
#'
#' @param achados tibble de `consolidar_achados()`.
#' @return tibble com `proveniencia`, `decisoes` e `limitacoes`.
contar_proveniencia <- function(achados) {
  contar <- function(coluna, nome) {
    achados |>
      dplyr::filter(!is.na(.data[[coluna]])) |>
      dplyr::count(.data$proveniencia, name = nome)
  }

  # As decisões mandam na proveniência quando o achado tem as duas, pela
  # regra declarada em `registrar_achados()`, então a contagem de limitações
  # exclui quem já entrou como decisão para as duas colunas não somarem o
  # mesmo achado duas vezes.
  so_limitacao <- dplyr::filter(achados, is.na(.data$decisao))

  tibble::tibble(proveniencia = proveniencias_achado()) |>
    dplyr::left_join(contar("decisao", "decisoes"), by = "proveniencia") |>
    dplyr::left_join(
      dplyr::count(
        dplyr::filter(so_limitacao, !is.na(.data$limitacao)),
        .data$proveniencia,
        name = "limitacoes"
      ),
      by = "proveniencia"
    ) |>
    dplyr::mutate(dplyr::across(c("decisoes", "limitacoes"), ~ dplyr::coalesce(.x, 0L)))
}

#' Resume a proveniência das decisões em números citáveis
#'
#' É a resposta direta à pergunta de banca, e fica como objeto e não como
#' prosa para o texto não poder discordar da contagem.
#'
#' @param achados tibble de `consolidar_achados()`.
#' @return Lista com as contagens e os códigos das decisões que dependem do
#'   gerador.
resumir_proveniencia_das_decisoes <- function(achados) {
  decisoes <- dplyr::filter(achados, !is.na(.data$decisao))
  dependentes <- dplyr::filter(decisoes, .data$proveniencia == "depende do gerador")

  list(
    n_achados = nrow(achados),
    n_decisoes = nrow(decisoes),
    n_limitacoes = sum(!is.na(achados$limitacao)),
    fato_real = sum(decisoes$proveniencia == "fato real"),
    estrutural = sum(decisoes$proveniencia == "estrutural"),
    depende_do_gerador = nrow(dependentes),
    codigos_dependentes = dependentes$codigo,
    decisoes_dependentes = dplyr::select(dependentes, "codigo", "decisao", "limitacao")
  )
}

#' Cruza a etiqueta epistêmica contra a proveniência
#'
#' O cruzamento é o que mostra os dois eixos serem de fato independentes, e
#' não um refinamento do outro. A célula que interessa é a de achado
#' `instrumento` com proveniência `fato real`: é numerosa, e é ela que
#' explica como um capítulo rodado sobre dado sintético sustenta decisão
#' sobre a empresa.
#'
#' @param achados tibble de `consolidar_achados()`.
#' @return tibble longo com `etiqueta`, `proveniencia` e `n`.
cruzar_etiqueta_e_proveniencia <- function(achados) {
  achados |>
    dplyr::filter(!is.na(.data$proveniencia)) |>
    dplyr::count(.data$etiqueta, .data$proveniencia) |>
    tidyr::complete(
      etiqueta = etiquetas_achado(),
      proveniencia = proveniencias_achado(),
      fill = list(n = 0L)
    )
}
