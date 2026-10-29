# Convenção do projeto verificada por código.
#
# `tibble()` avalia os argumentos em sequência e deixa cada coluna já criada
# visível às seguintes. Isso é recurso documentado do pacote, mas neste
# projeto produziu quatro defeitos distintos, todos silenciosos ou de
# diagnóstico obscuro:
#
#   1. `linhas = nrow(tabela)` leu a coluna `tabela` recém-criada, e a coluna
#      `linhas` simplesmente sumiu do resultado, sem erro.
#   2. `integra = length(orfaos) == 0` leu a coluna `orfaos`, já reduzida a um
#      inteiro, e reportou violação de integridade inexistente.
#   3. `razao = observado / (esperado * sum(observado))` leu a coluna
#      `esperado` já escalada e dividiu duas vezes, errando por três ordens
#      de grandeza.
#   4. `maior_componente = max(componentes$csize)` leu a coluna `componentes`
#      atômica, e falhou com "$ operator is invalid for atomic vectors".
#
# O que os quatro têm em comum, e o que os separa do uso legítimo: a coluna
# recebia algo **diferente** da variável local de mesmo nome. Onde a coluna
# apenas repassa a variável (`data_inicio = data_inicio`), referenciá-la
# depois é inofensivo, porque as duas valem a mesma coisa, e é assim que boa
# parte dos geradores da wave 1 está escrita, corretamente.
#
# Daí a regra verificada aqui: é defeito quando a coluna é definida por uma
# expressão que **transforma** o valor (`x = f(x)`, ou `x = outra_coisa`) e
# esse nome é referenciado por um argumento posterior. Nesse caso o argumento
# posterior lê o valor transformado sem que nada no código indique isso.

nomes_de_simbolos <- function(expressao) {
  if (is.symbol(expressao)) return(as.character(expressao))
  # Em `a$b`, só `a` é busca de variável; `b` é nome de campo. O caso 4 acima
  # é o lado esquerdo, e continua visto. O direito acusaria `pares$x` só por
  # haver uma coluna `x` antes, sem que nada leia essa coluna.
  if (is.call(expressao) && identical(expressao[[1]], as.symbol("$"))) {
    return(nomes_de_simbolos(expressao[[2]]))
  }
  if (is.call(expressao) || is.pairlist(expressao)) {
    return(unlist(lapply(as.list(expressao), nomes_de_simbolos)))
  }
  character(0)
}

encontrar_sombreamento_em_tibble <- function(expressao, arquivo) {
  achados <- list()

  visitar <- function(no) {
    if (!is.call(no)) return(invisible(NULL))

    alvo <- no[[1]]
    nome_funcao <- if (is.symbol(alvo)) {
      as.character(alvo)
    } else if (is.call(alvo) && identical(as.character(alvo[[1]]), "::")) {
      as.character(alvo[[3]])
    } else {
      ""
    }

    if (nome_funcao %in% c("tibble", "tribble")) {
      argumentos <- as.list(no)[-1]
      nomes <- names(argumentos)
      if (!is.null(nomes)) {
        # Coluna que só repassa a variável de mesmo nome (`x = x`) não é
        # risco: coluna e variável valem a mesma coisa, e referenciar
        # qualquer uma das duas depois dá o mesmo resultado.
        apenas_repassa <- vapply(
          seq_along(argumentos),
          function(k) {
            nzchar(nomes[k]) && is.symbol(argumentos[[k]]) &&
              identical(as.character(argumentos[[k]]), nomes[k])
          },
          logical(1)
        )

        for (i in seq_along(argumentos)) {
          if (i == 1 || !nzchar(nomes[i])) next
          anteriores <- nomes[seq_len(i - 1)][!apenas_repassa[seq_len(i - 1)]]
          anteriores <- anteriores[nzchar(anteriores)]
          colisao <- intersect(anteriores, nomes_de_simbolos(argumentos[[i]]))
          if (length(colisao) > 0) {
            achados[[length(achados) + 1]] <<- paste0(
              arquivo, ": coluna '", nomes[i], "' referencia '",
              paste(colisao, collapse = ", "), "', criada antes dela"
            )
          }
        }
      }
    }

    lapply(as.list(no), visitar)
    invisible(NULL)
  }

  visitar(expressao)
  achados
}

test_that("nenhum tibble() referencia coluna criada antes, no codigo de R/", {
  arquivos <- list.files(
    here::here("R"), pattern = "[.]R$", recursive = TRUE, full.names = TRUE
  )

  ocorrencias <- unlist(lapply(arquivos, function(arquivo) {
    expressoes <- parse(arquivo, keep.source = FALSE)
    unlist(lapply(
      expressoes,
      encontrar_sombreamento_em_tibble,
      arquivo = basename(arquivo)
    ))
  }))

  expect_equal(
    ocorrencias, NULL,
    info = paste(
      "Resolva o derivado numa variavel local antes da chamada.",
      paste(ocorrencias, collapse = " | ")
    )
  )
})

test_that("o detector olha so o lado esquerdo do cifrao", {
  # Lado esquerdo e a coluna recem-criada: defeito, o caso 4 do cabecalho.
  esquerdo <- quote(tibble::tibble(componentes = 3, maior = max(componentes$csize)))
  expect_length(encontrar_sombreamento_em_tibble(esquerdo, "exemplo.R"), 1)

  # Lado direito e so nome de campo de outro objeto: nada le a coluna.
  direito <- quote(tibble::tibble(acerto = mean(x), diferenca = mean(pares$acerto) - 1))
  expect_length(encontrar_sombreamento_em_tibble(direito, "exemplo.R"), 0)
})

test_that("o detector reconhece o padrao que ele existe para pegar", {
  # Sem este teste, um detector quebrado passaria como se estivesse limpo.
  codigo <- quote(tibble::tibble(orfaos = length(x), integra = length(orfaos) == 0))

  achados <- encontrar_sombreamento_em_tibble(codigo, "exemplo.R")

  expect_length(achados, 1)
  expect_match(achados[[1]], "integra")
  expect_match(achados[[1]], "orfaos")
})

test_that("o detector nao acusa uso legitimo de variavel local", {
  codigo <- quote(tibble::tibble(a = comprimento, b = comprimento * 2))

  expect_length(encontrar_sombreamento_em_tibble(codigo, "exemplo.R"), 0)
})

test_that("o detector nao acusa coluna que apenas repassa a variavel local", {
  # Padrao correto e comum nos geradores da wave 1: a coluna vale o mesmo
  # que a variavel, entao a referencia posterior da o mesmo resultado.
  codigo <- quote(tibble::tibble(
    data_inicio = data_inicio,
    duracao_dias = duracao_dias,
    data_fim = data_inicio + duracao_dias
  ))

  expect_length(encontrar_sombreamento_em_tibble(codigo, "exemplo.R"), 0)
})

test_that("o detector acusa coluna transformada e depois referenciada", {
  # Foi este o defeito real: `esperado` na coluna ja vinha escalado.
  codigo <- quote(tibble::tibble(
    esperado = esperado * total,
    razao = observado / (esperado * total)
  ))

  achados <- encontrar_sombreamento_em_tibble(codigo, "exemplo.R")

  expect_length(achados, 1)
  expect_match(achados[[1]], "razao")
})
