#' Etiquetas de status epistêmico admitidas para um achado
#'
#' Toda conclusão deste capítulo declara o que ela é capaz de afirmar, porque
#' a EDA roda sobre dado cujo mecanismo gerador é conhecido e foi escrito
#' neste projeto. Sem a etiqueta, achado sobre a Peers e achado sobre o
#' gerador ficam indistinguíveis para quem lê.
#'
#' * `âncora real`: o número é calibrado contra agregado documentado em
#'   `docs/mapeamento_schema.md`. Fala sobre a Peers, com as ressalvas de
#'   cobertura das fontes já registradas.
#' * `consequência do gerador`: decorre de suposição de modelagem, não de
#'   observação. Fala sobre o instrumento, não sobre a empresa.
#' * `instrumento`: o valor está no diagnóstico ter sido construído e
#'   validado, aplicável ao dado real se o acesso for restabelecido.
#'
#' @return Vetor de texto com as três etiquetas.
etiquetas_achado <- function() {
  c("âncora real", "consequência do gerador", "instrumento")
}

#' Proveniências admitidas para a decisão ou a limitação de um achado
#'
#' Eixo diferente do da etiqueta, e a distinção é o que dá sentido aos dois.
#' A **etiqueta** diz sobre o que o número fala. A **proveniência** diz se a
#' decisão que ele habilita continua de pé caso o gerador fosse outro.
#'
#' São ortogonais de propósito: um achado pode ser `instrumento`, porque o
#' valor está no diagnóstico e não na medida, e ainda assim ter proveniência
#' `fato real`, porque a decisão se apoia num agregado da Peers e não no
#' sorteio do gerador. É exatamente esse cruzamento que a síntese da Parte III
#' precisa exibir, e que a etiqueta sozinha não expressa.
#'
#' * `fato real`: a decisão se apoia em agregado documentado em
#'   `docs/mapeamento_schema.md` ou em confirmação do dono do dado. Sobrevive
#'   a qualquer gerador.
#' * `estrutural`: apoia-se em propriedade matemática ou definicional, que
#'   vale em qualquer conjunto de dados com este esquema.
#' * `depende do gerador`: mudaria se o gerador mudasse. Precisa ficar
#'   declarada como ressalva, não escondida.
#'
#' @return Vetor de texto com as três proveniências.
proveniencias_achado <- function() {
  c("fato real", "estrutural", "depende do gerador")
}

#' Valida e registra os achados numerados de um bloco da EDA
#'
#' Cada bloco declara seus achados como dado, ao lado do código que os
#' produziu, em vez de apenas como prosa no documento. A síntese da Parte III
#' empilha os arquivos em vez de reler catorze documentos e transcrever à
#' mão, transcrição que sairia de sincronia com o bloco de origem na primeira
#' revisão.
#'
#' A numeração é por bloco (`I.1.1`, `II.4.2`), não uma sequência única do
#' capítulo. Com um documento por bloco, numeração global exigiria estado
#' compartilhado entre arquivos e quebraria a cada reordenação.
#'
#' **Os campos `decisao`, `limitacao` e `proveniencia` existem para a Parte
#' III ser derivada e não redigida.** Antes deles, a decisão que um achado
#' habilita vivia em negrito dentro da prosa de `implicacao`, e montar a
#' síntese exigiria varrer texto ou transcrever à mão, que é o que o
#' parágrafo acima diz para não fazer. Os três são opcionais porque a maior
#' parte dos achados caracteriza sem decidir.
#'
#' Duas regras de preenchimento, sem as quais os campos ficam ambíguos:
#'
#' * **`decisao` é reservada a decisão que amarra a wave 3 ou a wave 4.**
#'   Escolha interna do instrumento, como qual nulo usar na comparação de
#'   grafo, fica na prosa de `implicacao`. Sem esse corte, a síntese
#'   misturaria compromisso de modelagem com detalhe de implementação, e a
#'   contagem de decisões perderia sentido.
#' * **Onde o achado tem decisão e limitação, a proveniência se refere à
#'   decisão**, porque é a decisão que a síntese existe para defender. A
#'   ressalva própria da limitação, quando divergir, fica dita no texto dela.
#'   O caso típico é decisão apoiada em fato real cuja magnitude de apoio é
#'   sintética, como em II.7.6.
#'
#' A validação é deliberadamente rígida. Achado sem etiqueta epistêmica ou
#' sem implicação declarada é exatamente o tipo de conclusão solta que este
#' capítulo não deve produzir, e é mais barato falhar aqui do que descobrir
#' na revisão do texto. Pelo mesmo motivo, **achado que declara decisão ou
#' limitação é obrigado a declarar a proveniência**: é a pergunta que a banca
#' faz, e deixá-la opcional seria deixá-la sem resposta justamente onde ela
#' importa.
#'
#' @param bloco Identificador do bloco, como `"I.1"`.
#' @param achados Data frame com as colunas `codigo`, `etiqueta`,
#'   `enunciado` e `implicacao`, e opcionalmente `decisao`, `limitacao` e
#'   `proveniencia`.
#' @return O próprio `achados`, validado, com a coluna `bloco` à frente e as
#'   três colunas opcionais presentes, preenchidas com `NA` onde ausentes.
registrar_achados <- function(bloco, achados) {
  obrigatorias <- c("codigo", "etiqueta", "enunciado", "implicacao")
  faltantes <- setdiff(obrigatorias, names(achados))
  if (length(faltantes) > 0) {
    stop("Faltam colunas no registro de achados: ", paste(faltantes, collapse = ", "), call. = FALSE)
  }

  invalidas <- setdiff(achados$etiqueta, etiquetas_achado())
  if (length(invalidas) > 0) {
    stop(
      "Etiqueta de achado não reconhecida: ", paste(unique(invalidas), collapse = ", "),
      ". Use uma de: ", paste(etiquetas_achado(), collapse = ", "),
      call. = FALSE
    )
  }

  fora_do_bloco <- achados$codigo[!startsWith(achados$codigo, paste0(bloco, "."))]
  if (length(fora_do_bloco) > 0) {
    stop(
      "Código de achado fora do bloco ", bloco, ": ",
      paste(fora_do_bloco, collapse = ", "),
      call. = FALSE
    )
  }

  if (any(duplicated(achados$codigo))) {
    stop("Código de achado repetido no bloco ", bloco, ".", call. = FALSE)
  }

  vazios <- achados$codigo[!nzchar(trimws(achados$enunciado)) |
    !nzchar(trimws(achados$implicacao))]
  if (length(vazios) > 0) {
    stop(
      "Achado sem enunciado ou sem implicação declarada: ",
      paste(vazios, collapse = ", "),
      call. = FALSE
    )
  }

  achados <- tibble::as_tibble(achados) |>
    dplyr::mutate(bloco = bloco, .before = 1)

  for (coluna in c("decisao", "limitacao", "proveniencia")) {
    if (!coluna %in% names(achados)) {
      achados[[coluna]] <- NA_character_
    }
  }

  # A conversão para texto antes do teste importa: num bloco que não declara
  # nenhuma decisão, a coluna chega inteiramente `NA` e portanto lógica, e
  # `coalesce` recusaria a mistura com texto.
  achados <- dplyr::mutate(
    achados,
    dplyr::across(
      c("decisao", "limitacao", "proveniencia"),
      ~ dplyr::if_else(
        nzchar(trimws(dplyr::coalesce(as.character(.x), ""))),
        as.character(.x), NA_character_
      )
    )
  )

  proveniencias_invalidas <- setdiff(
    stats::na.omit(achados$proveniencia), proveniencias_achado()
  )
  if (length(proveniencias_invalidas) > 0) {
    stop(
      "Proveniência de achado não reconhecida: ",
      paste(unique(proveniencias_invalidas), collapse = ", "),
      ". Use uma de: ", paste(proveniencias_achado(), collapse = ", "),
      call. = FALSE
    )
  }

  # Decisão ou limitação sem proveniência é a lacuna que estes campos existem
  # para fechar: é onde a banca pergunta se a conclusão é da Peers ou do
  # gerador, e onde a omissão passaria despercebida.
  sem_proveniencia <- achados$codigo[
    (!is.na(achados$decisao) | !is.na(achados$limitacao)) &
      is.na(achados$proveniencia)
  ]
  if (length(sem_proveniencia) > 0) {
    stop(
      "Achado com decisão ou limitação e sem proveniência declarada: ",
      paste(sem_proveniencia, collapse = ", "),
      call. = FALSE
    )
  }

  orfas <- achados$codigo[
    !is.na(achados$proveniencia) &
      is.na(achados$decisao) & is.na(achados$limitacao)
  ]
  if (length(orfas) > 0) {
    stop(
      "Achado com proveniência e sem decisão nem limitação a que ela se refira: ",
      paste(orfas, collapse = ", "),
      call. = FALSE
    )
  }

  # Ordem numérica e não alfabética: `arrange(codigo)` punha o décimo achado
  # de um bloco antes do segundo.
  achados[ordem_de_leitura(achados$codigo), ]
}

#' Acopla a síntese de decisão e limitação aos achados de um bloco
#'
#' Os achados narrativos têm `enunciado` e `implicacao` montados por `paste0`
#' de muitas linhas, e acrescentar três colunas dentro daquele tribble
#' tornaria cada linha ilegível. Aqui a síntese vive num tribble próprio, e o
#' efeito colateral é bom: as decisões e limitações do bloco ficam visíveis
#' juntas, numa tabela curta, que é a forma em que se revisa se o bloco
#' concluiu o que devia.
#'
#' **O guarda contra código inexistente é o motivo de isto ser uma função.**
#' Num `left_join` solto, errar `"II.8.1"` por `"II.8.10"` produziria `NA`
#' silencioso, e a Parte III perderia a linha sem ninguém notar.
#'
#' @param achados Tribble narrativo do bloco, com `codigo`.
#' @param sintese Tribble com `codigo` e as colunas de síntese a acoplar.
#' @return `achados` com as colunas de `sintese` acopladas por `codigo`.
com_sintese <- function(achados, sintese) {
  desconhecidos <- setdiff(sintese$codigo, achados$codigo)
  if (length(desconhecidos) > 0) {
    stop(
      "A síntese refere achado que o bloco não declara: ",
      paste(desconhecidos, collapse = ", "),
      call. = FALSE
    )
  }

  if (any(duplicated(sintese$codigo))) {
    stop("Código repetido na síntese do bloco.", call. = FALSE)
  }

  dplyr::left_join(achados, sintese, by = "codigo")
}
