#' Executa a Parte III da EDA, síntese do capítulo
#'
#' **Propriedade medida.** Quais achados do capítulo sustentam peso, que
#' decisão cada um habilita, que limitação cada um impõe, e de onde vem o
#' apoio de cada decisão.
#'
#' **Decisão que discrimina.** Nenhuma nova. A Parte III não conclui nada que
#' os blocos não tenham concluído: ela consolida, ordena e torna auditável.
#' Achado novo aqui seria decisão tomada sem bloco que a sustente.
#'
#' **Por que a síntese é derivada e não redigida.** Os campos `decisao`,
#' `limitacao` e `proveniencia` são declarados no bloco de origem, ao lado do
#' código que produziu o número. Este executor só empilha. A alternativa,
#' transcrever catorze documentos à mão, sairia de sincronia na primeira
#' revisão de texto, e o leitor não teria como saber qual versão está velha.
#'
#' **A coluna de proveniência responde a uma pergunta de banca**, e é o único
#' acréscimo ao que o roadmap especificou: se o modelo que a wave 4 pode
#' prometer é menor do que a ambição das pré-definições, isso é limitação do
#' dado real da Peers ou defeito da sintetização feita neste projeto. A
#' resposta não é retórica, é uma contagem.
#'
#' @param achados_por_bloco Lista com os `achados` de cada bloco.
#' @return Lista com `objetos`, `tabelas`, `figuras` e `achados`.
executar_iii <- function(achados_por_bloco) {
  consolidados <- consolidar_achados(achados_por_bloco)

  sintese <- tabular_sintese(consolidados)
  proveniencia <- contar_proveniencia(consolidados)
  resumo <- resumir_proveniencia_das_decisoes(consolidados)
  cruzamento <- cruzar_etiqueta_e_proveniencia(consolidados)

  blocos_com_decisao <- dplyr::n_distinct(
    consolidados$bloco[!is.na(consolidados$decisao)]
  )
  so_caracterizam <- resumo$n_achados - nrow(sintese)

  # A célula que explica o capítulo: achado cujo número é sobre o instrumento
  # e cuja decisão se apoia em fato real. Sem ela, rodar a EDA sobre sintético
  # e ainda decidir sobre a empresa pareceria contradição.
  instrumento_com_fato_real <- cruzamento$n[
    cruzamento$etiqueta == "instrumento" & cruzamento$proveniencia == "fato real"
  ]

  objetos <- list(
    consolidados = consolidados,
    sintese = sintese,
    proveniencia = proveniencia,
    resumo = resumo,
    cruzamento = cruzamento,
    blocos_com_decisao = blocos_com_decisao,
    so_caracterizam = so_caracterizam,
    instrumento_com_fato_real = instrumento_com_fato_real
  )

  tabelas <- list(
    # A tabela integral é o artefato que o roadmap pede, e vai para o CSV que
    # o auditor confere. No documento ela entra partida em duas, porque 65
    # linhas de texto longo num kable só não se lê.
    iii_sintese = sintese |>
      dplyr::transmute(
        Achado = .data$codigo,
        `Decisão que habilita` = dplyr::coalesce(.data$decisao, "--"),
        `Limitação que impõe` = dplyr::coalesce(.data$limitacao, "--"),
        `Proveniência` = .data$proveniencia
      ),
    iii_decisoes = sintese |>
      dplyr::filter(!is.na(.data$decisao)) |>
      dplyr::transmute(
        Achado = .data$codigo,
        `Decisão que habilita` = .data$decisao,
        `Proveniência` = .data$proveniencia
      ),
    # As que também trazem decisão já apareceram na tabela anterior, e a
    # proveniência delas se refere à decisão e não à limitação, pela regra de
    # `registrar_achados()`. Repeti-las aqui sugeriria que a etiqueta vale
    # para a limitação.
    iii_limitacoes = sintese |>
      dplyr::filter(!is.na(.data$limitacao), is.na(.data$decisao)) |>
      dplyr::transmute(
        Achado = .data$codigo,
        `Limitação que impõe` = .data$limitacao,
        `Proveniência` = .data$proveniencia
      ),
    iii_proveniencia = proveniencia |>
      dplyr::transmute(
        `Proveniência` = .data$proveniencia,
        `Decisões` = .data$decisoes,
        `Limitações` = .data$limitacoes
      ),
    iii_cruzamento = cruzamento |>
      tidyr::pivot_wider(
        names_from = "proveniencia", values_from = "n", values_fill = 0L
      ) |>
      dplyr::rename(`Etiqueta do achado` = "etiqueta"),
    iii_dependentes = resumo$decisoes_dependentes |>
      dplyr::transmute(
        Achado = .data$codigo,
        `Decisão` = .data$decisao,
        `O que ela exige para ser revista` = dplyr::coalesce(.data$limitacao, "--")
      )
  )

  figuras <- list(
    iii_proveniencia = figura_proveniencia_das_decisoes(
      consolidados,
      titulo = "Proveniência das decisões, por bloco",
      subtitulo = "Onde o capítulo se apoia no dado real, onde na estrutura do problema, e onde no gerador"
    )
  )

  achados <- registrar_achados("III", com_sintese(tibble::tribble(
    ~codigo, ~etiqueta, ~enunciado, ~implicacao,

    "III.1", "instrumento",
    paste0(
      "Dos ", resumo$n_achados, " achados do capítulo, ", nrow(sintese),
      " sustentam peso e ", so_caracterizam, " apenas caracterizam. As ",
      resumo$n_decisoes, " decisões vêm de ", blocos_com_decisao,
      " blocos, e a proveniência delas é ", resumo$fato_real,
      " de fato real, ", resumo$estrutural, " estrutural e ",
      resumo$depende_do_gerador, " dependente do gerador."
    ),
    paste0(
      "**A forma da wave 3 e da wave 4 foi determinada, não escolhida.** Cada ",
      "peça do desenho responde a uma restrição medida, e a contagem mostra ",
      "que a maior parte delas se apoia em agregado da Peers ou em ",
      "propriedade do problema, não no instrumento. O número que importa é o ",
      "último: as decisões que cairiam se o gerador fosse outro são ",
      resumo$depende_do_gerador, " de ", resumo$n_decisoes,
      ", estão nomeadas no achado III.2 e nenhuma delas é das que definem a ",
      "arquitetura do modelo."
    ),

    "III.2", "instrumento",
    paste0(
      "As decisões que dependem do gerador são ", resumo$depende_do_gerador,
      " e estão identificadas: ",
      paste(resumo$codigos_dependentes, collapse = ", "), "."
    ),
    paste0(
      "**Ficam declaradas como ressalva e não escondidas.** A mais ",
      "consequente é a que autoriza o rótulo escalar, porque a ",
      "unidimensionalidade dos catorze itens é imposta pelo gerador e o acesso ",
      "à base de avaliação foi negado: se o formulário real não for ",
      "unidimensional, a média dos itens não é a escala certa. As outras três ",
      "são de segunda ordem, e cada uma diz o que exige para ser revista. ",
      "Nenhuma delas é a forma do híbrido, o mecanismo de `P`, o protocolo de ",
      "avaliação ou o alcance do otimizador."
    ),

    "III.3", "instrumento",
    paste0(
      "Etiqueta epistêmica e proveniência são eixos independentes, e o ",
      "cruzamento mostra: ", instrumento_com_fato_real,
      " achados têm número sobre o instrumento e decisão apoiada em fato ",
      "real."
    ),
    paste0(
      "**É a célula que explica como um capítulo rodado sobre dado sintético ",
      "decide sobre a empresa.** A etiqueta diz sobre o que o número fala; a ",
      "proveniência, se a decisão sobrevive fora deste conjunto. Confundir os ",
      "dois levaria a descartar o capítulo inteiro por ele rodar sobre ",
      "sintético, ou a aceitá-lo inteiro por ele citar âncoras reais, e ",
      "nenhuma das duas leituras é correta."
    )
  ), tibble::tribble(
    ~codigo, ~decisao, ~limitacao, ~proveniencia,

    "III.2", NA,
    paste0(
      "Quatro decisões do capítulo cairiam se o gerador fosse outro, e a de ",
      "maior consequência é a autorização do rótulo escalar: a consistência ",
      "interna do formulário real nunca pôde ser medida."
    ),
    "depende do gerador",

    "III.3",
    paste0(
      "A monografia declara as duas classificações por achado, etiqueta e ",
      "proveniência, em vez de uma só, porque a pergunta sobre o que o número ",
      "mede e a pergunta sobre o que a decisão exige são diferentes."
    ),
    NA,
    "estrutural"
  )))

  list(objetos = objetos, tabelas = tabelas, figuras = figuras, achados = achados)
}
