#' Executa o bloco II.3 da EDA, cold start e forma do híbrido
#'
#' **Âncora na literatura.** Burke (2002) organiza os híbridos de recomendação
#' em formas com exigências distintas sobre os componentes. Schein, Popescul,
#' Ungar e Pennock (2002) formalizam o cold start como o regime em que o
#' colaborativo não tem o que dizer e o conteúdo precisa responder sozinho.
#'
#' **Propriedade medida.** Cobertura de cada sinal disponível, separadamente,
#' e o cruzamento entre ausência de histórico e ausência de atributo.
#'
#' **Decisão que discrimina.** Qual forma de híbrido é viável. A resposta não
#' vem de preferência, vem do padrão de cobertura: forma que exige escore de
#' ambos os componentes para todo par é inviável quando um deles não cobre
#' parte das entidades.
#'
#' O bloco desfaz uma leitura apressada da Parte I. O achado I.1.3 registrou
#' que cerca de metade dos consultores não tem skill registrada, o que parecia
#' limitar o conteúdo à metade da população. Medindo por sinal, a conclusão
#' muda: experiência setorial, expertise funcional e idiomas cobrem todos os
#' consultores. E como `skills_requeridas[]` não existe como processo, `P` é
#' estimado do espaço setorial, onde as skills não teriam contraparte para
#' casar de todo modo.
#'
#' @param dados Lista nomeada de tibbles, de `carregar_dados()`.
#' @param estabilidade tibble de `resumir_estabilidade()`, opcional.
#' @return Lista com `objetos`, `tabelas`, `figuras` e `achados`.
executar_ii3 <- function(dados, estabilidade = NULL) {
  cobertura <- medir_cobertura_de_sinais(dados)
  cruzamento_skills <- cruzar_cold_start(dados, "skills_consultor")
  cruzamento_setor <- cruzar_cold_start(dados, "experiencia_setorial")
  formas <- avaliar_formas_de_hibrido()

  universais <- dplyr::filter(cobertura, cobertura == 1, tipo == "conteúdo")
  parciais <- dplyr::filter(cobertura, cobertura < 1, tipo == "conteúdo")
  interacao_consultor <- dplyr::filter(
    cobertura, tipo == "interação", lado == "Consultor"
  )
  interacao_projeto <- dplyr::filter(
    cobertura, tipo == "interação", lado == "Projeto"
  )

  formas_viaveis <- dplyr::filter(formas, viavel)

  combinacoes_setoriais <- nrow(dplyr::distinct(dados$projetos, setor, tipo_projeto))
  segmentos <- dplyr::n_distinct(dados$projetos$setor)
  consultores_por_segmento <- nrow(dados$consultores) / segmentos

  objetos <- list(
    cobertura = cobertura,
    cruzamento_skills = cruzamento_skills,
    cruzamento_setor = cruzamento_setor,
    formas = formas,
    formas_viaveis = formas_viaveis,
    universais = universais,
    parciais = parciais,
    interacao_consultor = interacao_consultor,
    interacao_projeto = interacao_projeto,
    combinacoes_setoriais = combinacoes_setoriais,
    segmentos = segmentos,
    consultores_por_segmento = consultores_por_segmento
  )

  tabelas <- list(
    ii3_cobertura_sinais = cobertura |>
      dplyr::transmute(
        Lado = lado, Sinal = sinal, Tipo = tipo,
        Cobertas = entidades_cobertas, Total = total,
        Cobertura = formatar_pct(cobertura)
      ),
    ii3_quadrantes_skills = cruzamento_skills$quadrantes |>
      dplyr::transmute(
        `Situação` = situacao, Consultores = consultores,
        `Fração` = formatar_pct(fracao)
      ),
    ii3_formas_hibrido = formas |>
      dplyr::transmute(
        Forma = forma, Exige = exige,
        `Viável` = ifelse(viavel, "sim", "não"),
        Justificativa = justificativa
      )
  )

  figuras <- list(
    ii3_cobertura_sinais = figura_cobertura_de_sinais(
      cobertura,
      titulo = "Cobertura de cada sinal, separadamente",
      subtitulo = "Medir os sinais de conteúdo em bloco esconde a diferença que decide a arquitetura"
    )
  )

  achados <- registrar_achados("II.3", tibble::tribble(
    ~codigo, ~etiqueta, ~enunciado, ~implicacao,

    "II.3.1", "instrumento",
    paste0(
      "Os sinais de conteúdo têm cobertura radicalmente diferente entre si: ",
      nrow(universais), " deles cobrem todos os consultores (",
      paste(universais$sinal, collapse = ", "), "), enquanto skills técnicas ",
      "cobrem ", formatar_pct(cobertura$cobertura[cobertura$sinal == "skills técnicas"]),
      " e certificações, ",
      formatar_pct(cobertura$cobertura[cobertura$sinal == "certificações"]), "."
    ),
    paste0(
      "Desfaz a leitura apressada do achado I.1.3, que tratava a cobertura de ",
      "skills como se limitasse o conteúdo à metade da população. O conteúdo ",
      "opera sobre todos os consultores, desde que o casamento seja feito no ",
      "espaço setorial e não no de skills."
    ),

    "II.3.2", "âncora real",
    paste0(
      "A zona morta depende inteiramente de qual sinal de conteúdo se adota. ",
      "Com skills, ", cruzamento_skills$zona_morta, " consultores (",
      formatar_pct(cruzamento_skills$zona_morta / nrow(dados$consultores)),
      ") não têm histórico nem atributo. Com experiência setorial, a zona ",
      "morta é de ", cruzamento_setor$zona_morta, " consultores."
    ),
    paste0(
      "A ausência de `skills_requeridas[]` como processo na Peers, registrada ",
      "como achado de pesquisa, tem aqui uma consequência que não era óbvia: ",
      "como `P` é estimado do espaço setorial, as skills não teriam ",
      "contraparte para casar de todo modo, e sua cobertura parcial deixa de ",
      "ser o gargalo que parecia ser."
    ),

    "II.3.3", "instrumento",
    paste0(
      "Das cinco formas de híbrido da taxonomia de Burke (2002), ",
      nrow(formas_viaveis), " são viáveis sob o padrão de cobertura medido: ",
      paste(formas_viaveis$forma, collapse = " e "),
      ". As demais exigem que a saída do colaborativo seja confiável para ",
      "todo par, o que os blocos II.1 e II.2 mostram não ser o caso."
    ),
    paste0(
      "**A decisão habilitada é cascata com o conteúdo à frente**: o conteúdo ",
      "gera o conjunto candidato, com cobertura universal no espaço setorial, ",
      "e o colaborativo reordena apenas onde há histórico. A forma ponderada, ",
      "que a arquitetura conceitual das pré-definições sugeria, fica ",
      "descartada por exigir escore do colaborativo para entidade sem ",
      "histórico, que são ",
      formatar_pct(1 - interacao_consultor$cobertura), " dos consultores e ",
      formatar_pct(1 - interacao_projeto$cobertura), " dos projetos."
    ),

    "II.3.4", "consequência do gerador",
    paste0(
      "O espaço setorial que sustenta o conteúdo é grosso: ", segmentos,
      " segmentos e ", combinacoes_setoriais,
      " combinações de segmento e offering observadas entre os projetos, o ",
      "que dá em média ", formatar_decimal(consultores_por_segmento, 0),
      " consultores por segmento."
    ),
    paste0(
      "Cobertura universal não é o mesmo que poder discriminante. Um ",
      "casamento que só distingue segmento não separa consultores dentro do ",
      "mesmo segmento, e é justamente essa discriminação que a alocação ",
      "precisa. Quanto de sinal o espaço setorial de fato carrega é a ",
      "pergunta do bloco II.6."
    )
  ))

  list(objetos = objetos, tabelas = tabelas, figuras = figuras, achados = achados)
}
