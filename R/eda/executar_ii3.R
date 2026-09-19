#' Executa o bloco II.3 da EDA, cold start e forma do híbrido
#'
#' **Âncora na literatura.** Burke (2002) organiza os híbridos de recomendação
#' em formas com exigências distintas sobre os componentes. Schein, Popescul,
#' Ungar e Pennock (2002) formalizam o cold start como o regime em que o
#' colaborativo não tem o que dizer e o conteúdo precisa responder sozinho.
#'
#' **Propriedade medida.** Cobertura de cada sinal, separadamente; se os
#' sinais de conteúdo cobrem as mesmas pessoas ou pessoas diferentes; e o
#' cruzamento entre ausência de histórico e ausência de atributo.
#'
#' **Decisão que discrimina.** Qual forma de híbrido é viável. A resposta não
#' vem de preferência, vem do padrão de cobertura: forma que exige escore de
#' ambos os componentes para todo par é inviável quando nenhum dos dois cobre
#' todo par.
#'
#' **Esta é a segunda versão do bloco, e a primeira estava errada.** A
#' primeira versão concluiu que experiência setorial, expertise funcional e
#' soft skills cobriam todos os consultores, que a zona morta desaparecia ao
#' adotar um desses sinais no lugar de skills, e que a forma habilitada era
#' cascata com o conteúdo à frente. As três conclusões vinham de um defeito do
#' gerador, não do dado: cada um desses sinais sorteava sua própria cobertura,
#' quando na fonte real os quatro saem da mesma campanha de formulários
#' (docs/mapeamento_schema.md) e quem não respondeu não tem nenhum deles. O
#' achado I.1.4 já registrava que aquelas coberturas integrais eram artefato,
#' e a primeira versão deste bloco construiu uma decisão sobre elas mesmo
#' assim. O registro fica porque o erro é do tipo que a etiqueta epistêmica
#' existe para pegar, e vale mais documentado do que apagado.
#'
#' @param dados Lista nomeada de tibbles, de `carregar_dados()`.
#' @param estabilidade tibble de `resumir_estabilidade()`, opcional.
#' @return Lista com `objetos`, `tabelas`, `figuras` e `achados`.
executar_ii3 <- function(dados, estabilidade = NULL) {
  n_consultores <- nrow(dados$consultores)

  cobertura <- medir_cobertura_de_sinais(dados)
  coincidencia <- verificar_coincidencia_de_sinais(dados)
  cruzamento_skills <- cruzar_cold_start(dados, "skills_consultor")
  cruzamento_setor <- cruzar_cold_start(dados, "experiencia_setorial")
  discriminacao_idiomas <- medir_discriminacao_de_sinal(
    dados$idiomas_consultor, "idioma", n_consultores
  )

  conteudo_consultor <- dplyr::filter(cobertura, tipo == "conteúdo", lado == "Consultor")
  universais <- dplyr::filter(conteudo_consultor, cobertura == 1)
  parciais <- dplyr::filter(conteudo_consultor, cobertura < 1)

  # Os quatro sinais de formulário têm a mesma cobertura porque são as mesmas
  # pessoas. Certificações é campo opcional dentro do mesmo formulário e fica
  # abaixo deles, então o máximo dos parciais é o teto do que o conteúdo
  # derivado de formulário alcança.
  sinais_formulario <- dplyr::filter(
    parciais, tabela %in% tabelas_derivadas_de_formulario()
  )
  cobertura_formulario <- max(sinais_formulario$cobertura)
  cobertura_idiomas <- universais$cobertura[universais$sinal == "idiomas"]

  interacao_consultor <- dplyr::filter(cobertura, tipo == "interação", lado == "Consultor")
  interacao_projeto <- dplyr::filter(cobertura, tipo == "interação", lado == "Projeto")

  formas <- avaliar_formas_de_hibrido(
    cobertura_conteudo = cobertura_formulario,
    cobertura_interacao_consultor = interacao_consultor$cobertura,
    cobertura_interacao_projeto = interacao_projeto$cobertura,
    colaborativo_identificavel = FALSE
  )
  formas_viaveis <- dplyr::filter(formas, viavel)

  combinacoes_setoriais <- nrow(dplyr::distinct(dados$projetos, setor, tipo_projeto))
  segmentos <- dplyr::n_distinct(dados$projetos$setor)
  consultores_por_segmento <- n_consultores / segmentos

  zona_morta_fracao <- cruzamento_setor$zona_morta / n_consultores
  mesma_zona_morta <- cruzamento_skills$zona_morta == cruzamento_setor$zona_morta

  quadrantes <- cruzamento_setor$quadrantes
  so_colaborativo <- quadrantes$fracao[quadrantes$interacao & !quadrantes$conteudo]

  objetos <- list(
    cobertura = cobertura,
    coincidencia = coincidencia,
    cruzamento_skills = cruzamento_skills,
    cruzamento_setor = cruzamento_setor,
    discriminacao_idiomas = discriminacao_idiomas,
    formas = formas,
    formas_viaveis = formas_viaveis,
    universais = universais,
    parciais = parciais,
    sinais_formulario = sinais_formulario,
    cobertura_formulario = cobertura_formulario,
    cobertura_idiomas = cobertura_idiomas,
    interacao_consultor = interacao_consultor,
    interacao_projeto = interacao_projeto,
    zona_morta_fracao = zona_morta_fracao,
    mesma_zona_morta = mesma_zona_morta,
    so_colaborativo = so_colaborativo,
    combinacoes_setoriais = combinacoes_setoriais,
    segmentos = segmentos,
    consultores_por_segmento = consultores_por_segmento,
    n_consultores = n_consultores
  )

  tabelas <- list(
    ii3_cobertura_sinais = cobertura |>
      dplyr::transmute(
        Lado = lado, Sinal = sinal, Tipo = tipo,
        Cobertas = entidades_cobertas, Total = total,
        Cobertura = formatar_pct(cobertura)
      ),
    ii3_quadrantes_conteudo = quadrantes |>
      dplyr::transmute(
        `Situação` = situacao, Consultores = consultores,
        `Fração` = formatar_pct(fracao)
      ),
    ii3_formas_hibrido = formas |>
      dplyr::transmute(
        Forma = forma, Exige = exige,
        `Viável` = ifelse(viavel, "sim", "não"),
        Justificativa = justificativa
      ),
    ii3_perfis_idioma = discriminacao_idiomas$perfis |>
      dplyr::transmute(
        Perfil = perfil, Consultores = n, `Fração` = formatar_pct(fracao)
      )
  )

  figuras <- list(
    ii3_cobertura_sinais = figura_cobertura_de_sinais(
      cobertura,
      titulo = "Cobertura de cada sinal, separadamente",
      subtitulo = "A cobertura acompanha a fonte do sinal: tudo que vem da campanha de formulários cobre exatamente as mesmas pessoas"
    )
  )

  achados <- registrar_achados("II.3", tibble::tribble(
    ~codigo, ~etiqueta, ~enunciado, ~implicacao,

    "II.3.1", "âncora real",
    paste0(
      "A cobertura dos sinais de conteúdo acompanha a **fonte**, não o sinal. ",
      "Os ", nrow(sinais_formulario), " sinais derivados da campanha de ",
      "formulários de skills (", paste(sinais_formulario$sinal, collapse = ", "),
      ") cobrem ", formatar_pct(cobertura_formulario), " dos consultores, e ",
      if (coincidencia$coincidem) {
        "verifica-se no dado que são exatamente as mesmas pessoas em todos eles"
      } else {
        "os conjuntos cobertos divergem entre si, o que o gerador não deveria produzir"
      },
      ". Idiomas cobre ", formatar_pct(cobertura_idiomas),
      " por vir de base corporativa, e não de formulário."
    ),
    paste0(
      "Trocar um sinal de conteúdo por outro não recupera nenhum consultor. A ",
      "cobertura do conteúdo é a da campanha, ", formatar_pct(cobertura_formulario),
      ", e não a união de vários sinais, que é o que uma leitura sinal a sinal ",
      "sugeriria. Corrige o erro central da primeira versão deste bloco."
    ),

    "II.3.2", "âncora real",
    paste0(
      "A zona morta, consultores sem histórico **e** sem atributo declarado, é ",
      "de ", cruzamento_setor$zona_morta, " consultores (",
      formatar_pct(zona_morta_fracao), ")",
      if (mesma_zona_morta) {
        ", idêntica com skills ou com experiência setorial como sinal de conteúdo"
      } else {
        ", e difere conforme o sinal de conteúdo adotado"
      },
      if (is.null(estabilidade)) {
        "."
      } else {
        paste0(
          ". Sobre as realizações, fica em ",
          formatar_intervalo(estabilidade, "zona_morta_setor"), "."
        )
      }
    ),
    paste0(
      "Nenhum componente do híbrido tem o que dizer sobre esses consultores, e ",
      "nenhuma escolha de sinal os resgata. O modelo precisa de uma terceira ",
      "resposta declarada para eles, e a única que não exige nem histórico nem ",
      "atributo é a linha de base de popularidade do bloco II.1."
    ),

    "II.3.3", "instrumento",
    paste0(
      "Das cinco formas de híbrido da taxonomia de Burke (2002), ",
      nrow(formas_viaveis), " é viável sob o padrão de cobertura medido: ",
      paste(formas_viaveis$forma, collapse = " e "),
      ". A cascata cai pelos dois lados: o conteúdo não pode gerar o conjunto ",
      "candidato porque cobre ", formatar_pct(cobertura_formulario),
      " dos consultores, e o colaborativo, que cobre ",
      formatar_pct(interacao_consultor$cobertura),
      ", não ordena de forma identificável nesta escala (bloco II.2)."
    ),
    paste0(
      "**A decisão habilitada é o híbrido chaveado**, tendo como critério o ",
      "quadrante observado: ter histórico e ter respondido o formulário. A ",
      "cascata com o conteúdo à frente, que a primeira versão deste bloco ",
      "recomendava, fica descartada, porque nunca colocaria ",
      formatar_pct(1 - cobertura_formulario),
      " dos consultores no conjunto candidato. A forma ponderada, que a ",
      "arquitetura conceitual das pré-definições sugeria, segue descartada, ",
      "agora por ausência nos dois componentes e não apenas num deles."
    ),

    "II.3.4", "instrumento",
    paste0(
      "Cobertura não é poder discriminante, e o único sinal de conteúdo com ",
      "cobertura universal é o que menos discrimina: idiomas separa os ",
      n_consultores, " consultores em apenas ", discriminacao_idiomas$n_perfis,
      " perfis distintos, com ",
      formatar_pct(discriminacao_idiomas$fracao_maior_classe),
      " deles no mesmo perfil. O espaço setorial é grosso pelo mesmo motivo: ",
      segmentos, " segmentos e ", combinacoes_setoriais,
      " combinações de segmento e offering, cerca de ",
      formatar_decimal(consultores_por_segmento, 0), " consultores por segmento."
    ),
    paste0(
      "Fecha a saída que a primeira versão deste bloco deixara aberta. Não ",
      "adianta apoiar a cascata no sinal de cobertura universal: um casamento ",
      "que não separa pessoas dentro da classe majoritária não ordena candidato ",
      "nenhum, e é a separação dentro do segmento que a alocação precisa fazer. ",
      "Quanto de sinal o espaço setorial de fato carrega é a pergunta do bloco ",
      "II.6."
    ),

    "II.3.5", "instrumento",
    paste0(
      "O enquadramento usual do cold start se inverte neste problema: o ",
      "colaborativo cobre **mais** consultores (",
      formatar_pct(interacao_consultor$cobertura), ") do que o conteúdo (",
      formatar_pct(cobertura_formulario),
      "). O quadrante mais populoso depois do completo é o de quem tem ",
      "histórico e não tem atributo, com ", formatar_pct(so_colaborativo),
      " dos consultores."
    ),
    paste0(
      "A literatura de cold start (Schein et al., 2002) supõe o conteúdo como o ",
      "componente de cobertura ampla e o colaborativo como o escasso. Aqui é o ",
      "contrário, porque o atributo depende de resposta voluntária a formulário ",
      "e a interação é registro administrativo. O componente amplo é o não ",
      "identificável e o identificável é o estreito, o que explica por que ",
      "nenhuma forma de composição contínua sobrevive e sobra o chaveamento."
    )
  ))

  list(objetos = objetos, tabelas = tabelas, figuras = figuras, achados = achados)
}
