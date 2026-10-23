#' Executa o bloco II.6 da EDA, poder discriminante do perfil
#'
#' **Âncora no projeto, não na literatura.** A pergunta vem do obstáculo de
#' `skills_requeridas[]` registrado em `docs/predefinicoes.md`: como a organização
#' não formaliza o requisito técnico da demanda, `P` precisa ser estimado, e a
#' decisão de 2026-07-31 deixou o **mecanismo** de estimação em aberto para a
#' EDA. Os dois candidatos são o one-hot das categorias do projeto e o perfil
#' médio de skills de quem já atuou em projetos parecidos.
#'
#' **Propriedade medida.** A resolução de cada espaço de categorias, a
#' capacidade de desempate de cada mecanismo, e a separação entre quem foi e
#' quem não foi alocado, sobre alocações retidas.
#'
#' **Decisão que discrimina.** Qual mecanismo estima `P`, e o que ele pode ou
#' não prometer.
#'
#' **O bloco responde à pergunta que o achado II.3.4 deixou aberta**, que era
#' quanto de sinal o espaço setorial de fato carrega.
#'
#' @param dados Lista nomeada de tibbles, de `carregar_dados()`.
#' @param estabilidade tibble de `resumir_estabilidade()`, opcional.
#' @param minimo_por_classe Abaixo disto uma classe é tratada como rala.
#' @return Lista com `objetos`, `tabelas`, `figuras` e `achados`.
executar_ii6 <- function(dados, estabilidade = NULL, minimo_por_classe = 5) {
  setor <- setor_predominante_observado(dados$experiencia_setorial)
  rotulo_setor <- setor$segmento[match(dados$consultores$consultor_id, setor$consultor_id)]
  rotulo_offering <- dados$expertise_funcional$expertise_funcional[
    match(dados$consultores$consultor_id, dados$expertise_funcional$consultor_id)
  ]
  rotulo_cruzado <- ifelse(
    is.na(rotulo_setor) | is.na(rotulo_offering),
    NA_character_, paste(rotulo_setor, rotulo_offering)
  )

  granularidades <- list(
    "Setor" = rotulo_setor,
    "Offering" = rotulo_offering,
    "Setor e offering" = rotulo_cruzado
  )
  resolucao <- purrr::imap(granularidades, function(rotulo, nome) {
    medida <- medir_resolucao_de_categorias(rotulo, minimo_por_classe)
    tibble::tibble(
      granularidade = nome,
      classes = medida$n_classes,
      mediana_por_classe = medida$mediana_por_classe,
      empate_medio = medida$empate_medio,
      classes_ralas = medida$classes_ralas
    )
  }) |>
    purrr::list_rbind()

  comparacao <- comparar_mecanismos_de_p(dados)
  ponte <- referencias_ponte_frente_offering()

  resultados <- comparacao$resultados
  pega <- function(nome) resultados[resultados$mecanismo == nome, ]
  aparente <- pega("perfil_por_alocacao")
  sem_popularidade <- pega("perfil_medio")
  sem_vazamento <- pega("perfil_sem_vazamento")
  one_hot <- pega("one_hot")
  popularidade <- pega("popularidade")

  vazamento <- sem_popularidade$auc - sem_vazamento$auc
  contaminacao <- aparente$auc - sem_popularidade$auc

  grossa <- resolucao[resolucao$granularidade == "Setor", ]
  fina <- resolucao[resolucao$granularidade == "Setor e offering", ]

  # A folga entre o one-hot e a linha de base é o que este bloco afirma ser
  # positivo, e por isso é ela, e não cada AUC em separado, que precisa de
  # intervalo. Enquanto não havia réplica, o texto citava a mediana e calava
  # sobre a dispersão.
  folga <- one_hot$auc - popularidade$auc
  folga_separa <- intervalo_separa_de_zero(estabilidade, "folga_one_hot_popularidade")
  # Três estados, não dois. Sem réplicas a folga não foi medida, e tratar
  # isso como "não se sustenta" afirmaria um resultado que ninguém mediu.
  folga_sustentada <- isTRUE(folga_separa)
  folga_cai <- isFALSE(folga_separa)

  objetos <- list(
    resolucao = resolucao,
    comparacao = comparacao,
    resultados = resultados,
    ponte = ponte,
    aparente = aparente,
    sem_popularidade = sem_popularidade,
    sem_vazamento = sem_vazamento,
    one_hot = one_hot,
    popularidade = popularidade,
    vazamento = vazamento,
    contaminacao = contaminacao,
    folga = folga,
    folga_sustentada = folga_sustentada,
    # O intervalo já formatado, e não a tabela de estabilidade inteira, para o
    # documento não recarregar um alvo que ele não usa para mais nada.
    folga_intervalo = formatar_intervalo(
      estabilidade, "folga_one_hot_popularidade", \(x) formatar_decimal(x, 3)
    ),
    grossa = grossa,
    fina = fina,
    minimo_por_classe = minimo_por_classe
  )

  tabelas <- list(
    ii6_resolucao = resolucao |>
      dplyr::transmute(
        Granularidade = granularidade,
        Classes = classes,
        `Mediana por classe` = formatar_decimal(mediana_por_classe, 1),
        `Empate médio` = formatar_pct(empate_medio),
        `Classes ralas` = formatar_pct(classes_ralas)
      ),
    ii6_mecanismos = resultados |>
      dplyr::transmute(
        Mecanismo = rotular_mecanismo(mecanismo),
        AUC = formatar_decimal(auc, 3),
        `Valores distintos` = formatar_inteiro(valores_distintos),
        `Fração empatada` = formatar_pct(fracao_empatada)
      ),
    ii6_ponte_real = ponte |>
      dplyr::transmute(
        `Frente da pessoa` = frente,
        `Offering do projeto` = offering,
        `Share das horas` = formatar_pct(share_horas),
        Lift = formatar_decimal(lift, 2)
      )
  )

  figuras <- list(
    ii6_decomposicao_auc = figura_decomposicao_auc(
      dplyr::mutate(resultados, mecanismo = rotular_mecanismo(mecanismo)),
      titulo = "O que sustenta a separação de cada mecanismo",
      subtitulo = paste0(
        "Linha tracejada: acaso. O perfil médio cai para o acaso quando ",
        "vazamento e popularidade saem"
      )
    )
  )

  achados <- registrar_achados("II.6", com_sintese(tibble::tribble(
    ~codigo, ~etiqueta, ~enunciado, ~implicacao,

    "II.6.1", "instrumento",
    paste0(
      "Não existe granularidade boa para o one-hot. Em ",
      tolower(grossa$granularidade), ", são ", grossa$classes,
      " classes com mediana de ", formatar_decimal(grossa$mediana_por_classe, 1),
      " consultores, e dois consultores sorteados caem na mesma classe em ",
      formatar_pct(grossa$empate_medio), " das vezes. Cruzando com offering, ",
      "o empate cai para ", formatar_pct(fina$empate_medio), " mas ",
      formatar_pct(fina$classes_ralas), " das ", fina$classes,
      " classes ficam com menos de ", minimo_por_classe, " consultores."
    ),
    paste0(
      "É o compromisso que decide o mecanismo, e ele não tem ponto ótimo: ",
      "espaço grosso não ordena dentro da classe, que é exatamente o que a ",
      "alocação precisa fazer, e espaço fino não tem massa para estimar. ",
      "Responde a pergunta que o achado II.3.4 deixou aberta, e a resposta é ",
      "que o espaço setorial carrega pouco sinal **por construção do espaço**, ",
      "antes de qualquer questão sobre o dado."
    ),

    "II.6.2", "instrumento",
    paste0(
      "O one-hot tem a maior separação medida nesta realização, com AUC de ",
      formatar_decimal(one_hot$auc, 2),
      if (folga_cai) ", vantagem que o achado II.6.6 mostra não se sustentar entre realizações" else "",
      ", e ao mesmo tempo é o que menos ",
      "serve para ordenar: ele produz ", one_hot$valores_distintos,
      " valores distintos, então empata ",
      formatar_pct(one_hot$fracao_empatada), " dos pares avaliados."
    ),
    paste0(
      "AUC alta e capacidade de ordenação são coisas diferentes, e medir só a ",
      "primeira levaria à escolha errada. Um escore binário separa bem a ",
      "classe certa da errada e é **indiferente** entre todos os consultores ",
      "da classe certa. Serve como filtro de conjunto candidato, não como ",
      "ordenador, que é precisamente o papel que o híbrido chaveado do bloco ",
      "II.3 reservou para o conteúdo."
    ),

    "II.6.3", "instrumento",
    paste0(
      "**Toda a separação aparente do perfil médio é artefato.** Ele mede AUC ",
      "de ", formatar_decimal(aparente$auc, 3),
      " na forma ingênua, cai para ", formatar_decimal(sem_popularidade$auc, 3),
      " quando a média passa a ponderar pessoas distintas em vez de alocações, ",
      "e cai para ", formatar_decimal(sem_vazamento$auc, 3),
      " quando o consultor avaliado sai do perfil da categoria que ele mesmo ",
      "ajudou a formar. O último valor é indistinguível do acaso."
    ),
    paste0(
      "São dois vícios somados, e nenhum deles é evidente sem medir. ",
      formatar_decimal(contaminacao, 3),
      " de AUC vinham de **popularidade disfarçada de conteúdo**: ponderar a ",
      "média por alocação faz o perfil da categoria tender ao perfil de quem é ",
      "muito alocado. Outros ", formatar_decimal(vazamento, 3),
      " vinham de **vazamento**: a divisão temporal separa alocações, não ",
      "pessoas, então o mesmo consultor aparece dos dois lados e o cosseno ",
      "dele contra o perfil da categoria mede em parte a semelhança dele ",
      "consigo mesmo. **A segunda lição é do bloco II.7**: divisão temporal de ",
      "interações não basta para avaliar atributo de entidade."
    ),

    "II.6.4", "consequência do gerador",
    paste0(
      "O valor limpo de ", formatar_decimal(sem_vazamento$auc, 3),
      " é o correto neste conjunto, e não um fracasso do mecanismo: o gerador ",
      "sorteia as skills do consultor por categoria de especialidade, sem ",
      "nenhum vínculo com a experiência setorial dele, então não há relação ",
      "entre skill e setor do projeto para ser encontrada."
    ),
    paste0(
      "**O sintético não pode decidir entre os dois mecanismos por separação**, ",
      "e forçar a decisão por esse critério seria medir o gerador. O que ele ",
      "decide, e decide bem, é o que não depende de vínculo plantado: a ",
      "estrutura de empate do achado II.6.1 e a decomposição do artefato do ",
      "II.6.3, que valem em qualquer conjunto. Plantar o vínculo tornaria a ",
      "comparação possível e circular ao mesmo tempo, que é o erro que o bloco ",
      "II.4 existe para não repetir."
    ),

    "II.6.5", "âncora real",
    paste0(
      "No dado real a afinidade entre atributo da pessoa e categoria do ",
      "projeto **existe e é fraca em volume**. A frente de Data Analytics tem ",
      "lift de ", formatar_decimal(ponte$lift[ponte$frente == "Data Analytics"], 1),
      " sobre a base para a offering correspondente, e ainda assim só ",
      formatar_pct(ponte$share_horas[ponte$frente == "Data Analytics"]),
      " das horas dela caem lá."
    ),
    paste0(
      "Os dois lados importam. O lift alto diz que o mecanismo de afinidade é ",
      "real, e não suposição do gerador, o que sustenta usar categoria como ",
      "filtro. O volume baixo diz que a maior parte das horas de qualquer ",
      "frente cai nas offerings grandes, então um filtro por categoria **exclui ",
      "candidatos que na prática seriam alocados**. Modelar como probabilidade ",
      "condicional com massa dispersa, nunca como pareamento de um para um. ",
      "Ressalva da fonte: metade das linhas de alocação resolve frente, 42% ",
      "das horas não têm chave para o RH, e os grupos têm de cinco a nove ",
      "pessoas."
    ),

    "II.6.6", "instrumento",
    paste0(
      "Contra a linha de base de popularidade, que mede ",
      formatar_decimal(popularidade$auc, 3),
      " sem sequer olhar o projeto, nenhum dos dois mecanismos entrega o que ",
      "se pediria dele. O one-hot mede ", formatar_decimal(one_hot$auc, 3),
      " e não ordena dentro da classe. ",
      if (is.na(folga_separa)) {
        paste0(
          "A folga de ", formatar_decimal(folga, 3),
          " sobre a linha não tem intervalo medido."
        )
      } else if (folga_sustentada) {
        paste0(
          "A folga dele sobre a linha é de ", formatar_decimal(folga, 3),
          ", e **se sustenta entre realizações**: ",
          formatar_intervalo(estabilidade, "folga_one_hot_popularidade", \(x) formatar_decimal(x, 3)),
          "."
        )
      } else {
        paste0(
          "**A folga dele sobre a linha não se sustenta**: ela mede ",
          formatar_decimal(folga, 3),
          " nesta realização e ",
          formatar_intervalo(estabilidade, "folga_one_hot_popularidade", \(x) formatar_decimal(x, 3)),
          " sobre as realizações, intervalo que contém o zero."
        )
      },
      " O perfil médio ordena, e limpo fica em ",
      formatar_decimal(sem_vazamento$auc, 3), ", abaixo da linha",
      if (is.null(estabilidade)) {
        "."
      } else {
        paste0(
          " e ",
          formatar_intervalo(
            estabilidade, "auc_perfil_sem_vazamento", \(x) formatar_decimal(x, 3)
          ),
          " sobre as realizações, indistinguível do acaso em qualquer delas."
        )
      }
    ),
    paste0(
      "**A decisão habilitada é estimar `P` por one-hot no nível do setor, ",
      "como filtro do conjunto candidato, e não adotar o perfil médio.** O ",
      "perfil médio não é rejeitado por ter ido mal: é rejeitado por não ser ",
      "avaliável, aqui porque o gerador não planta o vínculo e no real porque ",
      "não há registro que ligue skill de pessoa a requisito de projeto, que ",
      "é o próprio obstáculo de `skills_requeridas[]`. Adotá-lo seria escolher ",
      "sem evidência. A consequência casa com o bloco II.3: o conteúdo filtra ",
      "e não ordena, e a ordenação dentro do conjunto candidato precisa vir de ",
      "outra fonte, que os blocos II.7 e II.8 têm de resolver."
    ),

    "II.6.7", "consequência do gerador",
    paste0(
      "No sintético, o setor do projeto existe desde a criação do projeto, por ",
      "construção do gerador, e portanto está disponível no momento em que a ",
      "alocação é decidida. No real, o dicionário registra `setor` como ",
      "disponível sem dizer quando ele é preenchido."
    ),
    paste0(
      "**É vazamento de tempo, e não de divisão.** Se o setor só for ",
      "registrado depois de o projeto começar, um alocador prospectivo não o ",
      "teria na hora de decidir, e o filtro de conteúdo dependeria de uma ",
      "informação que ainda não existe. A wave 4 contorna o problema por ser ",
      "retrospectiva (II.8.4), mas a avaliação do conteúdo na wave 3 supõe o ",
      "setor conhecido no momento da alocação. Nenhuma medida sobre o ",
      "sintético responde a isso, porque o gerador cria o setor junto com o ",
      "projeto: fica como premissa declarada e pergunta ao dono do dado."
    )
  ), tibble::tribble(
    ~codigo, ~decisao, ~limitacao, ~proveniencia,

    "II.6.1", NA,
    paste0(
      "O espaço setorial carrega pouco sinal por construção do espaço, antes de ",
      "qualquer questão sobre qualidade do dado: espaço grosso não ordena ",
      "dentro da classe e espaço fino não tem massa para estimar."
    ),
    "estrutural",

    "II.6.2",
    paste0(
      "O one-hot serve como filtro do conjunto candidato e não como ordenador, ",
      "que é o papel que o híbrido chaveado do bloco II.3 já lhe reservava."
    ),
    NA,
    "estrutural",

    "II.6.3",
    paste0(
      "Avaliação de atributo com chave de entidade exige separação por ",
      "entidade, e não divisão temporal de interações, que separa alocações e ",
      "deixa a mesma pessoa dos dois lados."
    ),
    paste0(
      "Média de perfil ponderada por alocação injeta popularidade disfarçada de ",
      "conteúdo, então todo perfil de categoria pondera pessoas distintas e ",
      "exclui o avaliado."
    ),
    "estrutural",

    "II.6.4", NA,
    paste0(
      "O sintético não pode decidir entre os dois mecanismos por separação, ",
      "porque o gerador não planta vínculo entre skill e setor, e forçar a ",
      "decisão por esse critério seria medir o gerador."
    ),
    "depende do gerador",

    "II.6.5", NA,
    paste0(
      "O filtro por categoria exclui candidatos que na prática seriam alocados, ",
      "porque no dado real a afinidade tem lift alto e volume baixo: a maior ",
      "parte das horas de qualquer frente cai nas offerings grandes."
    ),
    "fato real",

    "II.6.6",
    paste0(
      "`P` é estimado por one-hot no nível do setor, como filtro do conjunto ",
      "candidato, e o perfil médio não é adotado, por não ser avaliável: no ",
      "real não existe registro que ligue skill de pessoa a requisito de ",
      "projeto."
    ),
    if (!folga_cai) {
      NA
    } else {
      paste0(
        "O one-hot é adotado como filtro **sem** poder alegar que supera a ",
        "linha de base de popularidade: a folga entre os dois não se separa de ",
        "zero entre realizações. O que sustenta a adoção é ele ser binário, e ",
        "portanto filtrar sem ordenar, mais a afinidade real medida no achado ",
        "II.6.5, e não a vantagem em AUC."
      )
    },
    "fato real",

    "II.6.7", NA,
    paste0(
      "Que o setor do projeto esteja disponível no momento da decisão de ",
      "alocação é premissa não verificada: o esquema real não registra quando ",
      "o setor é preenchido. Se for depois do início do projeto, o filtro de ",
      "conteúdo não serve a um alocador prospectivo."
    ),
    "fato real"
  )))

  list(objetos = objetos, tabelas = tabelas, figuras = figuras, achados = achados)
}
