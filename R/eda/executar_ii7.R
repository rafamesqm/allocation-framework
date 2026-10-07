#' Executa o bloco II.7 da EDA, protocolo de avaliação
#'
#' **Âncora na literatura.** Cremonesi, Koren e Turrin (2010) mostram que a
#' escolha do protocolo muda a ordem entre algoritmos, e Bellogín, Castells e
#' Cantador (2011) que a métrica precisa controlar popularidade para não
#' premiar quem só a redescobre.
#'
#' **Propriedade medida.** Quanta observação cada forma de divisão deixa para
#' cada componente do híbrido, e quanto ela vaza.
#'
#' **Decisão que discrimina.** A forma da divisão, a métrica e a linha de
#' base, fixadas **antes** de qualquer resultado de modelo. É o que o roadmap
#' registrou como razão de este bloco ficar na wave 2 e não na 3: decidir o
#' protocolo com o modelo em mãos é quando a decisão fica suscetível de ser
#' ajustada ao resultado.
#'
#' **O bloco herda três restrições duras.** Do II.4, que não há estimador não
#' enviesado aplicável, então a conclusão é enunciada sobre a população
#' observada. Do II.6, que divisão temporal separa alocações e não pessoas, e
#' atributo com chave de entidade vaza. Do II.1, que a linha de base de
#' popularidade reportada lá era limite superior e precisava ser refeita aqui.
#'
#' @param dados Lista nomeada de tibbles, de `carregar_dados()`.
#' @param estabilidade tibble de `resumir_estabilidade()`, opcional.
#' @param fracao_teste Fração retida em cada forma de divisão.
#' @param tamanhos Tamanhos de lista da linha de base.
#' @return Lista com `objetos`, `tabelas`, `figuras` e `achados`.
executar_ii7 <- function(dados, estabilidade = NULL, fracao_teste = 0.3,
                          tamanhos = c(5, 10, 20, 50)) {
  com_atributo <- unique(dados$experiencia_setorial$consultor_id)
  formas <- c("temporal", "aleatoria", "por_consultor", "por_projeto")

  divisoes <- purrr::map(formas, function(forma) {
    medir_divisao(
      dividir_interacoes(dados$alocacoes, forma, fracao_teste = fracao_teste),
      com_atributo
    )
  }) |>
    purrr::list_rbind()

  protocolos <- avaliar_protocolos(divisoes)

  temporal <- protocolos[protocolos$forma == "temporal", ]
  aleatoria <- protocolos[protocolos$forma == "aleatoria", ]
  por_consultor <- protocolos[protocolos$forma == "por_consultor", ]

  # O quanto a divisão aleatória infla a avaliabilidade do colaborativo, que
  # é o número que decide o bloco: ela só consegue pontuar porque deixa o
  # mesmo projeto dos dois lados, o que em produção não acontece.
  inflacao_aleatoria <- aleatoria$avaliavel_colaborativo /
    temporal$avaliavel_colaborativo

  divisao_temporal <- dividir_interacoes(
    dados$alocacoes, "temporal", fracao_teste = fracao_teste
  )
  linha_base <- medir_linha_base_sob_protocolo(
    divisao_temporal$treino, divisao_temporal$teste, dados$consultores, tamanhos
  )
  linha_base_otimista <- medir_linha_base_popularidade(
    dados$alocacoes, dados$consultores, tamanhos
  )

  comparacao_base <- dplyr::inner_join(
    dplyr::select(linha_base, n_recomendados, recall_protocolo = recall),
    dplyr::select(linha_base_otimista, n_recomendados, recall_otimista = recall),
    by = "n_recomendados"
  ) |>
    dplyr::mutate(diferenca = recall_otimista - recall_protocolo)
  queda_por_lista <- function(n) comparacao_base$diferenca[comparacao_base$n_recomendados == n]

  nenhuma_serve_aos_dois <- !any(protocolos$serve_colaborativo & protocolos$serve_conteudo)

  # O achado II.7.3 afirma que a correção do piso morde em lista de 20, o que
  # é afirmar o sinal de uma diferença. Três estados, como no bloco II.6: sem
  # réplicas a correção não foi medida, e isso não é evidência contra ela.
  correcao_separa <- intervalo_separa_de_zero(estabilidade, "correcao_linha_base_20")
  base_20 <- comparacao_base[comparacao_base$n_recomendados == 20, ]
  fora_ii73 <- descrever_ponto_fora(c(
    "piso sob o protocolo" = posicao_no_intervalo(
      estabilidade, "recall_protocolo_20", base_20$recall_protocolo
    ),
    "correção" = posicao_no_intervalo(
      estabilidade, "correcao_linha_base_20", base_20$diferenca
    )
  ))

  # Poder de cada componente sob o protocolo que lhe cabe. É aqui que o piso
  # de popularidade do conteúdo aparece como zero, e que o colaborativo
  # aparece como não validável; os dois resultados corrigem o que o achado
  # II.7.4 tinha prometido.
  lista <- 20
  poder <- planejar_poder_por_componente(
    dados$alocacoes, dados$consultores, dados$projetos, dados$experiencia_setorial,
    fracao_teste = fracao_teste, n_recomendados = lista
  )
  poder_de <- function(componente, piso) {
    poder[poder$componente == componente & poder$piso == piso, ]
  }
  poder_colaborativo <- poder_de("Colaborativo", "Popularidade no treino")
  popularidade_no_conteudo <- poder_de("Conteúdo", "Popularidade no treino")
  poder_conteudo <- poder_de("Conteúdo", "Filtro de setor e acaso")
  acaso_no_conteudo <- poder_de("Conteúdo", "Acaso")

  # O alvo de avaliação, achado II.7.8. O acerto em lista não usa o rótulo de
  # desempenho que o bloco II.5 pôs na célula de `R`, e estes dois números
  # dizem quanto da base o segundo nível de avaliação, que usa, alcança.
  cobertura_do_rotulo <- medir_cobertura_do_rotulo(dados)$cobertura_pares
  fim_a_fim <- cruzar_conteudo_e_rotulo(dados)$fracao_fim_a_fim

  pontos <- \(x) formatar_decimal(100 * x, 1)
  # Cita o intervalo de uma grandeza, ou nada se o bloco rodou sem réplicas.
  sobre_realizacoes <- function(medida, formatador = formatar_pct, sufixo = "") {
    if (is.null(estabilidade)) return("")
    paste0(
      ", ou ", formatar_intervalo(estabilidade, medida, formatador), sufixo,
      " sobre as realizações"
    )
  }
  fora_poder_colaborativo <- descrever_ponto_fora(c(
    "pares avaliáveis" = posicao_no_intervalo(
      estabilidade, "n_avaliavel_colaborativo", poder_colaborativo$n_avaliavel
    ),
    "ganho mínimo" = posicao_no_intervalo(
      estabilidade, "ganho_minimo_colaborativo", poder_colaborativo$ganho_independencia
    )
  ))
  fora_poder_conteudo <- descrever_ponto_fora(c(
    "piso" = posicao_no_intervalo(
      estabilidade, "piso_conteudo_filtro", poder_conteudo$valor_piso
    ),
    "ganho mínimo" = posicao_no_intervalo(
      estabilidade, "ganho_minimo_conteudo", poder_conteudo$ganho_independencia
    )
  ))

  objetos <- list(
    divisoes = divisoes,
    protocolos = protocolos,
    temporal = temporal,
    aleatoria = aleatoria,
    por_consultor = por_consultor,
    inflacao_aleatoria = inflacao_aleatoria,
    linha_base = linha_base,
    comparacao_base = comparacao_base,
    nenhuma_serve_aos_dois = nenhuma_serve_aos_dois,
    correcao_separa = correcao_separa,
    poder = poder,
    poder_colaborativo = poder_colaborativo,
    poder_conteudo = poder_conteudo,
    popularidade_no_conteudo = popularidade_no_conteudo,
    acaso_no_conteudo = acaso_no_conteudo,
    correcao_intervalo = formatar_intervalo(estabilidade, "correcao_linha_base_20"),
    protocolo_intervalo = formatar_intervalo(estabilidade, "recall_protocolo_20"),
    fracao_teste = fracao_teste
  )

  tabelas <- list(
    ii7_poder = poder |>
      dplyr::transmute(
        Componente = .data$componente,
        `Divisão` = .data$protocolo,
        Piso = .data$piso,
        `Pares avaliáveis` = formatar_inteiro(.data$n_avaliavel),
        `Acerto do piso` = formatar_pct(.data$valor_piso),
        `Ganho mínimo, otimista` = dplyr::if_else(
          is.na(.data$ganho_dominancia), "--", pontos(.data$ganho_dominancia)
        ),
        `Ganho mínimo, conservador` = dplyr::if_else(
          is.na(.data$ganho_independencia), "--", pontos(.data$ganho_independencia)
        )
      ),
    ii7_divisoes = protocolos |>
      dplyr::transmute(
        Forma = rotular_forma_de_divisao(forma),
        Teste = formatar_inteiro(n_teste),
        `Consultor já visto` = formatar_pct(vazamento_consultor),
        `Projeto já visto` = formatar_pct(vazamento_projeto),
        `Colaborativo pontua` = formatar_pct(avaliavel_colaborativo),
        `Conteúdo pontua` = formatar_pct(avaliavel_conteudo)
      ),
    ii7_vereditos = protocolos |>
      dplyr::transmute(
        Forma = rotular_forma_de_divisao(forma),
        `Serve ao colaborativo` = ifelse(serve_colaborativo, "sim", "não"),
        `Serve ao conteúdo` = ifelse(serve_conteudo, "sim", "não"),
        `Respeita a ordem temporal` = ifelse(respeita_ordem_temporal, "sim", "não")
      ),
    ii7_linha_base = comparacao_base |>
      dplyr::transmute(
        `Tamanho da lista` = n_recomendados,
        `Sob o protocolo` = formatar_pct(recall_protocolo),
        `Versão do bloco II.1` = formatar_pct(recall_otimista),
        `Diferença` = formatar_pct(diferenca)
      )
  )

  figuras <- list(
    ii7_avaliabilidade = figura_avaliabilidade(
      protocolos,
      titulo = "O que cada forma de divisão deixa para cada componente",
      subtitulo = "Vazamento alto invalida o conteúdo; avaliabilidade baixa invalida o colaborativo"
    )
  )

  achados <- registrar_achados("II.7", com_sintese(tibble::tribble(
    ~codigo, ~etiqueta, ~enunciado, ~implicacao,

    "II.7.1", "instrumento",
    paste0(
      "Sob divisão temporal, o colaborativo consegue pontuar apenas ",
      formatar_pct(temporal$avaliavel_colaborativo),
      " dos pares de teste, porque só ",
      formatar_pct(temporal$vazamento_projeto),
      " dos projetos do teste aparecem no treino. A divisão aleatória eleva ",
      "a avaliabilidade para ",
      formatar_pct(aleatoria$avaliavel_colaborativo), ", ou ",
      formatar_decimal(inflacao_aleatoria, 1), " vezes mais",
      if (is.null(estabilidade)) {
        "."
      } else {
        paste0(
          ". Sob divisão temporal, a avaliabilidade do colaborativo fica em ",
          formatar_intervalo(estabilidade, "avaliavel_colaborativo_temporal"),
          " sobre as realizações",
          descrever_ponto_fora(posicao_no_intervalo(
            estabilidade, "avaliavel_colaborativo_temporal",
            temporal$avaliavel_colaborativo
          )),
          "."
        )
      }
    ),
    paste0(
      "**A diferença inteira vem de deixar o mesmo projeto dos dois lados**, ",
      "que em produção nunca acontece: projeto de consultoria é item efêmero, ",
      "aparece uma vez e não volta. Avaliar o colaborativo com divisão ",
      "aleatória reportaria um componente que funciona e que não poderia ",
      "funcionar. Conjugado ao bloco II.2, que mostrou a fatoração não ser ",
      "identificável nesta escala, o colaborativo não é só fraco: é **quase ",
      "inavaliável** sob qualquer protocolo honesto."
    ),

    "II.7.2", "instrumento",
    paste0(
      if (nenhuma_serve_aos_dois) {
        "Nenhuma das quatro formas de divisão serve aos dois componentes ao "
      } else {
        "Alguma forma de divisão serve aos dois componentes ao "
      },
      "mesmo tempo. A temporal deixa ",
      formatar_pct(temporal$vazamento_consultor),
      " dos consultores do teste já vistos no treino, o que invalida ",
      "avaliação de atributo de entidade. A divisão por consultor zera esse ",
      "vazamento e zera junto a avaliabilidade do colaborativo."
    ),
    paste0(
      "A oposição não é defeito de medida, é definição: separar pessoas para ",
      "não vazar atributo é exatamente criar cold start, e cold start é o ",
      "regime em que o colaborativo não tem o que dizer. **A decisão ",
      "habilitada é adotar dois protocolos, um por componente**, o que casa ",
      "com o híbrido chaveado do bloco II.3: cada componente é avaliado no ",
      "regime em que ele atua. Um protocolo único obrigaria a mentir sobre um ",
      "dos dois."
    ),

    "II.7.3", "instrumento",
    paste0(
      "A linha de base de popularidade do bloco II.1 era otimista nas listas ",
      "que importam, como aquele bloco já suspeitava. Refeita com a ",
      "popularidade contada só no treino e com o acerto esperado sob ",
      "desempate aleatório, o acerto em lista de 20 cai de ",
      formatar_pct(comparacao_base$recall_otimista[comparacao_base$n_recomendados == 20]),
      " para ",
      formatar_pct(comparacao_base$recall_protocolo[comparacao_base$n_recomendados == 20]),
      ", e em lista de 10 cai de ",
      formatar_pct(comparacao_base$recall_otimista[comparacao_base$n_recomendados == 10]),
      " para ",
      formatar_pct(comparacao_base$recall_protocolo[comparacao_base$n_recomendados == 10]),
      ". A queda, em pontos, é de ",
      formatar_decimal(100 * queda_por_lista(5), 1), " em lista de 5, ",
      formatar_decimal(100 * queda_por_lista(20), 1), " em lista de 20 e ",
      formatar_decimal(100 * queda_por_lista(50), 1), " em lista de 50",
      if (is.na(correcao_separa)) {
        "."
      } else if (correcao_separa) {
        paste0(
          ". Em lista de 20 a correção **se sustenta entre realizações**: o ",
          "piso cai ",
          formatar_intervalo(estabilidade, "correcao_linha_base_20"),
          ", e o piso sob o protocolo fica em ",
          formatar_intervalo(estabilidade, "recall_protocolo_20"), fora_ii73, "."
        )
      } else {
        paste0(
          ". Em lista de 20, porém, **a correção não se separa do zero entre ",
          "realizações**: o piso cai ",
          formatar_intervalo(estabilidade, "correcao_linha_base_20"),
          ", intervalo que contém o zero, e o piso sob o protocolo fica em ",
          formatar_intervalo(estabilidade, "recall_protocolo_20"), fora_ii73, "."
        )
      }
    ),
    paste0(
      "Eram dois vícios. O primeiro, contar popularidade no mesmo dado em que ",
      "se avalia. O segundo, e menos evidente, **desempatar a favor do ",
      "consultor avaliado**: com contagens inteiras os empates são grandes, e ",
      "contar só quem está estritamente acima põe o focal na melhor posição ",
      "possível dentro do empate. A medida correta é o acerto esperado sob ",
      "desempate aleatório, a fração do bloco de empatados que cabe na lista, ",
      "e o efeito, quando existe, se concentra no meio da ",
      "faixa porque é lá que o bloco de empatados decide se o focal entra na ",
      "lista: em lista muito curta ele fica fora de qualquer jeito, e em lista ",
      "longa entra de qualquer jeito. ",
      if (isFALSE(correcao_separa)) {
        paste0(
          "**A correção é adotada por ser o cálculo certo, e não por mudar o ",
          "número de forma material.** Entre realizações a queda em lista de 20 ",
          "não se separa de zero, e a queda grande desta realização vem em boa ",
          "parte de o piso otimista ter sido um sorteio alto, como o achado ",
          "II.1.4 registra. Os dois vícios continuam sendo vícios; o que não se ",
          "sustenta é dizer que corrigi-los muda muito o piso. "
        )
      } else {
        ""
      },
      "**Este é o piso ao lado do qual o colaborativo é reportado** (II.7.6), ",
      "e ele fica fixado ",
      "agora, antes de existir modelo, que é o que impede ajustá-lo ao ",
      "resultado",
      if (is.null(estabilidade)) {
        "."
      } else {
        paste0(
          ". Fixado como intervalo e não como ponto: ",
          formatar_intervalo(estabilidade, "recall_protocolo_20"),
          " sobre as realizações, largura que a leitura do resultado da wave 3 ",
          "precisa levar em conta."
        )
      }
    ),

    "II.7.4", "instrumento",
    paste0(
      "O protocolo fica declarado: divisão por consultor para o conteúdo, ",
      "divisão temporal para o colaborativo, acerto em lista como métrica ",
      "com o piso de cada componente reportado ao lado em toda comparação, ",
      "popularidade para o colaborativo e filtro de setor com acaso para o ",
      "conteúdo, um segundo nível contra o desempenho entre os pares com ",
      "rótulo (II.7.8), e conclusão enunciada sobre a população observada."
    ),
    paste0(
      "Cada peça responde a uma restrição herdada e não a uma preferência. A ",
      "divisão por consultor vem do bloco II.6, que mediu o vazamento de ",
      "atributo de entidade. A temporal vem de não se prever o passado. O ",
      "piso ao lado vem do bloco II.1, onde a concentração torna a ",
      "popularidade forte o bastante para ser confundida com aprendizado. E ",
      "enunciar sobre a população observada vem do bloco II.4, onde nenhum ",
      "estimador não enviesado sobreviveu: o modelo não fala sobre os pares ",
      "que nunca tiveram chance de aparecer. **A primeira versão deste achado ",
      "prometia a popularidade como piso dos dois componentes**, e o achado ",
      "II.7.5 mostra que para o conteúdo ela é degenerada."
    ),

    "II.7.5", "instrumento",
    paste0(
      "Sob divisão por consultor, o piso de popularidade do componente de ",
      "conteúdo é de ", formatar_pct(popularidade_no_conteudo$valor_piso),
      " em lista de ", lista, ", sobre ",
      formatar_inteiro(popularidade_no_conteudo$n_avaliavel), " pares: nenhum ",
      "consultor de teste tem alocação no treino, e todos ficam empatados em ",
      "zero, abaixo de todo consultor visto. Filtro de setor com ordem ao ",
      "acaso dentro dele acerta ", formatar_pct(poder_conteudo$valor_piso),
      sobre_realizacoes("piso_conteudo_filtro"), ", com ",
      formatar_pct(poder_conteudo$fracao_passa_filtro),
      " dos pares passando o filtro, e o acaso puro acerta ",
      formatar_pct(acaso_no_conteudo$valor_piso), "."
    ),
    paste0(
      "**A popularidade, piso natural do colaborativo, é degenerada para o ",
      "conteúdo**: qualquer modelo supera zero, e a comparação não mediria ",
      "nada. Não é peculiaridade deste conjunto, é consequência da divisão, ",
      "que existe justamente para que o consultor de teste não tenha sido ",
      "visto. O piso que mede alguma coisa é o que o bloco II.6 já decidiu ",
      "que o conteúdo é, o filtro sem ordenação, e a pergunta da wave 3 para ",
      "esse componente passa a ser se algo ordena dentro do filtro melhor que ",
      "o acaso. O acaso puro fica como verificação de que o próprio filtro ",
      "carrega sinal."
    ),

    "II.7.6", "instrumento",
    paste0(
      "O colaborativo só é validável para ganhos grandes. Sob divisão ",
      "temporal restam ",
      formatar_inteiro(poder_colaborativo$n_avaliavel), " pares avaliáveis",
      sobre_realizacoes("n_avaliavel_colaborativo", formatar_inteiro),
      ", e contra o piso de popularidade de ",
      formatar_pct(poder_colaborativo$valor_piso),
      " um ganho só seria distinguível, num teste pareado a 5% com poder de ",
      "80%, se chegasse a ", pontos(poder_colaborativo$ganho_dominancia),
      " pontos percentuais no cenário otimista e a ",
      pontos(poder_colaborativo$ganho_independencia), " no conservador",
      sobre_realizacoes("ganho_minimo_colaborativo", pontos, " pontos percentuais"),
      fora_poder_colaborativo, "."
    ),
    paste0(
      "Não é falta de poder que mais dado resolveria neste desenho: os pares ",
      "avaliáveis são poucos porque projeto de consultoria aparece uma vez e ",
      "não volta (II.7.1). No cenário conservador, ser distinguível do piso ",
      "exige multiplicar o acerto dele por ",
      formatar_decimal(
        (poder_colaborativo$valor_piso + poder_colaborativo$ganho_independencia) /
          poder_colaborativo$valor_piso, 1
      ),
      ". **O colaborativo passa pelo mesmo teste que o conteúdo**, o pareado ",
      "de McNemar a 5% contra o piso de popularidade, com os empates do piso ",
      "desfeitos ao acaso sob semente declarada, e o ganho mínimo é reportado ",
      "ao lado do resultado. A diferença está na leitura: com tão poucos ",
      "pares, a não rejeição é o desfecho esperado de qualquer ganho moderado ",
      "e não autoriza dizer que o componente não funciona. O teste diz se o ",
      "colaborativo superou o piso por margem grande, e só isso."
    ),

    "II.7.7", "instrumento",
    paste0(
      "O componente de conteúdo tem poder. Com ",
      formatar_inteiro(poder_conteudo$n_avaliavel), " pares",
      sobre_realizacoes("n_avaliavel_conteudo", formatar_inteiro),
      ", o mesmo teste distingue do piso de filtro com acaso um ganho de ",
      pontos(poder_conteudo$ganho_dominancia),
      " pontos percentuais no cenário otimista e de ",
      pontos(poder_conteudo$ganho_independencia), " no conservador",
      sobre_realizacoes("ganho_minimo_conteudo", pontos, " pontos percentuais"),
      fora_poder_conteudo, "."
    ),
    paste0(
      "**O critério de sucesso fica fixado antes do modelo, e é um teste, não ",
      "um número.** O conteúdo é considerado superior ao piso se o teste ",
      "pareado de McNemar rejeitar a igualdade a 5%, com os empates do piso ",
      "desfeitos ao acaso sob semente declarada, para cada par ter um acerto ",
      "de zero ou um. O ganho mínimo serve para planejar e não para julgar: ",
      "ele diz que, no cenário conservador, o teste só enxerga ganhos a partir ",
      "de ", pontos(poder_conteudo$ganho_independencia), " pontos, e um ganho ",
      "menor que não rejeite não autoriza dizer que ",
      "nada ordena dentro do filtro. O planejamento usa o cenário de ",
      "independência porque ele não depende de o modelo imitar a popularidade."
    ),

    "II.7.8", "instrumento",
    paste0(
      "O acerto em lista mede se o modelo devolve quem de fato foi alocado, e ",
      "por isso mede aderência à prática atual de alocação, e não qualidade ",
      "da alocação. Das alocações, ", formatar_pct(cobertura_do_rotulo),
      " têm rótulo de desempenho, e ", formatar_pct(fim_a_fim),
      " são avaliáveis de ponta a ponta pelo componente de conteúdo, com ",
      "conteúdo declarado e rótulo ao mesmo tempo (II.5.11)."
    ),
    paste0(
      "**Os blocos II.5 e II.7 não conversavam.** O II.5 põe o desempenho na ",
      "célula de `R`, e o protocolo avaliava só por acerto contra as alocações ",
      "passadas, que é feedback implícito. Medido só assim, o modelo é ",
      "premiado por reproduzir a alocação centralizada e enviesada que motiva ",
      "a monografia (II.4), e o achado II.5.6 já avisava que esse sinal ",
      "carrega viés não corrigível. A avaliação passa a ter dois níveis, ",
      "declarados como tais: o acerto em lista, que diz quanto `ŝ` reproduz a ",
      "prática, e, entre os pares com rótulo, se `ŝ` ordena pelo desempenho ",
      "padronizado por avaliador (II.5.7), que diz quanto ele aponta para ",
      "alocações melhores. O critério do achado II.7.7 é de primeiro nível: ",
      "diz se algo ordena dentro do filtro na direção da prática, não na do ",
      "desempenho. A medida de ordenação do segundo nível fica para a wave 3; ",
      "o que fica fixado aqui é o alvo e a população."
    )
  ), tibble::tribble(
    ~codigo, ~decisao, ~limitacao, ~proveniencia,

    "II.7.1", NA,
    paste0(
      "O colaborativo é quase inavaliável sob qualquer protocolo honesto, ",
      "porque projeto de consultoria é item efêmero: aparece uma vez e não ",
      "volta, então quase nenhum projeto do teste existe no treino."
    ),
    "fato real",

    "II.7.2",
    paste0(
      "São adotados dois protocolos, um por componente, cada um avaliado no ",
      "regime em que de fato atua: um protocolo único obrigaria a mentir sobre ",
      "um dos dois."
    ),
    NA,
    "estrutural",

    "II.7.3",
    paste0(
      "A linha de base de popularidade é contada só no treino e desempatada ",
      "pelo acerto esperado sob desempate aleatório, e o valor resultante fica ",
      "fixado agora, antes de existir modelo, como o piso ao lado do qual o ",
      "componente colaborativo é reportado (II.7.6)."
    ),
    paste0(
      "O patamar numérico é o do sintético: a forma de calculá-lo transfere, ",
      "mas no dado real ele precisa ser recomputado sob o mesmo protocolo antes ",
      "de servir de comparação",
      if (is.null(estabilidade)) {
        "."
      } else {
        paste0(
          ". E mesmo no sintético ele é largo, ",
          formatar_intervalo(estabilidade, "recall_protocolo_20"),
          " entre realizações, então comparação que o trate como ponto ",
          "fixo confundiria sorteio com ganho."
        )
      }
    ),
    "depende do gerador",

    "II.7.4",
    paste0(
      "O protocolo fica declarado: divisão por consultor para o conteúdo, ",
      "divisão temporal para o colaborativo, acerto em lista como métrica com o ",
      "piso de cada componente ao lado, um segundo nível contra o desempenho ",
      "entre os pares com rótulo, e conclusão enunciada sobre a população ",
      "observada."
    ),
    NA,
    "fato real",

    "II.7.5",
    paste0(
      "O piso do componente de conteúdo é o filtro de setor com ordem ao acaso ",
      "dentro dele, e não a popularidade, que sob divisão por consultor é zero; ",
      "o acaso puro entra como verificação de que o filtro carrega sinal."
    ),
    paste0(
      "A magnitude desse piso é do gerador. No sintético a maior parte dos ",
      "pares passa o filtro, e no real o achado II.6.5 mediu afinidade de ",
      "volume baixo, então o piso real deve ser bem menor e precisa ser ",
      "recomputado."
    ),
    "estrutural",

    "II.7.6",
    paste0(
      "O critério de sucesso do componente colaborativo é o teste pareado de ",
      "McNemar a 5% contra o piso de popularidade no treino, sob divisão ",
      "temporal, com o ganho mínimo detectável reportado ao lado; a não ",
      "rejeição não é lida como ausência de efeito."
    ),
    paste0(
      "O piso de popularidade é a grandeza mais instável do capítulo entre ",
      "realizações (achado I.0.1), e o número de pares avaliáveis também é do ",
      "sintético. No dado real os dois, e com eles o ganho mínimo, precisam ser ",
      "recomputados sob o mesmo protocolo antes de o teste rodar."
    ),
    "fato real",

    "II.7.7",
    paste0(
      "O critério de sucesso do componente de conteúdo é o teste pareado de ",
      "McNemar a 5% contra o piso de filtro com acaso; o ganho mínimo ",
      "detectável orienta o planejamento e não o veredito."
    ),
    paste0(
      "O piso e o número de pares são os do sintético, e o ganho mínimo varia ",
      "entre realizações. No dado real os dois precisam ser recomputados sob o ",
      "mesmo protocolo antes de o teste rodar."
    ),
    "estrutural",

    "II.7.8",
    paste0(
      "A avaliação da wave 3 tem dois níveis: acerto em lista contra as ",
      "alocações, como medida de aderência à prática atual, e, entre os pares ",
      "com rótulo, a relação de `ŝ` com o desempenho padronizado por ",
      "avaliador, como medida de qualidade."
    ),
    paste0(
      "O segundo nível só existe sobre a fração rotulada, e no real a ",
      "cobertura e a escala do rótulo são desconhecidas, porque o acesso à ",
      "base de avaliação foi negado (II.5.6)."
    ),
    "estrutural"
  )))

  list(objetos = objetos, tabelas = tabelas, figuras = figuras, achados = achados)
}

#' Rótulo legível de cada forma de divisão
#'
#' Os nomes internos são identificadores, e a regra 4 do projeto exige
#' português legível em tudo que aparece no texto. Ponto único da tradução,
#' para figura e tabela não divergirem.
#'
#' @param forma Vetor com os nomes internos.
#' @return Vetor com os rótulos legíveis.
rotular_forma_de_divisao <- function(forma) {
  rotulos <- c(
    temporal = "Temporal",
    aleatoria = "Aleatória por alocação",
    por_consultor = "Por consultor",
    por_projeto = "Por projeto"
  )
  unname(rotulos[forma])
}
