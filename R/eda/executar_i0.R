#' Mapeia cada grandeza medida aos achados que ela sustenta
#'
#' Sem este mapa, a tabela de estabilidade é um diagnóstico solto. Com ele, a
#' fragilidade de uma grandeza vira uma afirmação sobre um achado específico,
#' que é o que permite decidir o que reescrever.
#'
#' @return tibble com `medida`, `achados` e `papel`.
mapa_medida_achado <- function() {
  tibble::tribble(
    ~medida, ~achados, ~papel,
    "cobertura_skills", "I.1.3", "cobertura do conteúdo",
    "fracao_sem_skills", "I.1.3", "cobertura do conteúdo",
    "cold_start_consultor", "I.1.3, II.3", "cold start",
    "cold_start_projeto", "I.1.3, II.3", "cold start",
    "densidade_R", "II.1", "esparsidade",
    "grau_medio_consultor", "II.1", "esparsidade",
    "cor_nivel_custo", "I.3.1", "correlação calibrada",
    "cor_nivel_tempo_casa", "I.3.1", "correlação calibrada",
    "cor_nivel_pct_alocacao", "I.3.1", "correlação calibrada",
    "cor_nivel_horas_mes", "I.3.1", "correlação calibrada",
    "v_contrato_nivel", "I.3.4", "associação categórica",
    "n_localizacoes", "I.2.1", "composição categórica",
    "agrupamento_grafo", "I.5.1", "estrutura relacional",
    "isolados_grafo", "I.5.4", "estrutura relacional",
    "acima_capacidade", "I.2.4", "capacidade",
    "gini_grau_consultor", "II.1", "cauda longa",
    "gini_grau_projeto", "II.1", "cauda longa",
    "dispersao_grau_consultor", "II.1", "sobredispersão",
    "dispersao_grau_projeto", "II.1", "sobredispersão",
    "recall_popularidade_20", "II.1", "linha de base",
    "projetos_subdeterminados_k5", "II.2", "identificabilidade",
    "consultores_subdeterminados_k5", "II.2", "identificabilidade",
    "zona_morta_skills", "II.3", "cold start cruzado",
    "cobertura_setor_observada", "I.1.3, II.3", "cobertura do conteúdo",
    "zona_morta_setor", "II.3", "cold start cruzado",
    "taxa_mesmo_setor", "II.4", "viés de seleção",
    "taxa_mesmo_setor_observavel", "II.4", "viés de seleção",
    "cobertura_covariavel_alocacoes", "II.4", "covariável de propensão",
    "pares_sem_positividade", "II.4", "positividade",
    "ess_relativo_ips", "II.4", "custo de reponderar",
    "massa_pct_alocacao_em_1", "I.2.3", "defeito corrigido",
    "massa_horas_mes_em_0", "I.2.3", "defeito corrigido",
    "auc_one_hot", "II.6.2, II.6.6", "poder discriminante",
    "auc_popularidade_discriminacao", "II.6.6", "linha de base do mecanismo",
    "auc_perfil_sem_vazamento", "II.6.3", "artefato removido",
    "folga_one_hot_popularidade", "II.6.6", "folga que sustenta a decisão",
    "recall_popularidade_50", "II.1.4", "linha de base",
    "obs_por_parametro_k10", "II.2.2", "identificabilidade",
    "avaliavel_colaborativo_temporal", "II.7.1", "avaliabilidade do colaborativo",
    "recall_protocolo_20", "II.7.3", "piso sob o protocolo",
    "correcao_linha_base_20", "II.7.3", "efeito da correção do piso",
    "reducao_porte_filtro", "II.8.1", "tratabilidade do otimizador",
    "n_avaliavel_colaborativo", "II.7.6", "poder do colaborativo",
    "piso_colaborativo", "II.7.6", "poder do colaborativo",
    "ganho_minimo_colaborativo", "II.7.6", "poder do colaborativo",
    "ganho_minimo_colaborativo_dominancia", "II.7.6", "poder do colaborativo",
    "n_avaliavel_conteudo", "II.7.7", "poder do conteúdo",
    "piso_conteudo_filtro", "II.7.5, II.7.7", "piso do conteúdo",
    "ganho_minimo_conteudo", "II.7.7", "poder do conteúdo",
    "ganho_minimo_conteudo_dominancia", "II.7.7", "poder do conteúdo"
  )
}

#' Grandezas que são diferença entre duas outras, e que se julgam pelo sinal
#'
#' A amplitude relativa divide a largura do intervalo pela mediana, o que
#' serve para magnitude e não serve para diferença. Uma diferença centrada
#' perto de zero tem mediana pequena, e a divisão a faz parecer a grandeza
#' mais frágil do capítulo quando a pergunta certa sobre ela é outra: se o
#' intervalo contém o zero. É a pergunta que os achados II.6.6 e II.7.3 já
#' fazem, via `intervalo_separa_de_zero()`.
#'
#' @return Vetor de nomes de grandeza.
grandezas_de_diferenca <- function() {
  c("folga_one_hot_popularidade", "correcao_linha_base_20")
}

#' Executa o bloco I.0 da EDA, preâmbulo metodológico e estabilidade
#'
#' Bloco de abertura do capítulo. Faz duas coisas que precisam vir antes de
#' qualquer análise.
#'
#' Declara o status epistêmico dos achados, já que a EDA roda sobre dado cujo
#' mecanismo gerador é conhecido e foi escrito neste projeto. Sem isso,
#' achado sobre a organização e achado sobre o gerador ficam indistinguíveis.
#'
#' E quantifica a variabilidade de Monte Carlo. O conjunto versionado é uma
#' realização, e todo número reportado nos demais blocos vem dela. Este bloco
#' mede quanto cada número se moveria se a população fosse sorteada de novo,
#' o que separa o que é estrutura do que é a semente.
#'
#' @param replicas tibble de `simular_estabilidade()`.
#' @param nivel Massa de probabilidade do intervalo reportado.
#' @param limite_fragilidade Amplitude relativa a partir da qual a grandeza é
#'   classificada como frágil. Convenção de leitura declarada, não teste.
#' @param limite_invariancia Amplitude relativa abaixo da qual a grandeza é
#'   tratada como invariante. Exigir desvio exatamente zero é estrito demais
#'   e deixa passar parâmetro disfarçado de resultado: uma fração imposta ao
#'   gerador pode variar na quinta casa decimal por arredondamento de uma
#'   contagem, o que bastaria para ela ser classificada como estável e
#'   escapar do aviso do achado I.0.3. Convenção de leitura declarada, não
#'   teste.
#' @param versionado tibble de uma linha, de `medir_grandezas()` aplicado ao
#'   conjunto versionado. Opcional; sem ele o achado I.0.5 declara que a
#'   comparação não foi feita.
#' @return Lista com `objetos`, `tabelas`, `figuras` e `achados`.
executar_i0 <- function(replicas, nivel = 0.90, limite_fragilidade = 0.15,
                         limite_invariancia = 0.001, versionado = NULL) {
  estabilidade <- resumir_estabilidade(replicas, nivel = nivel)

  classificado <- estabilidade |>
    dplyr::left_join(mapa_medida_achado(), by = "medida") |>
    dplyr::mutate(
      classe = dplyr::case_when(
        medida %in% grandezas_de_diferenca() ~ "diferença",
        is.na(amplitude_relativa) ~ "invariante",
        desvio_mc == 0 ~ "invariante",
        amplitude_relativa < limite_invariancia ~ "invariante",
        amplitude_relativa > limite_fragilidade ~ "frágil",
        .default = "estável"
      )
    )

  frageis <- dplyr::filter(classificado, classe == "frágil")
  invariantes <- dplyr::filter(classificado, classe == "invariante")
  de_magnitude <- dplyr::filter(classificado, classe != "diferença")
  mais_fragil <- de_magnitude[which.max(de_magnitude$amplitude_relativa), ]

  # O caso que mais importa é a fração de consultores sem atributo de
  # conteúdo, porque dela depende a leitura qualitativa do achado I.1.3.
  #
  # Há três regimes possíveis, e eles não querem dizer a mesma coisa. Se o
  # intervalo cruza a metade, a leitura depende da semente. Se não cruza mas
  # a grandeza varia, a leitura é robusta. E se a grandeza não varia, ela
  # parou de ser um resultado: virou o complemento do parâmetro de cobertura,
  # que é o que acontece desde que os sinais de formulário passaram a
  # compartilhar um conjunto fixo de respondentes calibrado em 48,4%. Chamar
  # esse terceiro caso de estabilidade seria vender parâmetro como achado,
  # exatamente o que o achado I.0.3 existe para denunciar.
  sem_skills <- estabilidade[estabilidade$medida == "fracao_sem_skills", ]
  sem_skills_invariante <- sem_skills$desvio_mc == 0
  cruza_metade <- sem_skills$inferior < 0.5 && sem_skills$superior > 0.5

  # O conjunto versionado é típico? A comparação usa a mesma classificação de
  # invariância deste bloco, para o I.0 não ter duas definições dela.
  tipicidade <- if (is.null(versionado)) {
    NULL
  } else {
    julgar_tipicidade_do_versionado(
      versionado, replicas, nivel = nivel, invariantes = invariantes$medida
    )
  }
  fora_do_intervalo <- if (is.null(tipicidade)) {
    NULL
  } else {
    tipicidade$situacao |>
      dplyr::filter(.data$posicao %in% c("abaixo", "acima")) |>
      dplyr::left_join(mapa_medida_achado(), by = "medida")
  }
  # A grandeza que explica a direção comum dos desvios, quando ela é um deles:
  # a fração das alocações cujo consultor tem setor observado. Alta, ela diz
  # que os respondentes do formulário calharam de estar entre os mais
  # alocados, e isso empurra junto tudo o que depende da cobertura do
  # conteúdo por alocação.
  covariavel <- if (is.null(fora_do_intervalo)) {
    NULL
  } else {
    fora_do_intervalo[fora_do_intervalo$medida == "cobertura_covariavel_alocacoes", ]
  }
  covariavel_alta <- !is.null(covariavel) && nrow(covariavel) == 1 &&
    covariavel$posicao == "acima"

  objetos <- list(
    replicas = replicas,
    estabilidade = estabilidade,
    classificado = classificado,
    frageis = frageis,
    invariantes = invariantes,
    mais_fragil = mais_fragil,
    sem_skills = sem_skills,
    cruza_metade = cruza_metade,
    sem_skills_invariante = sem_skills_invariante,
    n_replicas = nrow(replicas),
    nivel = nivel,
    limite_fragilidade = limite_fragilidade,
    tipicidade = tipicidade,
    fora_do_intervalo = fora_do_intervalo,
    covariavel_alta = covariavel_alta
  )

  tabelas <- list(
    i0_estabilidade = classificado |>
      dplyr::transmute(
        Medida = medida,
        Mediana = formatar_decimal(mediana, 4),
        `Desvio MC` = formatar_decimal(desvio_mc, 4),
        Inferior = formatar_decimal(inferior, 4),
        Superior = formatar_decimal(superior, 4),
        `Amplitude relativa` = dplyr::if_else(
          is.na(amplitude_relativa) | classe == "diferença", "n/a",
          formatar_pct(amplitude_relativa)
        ),
        Classe = classe,
        `Sustenta` = dplyr::coalesce(achados, "")
      ),
    i0_etiquetas = tibble::tibble(
      Etiqueta = etiquetas_achado(),
      `O que autoriza afirmar` = c(
        "Sobre a organização, com as ressalvas de cobertura das fontes documentadas",
        "Sobre o gerador, nunca sobre a empresa",
        "Sobre a validade do diagnóstico, aplicável ao dado real"
      )
    )
  )

  if (!is.null(tipicidade)) {
    tabelas$i0_tipicidade <- fora_do_intervalo |>
      dplyr::transmute(
        Grandeza = .data$medida,
        Sustenta = dplyr::coalesce(.data$achados, ""),
        `No conjunto versionado` = formatar_decimal(.data$valor_versionado, 4),
        `Intervalo das réplicas` = paste0(
          formatar_decimal(.data$inferior, 4), " a ",
          formatar_decimal(.data$superior, 4)
        ),
        Percentil = formatar_pct(.data$percentil, 0),
        `Posição` = .data$posicao
      )
  }

  figuras <- list(
    i0_estabilidade = figura_estabilidade(
      # As diferenças ficam de fora: julgam-se pelo sinal, e a amplitude
      # relativa delas não significa nada.
      de_magnitude,
      titulo = "Variabilidade de Monte Carlo das grandezas reportadas",
      subtitulo = paste0(
        "Amplitude do intervalo de ", round(100 * nivel),
        "% dividida pela mediana, sobre ", nrow(replicas), " realizações"
      )
    )
  )
  # Texto do achado I.0.5. A fronteira de 5% é convenção declarada para
  # separar sorteio comum de atípico, não teste de hipótese formal: o valor-p
  # é empírico, sobre as próprias réplicas.
  tipico <- !is.null(tipicidade) && tipicidade$valor_p >= 0.05
  n_acima <- if (is.null(fora_do_intervalo)) 0 else sum(fora_do_intervalo$posicao == "acima")
  direcao_comum <- if (covariavel_alta) {
    paste0(
      ", e a que explica a direção é a fração das alocações cujo consultor ",
      "tem setor observado, ", formatar_pct(covariavel$valor_versionado),
      " contra ", formatar_pct(covariavel$inferior), " a ",
      formatar_pct(covariavel$superior), " nas réplicas: neste sorteio os ",
      "respondentes do formulário calharam de estar entre os mais alocados, o ",
      "que empurra junto tudo o que depende da cobertura do conteúdo por ",
      "alocação"
    )
  } else {
    ""
  }
  enunciado_i05 <- if (is.null(tipicidade)) {
    "O conjunto versionado não foi comparado com as réplicas nesta execução."
  } else {
    paste0(
      "Medido pela mesma função que as réplicas, o conjunto versionado cai ",
      "fora do intervalo de ", round(100 * nivel), "% em ", tipicidade$n_fora,
      " das ", tipicidade$n_comparaveis, " grandezas que variam entre ",
      "realizações, contra ",
      formatar_decimal(tipicidade$fora_sob_independencia, 1),
      " que se esperariam se elas fossem independentes. Tratando cada réplica ",
      "como se fosse o conjunto versionado, a mediana é de ",
      tipicidade$mediana_referencia, " grandezas fora, e ",
      formatar_pct(tipicidade$valor_p, 0),
      " das réplicas têm tantas ou mais quanto ele."
    )
  }
  implicacao_i05 <- if (is.null(tipicidade)) {
    paste0(
      "Sem a comparação, os pontos citados nos blocos não podem ser situados ",
      "como típicos ou atípicos."
    )
  } else if (tipico) {
    paste0(
      "**O conjunto versionado é um sorteio comum.** O excesso sobre a conta ",
      "de independência é efeito de as grandezas serem correlacionadas: ",
      "várias respondem à mesma concentração da alocação, e caem fora juntas. ",
      "É por isso que a referência é a contagem nas próprias réplicas, que ",
      "herda a dependência sem precisar modelá-la. O que o conjunto tem de ",
      "particular é direção, não magnitude: ", n_acima, " das ",
      tipicidade$n_fora, " grandezas fora estão acima do intervalo",
      direcao_comum, ". Os pontos citados nos blocos continuam sendo os ",
      "valores do conjunto que o auditor executa, e cada bloco já declara ",
      "quando o seu cai fora."
    )
  } else {
    paste0(
      "**O conjunto versionado é um sorteio atípico**, e os números pontuais ",
      "do capítulo precisam ser lidos pela mediana das realizações e não pelo ",
      "ponto. ", n_acima, " das ", tipicidade$n_fora,
      " grandezas fora estão acima do intervalo", direcao_comum, "."
    )
  }
  limitacao_i05 <- if (covariavel_alta) {
    paste0(
      "Os valores pontuais que dependem da cobertura do conteúdo por alocação ",
      "saem altos neste conjunto, porque os respondentes do formulário ",
      "calharam de estar entre os mais alocados; para eles a leitura correta é ",
      "o intervalo das realizações, e não o ponto."
    )
  } else if (!is.null(tipicidade) && !tipico) {
    paste0(
      "Os valores pontuais do capítulo vêm de um sorteio atípico e devem ser ",
      "lidos pela mediana das realizações."
    )
  } else {
    NA_character_
  }

  if (!is.null(tipicidade)) {
    figuras$i0_tipicidade <- figura_tipicidade_do_versionado(
      tipicidade$referencia, tipicidade$n_fora,
      titulo = "Grandezas fora do intervalo num sorteio comum",
      subtitulo = paste0(
        "Cada réplica comparada com as outras ", nrow(replicas) - 1,
        "; a linha marca o conjunto versionado"
      )
    )
  }

  achados <- registrar_achados("I.0", com_sintese(tibble::tribble(
    ~codigo, ~etiqueta, ~enunciado, ~implicacao,

    "I.0.1", "instrumento",
    paste0(
      "O conjunto versionado é uma realização entre muitas, e a variabilidade ",
      "entre elas não é desprezível: das ", nrow(de_magnitude),
      " grandezas de magnitude que sustentam achados do capítulo, ",
      nrow(frageis), " têm amplitude relativa acima de ",
      formatar_pct(limite_fragilidade), ". A mais frágil é ",
      mais_fragil$medida, ", com ",
      formatar_pct(mais_fragil$amplitude_relativa), ". As ",
      nrow(classificado) - nrow(de_magnitude), " grandezas que são diferença ",
      "entre duas outras se julgam pelo sinal, e não pela amplitude."
    ),
    paste0(
      "Nenhum número do capítulo deve ser lido como propriedade fixa do ",
      "modelo. O intervalo mede quanto o valor se moveria se a população ",
      "fosse sorteada de novo, e **não** é incerteza sobre a organização: essa vem ",
      "das ressalvas de cobertura das fontes, não daqui."
    ),

    "I.0.2", "instrumento",
    paste0(
      "A fração de consultores sem nenhum atributo de conteúdo declarado ",
      "fica em ", formatar_intervalo(estabilidade, "fracao_sem_skills"),
      if (sem_skills_invariante) {
        ", sem variação entre realizações."
      } else {
        paste0(
          ", intervalo que ", if (cruza_metade) "cruza" else "não cruza",
          " a marca de metade da população."
        )
      }
    ),
    paste0(
      if (sem_skills_invariante) {
        paste0(
          "A ausência de variação **não** é robustez do achado I.1.3, e ler ",
          "assim seria o erro que o achado I.0.3 descreve. Desde que os ",
          "sinais de formulário passaram a compartilhar um conjunto fixo de ",
          "respondentes, esta fração é o complemento exato da cobertura real ",
          "calibrada de 48,4%: ela não é estimada pelo sintético, é imposta a ",
          "ele. O que o sintético sustenta aqui é a estrutura de quem fica de ",
          "fora, não a magnitude, que vem inteira da âncora real."
        )
      } else if (cruza_metade) {
        paste0(
          "A afirmação qualitativa de que mais da metade dos consultores não ",
          "tem atributo de conteúdo, escrita no achado I.1.3 a partir de uma ",
          "realização, não é estável: inverte de sinal dentro do intervalo. ",
          "O enunciado passa a reportar o intervalo, não o ponto."
        )
      } else {
        "A leitura qualitativa do achado I.1.3 é estável no intervalo medido."
      }
    ),

    "I.0.3", "consequência do gerador",
    paste0(
      nrow(invariantes), " grandezas não variam entre realizações: ",
      paste(invariantes$medida, collapse = ", "), "."
    ),
    paste0(
      "Invariância aqui não é robustez, é parâmetro, e as três famílias da ",
      "lista têm origens diferentes. Cold start do consultor é fração imposta ",
      "ao gerador, e reportá-la como achado devolve a entrada em forma de ",
      "resultado. As coberturas de conteúdo são ",
      "o complemento exato da âncora real de 48,4%, sorteada como contagem ",
      "fixa e não como Bernoulli, então não flutuam por construção (ver ",
      "I.0.2). As medidas de massa nos limites são invariantes por terem sido ",
      "corrigidas para zero exato."
    ),

    "I.0.4", "instrumento",
    paste0(
      "As quatro correlações calibradas têm estabilidade muito desigual: ",
      "nível por custo varia ",
      formatar_pct(classificado$amplitude_relativa[
        classificado$medida == "cor_nivel_custo"
      ]),
      " enquanto nível por tempo de casa varia ",
      formatar_pct(classificado$amplitude_relativa[
        classificado$medida == "cor_nivel_tempo_casa"
      ]), "."
    ),
    paste0(
      "A correlação com tempo de casa é ruído em escala de realização: numa ",
      "amostra de 285 pessoas, uma correlação em torno de 0,34 tem erro ",
      "padrão de cerca de 0,05, e o intervalo observado é compatível com ",
      "isso. Comparação pontual dela contra o alvo real não sustenta ",
      "conclusão, e a diferença relativa reportada na validação de utilidade ",
      "é métrica enganosa nessa escala."
    ),

    "I.0.5", "instrumento", enunciado_i05, implicacao_i05
  ), tibble::tribble(
    ~codigo, ~decisao, ~limitacao, ~proveniencia,

    "I.0.1", NA,
    paste0(
      "Nenhum número do capítulo vale como propriedade fixa do modelo: ",
      "grandeza que sustenta achado vem acompanhada do intervalo entre ",
      "realizações, e comparação pontual contra alvo real não decide nada ",
      "onde a amplitude é larga."
    ),
    "estrutural",

    "I.0.2", NA,
    paste0(
      "A fração sem atributo de conteúdo não é estimada pelo sintético, é ",
      "imposta a ele como complemento da cobertura real de 48,4%, e a ",
      "ausência de variação entre realizações não pode ser lida como ",
      "robustez do achado."
    ),
    "fato real",

    "I.0.3", NA,
    paste0(
      "Grandeza invariante entre realizações é parâmetro do gerador devolvido ",
      "em forma de resultado, e nenhuma delas sustenta conclusão sobre a ",
      "organização."
    ),
    "depende do gerador",

    "I.0.4", NA,
    paste0(
      "A correlação entre nível e tempo de casa é ruído na escala de 285 ",
      "pessoas, e a diferença relativa contra o alvo real é métrica enganosa ",
      "nessa faixa."
    ),
    "estrutural",

    "I.0.5", NA, limitacao_i05,
    if (is.na(limitacao_i05)) NA_character_ else "depende do gerador"
  )))

  list(objetos = objetos, tabelas = tabelas, figuras = figuras, achados = achados)
}
