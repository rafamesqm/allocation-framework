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
    "massa_horas_mes_em_0", "I.2.3", "defeito corrigido"
  )
}

#' Executa o bloco I.0 da EDA, preâmbulo metodológico e estabilidade
#'
#' Bloco de abertura do capítulo. Faz duas coisas que precisam vir antes de
#' qualquer análise.
#'
#' Declara o status epistêmico dos achados, já que a EDA roda sobre dado cujo
#' mecanismo gerador é conhecido e foi escrito neste projeto. Sem isso,
#' achado sobre a Peers e achado sobre o gerador ficam indistinguíveis.
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
#' @return Lista com `objetos`, `tabelas`, `figuras` e `achados`.
executar_i0 <- function(replicas, nivel = 0.90, limite_fragilidade = 0.15) {
  estabilidade <- resumir_estabilidade(replicas, nivel = nivel)

  classificado <- estabilidade |>
    dplyr::left_join(mapa_medida_achado(), by = "medida") |>
    dplyr::mutate(
      classe = dplyr::case_when(
        is.na(amplitude_relativa) ~ "invariante",
        desvio_mc == 0 ~ "invariante",
        amplitude_relativa > limite_fragilidade ~ "frágil",
        .default = "estável"
      )
    )

  frageis <- dplyr::filter(classificado, classe == "frágil")
  invariantes <- dplyr::filter(classificado, classe == "invariante")
  mais_fragil <- classificado[which.max(classificado$amplitude_relativa), ]

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
    limite_fragilidade = limite_fragilidade
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
          is.na(amplitude_relativa), "n/a", formatar_pct(amplitude_relativa)
        ),
        Classe = classe,
        `Sustenta` = dplyr::coalesce(achados, "")
      ),
    i0_etiquetas = tibble::tibble(
      Etiqueta = etiquetas_achado(),
      `O que autoriza afirmar` = c(
        "Sobre a Peers, com as ressalvas de cobertura de join documentadas",
        "Sobre o gerador, nunca sobre a empresa",
        "Sobre a validade do diagnóstico, aplicável ao dado real"
      )
    )
  )

  figuras <- list(
    i0_estabilidade = figura_estabilidade(
      classificado,
      titulo = "Variabilidade de Monte Carlo das grandezas reportadas",
      subtitulo = paste0(
        "Amplitude do intervalo de ", round(100 * nivel),
        "% dividida pela mediana, sobre ", nrow(replicas), " realizações"
      )
    )
  )

  achados <- registrar_achados("I.0", tibble::tribble(
    ~codigo, ~etiqueta, ~enunciado, ~implicacao,

    "I.0.1", "instrumento",
    paste0(
      "O conjunto versionado é uma realização entre muitas, e a variabilidade ",
      "entre elas não é desprezível: das ", nrow(classificado),
      " grandezas que sustentam achados do capítulo, ", nrow(frageis),
      " têm amplitude relativa acima de ",
      formatar_pct(limite_fragilidade), ". A mais frágil é ",
      mais_fragil$medida, ", com ",
      formatar_pct(mais_fragil$amplitude_relativa), "."
    ),
    paste0(
      "Nenhum número da Parte I deve ser lido como propriedade fixa do ",
      "modelo. O intervalo mede quanto o valor se moveria se a população ",
      "fosse sorteada de novo, e **não** é incerteza sobre a Peers: essa vem ",
      "das ressalvas de cobertura de join, não daqui."
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
      "Invariância aqui não é robustez, é parâmetro. A fração de cold start ",
      "do consultor é imposta ao gerador, e reportá-la como achado devolve o ",
      "parâmetro de entrada em forma de resultado. As duas medidas de massa ",
      "nos limites são invariantes por terem sido corrigidas para zero exato."
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
    )
  ))

  list(objetos = objetos, tabelas = tabelas, figuras = figuras, achados = achados)
}
