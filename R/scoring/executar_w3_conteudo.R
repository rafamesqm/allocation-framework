#' Registra o achado da Etapa 1: os pisos reproduzem o pré-registro
#'
#' @param conferencia tibble de `conferir_pisos()`.
#' @param pisos tibble de `medir_pisos_pre_registrados()`.
#' @param estabilidade tibble de `resumir_estabilidade()` das réplicas da EDA.
#' @return Achados validados por `registrar_achados()`.
registrar_achados_w3_pisos <- function(conferencia, pisos, estabilidade) {
  linha <- function(componente, piso) pisos[pisos$componente == componente & pisos$piso == piso, ]
  colaborativo <- linha("Colaborativo", "Popularidade no treino")
  conteudo <- linha("Conteúdo", "Filtro de setor e acaso")

  registrar_achados("W3.1", tibble::tibble(
    codigo = "W3.1.1",
    etiqueta = "instrumento",
    enunciado = paste0(
      "Os ", nrow(conferencia), " valores de verificação do pré-registro reproduzem na ",
      "pipeline da modelagem. O colaborativo tem ", formatar_inteiro(colaborativo$n_avaliavel),
      " pares avaliáveis e piso de popularidade de ", formatar_pct(colaborativo$valor_piso),
      ", ou ", formatar_intervalo(estabilidade, "piso_colaborativo"),
      " entre realizações; o conteúdo tem ", formatar_inteiro(conteudo$n_avaliavel),
      " pares e piso de filtro com acaso de ", formatar_pct(conteudo$valor_piso), ", ou ",
      formatar_intervalo(estabilidade, "piso_conteudo_filtro"), " entre realizações."
    ),
    implicacao = paste0(
      "A comparação da wave 3 é feita contra os pisos combinados antes de existir ",
      "modelo. A conferência fica na pipeline, e não neste texto: se algum piso ",
      "mudar, a execução para antes de qualquer teste."
    )
  ))
}

#' Executa o bloco da Etapa 2, o componente de conteúdo
#'
#' Reúne o teste único da realização versionada, o diagnóstico descritivo da
#' emenda de 2026-10-24 e as realizações, em que todo o procedimento é
#' refeito, e devolve tabelas, figura e achados no formato da EDA.
#'
#' @param avaliacao Lista de `avaliar_componente_de_conteudo()` sobre o
#'   conjunto versionado.
#' @param replicas tibble de `simular_avaliacao_do_conteudo()`.
#' @param registro Lista de `ler_pre_registro()`.
#' @return Lista com `objetos`, `tabelas`, `figuras` e `achados`.
executar_w3_conteudo <- function(avaliacao, replicas, registro) {
  versionado <- resumir_avaliacao_do_conteudo(avaliacao, registro) |>
    dplyr::mutate(mapeamento_menos_constante = acerto_mapeamento - acerto_constante)
  replicas <- replicas |>
    dplyr::mutate(mapeamento_menos_constante = acerto_mapeamento - acerto_constante)
  estabilidade <- resumir_estabilidade(replicas, nivel = registro$estabilidade$nivel)

  v <- as.list(versionado)
  pontos <- \(x) formatar_decimal(100 * x, 1)
  intervalo <- \(medida, formatador = formatar_pct) formatar_intervalo(estabilidade, medida, formatador)
  intervalo_pontos <- \(medida) formatar_intervalo_em_pontos(estabilidade, medida)
  fora <- \(rotulos) descrever_ponto_fora(vapply(
    names(rotulos), \(m) posicao_no_intervalo(estabilidade, m, v[[m]]), character(1)
  ) |> stats::setNames(rotulos))

  escolhidos <- avaliacao$selecao$escolhidos
  superficie <- avaliacao$selecao$grade |>
    dplyr::summarise(
      maximo = max(acerto), mediana = stats::median(acerto),
      empatadas = sum(acerto == max(acerto)), configuracoes = dplyr::n(), .by = grupo
    ) |>
    dplyr::left_join(avaliacao$piso_na_validacao, by = "grupo") |>
    dplyr::left_join(escolhidos |> dplyr::select(grupo, fatores, lambda, iteracoes, lambda_mapeamento), by = "grupo")
  rho_grupos <- avaliacao$spearman[avaliacao$spearman$grupo != "reunida", ]
  rejeicoes <- mean(replicas$supera_piso)
  rejeicoes_contra <- mean(replicas$comum_rejeita_contra)
  supera <- v$supera_piso == 1
  separa_constante <- intervalo_separa_de_zero(estabilidade, "mapeamento_menos_constante")
  separa_diferenca <- intervalo_separa_de_zero(estabilidade, "diferenca_mapeamento")

  leitura_rho <- function(rho) {
    if (abs(rho) < 0.1) {
      "fraca, então o que o escore diz do consultor não é popularidade disfarçada"
    } else if (rho >= 0.3) {
      "forte, então o escore reproduz em boa parte a popularidade, prevista pelos atributos"
    } else {
      "moderada, então parte do que o escore diz do consultor é popularidade prevista pelos atributos"
    }
  }

  objetos <- list(
    versionado = versionado, estabilidade = estabilidade, superficie = superficie,
    spearman = avaliacao$spearman, rejeicoes = rejeicoes, n_replicas = nrow(replicas)
  )

  linha_estavel <- function(medida, rotulo, formatador = formatar_pct) {
    tibble::tibble(
      Medida = rotulo,
      `Realização versionada` = formatador(v[[medida]]),
      `Entre realizações` = intervalo(medida, formatador)
    )
  }
  tabelas <- list(
    w3_conteudo_teste = dplyr::bind_rows(
      linha_estavel("n_pares_conteudo", "Pares de respondentes, cinco grupos reunidos", formatar_inteiro),
      linha_estavel("acerto_mapeamento", "Acerto do mapeamento em lista de 20"),
      linha_estavel("acerto_piso_filtro", "Acerto do filtro com acaso"),
      linha_estavel("diferenca_mapeamento", "Diferença, em pontos percentuais", pontos),
      linha_estavel("discordantes_a_favor", "Pares discordantes a favor do mapeamento", formatar_inteiro),
      linha_estavel("discordantes_contra", "Pares discordantes a favor do piso", formatar_inteiro),
      tibble::tibble(
        Medida = "Valor-p do McNemar exato bilateral",
        `Realização versionada` = formatar_decimal(v$valor_p_mcnemar, 3),
        `Entre realizações` = intervalo("valor_p_mcnemar", \(x) formatar_decimal(x, 3))
      ),
      tibble::tibble(
        Medida = "Supera o piso a 5%",
        `Realização versionada` = if (supera) "sim" else "não",
        `Entre realizações` = paste0(formatar_pct(rejeicoes, 0), " das realizações")
      ),
      linha_estavel("ganho_minimo_dominancia", "Ganho mínimo, otimista (pontos)", pontos),
      linha_estavel("ganho_minimo_independencia", "Ganho mínimo, conservador (pontos)", pontos),
      linha_estavel("fracao_passa_filtro", "Pares que passam o filtro"),
      linha_estavel("acaso_puro", "Acaso puro (verificação descritiva)")
    ),
    w3_conteudo_diagnostico = dplyr::bind_rows(
      tibble::tibble(
        Medida = paste0("Spearman do escore com a popularidade, grupo ", rho_grupos$grupo),
        `Realização versionada` = formatar_decimal(rho_grupos$rho, 3),
        `Entre realizações` = "--"
      ),
      linha_estavel("spearman_reunida", "Spearman do escore com a popularidade, reunida", \(x) formatar_decimal(x, 3)),
      linha_estavel("acerto_constante", "Acerto da variante de escore constante"),
      linha_estavel("diferenca_constante", "Variante menos o piso (pontos)", pontos),
      linha_estavel("mapeamento_menos_constante", "Mapeamento menos a variante (pontos)", pontos)
    ),
    w3_conteudo_superficie = superficie |>
      dplyr::transmute(
        Grupo = grupo,
        `Escolha (fatores, lambda, iterações, lambda do mapa)` = paste0(
          fatores, "; ", formatar_decimal(lambda, 1), "; ", iteracoes, "; ",
          formatar_decimal(lambda_mapeamento, 1)
        ),
        `Melhor da grade` = formatar_pct(maximo),
        `Mediana da grade` = formatar_pct(mediana),
        `Piso na mesma validação` = formatar_pct(piso),
        `Empatadas no máximo` = paste0(empatadas, " de ", configuracoes)
      ),
    w3_conteudo_estabilidade = estabilidade |>
      dplyr::mutate(
        versionado = purrr::map_dbl(medida, \(m) v[[m]]),
        posicao = purrr::map2_chr(medida, versionado, \(m, x) posicao_no_intervalo(estabilidade, m, x))
      ) |>
      dplyr::transmute(
        Medida = medida,
        `Realização versionada` = formatar_decimal(versionado, 4),
        Mediana = formatar_decimal(mediana, 4),
        Inferior = formatar_decimal(inferior, 4),
        Superior = formatar_decimal(superior, 4),
        `Posição da versionada` = posicao,
        `Réplicas` = n_replicas
      )
  )

  figuras <- list(
    w3_conteudo_diferencas = figura_diferencas_entre_realizacoes(
      replicas, versionado,
      rotulos = c(
        diferenca_mapeamento = "Mapeamento de atributos",
        diferenca_constante = "Variante de escore constante"
      ),
      coluna_ganho = "ganho_minimo_independencia",
      titulo = "Diferença para o filtro com acaso, entre realizações",
      subtitulo = paste(
        "Cada ponto é uma realização com a escolha de hiperparâmetros refeita;",
        "o losango é a versionada, e a linha tracejada o ganho mínimo conservador"
      ),
      eixo_x = "Diferença de acerto em lista de 20 para o filtro com acaso (pontos percentuais)"
    )
  )

  achados <- registrar_achados("W3.2", com_sintese(tibble::tribble(
    ~codigo, ~etiqueta, ~enunciado, ~implicacao,

    "W3.2.1", "instrumento",
    paste0(
      "No teste único, sobre os ", formatar_inteiro(v$n_pares_conteudo), " pares de ",
      "respondentes reunidos dos cinco grupos, o mapeamento de atributos para fatores ",
      "acerta ", formatar_pct(v$acerto_mapeamento), " em lista de 20, contra ",
      formatar_pct(v$acerto_piso_filtro), " do filtro com acaso, uma diferença de ",
      formatar_pontos(v$diferenca_mapeamento, percentual = TRUE), ". O piso aqui é binário, com ",
      "os empates desfeitos pela semente de desempate, e difere dos ",
      formatar_pct(registro$verificacao$conteudo$piso_filtro_acaso), " esperados do ",
      "pré-registro só pelo sorteio dos empates. Dos pares discordantes, ",
      v$discordantes_a_favor, " favorecem o mapeamento e ", v$discordantes_contra,
      " o piso, e o McNemar exato bilateral dá p = ", formatar_decimal(v$valor_p_mcnemar, 3),
      ": o mapeamento ", if (supera) "supera" else "não supera", " o piso a 5%. O ganho ",
      "mínimo detectável é de ", formatar_pontos(v$ganho_minimo_dominancia), " no cenário ",
      "otimista e de ", pontos(v$ganho_minimo_independencia), " no conservador. Entre ",
      nrow(replicas), " realizações, com a escolha de hiperparâmetros refeita em cada uma, ",
      "a diferença fica em ", intervalo_pontos("diferenca_mapeamento"), ", e o teste ",
      "rejeita a favor do mapeamento em ", formatar_pct(rejeicoes, 0), " delas",
      fora(c(diferenca_mapeamento = "diferença", acerto_piso_filtro = "piso")), "."
    ),
    if (supera) {
      paste0(
        "**Pela regra pré-registrada, o mapeamento passa a ordenar dentro do filtro** ",
        "no ramo de quem só respondeu o formulário. A adoção vem com a leitura do ",
        "diagnóstico W3.2.2, que diz se a vantagem é afinidade com o projeto ou ",
        "popularidade prevista pelos atributos, e com a frequência de rejeição entre ",
        "realizações, que diz quanto dela é sorteio desta realização."
      )
    } else {
      paste0(
        "**Pela regra pré-registrada, a ordem dentro do filtro fica ao acaso** no ramo ",
        "de quem só respondeu o formulário. A não rejeição não autoriza dizer que nada ",
        "ordena dentro do filtro: o teste só enxergaria, no cenário conservador, um ganho ",
        "a partir de ", formatar_pontos(v$ganho_minimo_independencia), ". O que o resultado ",
        "sustenta é que o mapeamento não mostrou vantagem detectável",
        if (isFALSE(separa_diferenca)) {
          paste0(
            ", e entre realizações a diferença para o piso não se separa de zero, então ",
            "o desfecho não é peculiaridade desta realização."
          )
        } else if (isTRUE(separa_diferenca)) {
          paste0(
            ", embora entre realizações a diferença para o piso se separe de zero: a ",
            "não rejeição aqui é do sorteio desta realização, e a monografia precisa ",
            "dizer as duas coisas."
          )
        } else {
          "."
        },
        " Entre realizações, o teste rejeita a favor do mapeamento em ",
        formatar_pct(rejeicoes, 0), " e contra em ", formatar_pct(rejeicoes_contra, 0),
        ", quando sem efeito cada direção ficaria em até 2,5%. O excesso nas duas ",
        "direções não indica que o mapeamento ordene algo em parte das realizações: ",
        "indica que o teste é anticonservador, porque os pares se agrupam por consultor ",
        "(W3.2.10).",
        " É o desfecho que a síntese da EDA anteviu como possível (III.4): o perfil de ",
        "competências não carrega, nesta base, sinal de ordem dentro do filtro que o ",
        "teste consiga ver."
      )
    },

    "W3.2.2", "instrumento",
    paste0(
      "A correlação de Spearman entre o escore do mapeamento e a popularidade do ",
      "consultor na estimação, sobre os candidatos filtrados dos projetos de teste, ",
      "vai de ", formatar_decimal(min(rho_grupos$rho), 2), " a ",
      formatar_decimal(max(rho_grupos$rho), 2), " entre os grupos e fica em ",
      formatar_decimal(v$spearman_reunida, 2), " reunida, ou ",
      intervalo("spearman_reunida", \(x) formatar_decimal(x, 2)), " entre realizações. ",
      "A variante de escore constante, que dá a cada consultor a média do seu escore ",
      "em todo projeto, acerta ", formatar_pct(v$acerto_constante), ", ou ",
      formatar_pontos(v$diferenca_constante), " sobre o piso, contra ",
      pontos(v$diferenca_mapeamento), " do mapeamento; a distância entre o mapeamento ",
      "e a variante é de ", formatar_pontos(v$mapeamento_menos_constante), ", ou ",
      intervalo_pontos("mapeamento_menos_constante"), " entre realizações."
    ),
    paste0(
      "Diagnóstico descritivo da emenda de 2026-10-24, sem função decisória. ",
      if (isTRUE(separa_constante) && stats::median(replicas$mapeamento_menos_constante) > 0) {
        paste0(
          "**O mapeamento se separa da sua versão sem projeto**: há algo de afinidade ",
          "entre o consultor e o projeto além do que se prevê do consultor sozinho. "
        )
      } else {
        paste0(
          "**O mapeamento não se separa da sua versão sem projeto**: o que ele ordena é ",
          "sobretudo o consultor, e não a afinidade do consultor com o projeto. "
        )
      },
      "A correlação reunida com a popularidade é ", leitura_rho(v$spearman_reunida),
      ", ainda que chegue a ", formatar_decimal(max(rho_grupos$rho), 2), " no grupo de ",
      "maior correlação. Reunir os grupos mistura popularidades contadas em estimações ",
      "diferentes, e por isso o valor reunido fica abaixo dos de vários grupos."
    ),

    "W3.2.3", "instrumento",
    paste0(
      "A escolha de hiperparâmetros é instável entre os grupos: os cinco escolheram ",
      v$escolha_configuracoes_distintas, " configurações distintas, com ",
      min(escolhidos$fatores), " a ", max(escolhidos$fatores), " fatores e ",
      "regularização do mapa de ", formatar_decimal(min(escolhidos$lambda_mapeamento), 1),
      " a ", formatar_decimal(max(escolhidos$lambda_mapeamento), 1), ". Entre realizações, ",
      "os cinco grupos escolhem ", intervalo("escolha_configuracoes_distintas", formatar_inteiro),
      " configurações distintas e ", intervalo("escolha_fatores_distintos", formatar_inteiro),
      " valores distintos de fatores."
    ),
    paste0(
      "A validação interna não identifica os hiperparâmetros do mapeamento nesta ",
      "escala. Nenhum valor escolhido deve ser lido como propriedade do problema, ",
      "como um número de fatores adequado à organização: configurações muito ",
      "diferentes servem igualmente. O resultado do teste é o do procedimento de ",
      "escolha, e não de uma configuração."
    ),

    "W3.2.4", "instrumento",
    paste0(
      "A grade de ", superficie$configuracoes[1], " configurações é plana. Em cada grupo, ",
      "a melhor supera a mediana da grade em ", formatar_pontos(v$grade_maximo_menos_mediana),
      ", em média, e a mediana fica a ", formatar_pontos(v$grade_mediana_menos_piso),
      " do piso medido nos mesmos pares de validação; empatam no máximo, em ",
      "média, ", formatar_decimal(v$grade_empatadas_no_maximo, 1), " configurações por ",
      "grupo. Entre realizações, a melhor fica ", intervalo_pontos("grade_maximo_menos_piso"),
      " acima do piso na validação, e a mediana, ", intervalo_pontos("grade_mediana_menos_piso"), "."
    ),
    paste0(
      "A vantagem da melhor configuração na validação é o máximo de muitas medidas ",
      "feitas nos mesmos pares, e por isso otimista: na validação ela passava do piso ",
      "por ", formatar_pontos(v$grade_maximo_menos_piso), ", e no teste a diferença foi de ",
      pontos(v$diferenca_mapeamento), ". A configuração típica da grade não se afasta do ",
      "piso, que é a forma esperada quando o atributo carrega pouco sinal de ordem ",
      "dentro do filtro, como o bloco II.6 antecipou (II.6.6)."
    ),

    "W3.2.5", "consequência do gerador",
    paste0(
      "O filtro de setor carrega sinal: com ordem ao acaso dentro dele acerta ",
      formatar_pct(v$acerto_piso_filtro), " em lista de 20, contra ",
      formatar_pct(v$acaso_puro), " do acaso puro sobre o quadro, com ",
      formatar_pct(v$fracao_passa_filtro), " dos pares passando o filtro."
    ),
    paste0(
      "Verificação descritiva pré-registrada, sem função decisória. A magnitude é do ",
      "gerador, que planta afinidade setorial, e o achado II.7.5 já registrou que no ",
      "dado real o piso deve ser bem menor."
    )
  ), tibble::tribble(
    ~codigo, ~decisao, ~limitacao, ~proveniencia,

    "W3.2.1",
    if (supera) {
      "No ramo de quem só respondeu o formulário, o mapeamento de atributos para fatores ordena os candidatos dentro do filtro de setor."
    } else {
      "No ramo de quem só respondeu o formulário, a ordem dos candidatos dentro do filtro de setor é ao acaso."
    },
    paste0(
      "O teste roda sobre o sintético, em que o gerador não planta vínculo entre ",
      "competência e projeto (II.6.4): o desfecho mede o gerador, e a mesma regra ",
      "precisa ser aplicada ao dado real antes de valer para a organização."
    ),
    "depende do gerador",

    "W3.2.2", NA,
    paste0(
      "O diagnóstico é descritivo e, no sintético, separa o que o gerador plantou: ",
      "não diz como o escore se comportaria com o vínculo real entre competência e ",
      "projeto, que não existe em registro nenhum (II.6.6)."
    ),
    "depende do gerador",

    "W3.2.3", NA,
    paste0(
      "Os hiperparâmetros do mapeamento não são identificados pela validação interna ",
      "nesta escala; o resultado vale para o procedimento de escolha, e não para uma ",
      "configuração."
    ),
    "depende do gerador",

    "W3.2.4", NA,
    paste0(
      "Escolher o máximo de uma grade plana infla a vantagem na validação, e nenhuma ",
      "vantagem medida ali deve ser citada como ganho do modelo."
    ),
    "estrutural"
  )))

  # A emenda 2 passou a decidir o conteúdo: estes achados ficam preservados,
  # identificados como análise conforme a emenda 1, e o W3.2.1 sem decidir.
  achados <- marcar_como_emenda_1(achados)

  list(objetos = objetos, tabelas = tabelas, figuras = figuras, achados = achados)
}
