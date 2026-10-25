#' Executa o bloco da Etapa 3, o componente colaborativo
#'
#' Reúne a sequência pré-registrada de testes na realização versionada, H1 e
#' depois H2, e as realizações, em que todo o procedimento é refeito, e
#' devolve tabelas, figura e achados no formato da EDA. O acerto é reportado
#' nos pares avaliáveis, e a fração de pares avaliáveis, à parte.
#'
#' @param avaliacao Lista de `avaliar_componente_colaborativo()` sobre o
#'   conjunto versionado.
#' @param replicas tibble de `simular_avaliacao_do_colaborativo()`.
#' @param registro Lista de `ler_pre_registro()`.
#' @return Lista com `objetos`, `tabelas`, `figuras` e `achados`.
executar_w3_colaborativo <- function(avaliacao, replicas, registro) {
  versionado <- resumir_avaliacao_do_colaborativo(avaliacao, registro)
  estabilidade <- resumir_estabilidade(replicas, nivel = registro$estabilidade$nivel)
  v <- as.list(versionado)

  pontos <- \(x) formatar_decimal(100 * x, 1)
  intervalo <- \(medida, formatador = formatar_pct) formatar_intervalo(estabilidade, medida, formatador)
  intervalo_pontos <- \(medida) paste0(intervalo(medida, pontos), " pontos")
  fora <- \(rotulos) descrever_ponto_fora(vapply(
    names(rotulos), \(m) posicao_no_intervalo(estabilidade, m, v[[m]]), character(1)
  ) |> stats::setNames(rotulos))
  separa <- \(medida) intervalo_separa_de_zero(estabilidade, medida)

  supera_h1 <- v$supera_h1 == 1
  supera_h2 <- v$supera_h2 == 1
  rejeicoes_h1 <- mean(replicas$supera_h1)
  rejeicoes_h2 <- mean(replicas$supera_h2)
  escolhidos <- avaliacao$selecao$escolhidos
  validacao <- avaliacao$selecao$validacao

  # Leitura da correlação do diagnóstico de convergência contra a previsão
  # do achado II.2.4: onde falta evidência, a fatoração regularizada reproduz
  # a popularidade.
  leitura_convergencia <- function(rho) {
    if (rho >= 0.7) {
      paste0(
        "**A fatoração escolhida converge para a popularidade**, como o achado II.2.4 ",
        "previa para a forma regularizada nesta escala: o escore dela ordena os ",
        "consultores quase como a contagem de alocações."
      )
    } else if (rho >= 0.4) {
      paste0(
        "A fatoração escolhida segue a popularidade em boa parte, na direção que o ",
        "achado II.2.4 previa, sem se reduzir a ela por inteiro."
      )
    } else {
      paste0(
        "A fatoração escolhida não se reduz à popularidade, ao contrário do que o ",
        "achado II.2.4 previa, embora empate com ela no acerto."
      )
    }
  }

  objetos <- list(
    versionado = versionado, estabilidade = estabilidade, adotado = avaliacao$adotado,
    rejeicoes_h1 = rejeicoes_h1, rejeicoes_h2 = rejeicoes_h2, n_replicas = nrow(replicas)
  )

  linha_estavel <- function(medida, rotulo, formatador = formatar_pct) {
    tibble::tibble(
      Medida = rotulo,
      `Realização versionada` = formatador(v[[medida]]),
      `Entre realizações` = intervalo(medida, formatador)
    )
  }
  linha_teste <- function(rotulo, supera, rejeicoes) {
    tibble::tibble(
      Medida = rotulo,
      `Realização versionada` = if (supera) "sim" else "não",
      `Entre realizações` = paste0(formatar_pct(rejeicoes, 0), " das realizações")
    )
  }
  valor_p <- \(x) formatar_decimal(x, 3)

  tabelas <- list(
    w3_colaborativo_teste = dplyr::bind_rows(
      linha_estavel("n_teste_temporal", "Pares do teste temporal", formatar_inteiro),
      linha_estavel("n_avaliavel", "Pares avaliáveis pelo colaborativo", formatar_inteiro),
      linha_estavel("fracao_avaliavel", "Fração avaliável"),
      linha_estavel(
        "acerto_popularidade_esperado",
        "Piso de popularidade, acerto esperado sob desempate aleatório (o pré-registrado)"
      ),
      linha_estavel(
        "acerto_popularidade",
        "Piso de popularidade, empates desfeitos por sorteio (o usado no McNemar)"
      ),
      linha_estavel("acerto_binario", "Acerto da fatoração binária (alfa = 0)"),
      linha_estavel("acerto_confianca", "Acerto da fatoração com confiança (alfa = 1)"),
      linha_estavel("diferenca_h1", "H1: binária menos popularidade (pontos)", pontos),
      linha_estavel("discordantes_h1_a_favor", "H1: discordantes a favor da fatoração", formatar_inteiro),
      linha_estavel("discordantes_h1_contra", "H1: discordantes a favor da popularidade", formatar_inteiro),
      linha_estavel("valor_p_h1", "H1: valor-p do McNemar exato", valor_p),
      linha_teste("H1: supera a popularidade a 5%", supera_h1, rejeicoes_h1),
      linha_estavel("ganho_minimo_h1_dominancia", "H1: ganho mínimo, otimista (pontos)", pontos),
      linha_estavel("ganho_minimo_h1_independencia", "H1: ganho mínimo, conservador (pontos)", pontos),
      linha_estavel("diferenca_h2", "H2: confiança menos binária (pontos)", pontos),
      linha_estavel("discordantes_h2_a_favor", "H2: discordantes a favor da confiança", formatar_inteiro),
      linha_estavel("discordantes_h2_contra", "H2: discordantes a favor do binário", formatar_inteiro),
      linha_estavel("valor_p_h2", "H2: valor-p do McNemar exato", valor_p),
      linha_teste(
        if (supera_h1) "H2: confiança supera o binário a 5%" else "H2, descritiva: confiança supera o binário a 5%",
        supera_h2, rejeicoes_h2
      ),
      linha_estavel("ganho_minimo_h2_independencia", "H2: ganho mínimo, conservador (pontos)", pontos),
      linha_estavel(
        "concordancia_binario_popularidade",
        "Convergência: pares com o mesmo resultado na fatoração e na popularidade"
      ),
      linha_estavel(
        "spearman_binario_popularidade",
        "Convergência: Spearman do escore da fatoração com a popularidade",
        \(x) formatar_decimal(x, 3)
      )
    ),
    w3_colaborativo_estabilidade = estabilidade |>
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
    w3_colaborativo_diferencas = figura_diferencas_entre_realizacoes(
      replicas, versionado,
      rotulos = c(
        diferenca_h1 = "H1: fatoração binária menos popularidade",
        diferenca_h2 = "H2: confiança menos binária"
      ),
      coluna_ganho = "ganho_minimo_h1_independencia",
      titulo = "Diferenças de acerto do colaborativo, entre realizações",
      subtitulo = paste(
        "Pares avaliáveis do teste temporal; o losango é a versionada, e a linha",
        "tracejada o ganho mínimo conservador de H1"
      ),
      eixo_x = "Diferença de acerto em lista de 20 (pontos percentuais)"
    )
  )

  achados <- registrar_achados("W3.3", com_sintese(tibble::tribble(
    ~codigo, ~etiqueta, ~enunciado, ~implicacao,

    "W3.3.1", "instrumento",
    paste0(
      "Sob a separação temporal, o colaborativo só pontua ", formatar_inteiro(v$n_avaliavel),
      " dos ", formatar_inteiro(v$n_teste_temporal), " pares do teste, ou ",
      formatar_pct(v$fracao_avaliavel), ", porque nos demais o consultor ou o projeto não ",
      "aparece na estimação. Entre realizações, a fração avaliável fica em ",
      intervalo("fracao_avaliavel"), ", com ", intervalo("n_avaliavel", formatar_inteiro),
      " pares avaliáveis", fora(c(fracao_avaliavel = "fração avaliável")), "."
    ),
    paste0(
      "Confirma o achado II.7.1 na pipeline da modelagem. Todo acerto do colaborativo ",
      "abaixo é medido nesses pares, e a conclusão vale para eles: sobre os outros ",
      formatar_pct(1 - v$fracao_avaliavel), " do teste o colaborativo não tem o que dizer, ",
      "porque projeto de consultoria raramente volta."
    ),

    "W3.3.2", "instrumento",
    paste0(
      "H1: nos ", formatar_inteiro(v$n_avaliavel), " pares avaliáveis, a fatoração binária ",
      "acerta ", formatar_pct(v$acerto_binario), " em lista de 20, contra ",
      formatar_pct(v$acerto_popularidade), " da popularidade contada na estimação, uma ",
      "diferença de ", pontos(v$diferenca_h1), " pontos. O piso tem duas medidas, e as ",
      "duas são reportadas: o acerto esperado sob desempate aleatório, que é o ",
      "pré-registrado, fica em ", formatar_pct(v$acerto_popularidade_esperado), ", ou ",
      intervalo("acerto_popularidade_esperado"), " entre realizações; com os empates ",
      "desfeitos por sorteio, como o McNemar exige, fica em ",
      formatar_pct(v$acerto_popularidade), ", ou ", intervalo("acerto_popularidade"),
      ". Dos pares discordantes, ",
      v$discordantes_h1_a_favor, " favorecem a fatoração e ", v$discordantes_h1_contra,
      " a popularidade, e o McNemar exato bilateral dá p = ", valor_p(v$valor_p_h1),
      ": a fatoração ", if (supera_h1) "supera" else "não supera", " a popularidade a 5%. ",
      "O ganho mínimo detectável é de ", pontos(v$ganho_minimo_h1_dominancia), " pontos no ",
      "cenário otimista e de ", pontos(v$ganho_minimo_h1_independencia), " no ",
      "conservador. Entre ", nrow(replicas), " realizações, a diferença fica em ",
      intervalo_pontos("diferenca_h1"), ", e H1 rejeita a favor da fatoração em ",
      formatar_pct(rejeicoes_h1, 0), " delas",
      fora(c(diferenca_h1 = "diferença", acerto_popularidade = "piso")), "."
    ),
    if (supera_h1) {
      paste0(
        "**Pela sequência pré-registrada, a fatoração passa a ordenar o ramo ",
        "colaborativo, e H2 decide o sinal.** Com tão poucos pares, a rejeição só ",
        "acontece para ganho grande, e a frequência entre realizações diz quanto dela ",
        "é sorteio desta realização."
      )
    } else {
      paste0(
        "**Pela sequência pré-registrada, o ramo colaborativo é ordenado pela ",
        "popularidade, o sinal fica binário e a comparação de alfa é só descritiva.** ",
        "A não rejeição não é lida como ausência de efeito (II.7.6): com ",
        formatar_inteiro(v$n_avaliavel), " pares o teste só enxerga, no cenário ",
        "conservador, ganho a partir de ", pontos(v$ganho_minimo_h1_independencia),
        " pontos. ",
        if (isFALSE(separa("diferenca_h1"))) {
          "Entre realizações a diferença não se separa de zero, então o desfecho não é peculiaridade desta realização. "
        } else if (isTRUE(separa("diferenca_h1"))) {
          "Entre realizações, porém, a diferença se separa de zero: a não rejeição aqui é sorteio desta realização, e a monografia precisa dizer as duas coisas. "
        } else {
          ""
        },
        "A frequência de rejeição entre realizações, de ", formatar_pct(rejeicoes_h1, 0),
        ", fica ", if (rejeicoes_h1 > 0.025) "acima" else "dentro", " dos 2,5% que o teste ",
        "bilateral daria a favor da fatoração sem efeito nenhum. ",
        "É o desfecho que a EDA declarou esperado: onde falta evidência, a fatoração ",
        "reproduz a popularidade (II.2.4)."
      )
    },

    "W3.3.3", "instrumento",
    paste0(
      if (supera_h1) "H2" else "A comparação de alfa, descritiva porque H1 não rejeitou",
      ": a fatoração com confiança por horas acerta ", formatar_pct(v$acerto_confianca),
      ", contra ", formatar_pct(v$acerto_binario), " da binária, uma diferença de ",
      pontos(v$diferenca_h2), " pontos, com ", v$discordantes_h2_a_favor, " discordantes a ",
      "favor da confiança e ", v$discordantes_h2_contra, " a favor do binário (McNemar ",
      "exato, p = ", valor_p(v$valor_p_h2), "). Entre realizações, a diferença fica em ",
      intervalo_pontos("diferenca_h2"), ", e a confiança supera o binário a 5% em ",
      formatar_pct(rejeicoes_h2, 0), " delas."
    ),
    if (supera_h1 && supera_h2) {
      "**A célula de `R` passa a levar a confiança por horas**, pela sequência pré-registrada."
    } else if (supera_h1) {
      "**O sinal fica binário**: a confiança por horas não superou o binário, como o achado II.5.1 previa como regra."
    } else {
      paste0(
        "Sem função decisória, porque a sequência parou em H1. O número serve só para ",
        "dizer se a confiança mudaria alguma coisa, e a resposta ",
        if (isFALSE(separa("diferenca_h2"))) "é que não, entre realizações." else "fica registrada acima.",
        " As horas dependem da duração recalibrada das alocações, então a comparação ",
        "fala do gerador (II.5.1)."
      )
    },

    "W3.3.4", "instrumento",
    paste0(
      "A escolha de hiperparâmetros saiu de ", validacao$avaliaveis, " pares avaliáveis de ",
      "validação, entre os ", formatar_inteiro(validacao$pares), " da validação temporal ",
      "interna: cada acerto vale ", pontos(1 / validacao$avaliaveis), " pontos. A ",
      "binária escolhida tem ", escolhidos$fatores[escolhidos$alfa == 0], " fatores, e a ",
      "com confiança, ", escolhidos$fatores[escolhidos$alfa == 1], ". Na validação, a ",
      "melhor binária passa da popularidade por ", pontos(v$validacao_binario_menos_piso),
      " pontos, e a mediana da grade fica ", pontos(abs(v$validacao_mediana_binario_menos_piso)),
      " pontos ", if (v$validacao_mediana_binario_menos_piso < 0) "abaixo" else "acima",
      " dela. Entre realizações, os pares avaliáveis de validação ficam em ",
      intervalo("validacao_avaliaveis", formatar_inteiro), ", e a mediana da grade binária ",
      "menos a popularidade, em ", intervalo_pontos("validacao_mediana_binario_menos_piso"), "."
    ),
    paste0(
      "A escolha é quase ruído: uma diferença de poucos acertos decide a configuração, e ",
      "nenhum hiperparâmetro escolhido deve ser lido como propriedade do problema. A ",
      "fatoração típica da grade não passa da popularidade nem na validação, o que é a ",
      "forma esperada quando a matriz não identifica fatores nesta escala (II.2.4)."
    ),

    "W3.3.5", "instrumento",
    paste0(
      "Diagnóstico de convergência, descritivo e definido depois do teste: nos ",
      formatar_inteiro(v$n_avaliavel), " pares avaliáveis, a fatoração binária e a ",
      "popularidade têm o mesmo resultado, as duas acertando ou as duas errando, em ",
      formatar_pct(v$concordancia_binario_popularidade), ", ou ",
      intervalo("concordancia_binario_popularidade"), " entre realizações. A correlação ",
      "de Spearman entre o escore da fatoração e a popularidade na estimação, sobre os ",
      "consultores com fator e os projetos dos pares avaliáveis, é de ",
      formatar_decimal(v$spearman_binario_popularidade, 2), ", ou ",
      intervalo("spearman_binario_popularidade", \(x) formatar_decimal(x, 2)),
      " entre realizações."
    ),
    paste0(
      "Lida na realização típica, a mediana das realizações, e não na versionada: ",
      leitura_convergencia(stats::median(replicas$spearman_binario_popularidade)),
      " Na versionada a correlação, de ", formatar_decimal(v$spearman_binario_popularidade, 2),
      ", fica ", if (v$spearman_binario_popularidade > stats::median(replicas$spearman_binario_popularidade)) "acima" else "abaixo",
      " da mediana das realizações, e não deve ser citada como o valor típico.",
      " A concordância nos acertos é alta em parte por construção, porque as duas ",
      "ordenações erram a maior parte dos pares, e por isso a medida que diz se o ",
      "escore reproduz a popularidade é a correlação, e não a concordância."
    )
  ), tibble::tribble(
    ~codigo, ~decisao, ~limitacao, ~proveniencia,

    "W3.3.1", NA,
    paste0(
      "O colaborativo só fala sobre a fração avaliável do teste, porque projeto de ",
      "consultoria raramente volta; o número de pares é do sintético e precisa ser ",
      "recontado no dado real."
    ),
    "fato real",

    "W3.3.2",
    if (supera_h1) {
      "O ramo colaborativo do híbrido é ordenado pela fatoração para feedback implícito."
    } else {
      "O ramo colaborativo do híbrido é ordenado pela popularidade contada na estimação, e a célula de `R` fica com a ocorrência binária."
    },
    paste0(
      "Com tão poucos pares avaliáveis, o teste só distingue ganho grande; a decisão ",
      "segue a regra pré-registrada e não afirma que a fatoração não carrega sinal."
    ),
    "depende do gerador",

    # Sem H1, a regra já fixa o sinal binário em W3.3.2, e este achado não
    # decide nada.
    "W3.3.3",
    if (!supera_h1) {
      NA_character_
    } else if (supera_h2) {
      "A célula de `R` leva a confiança por horas, 1 + log(1 + horas)."
    } else {
      "A célula de `R` fica com a ocorrência binária da alocação."
    },
    "As horas que definem a confiança dependem da duração recalibrada das alocações, então a comparação de alfa fala do gerador.",
    "depende do gerador",

    "W3.3.4", NA,
    paste0(
      "A escolha dos hiperparâmetros do colaborativo saiu de ", validacao$avaliaveis,
      " pares de validação, e por isso é instável; o resultado vale para o procedimento ",
      "de escolha, e não para uma configuração."
    ),
    "fato real",

    "W3.3.5", NA,
    paste0(
      "O diagnóstico foi pedido depois do teste e não tem função decisória; a ",
      "magnitude da correlação é do sintético, e a previsão que ele confere, a do ",
      "achado II.2.4, se apoia em agregado real."
    ),
    "depende do gerador"
  )))

  list(objetos = objetos, tabelas = tabelas, figuras = figuras, achados = achados)
}
