#' Marca os achados da Etapa 2 como análise conforme a emenda 1
#'
#' A emenda 2 corrigiu a pontuação dentro do filtro e passou a decidir o
#' conteúdo. Os achados da emenda 1 ficam com os números preservados,
#' identificados, e o W3.2.1 deixa de decidir.
#'
#' @param achados Achados de `executar_w3_conteudo()`.
#' @return `achados` com a marca e sem a decisão do W3.2.1.
marcar_como_emenda_1 <- function(achados) {
  achados |>
    dplyr::mutate(
      enunciado = paste0("**Análise conforme a emenda 1.** ", enunciado),
      implicacao = dplyr::if_else(
        codigo == "W3.2.1",
        paste0(
          implicacao, " **Esta análise não decide**: a revisão de ponta a ponta achou ",
          "um defeito na pontuação dentro do filtro (W3.2.6), a emenda 2 o corrigiu, e ",
          "a decisão do conteúdo é a do achado W3.2.7."
        ),
        implicacao
      ),
      decisao = dplyr::if_else(codigo == "W3.2.1", NA_character_, decisao)
    )
}

#' Executa o bloco da Etapa 2 sob a emenda 2
#'
#' O teste único com as previsões cruzadas, que decide o conteúdo, o
#' diagnóstico do R² do mapa e da popularidade prevista, e as realizações,
#' cada uma refazendo o procedimento inteiro. Inclui o achado do defeito e
#' da correção.
#'
#' @param avaliacao Lista de `avaliar_componente_de_conteudo_cruzado()`.
#' @param replicas tibble de `simular_avaliacao_do_conteudo_cruzado()`.
#' @param selecao_emenda_1 Lista de `selecionar_hiperparametros_do_conteudo()`.
#' @param avaliacao_emenda_1 Lista de `avaliar_componente_de_conteudo()`.
#' @param correlacao tibble de `medir_correlacao_do_mapa()`.
#' @param dispersao tibble de `medir_dispersao_da_lista()`.
#' @param registro Lista de `ler_pre_registro()`.
#' @return Lista com `objetos`, `tabelas`, `figuras` e `achados`.
executar_w3_conteudo_e2 <- function(avaliacao, replicas, selecao_emenda_1, avaliacao_emenda_1,
                                    correlacao, dispersao, registro) {
  versionado <- resumir_avaliacao_do_conteudo_cruzado(avaliacao, registro) |>
    dplyr::mutate(mapeamento_menos_constante = acerto_mapeamento - acerto_constante)
  replicas <- dplyr::mutate(replicas, mapeamento_menos_constante = acerto_mapeamento - acerto_constante)
  estabilidade <- resumir_estabilidade(replicas, nivel = registro$estabilidade$nivel)
  v <- as.list(versionado)
  e1 <- as.list(resumir_avaliacao_do_conteudo(avaliacao_emenda_1, registro))

  pontos <- \(x) formatar_decimal(100 * x, 1)
  decimal <- \(x) formatar_decimal(x, 2)
  intervalo <- \(medida, formatador = formatar_pct) formatar_intervalo(estabilidade, medida, formatador)
  intervalo_pontos <- \(medida) paste0(intervalo(medida, pontos), " pontos")
  separa <- \(medida) intervalo_separa_de_zero(estabilidade, medida)
  supera <- v$supera_piso == 1
  rejeicoes <- mean(replicas$supera_piso)
  r2 <- avaliacao$r2
  # Descritivo, ao lado do teste: a diferença pareada com o intervalo de
  # escore híbrido de Newcombe (1998).
  newcombe <- intervalo_da_diferenca_pareada(avaliacao$pares$acerto_modelo, avaliacao$pares$acerto_piso)
  faixa_newcombe <- paste0(
    pontos(newcombe$diferenca), " pontos (", pontos(newcombe$inferior), " a ", pontos(newcombe$superior), ")"
  )
  newcombe_e1 <- intervalo_da_diferenca_pareada(
    avaliacao_emenda_1$pares$acerto_modelo, avaliacao_emenda_1$pares$acerto_piso
  )
  faixa_newcombe_e1 <- paste0(
    pontos(newcombe_e1$diferenca), " pontos (", pontos(newcombe_e1$inferior), " a ",
    pontos(newcombe_e1$superior), ")"
  )

  # Medida descritiva única da hipótese da dispersão. A hipótese se confirma
  # se, na emenda 1, os candidatos de teste ficam mais dispersos que os da
  # estimação, mais do que na emenda 2, e caem mais vezes nos extremos.
  na_emenda <- \(emenda, coluna) stats::median(dispersao[[coluna]][dispersao$emenda == emenda])
  razao_e1 <- na_emenda("emenda 1", "razao_dispersao")
  razao_e2 <- na_emenda("emenda 2", "razao_dispersao")
  extremos_e1 <- na_emenda("emenda 1", "nos_extremos")
  extremos_e2 <- na_emenda("emenda 2", "nos_extremos")
  mecanismo_confirmado <- razao_e1 > 1 && razao_e1 > razao_e2 && extremos_e1 > extremos_e2

  faixa_correlacao <- \(coluna) paste0(
    formatar_decimal(min(correlacao[[coluna]], na.rm = TRUE), 2), " a ",
    formatar_decimal(max(correlacao[[coluna]], na.rm = TRUE), 2)
  )
  rho_grupos <- avaliacao$spearman[avaliacao$spearman$grupo != "reunida", ]

  # O defeito, medido na validação interna: a melhor configuração do grupo 1
  # sob a emenda 1 e sob a emenda 2, nos mesmos pares.
  melhor <- \(selecao, g) max(selecao$grade$acerto[selecao$grade$grupo == g])
  grupo_1_e1 <- melhor(selecao_emenda_1, 1)
  grupo_1_e2 <- melhor(avaliacao$selecao, 1)

  superficie <- avaliacao$selecao$grade |>
    dplyr::summarise(
      maximo = max(acerto), mediana = stats::median(acerto),
      empatadas = sum(acerto == max(acerto)), configuracoes = dplyr::n(), .by = grupo
    ) |>
    dplyr::left_join(avaliacao$piso_na_validacao, by = "grupo") |>
    dplyr::left_join(
      avaliacao$selecao$escolhidos |> dplyr::select(grupo, fatores, lambda, iteracoes, lambda_mapeamento),
      by = "grupo"
    )
  escolhidos <- avaliacao$selecao$escolhidos

  objetos <- list(
    versionado = versionado, estabilidade = estabilidade, superficie = superficie,
    r2 = r2, spearman = avaliacao$spearman, rejeicoes = rejeicoes,
    grupo_1_e1 = grupo_1_e1, grupo_1_e2 = grupo_1_e2, emenda_1 = e1
  )

  linha_estavel <- function(medida, rotulo, formatador = formatar_pct) {
    tibble::tibble(
      Medida = rotulo, `Realização versionada` = formatador(v[[medida]]),
      `Entre realizações` = intervalo(medida, formatador)
    )
  }
  tabelas <- list(
    w3_conteudo_e2_teste = dplyr::bind_rows(
      linha_estavel("n_pares_conteudo", "Pares de respondentes, cinco grupos reunidos", formatar_inteiro),
      linha_estavel("acerto_mapeamento", "Acerto do mapeamento em lista de 20"),
      linha_estavel("acerto_piso_filtro", "Acerto do filtro com acaso"),
      linha_estavel("diferenca_mapeamento", "Diferença, em pontos percentuais", pontos),
      linha_estavel("discordantes_a_favor", "Pares discordantes a favor do mapeamento", formatar_inteiro),
      linha_estavel("discordantes_contra", "Pares discordantes a favor do piso", formatar_inteiro),
      linha_estavel("valor_p_mcnemar", "Valor-p do McNemar exato bilateral", \(x) formatar_decimal(x, 3)),
      tibble::tibble(
        Medida = "Supera o piso a 5%", `Realização versionada` = if (supera) "sim" else "não",
        `Entre realizações` = paste0(formatar_pct(rejeicoes, 0), " das realizações")
      ),
      linha_estavel("ganho_minimo_dominancia", "Ganho mínimo, otimista (pontos)", pontos),
      linha_estavel("ganho_minimo_independencia", "Ganho mínimo, conservador (pontos)", pontos),
      tibble::tibble(
        Medida = "Diferença pareada, IC 95% de Newcombe (descritivo)",
        `Realização versionada` = faixa_newcombe,
        `Entre realizações` = paste0(intervalo("diferenca_mapeamento", pontos), " pontos")
      )
    ),
    w3_conteudo_e2_diagnostico = dplyr::bind_rows(
      tibble::tibble(
        Medida = paste0("R² do mapa no grupo ", r2$grupo, ", dentro / fora da amostra"),
        `Realização versionada` = paste0(decimal(r2$r2_dentro), " / ", decimal(r2$r2_fora)),
        `Entre realizações` = "--"
      ),
      linha_estavel("r2_mapa_dentro", "R² médio do mapa, dentro da amostra", decimal),
      linha_estavel("r2_mapa_fora", "R² médio do mapa, fora da amostra", decimal),
      tibble::tibble(
        Medida = paste0(
          "Correlação do fator previsto com o ajustado, grupo ", correlacao$grupo,
          ", dimensão ", correlacao$dimensao, ", fora / dentro da amostra"
        ),
        `Realização versionada` = paste0(decimal(correlacao$correlacao_fora), " / ", decimal(correlacao$correlacao_dentro)),
        `Entre realizações` = "--"
      ),
      tibble::tibble(
        Medida = paste0("Spearman do escore com a popularidade, grupo ", rho_grupos$grupo),
        `Realização versionada` = formatar_decimal(rho_grupos$rho, 3), `Entre realizações` = "--"
      ),
      linha_estavel("spearman_reunida", "Spearman do escore com a popularidade, reunida", \(x) formatar_decimal(x, 3)),
      linha_estavel("acerto_constante", "Acerto da variante de escore constante"),
      linha_estavel("mapeamento_menos_constante", "Mapeamento menos a variante (pontos)", pontos)
    ),
    w3_conteudo_e2_superficie = superficie |>
      dplyr::transmute(
        Grupo = grupo,
        `Escolha (fatores, lambda, iterações, lambda do mapa)` = paste0(
          fatores, "; ", formatar_decimal(lambda, 1), "; ", iteracoes, "; ", formatar_decimal(lambda_mapeamento, 1)
        ),
        `Melhor da grade` = formatar_pct(maximo), `Mediana da grade` = formatar_pct(mediana),
        `Piso na mesma validação` = formatar_pct(piso), `Empatadas no máximo` = paste0(empatadas, " de ", configuracoes)
      ),
    w3_conteudo_e2_dispersao = dispersao |>
      dplyr::transmute(
        Emenda = emenda, Grupo = grupo, `Pares que passam o filtro` = pares,
        `Dispersão dos candidatos de teste sobre a da estimação` = decimal(razao_dispersao),
        `Distância do consultor de teste à média da estimação (desvios)` = decimal(distancia_focal),
        `Percentil mediano do consultor de teste` = decimal(percentil_mediana),
        `Consultor de teste nos extremos da lista` = formatar_pct(nos_extremos)
      ),
    w3_conteudo_emendas = tibble::tibble(
      Medida = c(
        "Acerto do mapeamento", "Acerto do filtro com acaso", "Diferença (pontos)",
        "Discordantes a favor / contra", "Valor-p do McNemar exato", "Decide o conteúdo"
      ),
      `Emenda 1` = c(
        formatar_pct(e1$acerto_mapeamento), formatar_pct(e1$acerto_piso_filtro), pontos(e1$diferenca_mapeamento),
        paste0(e1$discordantes_a_favor, " / ", e1$discordantes_contra), formatar_decimal(e1$valor_p_mcnemar, 3), "não"
      ),
      `Emenda 2` = c(
        formatar_pct(v$acerto_mapeamento), formatar_pct(v$acerto_piso_filtro), pontos(v$diferenca_mapeamento),
        paste0(v$discordantes_a_favor, " / ", v$discordantes_contra), formatar_decimal(v$valor_p_mcnemar, 3), "sim"
      )
    ),
    w3_conteudo_e2_estabilidade = estabilidade |>
      dplyr::mutate(
        versionado = purrr::map_dbl(medida, \(m) v[[m]]),
        posicao = purrr::map2_chr(medida, versionado, \(m, x) posicao_no_intervalo(estabilidade, m, x))
      ) |>
      dplyr::transmute(
        Medida = medida, `Realização versionada` = formatar_decimal(versionado, 4),
        Mediana = formatar_decimal(mediana, 4), Inferior = formatar_decimal(inferior, 4),
        Superior = formatar_decimal(superior, 4), `Posição da versionada` = posicao, `Réplicas` = n_replicas
      )
  )

  figuras <- list(
    w3_conteudo_e2_diferencas = figura_diferencas_entre_realizacoes(
      replicas, versionado,
      rotulos = c(
        diferenca_mapeamento = "Mapeamento de atributos",
        diferenca_constante = "Variante de escore constante"
      ),
      coluna_ganho = "ganho_minimo_independencia",
      titulo = "Diferença para o filtro com acaso sob a emenda 2, entre realizações",
      subtitulo = paste(
        "Previsões cruzadas do mapa; cada ponto é uma realização com a escolha refeita,",
        "o losango é a versionada, e a linha tracejada o ganho mínimo conservador"
      ),
      eixo_x = "Diferença de acerto em lista de 20 para o filtro com acaso (pontos percentuais)"
    )
  )

  achados <- registrar_achados("W3.2", com_sintese(tibble::tribble(
    ~codigo, ~etiqueta, ~enunciado, ~implicacao,

    "W3.2.6", "instrumento",
    paste0(
      "Na emenda 1, os candidatos da estimação eram pontuados por previsões dentro da ",
      "amostra do mapa, com R² de até ", formatar_decimal(floor(1000 * max(r2$r2_dentro)) / 1000, 3),
      " por grupo, e o consultor de teste por uma previsão fora dela. Com isso, a escala ",
      "comum exigida pela regra não existia. Sob a emenda 1, houve ", e1$discordantes_a_favor,
      " discordantes a favor do mapeamento e ", e1$discordantes_contra, " contra, uma ",
      "diferença de ", faixa_newcombe_e1, " pelo intervalo de 95% de Newcombe. Sob a ",
      "emenda 2, ", v$discordantes_a_favor, " a favor e ", v$discordantes_contra,
      " contra, uma diferença de ", pontos(v$diferenca_mapeamento), " pontos. Na validação ",
      "interna do grupo 1, a melhor configuração caiu de ", formatar_pct(grupo_1_e1),
      " para ", formatar_pct(grupo_1_e2), "."
    ),
    paste0(
      "Um defeito de escala não tem direção determinada por construção: ele faz a posição ",
      "do consultor de teste depender da dispersão das previsões, e não da informação que ",
      "elas carregam. Aqui ele ",
      if (e1$diferenca_mapeamento > v$diferenca_mapeamento) "favoreceu" else "desfavoreceu",
      " o mapeamento, e a correção ",
      if (e1$diferenca_mapeamento > v$diferenca_mapeamento) "reduziu a vantagem" else "aumentou a vantagem",
      " e ", if (v$valor_p_mcnemar > e1$valor_p_mcnemar) "elevou" else "reduziu", " o p de ",
      formatar_decimal(e1$valor_p_mcnemar, 3), " para ", formatar_decimal(v$valor_p_mcnemar, 3), ". ",
      if (e1$diferenca_mapeamento > v$diferenca_mapeamento) {
        paste0(
          "A correção foi motivada pela validade da comparação e operou contra o ",
          "resultado do modelo, o que afasta a hipótese de ter sido escolhida pelo resultado."
        )
      } else {
        "A correção foi motivada pela validade da comparação, registrada antes de qualquer ajuste sob a regra nova."
      }
    ),

    "W3.2.7", "instrumento",
    paste0(
      "Sob a emenda 2, sobre os ", formatar_inteiro(v$n_pares_conteudo), " pares de ",
      "respondentes, o mapeamento acerta ", formatar_pct(v$acerto_mapeamento),
      " em lista de 20, contra ", formatar_pct(v$acerto_piso_filtro), " do filtro com ",
      "acaso, uma diferença de ", pontos(v$diferenca_mapeamento), " pontos. Dos pares ",
      "discordantes, ", v$discordantes_a_favor, " favorecem o mapeamento e ",
      v$discordantes_contra, " o piso, e o McNemar exato bilateral dá p = ",
      formatar_decimal(v$valor_p_mcnemar, 3), ": o mapeamento ",
      if (supera) "supera" else "não supera", " o piso a 5%. Sob a emenda 1 o valor-p era ",
      formatar_decimal(e1$valor_p_mcnemar, 3), ". O ganho mínimo detectável é de ",
      pontos(v$ganho_minimo_dominancia), " pontos no cenário otimista e de ",
      pontos(v$ganho_minimo_independencia), " no conservador. Entre ", nrow(replicas),
      " realizações, com a escolha refeita em cada uma, a diferença fica em ",
      intervalo_pontos("diferenca_mapeamento"), ", e o teste rejeita a favor do mapeamento ",
      "em ", formatar_pct(rejeicoes, 0), " delas. Como medida descritiva, a diferença ",
      "pareada é de ", faixa_newcombe, ", pelo intervalo de 95% de escore híbrido de ",
      "Newcombe (1998) para proporções pareadas."
    ),
    paste0(
      if (supera) {
        "**O mapeamento passa a ordenar dentro do filtro** no ramo de quem só respondeu o formulário. "
      } else {
        "**A ordem dentro do filtro fica ao acaso** no ramo de quem só respondeu o formulário, pela regra pré-registrada e sob a emenda 2. "
      },
      "A não rejeição não autoriza dizer que nada ordena dentro do filtro: o teste só ",
      "enxergaria, no cenário conservador, um ganho a partir de ",
      pontos(v$ganho_minimo_independencia), " pontos",
      if (isFALSE(separa("diferenca_mapeamento"))) {
        ", e entre realizações a diferença não se separa de zero."
      } else {
        "."
      },
      if (supera == (e1$supera_piso == 1)) " A correção não mudou a decisão" else " A correção mudou a decisão",
      if (v$diferenca_mapeamento < e1$diferenca_mapeamento) {
        ": com a pontuação na mesma escala, a vantagem do mapeamento ficou menor, e não maior."
      } else {
        ": com a pontuação na mesma escala, a vantagem do mapeamento não diminuiu."
      }
    ),

    "W3.2.8", "instrumento",
    paste0(
      "Fora da amostra, o R² do mapa ",
      if (all(r2$r2_fora < 0)) "é negativo em todos os grupos, de " else "vai de ",
      decimal(min(r2$r2_fora)), " a ", decimal(max(r2$r2_fora)), ", e o médio fica em ",
      intervalo("r2_mapa_fora", decimal), " entre realizações, contra ",
      intervalo("r2_mapa_dentro", decimal), " dentro da amostra. A correlação entre o ",
      "fator previsto fora da amostra e o fator ajustado vai de ",
      faixa_correlacao("correlacao_fora"), " entre grupos e dimensões, contra ",
      faixa_correlacao("correlacao_dentro"), " dentro da amostra. A correlação de ",
      "Spearman entre o escore e a popularidade na estimação fica em ",
      formatar_decimal(v$spearman_reunida, 2), " reunida, ou ",
      intervalo("spearman_reunida", decimal), " entre realizações, e a distância entre o ",
      "mapeamento e a variante de escore constante, em ",
      intervalo_pontos("mapeamento_menos_constante"), "."
    ),
    paste0(
      "Diagnóstico descritivo da emenda 2, sem função decisória. ",
      if (stats::median(replicas$r2_mapa_fora) < 0) {
        paste0(
          "**Os atributos declarados não preveem os fatores**: fora da amostra o mapa erra ",
          "mais que a média, e todo o ajuste que ele mostrava dentro da amostra era ",
          "memória do histórico. "
        )
      } else {
        "Os atributos declarados preveem pouco dos fatores fora da amostra, muito menos do que o ajuste dentro dela sugeria. "
      },
      "Sem ",
      "o defeito, o escore deixa de carregar a popularidade, e o que sobra é ruído em ",
      "torno do piso. É a forma que se espera no sintético, em que o gerador não planta ",
      "vínculo entre competência e projeto (II.6.4)."
    ),

    "W3.2.9", "instrumento",
    paste0(
      "Sob a emenda 2, os cinco grupos escolheram ", v$escolha_configuracoes_distintas,
      " configurações distintas, com ", min(escolhidos$fatores), " a ",
      max(escolhidos$fatores), " fatores e regularização do mapa de ",
      formatar_decimal(min(escolhidos$lambda_mapeamento), 1), " a ",
      formatar_decimal(max(escolhidos$lambda_mapeamento), 1), ". A melhor configuração ",
      "supera a mediana da grade em ", pontos(v$grade_maximo_menos_mediana), " pontos, em ",
      "média, e a mediana fica a ", pontos(v$grade_mediana_menos_piso), " pontos do piso ",
      "nos mesmos pares. Entre realizações, a melhor fica ",
      intervalo_pontos("grade_maximo_menos_piso"), " acima do piso na validação."
    ),
    paste0(
      "A escolha segue instável e a superfície segue plana, como sob a emenda 1 ",
      "(W3.2.3, W3.2.4): a correção removeu a memória do histórico, e não criou sinal ",
      "onde ele não existia."
    )
  ), tibble::tribble(
    ~codigo, ~decisao, ~limitacao, ~proveniencia,

    "W3.2.6",
    "A decisão do conteúdo passa à emenda 2.",
    paste0(
      if (mecanismo_confirmado) {
        "O mecanismo da direção foi medido de forma descritiva: "
      } else {
        "O mecanismo da direção não foi identificado: "
      },
      "na lista, a dispersão dos escores dos candidatos de teste dividida pela dos ",
      "candidatos da estimação fica em ", decimal(razao_e1), " na emenda 1 e em ",
      decimal(razao_e2), " na emenda 2, na mediana entre grupos, e o consultor de teste ",
      "cai nos extremos da lista em ", formatar_pct(extremos_e1), " e ", formatar_pct(extremos_e2),
      " das listas", if (mecanismo_confirmado) "." else ", o contrário do que a hipótese da dispersão previa. A investigação parou nessa medida única.",
      " Só o mapa é cruzado: a fatoração segue ajustada com toda a estimação, e o fator de ",
      "cada projeto carrega as alocações dos candidatos da estimação naquele projeto. O ",
      "canal pelo qual esse resíduo chega ao escore é o fator previsto pelos atributos, e ",
      "ele é fraco: fora da amostra, a correlação entre o fator previsto e o ajustado vai ",
      "de ", faixa_correlacao("correlacao_fora"), ", e o R² do mapa, de ",
      decimal(min(r2$r2_fora)), " a ", decimal(max(r2$r2_fora)), ". No uso real, o projeto ",
      "a alocar ainda não tem alocações, e por isso o fator dele não carrega esse resíduo."
    ),
    "estrutural",

    "W3.2.7",
    if (supera) {
      "No ramo de quem só respondeu o formulário, o mapeamento de atributos para fatores ordena os candidatos dentro do filtro de setor."
    } else {
      "No ramo de quem só respondeu o formulário, a ordem dos candidatos dentro do filtro de setor é ao acaso."
    },
    paste0(
      "O teste roda sobre o sintético, em que o gerador não planta vínculo entre ",
      "competência e projeto (II.6.4): o desfecho mede o gerador, e a mesma regra precisa ",
      "ser aplicada ao dado real antes de valer para a organização."
    ),
    "depende do gerador",

    "W3.2.8", NA,
    paste0(
      "O diagnóstico é descritivo; no sintético ele mede o que o gerador plantou, e não ",
      "como o mapa se comportaria com o vínculo real entre competência e projeto."
    ),
    "depende do gerador"
  )))

  list(objetos = objetos, tabelas = tabelas, figuras = figuras, achados = achados)
}
