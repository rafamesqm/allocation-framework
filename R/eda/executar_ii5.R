#' Executa o bloco II.5 da EDA, construção do rótulo
#'
#' **Âncora na literatura.** Hu, Koren e Volinsky (2008) formalizam o
#' colaborativo sobre feedback implícito, que é o caminho de reserva quando
#' não há rótulo explícito de qualidade. A distinção entre rótulo explícito e
#' implícito é o que este bloco precisa decidir antes da wave 3.
#'
#' **Propriedade medida.** Cobertura do rótulo explícito, divergência entre
#' as etapas de avaliação, dimensionalidade dos itens, decomposição da
#' variância do escore, e se a margem pode ser atribuída a uma alocação.
#'
#' **Decisão que discrimina.** Qual dos quatro candidatos de rótulo de
#' `docs/dicionario_de_dados.md` se sustenta.
#'
#' **Este bloco motivou uma correção no gerador, e a correção veio antes das
#' conclusões.** A primeira execução mediu alfa de Cronbach negativo,
#' primeiro componente em 8,5% e correlação média entre itens de -0,002: os
#' 14 itens de cada avaliação eram sorteados de forma independente, e o
#' rótulo não tinha sinal nenhum sobre a pessoa. Concluir sobre construção de
#' rótulo em cima disso seria vacuoso, e pior, concluiria que a média dos
#' itens não é escala, o que é artefato e não propriedade do domínio. O
#' gerador passou a derivar as notas de uma qualidade latente compartilhada
#' (ver `construir_qualidade_latente()`), preservando as marginais
#' calibradas. As medidas abaixo são da versão corrigida.
#'
#' @param dados Lista nomeada de tibbles, de `carregar_dados()`.
#' @param estabilidade tibble de `resumir_estabilidade()`, opcional.
#' @param etapa_rotulo Etapa adotada como rótulo nas medidas que exigem uma.
#' @return Lista com `objetos`, `tabelas`, `figuras` e `achados`.
executar_ii5 <- function(dados, estabilidade = NULL, etapa_rotulo = "gestor_final") {
  cobertura <- medir_cobertura_do_rotulo(dados)
  cruzamento <- cruzar_conteudo_e_rotulo(dados)
  divergencia <- medir_divergencia_entre_avaliadores(dados$avaliacoes_desempenho)
  dimensionalidade <- medir_dimensionalidade_dos_itens(
    dados$avaliacoes_desempenho, etapa = etapa_rotulo
  )
  variancia_escore <- decompor_variancia_do_escore(
    dados$avaliacoes_desempenho, etapa = etapa_rotulo
  )
  margem <- medir_atribuibilidade_da_margem(dados)
  construcoes <- comparar_construcoes_de_rotulo(dados$avaliacoes_desempenho)
  escala_margem <- diagnosticar_escala_da_margem(dados)
  sobredeterminacao <- medir_sobredeterminacao_economica()

  candidatos <- avaliar_candidatos_de_rotulo(
    margem_atribuivel = margem$tem_consultor_na_margem,
    fracao_projetos_com_equipe = margem$fracao_projetos_com_equipe,
    tem_custo_mensal = margem$tem_custo_mensal,
    custo_zero_explicavel_estrutural = margem$custo_zero_estrutural_sem_alocacao,
    alfa_cronbach = dimensionalidade$alfa_ordinal,
    fracao_margem_negativa = margem$fracao_margem_estrutural_negativa
  )
  candidatos_viaveis <- dplyr::filter(candidatos, viavel)

  par_mais_divergente <- divergencia$pares[1, ]
  posicao_variancia <- posicao_no_intervalo(
    estabilidade, "fracao_variancia_entre_consultores",
    variancia_escore$fracao_entre_consultores
  )
  etapa_mais_lenient <- divergencia$por_etapa[1, ]
  etapa_mais_severa <- divergencia$por_etapa[nrow(divergencia$por_etapa), ]

  por_nota <- dados$avaliacoes_desempenho |>
    dplyr::count(etapa, nota) |>
    dplyr::mutate(fracao = n / sum(n), .by = etapa)

  objetos <- list(
    cobertura = cobertura,
    cruzamento = cruzamento,
    divergencia = divergencia,
    dimensionalidade = dimensionalidade,
    variancia_escore = variancia_escore,
    margem = margem,
    escala_margem = escala_margem,
    sobredeterminacao = sobredeterminacao,
    construcoes = construcoes,
    bruta = construcoes[construcoes$forma == "Nota bruta", ],
    normalizada = construcoes[construcoes$forma == "Normalizada por avaliador", ],
    candidatos = candidatos,
    candidatos_viaveis = candidatos_viaveis,
    par_mais_divergente = par_mais_divergente,
    etapa_mais_lenient = etapa_mais_lenient,
    etapa_mais_severa = etapa_mais_severa,
    por_nota = por_nota,
    etapa_rotulo = etapa_rotulo,
    posicao_variancia = posicao_variancia,
    estabilidade = estabilidade
  )

  tabelas <- list(
    ii5_conteudo_rotulo = cruzamento$quadrantes |>
      dplyr::transmute(
        `Situação` = .data$situacao,
        Consultores = formatar_inteiro(.data$consultores),
        `Com rótulo` = formatar_inteiro(.data$com_rotulo),
        `Fração com rótulo` = formatar_pct(.data$fracao_com_rotulo)
      ),
    ii5_cobertura_rotulo = tibble::tibble(
      `Denominador` = c("Alocações", "Consultores", "Projetos"),
      `Com avaliação` = formatar_inteiro(c(
        cobertura$pares_avaliados, cobertura$consultores_avaliados,
        cobertura$projetos_avaliados
      )),
      Total = formatar_inteiro(c(
        cobertura$pares_totais, nrow(dados$consultores), nrow(dados$projetos)
      )),
      Cobertura = formatar_pct(c(
        cobertura$cobertura_pares, cobertura$cobertura_consultores,
        cobertura$cobertura_projetos
      ))
    ),
    ii5_divergencia_etapas = divergencia$por_etapa |>
      dplyr::transmute(
        Etapa = etapa,
        `Nível médio` = formatar_decimal(nivel_medio, 2),
        `Fração ACIMA` = formatar_pct(fracao_acima),
        `Fração ABAIXO` = formatar_pct(fracao_abaixo)
      ),
    ii5_concordancia_pares = divergencia$pares |>
      dplyr::transmute(
        `Etapa A` = etapa_a, `Etapa B` = etapa_b,
        `Concordância` = formatar_pct(concordancia),
        `Correlação` = formatar_decimal(correlacao, 3)
      ),
    ii5_dimensionalidade = tibble::tibble(
      Medida = c(
        "Itens avaliados",
        "Avaliações completas",
        "Variância do primeiro componente",
        "Variância dos dois primeiros",
        "Se os itens fossem independentes",
        "Correlação média entre itens (Pearson)",
        "Correlação média entre itens (policórica)",
        "Alfa de Cronbach (bruto)",
        "Alfa ordinal (policórico)",
        "Fração da variância entre consultores"
      ),
      Valor = c(
        formatar_inteiro(dimensionalidade$n_itens),
        formatar_inteiro(dimensionalidade$n_avaliacoes),
        formatar_pct(dimensionalidade$variancia_primeiro_componente),
        formatar_pct(dimensionalidade$variancia_dois_primeiros),
        formatar_pct(1 / dimensionalidade$n_itens),
        formatar_decimal(dimensionalidade$correlacao_media_itens, 3),
        formatar_decimal(dimensionalidade$correlacao_media_policorica, 3),
        formatar_decimal(dimensionalidade$alfa_cronbach, 3),
        formatar_decimal(dimensionalidade$alfa_ordinal, 3),
        formatar_pct(variancia_escore$fracao_entre_consultores)
      )
    ),
    ii5_sobredeterminacao = sobredeterminacao$ancoras |>
      dplyr::transmute(
        `Âncora` = ancora,
        Medido = formatar_decimal(medido, 2),
        `Reconciliado` = formatar_decimal(reconciliado, 2),
        Ajuste = formatar_decimal(ajuste, 2),
        `Pode absorver` = ifelse(absorve, "sim", "não")
      ),
    ii5_escala_margem = escala_margem$custo_mensal |>
      dplyr::transmute(
        `Nível` = nivel_senioridade,
        Consultores = consultores,
        `Taxa (R$/h)` = formatar_decimal(taxa_mediana, 2),
        `Custo mensal implícito` = formatar_inteiro(round(custo_mensal_implicito))
      ),
    ii5_construcoes_rotulo = construcoes |>
      dplyr::transmute(
        `Forma do rótulo` = forma,
        Avaliadores = avaliadores,
        `Variância explicada pelo avaliador` = formatar_pct(contaminacao_avaliador),
        `Sinal entre consultores` = formatar_pct(sinal_entre_consultores)
      ),
    ii5_candidatos_rotulo = candidatos |>
      dplyr::transmute(
        Candidato = candidato, `Construção` = descricao, Exige = exige,
        `Mecanismo` = ifelse(viavel, "sim", "não"),
        `Escala calibrada` = ifelse(calibrado, "sim", "não"),
        Justificativa = justificativa
      )
  )

  figuras <- list(
    ii5_divergencia_etapas = figura_divergencia_etapas(
      por_nota,
      titulo = "Distribuição da nota em cada etapa de avaliação",
      subtitulo = "As três etapas avaliam a mesma pessoa no mesmo projeto sobre os mesmos itens"
    ),
    ii5_dimensionalidade_itens = figura_dimensionalidade_itens(
      dimensionalidade$variancia_componentes,
      titulo = "Estrutura comum entre os itens da avaliação",
      subtitulo = paste0(
        "Tracejado: o que cada componente explicaria se os ",
        dimensionalidade$n_itens, " itens fossem independentes entre si"
      )
    )
  )

  achados <- registrar_achados("II.5", com_sintese(tibble::tribble(
    ~codigo, ~etiqueta, ~enunciado, ~implicacao,

    "II.5.1", "instrumento",
    paste0(
      "Dizer que o rótulo é a avaliação de desempenho é ambíguo, porque as ",
      "etapas divergem. A autoavaliação põe ",
      formatar_pct(etapa_mais_lenient$fracao_acima),
      " das notas em ACIMA, contra ",
      formatar_pct(etapa_mais_severa$fracao_acima),
      " da etapa mais severa. O par mais divergente, ",
      par_mais_divergente$etapa_a, " e ", par_mais_divergente$etapa_b,
      ", concorda em ", formatar_pct(par_mais_divergente$concordancia),
      " dos itens, com correlação de ",
      formatar_decimal(par_mais_divergente$correlacao, 2), "."
    ),
    paste0(
      "**A etapa adotada é parte da definição do rótulo, não detalhe de ",
      "implementação.** Adotar a autoavaliação daria um rótulo em que quase ",
      "tudo é ACIMA, com pouco a discriminar. O achado II.5.7 mede o efeito ",
      "de normalizar, em vez de deduzi-lo daqui."
    ),

    "II.5.2", "instrumento",
    paste0(
      "Os ", dimensionalidade$n_itens, " itens medem uma coisa só, e a média ",
      "deles é escala confiável: primeiro componente em ",
      formatar_pct(dimensionalidade$variancia_primeiro_componente),
      " contra ", formatar_pct(1 / dimensionalidade$n_itens),
      " que itens independentes dariam. A correlação média entre itens é ",
      formatar_decimal(dimensionalidade$correlacao_media_itens, 2),
      " por Pearson e ",
      formatar_decimal(dimensionalidade$correlacao_media_policorica, 2),
      " por policórica, e o alfa vai de ",
      formatar_decimal(dimensionalidade$alfa_cronbach, 2), " bruto a ",
      formatar_decimal(dimensionalidade$alfa_ordinal, 2), " ordinal."
    ),
    paste0(
      "Autoriza o rótulo escalar: colapsar os itens numa média não destrói ",
      "estrutura, porque a estrutura é comum. A distância entre as duas ",
      "versões de cada medida é o preço da discretização: tratar nota de ",
      "três níveis como 1, 2 e 3 supõe espaçamento igual e atenua a ",
      "correlação, e a policórica troca essa suposição pela de normal ",
      "latente. As duas ficam reportadas porque nenhuma é neutra. **Este ",
      "bloco mediu alfa ",
      "negativo na primeira versão**, e a constatação motivou a correção do ",
      "gerador, que sorteava cada item de forma independente e produzia um ",
      "rótulo sem sinal nenhum sobre a pessoa. O efeito halo é dos fenômenos ",
      "mais bem documentados na literatura de avaliação, e um gerador que não ",
      "o reproduz torna a wave 3 impossível de demonstrar."
    ),

    "II.5.3", "âncora real",
    paste0(
      "A margem não é atribuível a uma alocação individual. ",
      "`margem_frente_mes` não tem `consultor_id`, o usuário confirmou que ",
      "frente e mês é a única granularidade que existe na Peers, e ",
      formatar_pct(margem$fracao_projetos_com_equipe),
      " dos projetos alocados têm mais de uma pessoa, com mediana de ",
      margem$mediana_equipe, " e máximo de ", margem$maior_equipe, "."
    ),
    paste0(
      "**Mata o candidato (b)**, que multiplicaria desempenho por margem na ",
      "célula de `R`. Ratear a margem de uma frente entre os membros do time ",
      "seria estimativa apresentada como captura, no mesmo espírito da ",
      "decisão já registrada sobre `skills_requeridas[]`."
    ),

    "II.5.4", "âncora real",
    paste0(
      "Não existe margem na granularidade mensal. A fonte que parecia ter ",
      "custo mensal é receita decomposta por origem, e a identidade contábil ",
      "fecha no sintético com erro máximo de ",
      formatar_decimal(margem$identidade_receita, 6),
      ". Em `margem_estrutural`, o custo zero coincide ",
      if (margem$custo_zero_estrutural_sem_alocacao) "exatamente" else "apenas em parte",
      " com projeto sem nenhuma alocação."
    ),
    paste0(
      "**Derruba o candidato (c), mas não pelo motivo que este bloco deu na ",
      "primeira versão.** Aquela versão lia os 44,7% de zeros de ",
      "`peers_custointerns_vlr` como incompletude de apontamento de custo, e ",
      "chamou isso de âncora real. A segunda consulta comercial mostrou que ",
      "a fonte é receita e que o zero é ausência de estagiário no mês. O ",
      "candidato (c) cai porque **não há custo mensal**, e não porque o custo ",
      "mensal seja ruim. A despesa nessa granularidade vive em outra tabela, ",
      "de cobertura parcial e também truncada, que o sintético ainda não ",
      "reproduz. O custo zero da margem estrutural segue explicável, porque ",
      "projeto sem alocação não é célula de `R`."
    ),

    "II.5.8", "instrumento",
    paste0(
      "Conferida a plausibilidade dos **valores**, e não só a estrutura dos ",
      "zeros, a margem estrutural não fecha: ",
      formatar_pct(margem$fracao_margem_estrutural_negativa),
      " dos projetos com contrato positivo saem com margem negativa, e no ",
      "quinto percentil o prejuízo chega a ",
      formatar_decimal(abs(margem$margem_estrutural_relativa_p05), 1),
      " vezes o valor do contrato. A mediana, ",
      formatar_pct(margem$margem_estrutural_relativa_mediana),
      ", é plausível; a cauda não."
    ),
    paste0(
      "**É o primeiro caso no capítulo de grandezas individualmente ",
      "calibradas que não fecham em conjunto**, e o achado II.5.10 mede o ",
      "tamanho exato do desacordo. Duas hipóteses foram levantadas aqui e ",
      "fechadas por consulta: a de granularidade do contrato, descartada ",
      "porque o valor é por projeto, e a de preço usado como custo, ",
      "confirmada e corrigida. Depois das duas correções a fração de ",
      "prejuízo caiu para perto da real e a mediana continua alta, porque o ",
      "projeto sintético acumula cerca de metade das horas que o contrato ",
      "implica. O resíduo é o da sobredeterminação, não um defeito à parte."
    ),

    "II.5.9", "âncora real",
    paste0(
      "A taxa que o gerador usava como custo era preço, e a segunda consulta ",
      "comercial confirmou com prova direta: a receita mensal do projeto é ",
      "0,87 vezes horas planejadas vezes a taxa, e a própria tabela deriva a ",
      "taxa do estagiário das tarifas cobradas do cliente. A fonte de custo ",
      "passou a ser a tabela de custo por cargo, que é **salário base por ",
      "hora, sem encargos**, e a razão entre custo e contrato deixou de ",
      "centrar em 1: agora fica em ",
      formatar_decimal(escala_margem$centro_da_razao, 2), "."
    ),
    paste0(
      "Fecha a hipótese que este bloco levantou. O sintoma era a razão ",
      "centrar em 1, que é o que se espera de duas medidas da mesma grandeza ",
      "e não de custo contra receita. `diagnosticar_escala_da_margem()` ",
      "continua rodando como **guarda de regressão**: se alguém repuser o ",
      "rate card como custo, a razão volta a centrar em 1 e o teste acusa. ",
      "Fica a consequência que a confirmação trouxe: como o custo é salário ",
      "base, o fator de carga é parâmetro declarado, e o achado II.5.10 ",
      "mostra que as âncoras não comportam um fator realista."
    ),

    "II.5.10", "âncora real",
    paste0(
      "As âncoras econômicas estão **sobredeterminadas e discordam**. Seis ",
      "agregados reais descrevem a mesma economia, e cinco deles implicam ",
      "contrato mediano de ",
      formatar_inteiro(round(sobredeterminacao$contrato_implicado)),
      " contra os ", formatar_inteiro(sobredeterminacao$contrato_medido),
      " medidos, um desacordo de ",
      formatar_decimal(sobredeterminacao$desacordo, 2), " vezes."
    ),
    paste0(
      "**O bloco não escolhe qual âncora relaxar**, porque a escolha não é ",
      "estatística: depende de saber o que cada número conta, e o acesso ",
      "encerrou. O que ele faz é reportar, para cada âncora, o valor que ",
      "reconciliaria o sistema, e descartar as que não podem absorver a ",
      "diferença: ", paste(sobredeterminacao$descartadas, collapse = " e "),
      ". A jornada teria de passar de um mês cheio de trabalho, e o fator de ",
      "receita teria de passar de 1, o que é impossível se o preço é a ",
      "tarifa cobrada. Sobram ", length(sobredeterminacao$candidatas),
      " candidatas, e a mais provável é o tamanho de equipe: os 2,51 vêm de ",
      "pares distintos entre colaborador e **código** de alocação, e um ",
      "projeto pode ter mais de um código. Se for isso, a equipe real é maior ",
      "e o resto fecha sem mexer em mais nada. É a explicação do resíduo que ",
      "o achado II.5.8 media: a margem estrutural sintética tem prejuízo em ",
      "linha com o real, e mediana alta, porque o projeto acumula cerca de ",
      "metade das horas que o contrato implica."
    ),

    "II.5.5", "instrumento",
    paste0(
      "Dos quatro candidatos de rótulo do dicionário, ",
      nrow(candidatos_viaveis), " têm mecanismo que se sustenta: ",
      paste(candidatos_viaveis$candidato, collapse = " e "),
      ". Nenhum dos dois tem escala calibrada no sintético: a de (a) porque ",
      "a base de avaliação nunca teve acesso liberado, a de (d) pelo motivo ",
      "do achado II.5.8."
    ),
    paste0(
      "**A decisão habilitada é o candidato (a) para `R` e o candidato (d) ",
      "para `f`**: a célula de `R` é o desempenho, normalizado por avaliador ",
      "e com a etapa declarada; a margem sai de `R` e entra como objetivo ",
      "separado no otimizador, calculada estruturalmente de custo carregado ",
      "por horas contra valor de contrato. É a combinação que o dicionário já ",
      "suspeitava ao registrar que a granularidade da margem pesava a favor ",
      "de (c) ou (d), agora com o (c) também descartado, por motivo que o ",
      "dicionário não previa."
    ),

    "II.5.7", "instrumento",
    paste0(
      "Normalizar por avaliador não é refinamento. Entre os ",
      objetos$bruta$avaliadores, " gestores que assinam avaliações, ",
      formatar_pct(objetos$bruta$contaminacao_avaliador),
      " da variância da nota bruta é explicada por **quem** avaliou, e não ",
      "por quem foi avaliado. Padronizando dentro de cada avaliador, a ",
      "contaminação vai a zero e o sinal entre consultores **sobe** de ",
      formatar_pct(objetos$bruta$sinal_entre_consultores), " para ",
      formatar_pct(objetos$normalizada$sinal_entre_consultores), "."
    ),
    paste0(
      "Fecha o candidato (a) de verdade, em vez de repetir a palavra ",
      "`normalizado` que o dicionário usa. A normalização remove variação ",
      "espúria e **ganha** sinal, então não há troca a ponderar. A forma ",
      "usada é padronização dentro do avaliador, que tira o nível e preserva ",
      "a ordem. **A medida deixou de depender de cenário simulado**: até a ",
      "consulta ao dono do dado de RH, `avaliador_id` constava do dicionário ",
      "como disponibilidade parcial e não existia no sintético, e o bloco ",
      "precisava sortear um avaliador por alocação para demonstrar o efeito. ",
      "A consulta confirmou que a avaliação é por projeto e tem gestor ",
      "identificado, o gerador passou a emitir o avaliador, e o número acima ",
      "é medido e não encenado."
    ),

    "II.5.6", "consequência do gerador",
    paste0(
      "A cobertura do rótulo é de ", formatar_pct(cobertura$cobertura_pares),
      " das alocações, e ", formatar_pct(variancia_escore$fracao_entre_consultores),
      " da variância do escore é entre consultores",
      if (is.null(estabilidade)) {
        "."
      } else {
        paste0(
          ", com ",
          formatar_intervalo(estabilidade, "fracao_variancia_entre_consultores"),
          " sobre as realizações",
          if (identical(posicao_variancia, "dentro")) {
            "."
          } else {
            paste0(
              ". O ponto versionado cai **", posicao_variancia,
              "** do intervalo, e é o intervalo que deve ser citado."
            )
          }
        )
      }
    ),
    paste0(
      "**Nenhum número deste bloco fala sobre a Peers, e agora isso é ",
      "definitivo.** A consulta ao dono do dado de RH negou o acesso até a ",
      "estatísticas agregadas sem dado individual, então distribuição, ",
      "cobertura e magnitude da estrutura de dependência são limitação ",
      "permanente e não pendência. O dicionário registrava esse acesso como ",
      "a alavanca de maior retorno do projeto; a alavanca está fechada. O que ",
      "a consulta trouxe foi **estrutura**, não número: que a avaliação é por ",
      "projeto, que tem gestor identificado e que a distribuição de nota ",
      "tende a variar por senioridade, e as três coisas o gerador passou a ",
      "reproduzir. O que transfere é a bateria de diagnósticos, pronta para ",
      "rodar no dia em que houver dado. E fica o acoplamento ",
      "que o dicionário nomeou: com a margem fora de `R`, o rótulo depende ",
      "inteiramente de desempenho, e sem esse acesso a alternativa é feedback ",
      "implícito (Hu, Koren e Volinsky, 2008), que o bloco II.4 mostrou ",
      "carregar viés de seleção não corrigível."
    ),

    "II.5.11", "consequência do gerador",
    paste0(
      "Cruzando as duas coberturas, só ",
      formatar_pct(cruzamento$fracao_fim_a_fim),
      " das alocações são avaliáveis de ponta a ponta pelo componente de ",
      "conteúdo, com o consultor tendo conteúdo declarado e o par tendo ",
      "rótulo, contra ", formatar_pct(cruzamento$fracao_fim_a_fim_independente),
      " que a independência entre as coberturas daria. Entre quem tem ",
      "histórico, a razão de chances entre ter conteúdo e ter rótulo é de ",
      formatar_decimal(cruzamento$razao_de_chances, 2), " (IC95 de ",
      formatar_decimal(cruzamento$ic_razao[1], 2), " a ",
      formatar_decimal(cruzamento$ic_razao[2], 2), ")."
    ),
    paste0(
      "**Não há terceiro viés empilhado no sintético, e isso é construção, ",
      "não achado**: o gerador sorteia as alocações avaliadas de forma ",
      "uniforme e os respondentes do formulário de forma independente. Ficam ",
      "duas coisas de valor. A primeira é o número: avaliado contra o ",
      "desempenho, o componente de conteúdo dispõe de pouco mais de um quarto ",
      "das alocações. A segunda é um aviso sobre o instrumento: o teste exato ",
      "de Fisher entre quem tem histórico é o que se aplicaria ao dado real, ",
      "mas nesta escala ele só detecta empilhamento forte, com razão de ",
      "chances fora do intervalo acima. **Não significativo, aqui, não quer ",
      "dizer independente.**"
    )
  ), tibble::tribble(
    ~codigo, ~decisao, ~limitacao, ~proveniencia,

    "II.5.1",
    paste0(
      "A etapa da avaliação é parte da definição do rótulo e fica declarada, ",
      "não é detalhe de implementação: a autoavaliação daria um rótulo em que ",
      "quase tudo é ACIMA, com pouco a discriminar."
    ),
    NA,
    "fato real",

    "II.5.2",
    paste0(
      "O rótulo escalar é autorizado: colapsar os catorze itens numa média não ",
      "destrói estrutura, porque a estrutura é comum."
    ),
    paste0(
      "A unidimensionalidade é imposta pelo gerador, que faz os itens ",
      "compartilharem uma qualidade latente, e não há medida real de ",
      "consistência interna do formulário para confrontar."
    ),
    "depende do gerador",

    "II.5.3",
    paste0(
      "O candidato (b) está morto: a margem não multiplica o desempenho na ",
      "célula de `R`, porque não é atribuível a uma alocação individual."
    ),
    NA,
    "fato real",

    "II.5.4",
    paste0(
      "O candidato (c) está descartado: não existe margem na granularidade ",
      "mensal, porque a fonte que parecia ter custo mensal é receita ",
      "decomposta por origem."
    ),
    NA,
    "fato real",

    "II.5.5",
    paste0(
      "A célula de `R` é o desempenho pelo candidato (a), normalizado por ",
      "avaliador e com a etapa declarada, e a margem sai de `R` e entra como ",
      "objetivo separado do otimizador pelo candidato (d)."
    ),
    NA,
    "fato real",

    "II.5.6", NA,
    paste0(
      "Distribuição, cobertura e magnitude da estrutura de dependência do ",
      "rótulo são limitação permanente e não pendência: a consulta ao dono do ",
      "dado de RH negou o acesso até a estatísticas agregadas sem dado ",
      "individual."
    ),
    "fato real",

    "II.5.7",
    paste0(
      "A normalização do rótulo é padronização dentro de cada avaliador, que ",
      "tira o nível e preserva a ordem: ela remove variação espúria e ganha ",
      "sinal, então não há troca a ponderar."
    ),
    paste0(
      "Que o avaliador contamine a nota é fato real, porque `avaliador_id` ",
      "existe e mais de um gestor assina por projeto, mas a magnitude da ",
      "contaminação é medida sobre o sintético."
    ),
    "fato real",

    "II.5.8", NA,
    paste0(
      "A margem estrutural tem mecanismo correto e escala não calibrada: a ",
      "mediana é plausível e a cauda não, e o resíduo é o da sobredeterminação ",
      "das âncoras, não um defeito à parte."
    ),
    "fato real",

    "II.5.9",
    paste0(
      "A fonte de custo é a tabela de salário base por cargo, e não o rate ",
      "card, que é preço cobrado do cliente; o fator de carga fica como ",
      "parâmetro declarado."
    ),
    NA,
    "fato real",

    "II.5.10", NA,
    paste0(
      "A margem entra no otimizador como direção e não como magnitude, porque ",
      "seis agregados reais descrevem a mesma economia e discordam por 1,45 ",
      "vezes, e escolher qual âncora relaxar exigiria um acesso que encerrou."
    ),
    "fato real",

    "II.5.11", NA,
    paste0(
      "Se no real quem não tem rótulo é desproporcionalmente quem não tem ",
      "conteúdo é inverificável, porque o acesso à base de avaliação foi ",
      "negado até em forma agregada (II.5.6). A fração avaliável de ponta a ",
      "ponta pode ser menor que a do sintético, e não há como saber daqui."
    ),
    "fato real"
  )))

  list(objetos = objetos, tabelas = tabelas, figuras = figuras, achados = achados)
}
