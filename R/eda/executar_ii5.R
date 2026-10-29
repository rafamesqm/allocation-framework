#' Executa o bloco II.5 da EDA, construção do sinal de interação
#'
#' **Âncora na literatura.** Hu, Koren e Volinsky (2008) formalizam o
#' colaborativo sobre feedback implícito: a ocorrência da interação é uma
#' preferência binária, e a intensidade dela, uma confiança.
#'
#' **Propriedade medida.** A forma do sinal de interação que a célula de `R`
#' carrega, e se a margem pode entrar nela ou só no vetor de objetivos `f`.
#'
#' **Decisão que discrimina.** O que é a célula de `R`, e por qual forma a
#' margem entra no otimizador.
#'
#' @param dados Lista nomeada de tibbles, de `carregar_dados()`.
#' @param estabilidade tibble de `resumir_estabilidade()`, opcional. O bloco
#'   não cita grandeza das réplicas, e o argumento fica pela uniformidade da
#'   assinatura dos blocos.
#' @return Lista com `objetos`, `tabelas`, `figuras` e `achados`.
executar_ii5 <- function(dados, estabilidade = NULL) {
  # O sinal é o que a wave 3 vai consumir, então é medido sobre o histórico
  # observado até a data de referência, com as horas censuradas nela. A
  # margem, abaixo, continua sobre o conjunto inteiro: ela confronta as horas
  # do projeto todo com o contrato do projeto todo.
  historico <- aplicar_janela_de_analise(dados)$alocacoes_janela
  sinal_binario <- construir_sinal_implicito(historico, alfa = 0)
  sinal_confianca <- construir_sinal_implicito(historico, alfa = 1)
  horas_quantis <- stats::quantile(sinal_confianca$horas, c(0.1, 0.5, 0.9), names = FALSE)
  confianca_quantis <- stats::quantile(sinal_confianca$confianca, c(0, 0.5, 1), names = FALSE)

  margem <- medir_atribuibilidade_da_margem(dados)
  escala_margem <- diagnosticar_escala_da_margem(dados)
  sobredeterminacao <- medir_sobredeterminacao_economica()
  horas_da_margem <- medir_horas_contra_contrato(dados, sobredeterminacao)

  formas <- avaliar_formas_da_margem(
    margem_atribuivel = margem$tem_consultor_na_margem,
    fracao_projetos_com_equipe = margem$fracao_projetos_com_equipe,
    tem_custo_mensal = margem$tem_custo_mensal,
    custo_zero_explicavel_estrutural = margem$custo_zero_estrutural_sem_alocacao,
    desvio_da_mediana_alvo = margem$desvio_da_mediana_alvo,
    desvio_do_prejuizo_alvo = margem$desvio_do_prejuizo_alvo
  )
  formas_viaveis <- dplyr::filter(formas, viavel)

  objetos <- list(
    sinal_binario = sinal_binario,
    sinal_confianca = sinal_confianca,
    horas_quantis = horas_quantis,
    confianca_quantis = confianca_quantis,
    margem = margem,
    escala_margem = escala_margem,
    sobredeterminacao = sobredeterminacao,
    horas_da_margem = horas_da_margem,
    formas = formas,
    formas_viaveis = formas_viaveis,
    estabilidade = estabilidade
  )

  tabelas <- list(
    ii5_sinal_implicito = tibble::tibble(
      Medida = c(
        "Pares observados", "Horas do par, P10", "Horas do par, mediana",
        "Horas do par, P90", "Confiança com alfa 1, mínimo",
        "Confiança com alfa 1, mediana", "Confiança com alfa 1, máximo"
      ),
      Valor = c(
        formatar_inteiro(nrow(sinal_confianca)),
        formatar_decimal(horas_quantis, 1),
        formatar_decimal(confianca_quantis, 2)
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
    ii5_formas_margem = formas |>
      dplyr::transmute(
        Forma = forma, `Construção` = descricao, Exige = exige,
        `Mecanismo` = ifelse(viavel, "sim", "não"),
        `Escala calibrada` = ifelse(calibrado, "sim", "não"),
        Justificativa = justificativa
      )
  )

  achados <- registrar_achados("II.5", com_sintese(tibble::tribble(
    ~codigo, ~etiqueta, ~enunciado, ~implicacao,

    "II.5.1", "instrumento",
    paste0(
      "O sinal de interação é implícito. Cada um dos ",
      formatar_inteiro(nrow(sinal_binario)), " pares observados vira uma ",
      "preferência binária, e as horas do consultor no projeto, uma ",
      "confiança de 1 mais alfa vezes o logaritmo de 1 mais as horas. As ",
      "horas do par variam de ", formatar_decimal(horas_quantis[1], 0),
      " no décimo percentil a ", formatar_decimal(horas_quantis[3], 0),
      " no nonagésimo, com mediana de ", formatar_decimal(horas_quantis[2], 0),
      ", e com alfa igual a 1 a confiança vai de ",
      formatar_decimal(confianca_quantis[1], 2), " a ",
      formatar_decimal(confianca_quantis[3], 2), "."
    ),
    paste0(
      "**A decisão da EDA é só a ocorrência binária.** A confiança por horas ",
      "não é decisão: fica como comparação pré-registrada, alfa igual a 0 ",
      "contra alfa igual a 1, decidida na wave 3 pelo mesmo teste pareado de ",
      "McNemar a 5% do protocolo do bloco II.7, e se alfa igual a 1 não ",
      "superar o binário fica o binário. A confiança depende do gerador, ",
      "porque as horas do par dependem da duração das alocações, que foi ",
      "recalibrada. O logaritmo existe porque as horas variam em ordem de ",
      "grandeza entre pares, e sem ele os pares longos dominariam a ",
      "confiança de todos os outros."
    ),

    "II.5.2", "âncora real",
    paste0(
      "A margem não é atribuível a uma alocação individual. A fonte real de ",
      "receita e custo tem grão de projeto e mês, sem `consultor_id`, e ",
      formatar_pct(margem$fracao_projetos_com_equipe),
      " dos projetos alocados têm mais de uma pessoa, com mediana de ",
      margem$mediana_equipe, " e máximo de ", margem$maior_equipe, "."
    ),
    paste0(
      "**A margem fica fora da célula de `R`.** Ratear a margem de um ",
      "projeto entre os membros do time seria estimativa apresentada como ",
      "captura, no mesmo espírito da decisão já registrada sobre ",
      "`skills_requeridas[]`."
    ),

    "II.5.3", "âncora real",
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
      "**Descarta a margem mensal como objetivo do otimizador.** Uma versão ",
      "anterior deste bloco lia os 44,7% de zeros da receita de estagiários ",
      "como incompletude de apontamento de custo. A segunda consulta ",
      "comercial mostrou que a fonte é receita e que o zero é ausência de ",
      "estagiário no mês: não há custo mensal, e a despesa nessa ",
      "granularidade vive em outra fonte, de cobertura parcial e também ",
      "truncada, que o sintético não reproduz. O custo zero da margem ",
      "estrutural segue explicável, porque projeto sem alocação não é célula ",
      "de `R`."
    ),

    "II.5.4", "instrumento",
    paste0(
      "Conferida a plausibilidade dos **valores**, e não só a estrutura dos ",
      "zeros, a margem estrutural não fecha: ",
      formatar_pct(margem$fracao_margem_estrutural_negativa),
      " dos projetos com contrato positivo saem com margem negativa, e ",
      # O sinal do quinto percentil decide a frase. Com 6% de projetos no
      # prejuízo ele era negativo; com menos de 5% ele é margem positiva, e
      # um abs() aqui a descrevia como prejuízo.
      if (margem$margem_estrutural_relativa_p05 < 0) {
        paste0(
          "no quinto percentil o prejuízo chega a ",
          formatar_decimal(abs(margem$margem_estrutural_relativa_p05), 1),
          " vezes o valor do contrato. "
        )
      } else {
        paste0(
          "mesmo no quinto percentil a margem é positiva, de ",
          formatar_pct(margem$margem_estrutural_relativa_p05), " do contrato. "
        )
      },
      "Contra os alvos reais, a mediana, ",
      formatar_pct(margem$margem_estrutural_relativa_mediana), ", fica ",
      formatar_decimal(100 * margem$desvio_da_mediana_alvo, 1),
      " pontos acima da real, de ", formatar_pct(margem$alvo_mediana),
      ", e a fração de prejuízo fica a ",
      formatar_decimal(100 * margem$desvio_do_prejuizo_alvo, 1),
      " pontos da real, de ", formatar_pct(margem$alvo_prejuizo), ". ",
      # O veredito sai da comparação e não do texto. Uma versão anterior dizia
      # que a mediana era plausível e a cauda não, o que valia antes das
      # correções de escala e passou a contradizer os próprios alvos.
      if (abs(margem$desvio_da_mediana_alvo) > abs(margem$desvio_do_prejuizo_alvo)) {
        "O desacordo está no centro da distribuição, e não na cauda."
      } else {
        "O desacordo está na cauda, e não no centro da distribuição."
      }
    ),
    paste0(
      "**É o primeiro caso no capítulo de grandezas individualmente ",
      "calibradas que não fecham em conjunto**, e o achado II.5.6 mede o ",
      "tamanho exato do desacordo. Duas hipóteses foram levantadas aqui e ",
      "fechadas por consulta: a de granularidade do contrato, descartada ",
      "porque o valor é por projeto, e a de preço usado como custo, ",
      "confirmada e corrigida. Depois das duas correções a fração de ",
      "prejuízo caiu para perto da real e a mediana continua alta, porque o ",
      "projeto sintético acumula ",
      formatar_pct(horas_da_margem$fracao_das_horas_implicadas),
      " das horas que o contrato implica. A sobredeterminação do achado ",
      "II.5.6 responde por um fator de ",
      formatar_decimal(sobredeterminacao$desacordo, 2),
      " desse desacordo; o resto vem de uma sétima âncora, a duração da ",
      "alocação, que o achado II.5.6 detalha."
    ),

    "II.5.5", "âncora real",
    paste0(
      "A taxa que o gerador usava como custo era preço, e a segunda consulta ",
      "comercial confirmou com prova direta: a receita mensal do projeto é ",
      "0,87 vezes horas planejadas vezes a taxa, e a própria tabela deriva a ",
      "taxa do estagiário das tarifas cobradas do cliente. A fonte de custo ",
      "passou a ser a tabela de custo por cargo, que é **salário base por ",
      "hora, sem encargos**, e a razão entre custo e contrato deixou de ",
      "centrar em 1: a média geométrica dela agora fica em ",
      formatar_decimal(escala_margem$centro_da_razao, 2),
      ", que é o centro adequado a uma razão e não deve ser confrontado com a ",
      "mediana da margem do achado II.5.4."
    ),
    paste0(
      "Fecha a hipótese que este bloco levantou. O sintoma era a razão ",
      "centrar em 1, que é o que se espera de duas medidas da mesma grandeza ",
      "e não de custo contra receita. `diagnosticar_escala_da_margem()` ",
      "continua rodando como **guarda de regressão**: se alguém repuser o ",
      "rate card como custo, a razão volta a centrar em 1 e o teste acusa. ",
      "Fica a consequência que a confirmação trouxe: como o custo é salário ",
      "base, o fator de carga é parâmetro declarado, e o achado II.5.6 ",
      "mostra que as âncoras não comportam um fator realista."
    ),

    "II.5.6", "âncora real",
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
      "e as seis fecham entre si sem mexer em mais nada. Não fecha o resíduo ",
      "inteiro, porque **a duração da alocação é uma ",
      "sétima âncora, e ela aumenta o desacordo.** A relação acima supõe cada ",
      "consultor presente pela duração do projeto, de ",
      formatar_decimal(sobredeterminacao$ancoras$medido[3], 1), " semanas, ",
      "mas a alocação real dura ",
      formatar_inteiro(horas_da_margem$duracao_alocacao_dias),
      " dias medianos. Com uma alocação por par entre consultor e projeto, ",
      "como o gerador supõe, o desacordo sobe para ",
      formatar_decimal(horas_da_margem$desacordo_com_alocacao, 1),
      " vezes. Ele fecharia se cada pessoa somasse várias alocações curtas no ",
      "mesmo projeto, o que a fonte não informa. É a explicação do resíduo ",
      "que o achado II.5.4 mede: o projeto sintético acumula ",
      formatar_pct(horas_da_margem$fracao_das_horas_implicadas),
      " das horas que o contrato implica."
    ),

    "II.5.7", "instrumento",
    paste0(
      "Das três formas de margem do dicionário, ",
      nrow(formas_viaveis),
      if (nrow(formas_viaveis) == 1) " tem" else " têm",
      " mecanismo que se sustenta: ",
      paste(tolower(formas_viaveis$forma), collapse = " e "),
      ". A escala dela não está calibrada no sintético, pelo motivo do ",
      "achado II.5.4."
    ),
    paste0(
      "**A decisão habilitada é o sinal implícito do achado II.5.1 para `R` ",
      "e a margem estrutural para `f`**: a célula de `R` é a ocorrência ",
      "binária da alocação, e a margem ",
      "entra como objetivo separado no otimizador, calculada de custo ",
      "carregado por horas contra valor de contrato, como direção e não como ",
      "magnitude (II.5.6)."
    )
  ), tibble::tribble(
    ~codigo, ~decisao, ~limitacao, ~proveniencia,

    "II.5.1",
    paste0(
      "A célula de `R` é a ocorrência binária da alocação."
    ),
    paste0(
      "O sinal implícito mede a ocorrência da alocação, e por isso a prática ",
      "atual de escolha, com o viés de seleção que o bloco II.4 mostrou não ",
      "ser corrigível nesta base. A confiança por horas, e a comparação entre ",
      "alfa igual a 0 e alfa igual a 1, ficam como comparação pré-registrada ",
      "a decidir na wave 3, fora da contagem de decisões: a confiança depende ",
      "do gerador, porque as horas dependem da duração recalibrada das ",
      "alocações."
    ),
    "estrutural",

    "II.5.2",
    paste0(
      "A margem não entra na célula de `R`, porque não é atribuível a uma ",
      "alocação individual."
    ),
    NA,
    "fato real",

    "II.5.3",
    paste0(
      "A margem mensal está descartada como objetivo do otimizador: a fonte ",
      "que parecia ter custo mensal é receita decomposta por origem."
    ),
    NA,
    "fato real",

    "II.5.4", NA,
    paste0(
      "A margem estrutural tem mecanismo correto e escala não calibrada: a ",
      "fração de prejuízo fica perto da real e a mediana fica muito acima ",
      "dela. O resíduo é o das âncoras, que discordam entre si, e cresce com ",
      "a duração real da alocação, porque o gerador supõe uma alocação por ",
      "par entre consultor e projeto e a fonte não diz quantas há."
    ),
    "fato real",

    "II.5.5",
    paste0(
      "A fonte de custo é a tabela de salário base por cargo, e não o rate ",
      "card, que é preço cobrado do cliente; o fator de carga fica como ",
      "parâmetro declarado."
    ),
    NA,
    "fato real",

    "II.5.6", NA,
    paste0(
      "A margem entra no otimizador como direção e não como magnitude, porque ",
      "seis agregados reais descrevem a mesma economia e discordam por ",
      formatar_decimal(sobredeterminacao$desacordo, 2), " vezes, ou por ",
      formatar_decimal(horas_da_margem$desacordo_com_alocacao, 1),
      " com a duração real da alocação, e escolher qual âncora relaxar ",
      "exigiria um acesso que encerrou."
    ),
    "fato real",

    "II.5.7",
    paste0(
      "A margem entra como objetivo separado do otimizador pela forma ",
      "estrutural, de custo carregado por horas contra valor de contrato."
    ),
    NA,
    "fato real"
  )))

  list(objetos = objetos, tabelas = tabelas, figuras = list(), achados = achados)
}
