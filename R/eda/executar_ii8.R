#' Executa o bloco II.8 da EDA, insumos e tratabilidade do otimizador
#'
#' **Propriedade medida.** O porte do problema sob cada formulação, o quanto o
#' histórico viola a restrição que a wave 4 pretende impor, e quais insumos
#' existem com escala utilizável.
#'
#' **Decisão que discrimina.** O que a wave 4 pode modelar e o que ela pode
#' prometer.
#'
#' **É o último bloco da Parte II, e o mais ingrato.** Os anteriores
#' eliminaram famílias de modelo; este conta o que sobrou de insumo. A
#' contagem é desconfortável, e o valor dela está em ser feita antes de a
#' wave 4 começar, e não depois de o otimizador já estar escrito.
#'
#' @param dados Lista nomeada de tibbles, de `carregar_dados()`.
#' @param estabilidade tibble de `resumir_estabilidade()`, opcional.
#' @param margem_calibrada Se a escala da margem fecha com a âncora real.
#' @return Lista com `objetos`, `tabelas`, `figuras` e `achados`.
executar_ii8 <- function(dados, estabilidade = NULL, margem_calibrada = FALSE) {
  meses_ativos <- dplyr::n_distinct(dados$ocupacao_consultor$ano_mes)
  filtro <- medir_reducao_pelo_filtro(
    dados$consultores, dados$projetos, dados$experiencia_setorial
  )
  porte <- medir_porte_do_problema(
    dados$consultores, dados$projetos, dados$alocacoes, meses_ativos,
    fracao_candidatos = filtro$fracao_candidatos
  )
  capacidade <- medir_violacao_de_capacidade(dados$ocupacao_consultor)

  insumos <- registrar_insumos_do_otimizador(
    margem_calibrada = margem_calibrada,
    capacidade_respeitada = capacidade$fracao_meses_acima == 0
  )

  sem_tempo <- porte[porte$formulacao == "Par consultor e projeto", ]
  com_tempo <- porte[porte$formulacao == "Trio consultor, projeto e mês", ]
  restrito <- porte[porte$formulacao == "Trio, restrito ao conjunto candidato", ]
  reducao <- com_tempo$variaveis / restrito$variaveis

  existem <- sum(insumos$existe)
  dimensionaveis <- sum(insumos$existe & insumos$dimensionavel)

  objetos <- list(
    porte = porte,
    filtro = filtro,
    capacidade = capacidade,
    insumos = insumos,
    sem_tempo = sem_tempo,
    com_tempo = com_tempo,
    restrito = restrito,
    reducao = reducao,
    existem = existem,
    dimensionaveis = dimensionaveis,
    n_insumos = nrow(insumos),
    meses_ativos = meses_ativos
  )

  tabelas <- list(
    ii8_porte = porte |>
      dplyr::transmute(
        `Formulação` = formulacao,
        `Variáveis de decisão` = formatar_inteiro(variaveis),
        `Restrições de capacidade` = formatar_inteiro(restricoes_capacidade),
        `Expressa capacidade` = ifelse(expressa_capacidade, "sim", "não")
      ),
    ii8_insumos = insumos |>
      dplyr::transmute(
        Insumo = insumo, Papel = papel,
        Existe = ifelse(existe, "sim", "não"),
        `Escala utilizável` = ifelse(dimensionavel, "sim", "não"),
        `Observação` = observacao
      ),
    ii8_capacidade = tibble::tibble(
      Medida = c(
        "Meses de consultor acima do limite",
        "Consultores com ao menos uma violação",
        "Excesso mediano onde há violação",
        "Ocupação máxima observada"
      ),
      Valor = c(
        formatar_pct(capacidade$fracao_meses_acima),
        formatar_pct(capacidade$consultores_com_violacao),
        formatar_pct(capacidade$excesso_mediano),
        formatar_decimal(capacidade$ocupacao_maxima, 2)
      )
    )
  )

  figuras <- list(
    ii8_insumos = figura_insumos_do_otimizador(
      insumos,
      titulo = "Insumos do otimizador, por existência e escala",
      subtitulo = "Existir e ter escala utilizável são exigências diferentes, e poucas peças atendem às duas"
    )
  )

  achados <- registrar_achados("II.8", com_sintese(tibble::tribble(
    ~codigo, ~etiqueta, ~enunciado, ~implicacao,

    "II.8.1", "instrumento",
    paste0(
      "Expressar a restrição de capacidade custa porte. A formulação por par ",
      "de consultor e projeto tem ", formatar_inteiro(sem_tempo$variaveis),
      " variáveis e **não** consegue expressá-la, porque capacidade é por ",
      "mês. Acrescentar o mês leva a ",
      formatar_inteiro(com_tempo$variaveis), " variáveis e ",
      formatar_inteiro(com_tempo$restricoes_capacidade), " restrições. ",
      "O filtro de setor, aplicado só a quem o declarou como o híbrido ",
      "chaveado manda, deixa passar ", formatar_pct(filtro$fracao_candidatos),
      " dos pares e reduz o problema para ",
      formatar_inteiro(restrito$variaveis), " variáveis, uma redução de ",
      formatar_decimal(reducao, 3), " vezes",
      if (is.null(estabilidade)) {
        "."
      } else {
        paste0(
          ", ou ",
          formatar_intervalo(
            estabilidade, "reducao_porte_filtro", \(x) formatar_decimal(x, 3)
          ),
          " sobre as realizações",
          descrever_ponto_fora(posicao_no_intervalo(
            estabilidade, "reducao_porte_filtro", reducao
          )),
          "."
        )
      }
    ),
    paste0(
      "**O filtro não torna o problema tratável.** Entre quem declarou setor ",
      "ele corta ", formatar_decimal(1 / filtro$fracao_casamento, 1),
      " vezes, mas quem não declarou, ",
      formatar_pct(filtro$consultores_sem_setor), " do quadro, não é ",
      "filtrado, e é essa metade que segura o porte. Uma versão anterior deste ",
      "bloco media redução de ",
      formatar_decimal(1 / filtro$fracao_excluindo_sem_setor, 1),
      " vezes porque deixava essas pessoas fora do conjunto candidato: o ",
      "otimizador nunca alocaria quem não respondeu o formulário, o que ",
      "contradizia o híbrido chaveado do bloco II.3. Com ε-constraint, que ",
      "resolve um inteiro a cada ponto da fronteira, a tratabilidade volta a ",
      "ser pergunta aberta da wave 4, e a resposta terá de vir de outro lugar ",
      "que não o filtro. Onde o corte existe, o custo dele é o do achado ",
      "II.6.5: exclui candidatos que na prática seriam alocados."
    ),

    "II.8.2", "instrumento",
    paste0(
      "O histórico viola a restrição que a wave 4 pretende impor, e não por ",
      "pouco: ", formatar_pct(capacidade$fracao_meses_acima),
      " dos meses de consultor passam de cem por cento de ocupação, ",
      formatar_pct(capacidade$consultores_com_violacao),
      " dos consultores têm ao menos um mês assim, o excesso mediano onde há ",
      "violação é de ", formatar_pct(capacidade$excesso_mediano),
      " e o máximo observado chega a ",
      formatar_decimal(capacidade$ocupacao_maxima, 1), " vezes a capacidade."
    ),
    paste0(
      "Um modelo que imponha capacidade como **limite duro declara infactível ",
      "boa parte do que de fato aconteceu**, o que é um resultado ruim de se ",
      "descobrir com o otimizador já escrito. Três saídas, e a escolha fica ",
      "para a wave 4: capacidade como penalidade em vez de limite, limite ",
      "acima de cem por cento calibrado pelo próprio histórico, ou aceitar a ",
      "infactibilidade como diagnóstico de sobrealocação. A terceira é a mais ",
      "interessante para a monografia e a menos útil para a empresa. Vale a ",
      "ressalva de que uma ocupação de ",
      formatar_decimal(capacidade$ocupacao_maxima, 1),
      " vezes é implausível como fato e sugere que a própria medida de ",
      "capacidade mereça revisão antes de virar restrição."
    ),

    "II.8.3", "instrumento",
    paste0(
      "Dos ", nrow(insumos), " insumos que o otimizador pediria, ", existem,
      " existem e apenas ", dimensionaveis,
      " têm escala utilizável: custo por hora e férias. Três não existem em ",
      "fonte nenhuma: skills requeridas, pirâmide de senioridade e demanda ",
      "futura."
    ),
    paste0(
      "Existir e ter escala utilizável são exigências diferentes, e separá-las ",
      "é o que impede uma contagem otimista. Margem e afinidade prevista são ",
      "calculáveis e não dimensionáveis, cada uma pelo seu motivo já ",
      "registrado: a margem porque as âncoras se sobredeterminam (II.5.10), a ",
      "afinidade porque o acesso à base de avaliação está negado (II.5.6). ",
      "A afinidade é `ŝ`, a predição que a wave 3 produz para cada par ",
      "candidato, e não o rótulo de `R`, que só existe para pares que ",
      "aconteceram e serve para treinar e avaliar `ŝ`. ",
      "**A ausência mais grave é a demanda futura**: `pipeline_demandas` traz ",
      "identificador e estágio, sem data, tamanho nem perfil. Sem ela o ",
      "otimizador não aloca para frente, só reavalia o passado."
    ),

    "II.8.4", "instrumento",
    paste0(
      "A wave 4 é viável como **exercício retrospectivo e multiobjetivo ",
      "parcial**, e não como alocador prospectivo. Ela pode rodar sobre os ",
      "projetos históricos, com capacidade e férias como restrições, custo ",
      "como objetivo dimensionado, a afinidade `ŝ` da wave 3 como coeficiente ",
      "do objetivo de desempenho, e o filtro de setor aplicado a quem o ",
      "declarou."
    ),
    paste0(
      "É a decisão que a contagem de insumos permite, e ela é menor do que a ",
      "ambição das pré-definições. A ponte entre as duas waves é `ŝ`, como as ",
      "pré-definições previam, e o achado III.4 registra o que a EDA deixa em ",
      "aberto sobre ela. O que fica de fora precisa ficar escrito: ",
      "sem demanda futura não há alocação prospectiva; sem margem dimensionada ",
      "o objetivo econômico entra como direção e não como magnitude; sem ",
      "pirâmide não há restrição de composição; e sem skills requeridas a ",
      "aderência é a do filtro grosso de categoria. **Nada disso invalida a ",
      "monografia**, porque o objeto dela é o método e o diagnóstico, não um ",
      "alocador em produção. Mas a diferença entre as duas coisas é ",
      "exatamente o que a Parte III precisa deixar explícito."
    )
  ), tibble::tribble(
    ~codigo, ~decisao, ~limitacao, ~proveniencia,

    "II.8.1",
    paste0(
      "A formulação do otimizador é o trio de consultor, projeto e mês, porque ",
      "o par não expressa capacidade, com o filtro de setor aplicado só a quem ",
      "o declarou."
    ),
    paste0(
      "Com o filtro coerente com o chaveado, o problema segue na casa das ",
      "centenas de milhares de binárias, e nada aqui mede se o ε-constraint o ",
      "resolve no tempo disponível: a tratabilidade é a primeira pergunta da ",
      "wave 4."
    ),
    "estrutural",

    "II.8.2", NA,
    paste0(
      "A capacidade não pode entrar como limite duro sem calibração, sob pena ",
      "de declarar infactível boa parte do que aconteceu, e a própria medida de ",
      "ocupação merece revisão antes de virar restrição: a magnitude da ",
      "violação é do gerador e o máximo observado é implausível como fato."
    ),
    "depende do gerador",

    "II.8.3", NA,
    paste0(
      "Dos oito insumos que o otimizador pediria, só custo por hora e férias ",
      "têm escala utilizável, e três não existem em fonte nenhuma: skills ",
      "requeridas, pirâmide de senioridade e demanda futura."
    ),
    "fato real",

    "II.8.4",
    paste0(
      "A wave 4 é exercício retrospectivo e multiobjetivo parcial, e não ",
      "alocador prospectivo: roda sobre os projetos históricos, com capacidade ",
      "e férias como restrições, custo como objetivo dimensionado, `ŝ` como ",
      "coeficiente de afinidade e o filtro de setor aplicado a quem o declarou."
    ),
    NA,
    "fato real"
  )))

  list(objetos = objetos, tabelas = tabelas, figuras = figuras, achados = achados)
}
