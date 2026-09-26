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

  achados <- registrar_achados("II.8", tibble::tribble(
    ~codigo, ~etiqueta, ~enunciado, ~implicacao,

    "II.8.1", "instrumento",
    paste0(
      "Expressar a restrição de capacidade custa porte. A formulação por par ",
      "de consultor e projeto tem ", formatar_inteiro(sem_tempo$variaveis),
      " variáveis e **não** consegue expressá-la, porque capacidade é por ",
      "mês. Acrescentar o mês leva a ",
      formatar_inteiro(com_tempo$variaveis), " variáveis e ",
      formatar_inteiro(com_tempo$restricoes_capacidade), " restrições. ",
      "Restringindo ao conjunto candidato do filtro de conteúdo, caem para ",
      formatar_inteiro(restrito$variaveis), ", uma redução de ",
      formatar_decimal(reducao, 1), " vezes."
    ),
    paste0(
      "**O filtro do bloco II.6 é o que torna o problema tratável**, e isso ",
      "amarra os dois blocos por um caminho que não era o esperado: lá ele ",
      "foi escolhido por filtrar e não ordenar, e aqui o mesmo corte reduz o ",
      "espaço de decisão na proporção do número de categorias. Com ",
      "ε-constraint, que resolve um inteiro a cada ponto da fronteira, a ",
      "diferença entre um milhão e setenta mil binárias decide se a fronteira ",
      "é calculável. O custo do corte está no achado II.6.5: ele exclui ",
      "candidatos que na prática seriam alocados."
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
      "é o que impede uma contagem otimista. Margem e qualidade da alocação ",
      "são calculáveis e não dimensionáveis, cada uma pelo seu motivo já ",
      "registrado: a margem porque as âncoras se sobredeterminam (II.5.10), a ",
      "qualidade porque o acesso à base de avaliação está negado (II.5.6). ",
      "**A ausência mais grave é a demanda futura**: `pipeline_demandas` traz ",
      "identificador e estágio, sem data, tamanho nem perfil. Sem ela o ",
      "otimizador não aloca para frente, só reavalia o passado."
    ),

    "II.8.4", "instrumento",
    paste0(
      "A wave 4 é viável como **exercício retrospectivo e multiobjetivo ",
      "parcial**, e não como alocador prospectivo. Ela pode rodar sobre os ",
      "projetos históricos, com capacidade e férias como restrições, custo ",
      "como objetivo dimensionado, e o conjunto candidato restrito pelo filtro ",
      "de conteúdo."
    ),
    paste0(
      "É a decisão que a contagem de insumos permite, e ela é menor do que a ",
      "ambição das pré-definições. O que fica de fora precisa ficar escrito: ",
      "sem demanda futura não há alocação prospectiva; sem margem dimensionada ",
      "o objetivo econômico entra como direção e não como magnitude; sem ",
      "pirâmide não há restrição de composição; e sem skills requeridas a ",
      "aderência é a do filtro grosso de categoria. **Nada disso invalida a ",
      "monografia**, porque o objeto dela é o método e o diagnóstico, não um ",
      "alocador em produção. Mas a diferença entre as duas coisas é ",
      "exatamente o que a Parte III precisa deixar explícito."
    )
  ))

  list(objetos = objetos, tabelas = tabelas, figuras = figuras, achados = achados)
}
