# Margem do projeto: atribuibilidade, formas possíveis como objetivo do
# otimizador, escala da margem estrutural e a sobredeterminação das âncoras
# econômicas. Alimenta o bloco II.5.

#' Mede se a margem pode ser atribuída a uma alocação individual
#'
#' Separa as três formas de margem de `avaliar_formas_da_margem()`. A fonte
#' real de receita e custo tem grão de projeto e mês, sem `consultor_id`, e
#' atribuir margem a uma pessoa dentro de um time exigiria regra de rateio,
#' que seria estimativa e não captura.
#'
#' A margem mensal não existe: a fonte mensal é **receita** decomposta por
#' origem, e o zero nela é ausência de estagiário no mês, não custo zerado.
#' A despesa mensal vive em outra fonte, de cobertura parcial, que o
#' sintético não reproduz.
#'
#' Sobra para a margem estrutural a plausibilidade dos valores, que tem
#' régua real: a margem líquida mediana do projeto é 0,320 e cerca de 2% dos
#' projetos dão prejuízo (`referencias_margem_projeto()`).
#'
#' @param dados Lista nomeada de tibbles.
#' @param ancora tibble de `referencias_margem_projeto()`.
#' @return Lista com a atribuibilidade e a qualidade da margem estrutural.
medir_atribuibilidade_da_margem <- function(dados,
                                             ancora = referencias_margem_projeto()) {
  equipe <- dplyr::count(dados$alocacoes, projeto_id, name = "consultores")

  receita_mes <- dados$receita_projeto_mes
  estrutural <- dados$margem_estrutural
  sem_alocacao <- !estrutural$projeto_id %in% dados$alocacoes$projeto_id

  alvo <- function(nome) ancora$valor_real[ancora$estatistica == nome]

  # Plausibilidade dos valores, e não só dos zeros. A primeira versão deste
  # bloco declarou a margem estrutural viável olhando só a estrutura dos zeros, e
  # não conferiu se as margens resultantes faziam sentido. Não faziam.
  com_contrato <- estrutural$valor_contrato > 0
  margem_relativa_estrutural <- estrutural$margem_estrutural[com_contrato] /
    estrutural$valor_contrato[com_contrato]

  list(
    tem_consultor_na_margem = "consultor_id" %in% names(receita_mes),
    tem_custo_mensal = any(grepl("custo|despesa", names(receita_mes))),
    margem_estrutural_relativa_mediana = stats::median(margem_relativa_estrutural),
    fracao_margem_estrutural_negativa = mean(margem_relativa_estrutural < 0),
    margem_estrutural_relativa_p05 = unname(
      stats::quantile(margem_relativa_estrutural, 0.05)
    ),
    alvo_mediana = alvo("mediana"),
    alvo_prejuizo = alvo("fracao_prejuizo"),
    desvio_da_mediana_alvo = stats::median(margem_relativa_estrutural) - alvo("mediana"),
    desvio_do_prejuizo_alvo = mean(margem_relativa_estrutural < 0) - alvo("fracao_prejuizo"),
    equipe = equipe,
    mediana_equipe = stats::median(equipe$consultores),
    maior_equipe = max(equipe$consultores),
    fracao_projetos_com_equipe = mean(equipe$consultores > 1),
    custo_zero_estrutural = mean(estrutural$custo_total == 0),
    custo_zero_estrutural_sem_alocacao = all(
      (estrutural$custo_total == 0) == sem_alocacao
    ),
    # Identidade da fonte: a receita total é a soma das duas origens próprias.
    identidade_receita = max(abs(
      receita_mes$receita_total - receita_mes$receita_nf - receita_mes$receita_interns
    ))
  )
}

#' Avalia qual forma de margem se sustenta como objetivo do otimizador
#'
#' Mesma forma de `avaliar_formas_de_hibrido()` e
#' `avaliar_estimadores_de_propensao()`: veredito calculado a partir das
#' grandezas medidas, e não declarado, para não envelhecer em silêncio.
#'
#' As três formas são as de `docs/dicionario_de_dados.md`, seção
#' "Construção do sinal de interação": margem atribuída a cada alocação,
#' margem por projeto e mês, e margem estrutural de custo por horas contra
#' valor de contrato.
#'
#' Separa **dois eixos** que colapsar num veredito só confundiria. `viavel`
#' diz se o mecanismo existe e está correto: se a grandeza é construível a
#' partir do que a organização tem. `calibrado` diz se o sintético reproduz
#' a escala dela de forma crível. A margem estrutural é o caso que obriga a
#' distinção: calcular margem de custo por horas contra contrato é a forma
#' certa, e ainda assim a margem que sai do sintético é implausível, porque
#' grandezas individualmente calibradas não fecham em conjunto. Reprovar a
#' forma por isso diria que o mecanismo está errado, que não é o caso;
#' aprová-la em silêncio esconderia que a wave 4 não pode usar o nível dessa
#' margem para nada.
#'
#' @param margem_atribuivel Se a margem existe por alocação individual.
#' @param fracao_projetos_com_equipe Fração de projetos com mais de uma
#'   pessoa, que é onde o rateio seria necessário.
#' @param tem_custo_mensal Se existe custo na mesma granularidade da receita
#'   mensal, sem o qual não há margem mensal.
#' @param custo_zero_explicavel_estrutural Se o custo zero da margem
#'   estrutural coincide com projeto sem alocação.
#' @param desvio_da_mediana_alvo,desvio_do_prejuizo_alvo Distância da margem
#'   estrutural sintética aos dois alvos reais, a mediana e a fração de
#'   prejuízo, como fração do contrato. São o teste de escala da margem
#'   estrutural.
#' @param limite_desvio_margem Distância máxima, em fração do contrato, a que
#'   cada alvo real pode ficar para a margem estrutural ser tratada como
#'   calibrada. Convenção declarada. Olhar só a fração de prejuízo marcaria
#'   a margem como calibrada com a mediana a mais de cinquenta pontos da
#'   real: a fração de prejuízo pode bater com o centro errado.
#' @return tibble com `forma`, `descricao`, `exige`, `viavel`, `calibrado` e
#'   `justificativa`.
avaliar_formas_da_margem <- function(margem_atribuivel,
                                     fracao_projetos_com_equipe,
                                     tem_custo_mensal,
                                     custo_zero_explicavel_estrutural,
                                     desvio_da_mediana_alvo,
                                     desvio_do_prejuizo_alvo,
                                     limite_desvio_margem = 0.10) {
  margem_calibrada <- abs(desvio_da_mediana_alvo) <= limite_desvio_margem &&
    abs(desvio_do_prejuizo_alvo) <= limite_desvio_margem

  tibble::tribble(
    ~forma, ~descricao, ~exige, ~viavel, ~calibrado, ~justificativa,

    "Por alocação", "margem atribuída a cada par de consultor e projeto",
    "margem atribuível a uma alocação individual",
    margem_atribuivel, FALSE,
    paste0(
      "a margem não existe por alocação, e ratear a de projeto e mês entre os ",
      "membros do time seria necessário em ",
      formatar_pct(fracao_projetos_com_equipe),
      " dos projetos alocados; o rateio é estimativa, não captura"
    ),

    "Mensal", "margem como objetivo na granularidade de projeto e mês",
    "custo na mesma granularidade da receita, para haver margem mensal",
    tem_custo_mensal, FALSE,
    paste0(
      "a fonte mensal é receita decomposta por origem, não custo: a identidade ",
      "contábil fecha, e a despesa mensal existe em outra fonte, de cobertura ",
      "parcial e também truncada, que o sintético não reproduz"
    ),

    "Estrutural", "margem de custo carregado por horas contra valor de contrato",
    "custo e contrato disponíveis para todo projeto com alocação",
    custo_zero_explicavel_estrutural,
    margem_calibrada,
    paste0(
      "mecanismo correto: não depende de apontamento de custo mensal e só é ",
      "zero onde não há ninguém alocado. ",
      if (margem_calibrada) {
        "A escala fecha no sintético, com mediana e fração de prejuízo perto das reais"
      } else {
        paste0(
          "Mas a escala não fecha no sintético: a mediana fica a ",
          formatar_decimal(100 * desvio_da_mediana_alvo, 1),
          " pontos da real e a fração de prejuízo a ",
          formatar_decimal(100 * desvio_do_prejuizo_alvo, 1),
          ", então o nível não é utilizável"
        )
      }
    )
  )
}

#' Diagnostica por que a escala da margem estrutural não fecha
#'
#' O bloco II.5 encontrou margem estrutural implausível e levantou duas
#' hipóteses. A primeira, de que `valor_contrato` teria grão de alocação e
#' pessoa e não de projeto, **foi descartada**: o usuário confirmou que em
#' fonte de alocação do sistema de gestão de projetos, o valor é por projeto. A segunda é a que esta
#' função mede.
#'
#' **A hipótese medida: a taxa usada como custo é preço, não custo.** A fonte
#' de `custo_carregado` é a tabela de taxa por hora, que a Quinta rodada
#' registrou textualmente como *rate card*, isto é, tabela de preço por
#' cargo. Preço é o que o cliente paga; custo carregado é o que a pessoa
#' custa à empresa. Se a tabela for preço, calcular margem como contrato
#' menos taxa vezes horas subtrai o faturamento dele mesmo, e o resultado
#' fica em torno de zero por construção, com o sinal decidido pelo ruído.
#'
#' Dois sinais que a função mede, e que não dependem de acreditar na
#' hipótese:
#'
#' * **Onde a razão centra.** Se custo e contrato fossem custo e receita, a
#'   razão entre eles centraria bem abaixo de 1, na margem típica do setor.
#'   Centrar em 1 é o que se espera de duas medidas da mesma grandeza.
#' * **Quanto a taxa implica de custo mensal por pessoa.** Multiplicada pela
#'   jornada mensal calibrada, ela dá o custo que cada nível teria para a
#'   empresa. Valores muito acima do plausível para salário mais encargos
#'   indicam preço, não custo.
#'
#' A função **não decide**: ela mede e deixa o veredito para quem conhece a
#' semântica da coluna. O acesso ao ambiente está encerrado e a distinção
#' entre preço e custo não é derivável do que ficou registrado.
#'
#' @param dados Lista nomeada de tibbles.
#' @param jornada_mensal Horas por mês usadas para converter taxa em custo
#'   mensal. Default na mediana calibrada de `horas_mes`.
#' @return Lista com o centro e a dispersão da log-razão, a tabela de custo
#'   mensal implícito por nível, e `razao_centra_em_um`.
diagnosticar_escala_da_margem <- function(dados, jornada_mensal = 120) {
  comparaveis <- dados$margem_estrutural |>
    dplyr::filter(valor_contrato > 0, custo_total > 0)

  log_razao <- log(comparaveis$custo_total / comparaveis$valor_contrato)
  centro <- exp(mean(log_razao))

  custo_mensal <- dados$custo_carregado |>
    dplyr::left_join(
      dplyr::select(dados$consultores, consultor_id, nivel_senioridade),
      by = "consultor_id"
    ) |>
    dplyr::summarise(
      taxa_mediana = stats::median(custo_carregado),
      consultores = dplyr::n(),
      .by = nivel_senioridade
    ) |>
    dplyr::mutate(custo_mensal_implicito = taxa_mediana * jornada_mensal) |>
    dplyr::arrange(nivel_senioridade)

  list(
    centro_da_razao = centro,
    dispersao_log_razao = stats::sd(log_razao),
    # Centrar em 1 é o que se espera de duas medidas da mesma grandeza, e não
    # de custo contra receita. A faixa é convenção de leitura declarada.
    razao_centra_em_um = centro > 0.7 && centro < 1.4,
    custo_mensal = custo_mensal,
    custo_mensal_mais_junior = min(custo_mensal$custo_mensal_implicito),
    custo_mensal_mais_senior = max(custo_mensal$custo_mensal_implicito),
    jornada_mensal = jornada_mensal
  )
}

#' Mede o fator de carga máximo compatível com as âncoras reais
#'
#' `custo_carregado` é salário base vezes um fator de carga que não existe em
#' base nenhuma consultada. Esta função não estima o fator: ela mede o **teto**
#' que as âncoras reais impõem a ele, o que é uma pergunta respondível.
#'
#' A aritmética tem três âncoras, todas de `docs/mapeamento_schema.md`. A
#' receita mensal é cerca de `fator_receita` vezes horas vezes preço. O custo
#' base é uma fração conhecida do preço, por cargo. E a despesa total do
#' projeto é o complemento da margem líquida mediana. Se o custo de pessoal
#' sozinho passa da despesa total, o fator é impossível.
#'
#' **O resultado é desconfortável e é para ficar assim.** Encargos,
#' benefícios, décimo terceiro, férias e FGTS levariam o fator para 1,7 ou
#' 2,0 no Brasil, e o teto medido fica bem abaixo. Ou a margem que o projeto
#' declara é bruta sobre salário direto, ou alguma das três âncoras não
#' significa o que o nome diz. Não dá para decidir com o acesso encerrado, e
#' medir o teto é o que separa a constatação da especulação.
#'
#' Nota sobre a companheira `diagnosticar_escala_da_margem()`: ela nasceu
#' para detectar o sintoma de usar preço como custo, e agora que a fonte foi
#' trocada ela serve de guarda de regressão. O sintoma era a razão entre
#' custo e contrato centrar em 1; com custo de verdade ela centra bem abaixo.
#'
#' @param razao_custo_preco Razão mediana entre custo base e preço, por cargo.
#' @param fator_receita Receita sobre horas vezes preço.
#' @param margem_liquida_mediana Margem líquida mediana do projeto.
#' @return Lista com o teto do fator, o custo de pessoal sobre a receita com
#'   fator 1, e a sobra para despesa não pessoal.
medir_fator_de_carga_implicito <- function(razao_custo_preco = 0.554,
                                            fator_receita = 0.87,
                                            margem_liquida_mediana = 0.320) {
  despesa_sobre_receita <- 1 - margem_liquida_mediana
  pessoal_sobre_receita <- razao_custo_preco / fator_receita

  list(
    fator_maximo = despesa_sobre_receita / pessoal_sobre_receita,
    pessoal_sobre_receita = pessoal_sobre_receita,
    despesa_sobre_receita = despesa_sobre_receita,
    sobra_nao_pessoal = despesa_sobre_receita - pessoal_sobre_receita,
    razao_custo_preco = razao_custo_preco,
    fator_receita = fator_receita
  )
}

#' Mede a sobredeterminação das âncoras econômicas
#'
#' Seis agregados reais descrevem a mesma economia por caminhos diferentes, e
#' juntos ela fica **sobredeterminada**: cinco bastam para calcular o sexto, e
#' o valor calculado não bate com o medido.
#'
#' A relação que os liga é `receita = fator_receita * horas * preco`, com
#' `horas = equipe * jornada * meses`. Todos os seis têm âncora registrada em
#' `docs/mapeamento_schema.md`, e nenhum é suposição do gerador.
#'
#' **O que a função faz, e o que ela não faz.** Ela não escolhe qual âncora
#' relaxar, porque essa escolha não é estatística: depende de saber o que cada
#' número de fato conta, e o acesso ao ambiente está encerrado. O que ela faz é
#' tornar a inconsistência auditável, reportando para cada âncora o valor que
#' reconciliaria o sistema se as outras cinco estivessem certas. Isso transforma
#' "não fecha" em "não fecha por este fator, e é este o ajuste que cada uma
#' exigiria".
#'
#' A suspeita mais provável, e é só suspeita, é o tamanho de equipe: os 2,51
#' vêm de pares distintos entre colaborador e **código de alocação**, e um
#' projeto pode ter mais de um código. Se for isso, a equipe real por projeto é
#' maior e o resto fecha sem mexer em mais nada.
#'
#' @param equipe Consultores por projeto.
#' @param jornada Horas por mês por consultor.
#' @param semanas Duração do projeto, em semanas.
#' @param contrato Valor de contrato mediano.
#' @param preco Preço por hora mediano.
#' @param fator_receita Receita sobre horas vezes preço.
#' @return Lista com o desacordo e a tabela de reconciliação por âncora.
medir_sobredeterminacao_economica <- function(equipe = 2.51,
                                               jornada = 120,
                                               semanas = 11.6,
                                               contrato = 457000,
                                               preco = 450,
                                               fator_receita = 0.87) {
  meses <- semanas * 7 / (365.25 / 12)
  horas <- equipe * jornada * meses
  contrato_implicado <- fator_receita * horas * preco
  desacordo <- contrato / contrato_implicado

  # Para cada âncora, o valor que fecharia o sistema com as outras cinco
  # fixas. As cinco que entram multiplicando escalam pelo desacordo; a
  # receita é a que está do outro lado da igualdade.
  ancoras <- tibble::tibble(
    ancora = c(
      "Consultores por projeto", "Horas por mês", "Duração em semanas",
      "Preço por hora", "Fator de receita", "Valor de contrato"
    ),
    medido = c(equipe, jornada, semanas, preco, fator_receita, contrato),
    reconciliado = c(
      equipe * desacordo, jornada * desacordo, semanas * desacordo,
      preco * desacordo, fator_receita * desacordo, contrato_implicado
    )
  )
  ancoras$ajuste <- ancoras$reconciliado / ancoras$medido

  # Duas âncoras não podem absorver a diferença, e dizer isso é o que torna a
  # tabela decisória em vez de descritiva. O fator de receita não passa de 1
  # porque preço é a tarifa cobrada e a receita não excede horas vezes tarifa.
  # A jornada não passa de um mês cheio de trabalho. As duas ficariam acima do
  # limite, então a diferença está nas outras quatro.
  limites <- c(NA, 168, NA, NA, 1, NA)
  ancoras$absorve <- is.na(limites) | ancoras$reconciliado <= limites

  list(
    contrato_implicado = contrato_implicado,
    contrato_medido = contrato,
    desacordo = desacordo,
    horas_implicadas = horas,
    ancoras = ancoras,
    candidatas = ancoras$ancora[ancoras$absorve],
    descartadas = ancoras$ancora[!ancoras$absorve]
  )
}

#' Mede quantas das horas que o contrato implica o projeto sintético acumula
#'
#' É o número que explica a mediana alta da margem estrutural (achado
#' II.5.4). Antes da recalibração da duração das alocações ele aparecia no texto como "cerca de metade",
#' digitado, e deixou de ser verdade quando a duração da alocação foi
#' recalibrada sem que nada acusasse. Agora sai do dado.
#'
#' As horas que o contrato implica são o contrato mediano dividido por preço
#' e fator de receita, as mesmas âncoras de
#' `medir_sobredeterminacao_economica()`. O desacordo com alocação refaz a
#' conta daquela função trocando a duração do projeto pela da alocação real,
#' que é o que vale se cada consultor tem uma alocação por projeto, como o
#' gerador supõe.
#'
#' @param dados Lista nomeada de tibbles, de `carregar_dados()`.
#' @param sobredeterminacao Saída de `medir_sobredeterminacao_economica()`.
#' @param dias_por_mes Divisor que converte dias em meses.
#' @return Lista com as horas medianas por projeto alocado, as horas que o
#'   contrato implica, a fração entre as duas, a duração real da alocação e
#'   o desacordo das âncoras com ela.
medir_horas_contra_contrato <- function(dados, sobredeterminacao,
                                        dias_por_mes = 365.25 / 12) {
  ancora <- function(nome) {
    sobredeterminacao$ancoras$medido[sobredeterminacao$ancoras$ancora == nome]
  }
  referencias <- referencias_reais()
  duracao_alocacao_dias <- referencias$valor_real[
    referencias$tabela == "alocacoes" & referencias$variavel == "duracao_dias" &
      referencias$estatistica == "mediana"
  ]

  horas_por_projeto <- dados$alocacoes |>
    dplyr::mutate(horas = horas_mes * duracao_dias / dias_por_mes) |>
    dplyr::summarise(horas = sum(horas), .by = projeto_id)

  horas_implicadas <- ancora("Valor de contrato") /
    (ancora("Fator de receita") * ancora("Preço por hora"))
  horas_com_alocacao <- ancora("Consultores por projeto") *
    ancora("Horas por mês") * duracao_alocacao_dias / dias_por_mes

  list(
    horas_mediana_sintetica = stats::median(horas_por_projeto$horas),
    horas_implicadas_pelo_contrato = horas_implicadas,
    fracao_das_horas_implicadas = stats::median(horas_por_projeto$horas) / horas_implicadas,
    duracao_alocacao_dias = duracao_alocacao_dias,
    desacordo_com_alocacao = horas_implicadas / horas_com_alocacao
  )
}
