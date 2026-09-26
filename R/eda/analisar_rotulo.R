#' Converte a nota ordinal de três níveis em número
#'
#' Ponto único da conversão, porque tratar ordinal como número é escolha, não
#' evidência, e escolha espalhada por vários lugares diverge. A escala é
#' igualmente espaçada, que é a suposição mais simples e a que a literatura
#' de avaliação costuma adotar; ela não é neutra, porque supõe que a distância
#' de ABAIXO para DENTRO é igual à de DENTRO para ACIMA, e nada no dado
#' sustenta isso. Fica declarada aqui, em um lugar só.
#'
#' @param nota Vetor de texto com os três níveis.
#' @return Vetor numérico de 1 a 3.
nota_numerica <- function(nota) {
  escala <- c(ABAIXO = 1, DENTRO = 2, ACIMA = 3)
  unname(escala[nota])
}

#' Mede a cobertura do rótulo explícito sobre a matriz de interações
#'
#' O rótulo explícito é a avaliação de desempenho, e ele só existe onde houve
#' ciclo de avaliação. A cobertura decide quanto de `R` tem valor de célula
#' informativo e quanto teria de cair para feedback implícito.
#'
#' @param dados Lista nomeada de tibbles, de `carregar_dados()`.
#' @return Lista com contagens e frações dos três denominadores.
medir_cobertura_do_rotulo <- function(dados) {
  pares_avaliados <- dplyr::n_distinct(
    paste(dados$avaliacoes_desempenho$consultor_id,
          dados$avaliacoes_desempenho$projeto_id)
  )

  list(
    pares_avaliados = pares_avaliados,
    pares_totais = nrow(dados$alocacoes),
    cobertura_pares = pares_avaliados / nrow(dados$alocacoes),
    consultores_avaliados = dplyr::n_distinct(dados$avaliacoes_desempenho$consultor_id),
    cobertura_consultores = dplyr::n_distinct(dados$avaliacoes_desempenho$consultor_id) /
      nrow(dados$consultores),
    projetos_avaliados = dplyr::n_distinct(dados$avaliacoes_desempenho$projeto_id),
    cobertura_projetos = dplyr::n_distinct(dados$avaliacoes_desempenho$projeto_id) /
      nrow(dados$projetos)
  )
}

#' Mede a divergência entre as etapas de avaliação
#'
#' As três etapas avaliam a **mesma** pessoa na **mesma** alocação sobre os
#' **mesmos** itens, então a comparação entre elas é pareada e direta. Se
#' divergem muito, dizer "o rótulo é a avaliação de desempenho" é ambíguo, e
#' a etapa adotada precisa ser declarada como parte da definição do rótulo.
#'
#' Reporta nível médio e concordância separadamente porque são coisas
#' distintas: dois avaliadores podem concordar na ordem e discordar no nível,
#' que é exatamente o caso da leniência da autoavaliação, e é a ordem que
#' importa para um recomendador.
#'
#' @param avaliacoes tibble de avaliações, longa por item e etapa.
#' @return Lista com `por_etapa`, `pares` e a tabela larga usada.
medir_divergencia_entre_avaliadores <- function(avaliacoes) {
  largo <- avaliacoes |>
    dplyr::select(consultor_id, projeto_id, item, etapa, nota) |>
    tidyr::pivot_wider(names_from = etapa, values_from = nota)

  etapas <- setdiff(names(largo), c("consultor_id", "projeto_id", "item"))

  por_etapa <- avaliacoes |>
    dplyr::summarise(
      nivel_medio = mean(nota_numerica(nota)),
      fracao_acima = mean(nota == "ACIMA"),
      fracao_abaixo = mean(nota == "ABAIXO"),
      .by = etapa
    ) |>
    dplyr::arrange(dplyr::desc(nivel_medio))

  combinacoes <- utils::combn(etapas, 2, simplify = FALSE)
  pares <- purrr::map(combinacoes, function(par) {
    a <- largo[[par[1]]]
    b <- largo[[par[2]]]

    tibble::tibble(
      etapa_a = par[1],
      etapa_b = par[2],
      concordancia = mean(a == b),
      correlacao = stats::cor(nota_numerica(a), nota_numerica(b))
    )
  }) |>
    purrr::list_rbind() |>
    dplyr::arrange(correlacao)

  list(por_etapa = por_etapa, pares = pares, largo = largo)
}

#' Mede a dimensionalidade dos itens de uma etapa de avaliação
#'
#' A pergunta é se os 14 itens medem uma coisa ou várias. Se medem uma, o
#' rótulo escalar é a média deles e não perde nada. Se medem várias coisas
#' independentes, colapsar em escalar destrói informação, e o rótulo precisa
#' declarar qual dimensão adota.
#'
#' Três medidas da mesma propriedade, porque nenhuma sozinha convence. A
#' variância do primeiro componente diz quanto de estrutura comum existe. A
#' correlação média entre itens diz o mesmo sem depender de decomposição. O
#' alfa de Cronbach diz se a média dos itens é uma escala confiável, e tem a
#' vantagem de ter faixa de referência consolidada na literatura, com 0,7
#' como piso usual de aceitação.
#'
#' Alfa negativo, que é o que a primeira versão do gerador produzia, não é um
#' valor baixo: é sinal de que os itens não compartilham nada e que a média
#' deles não é escala nenhuma.
#'
#' **Cada medida sai em duas versões, de Pearson e policórica**, e as duas
#' são reportadas lado a lado. A de Pearson trata a nota de três níveis como
#' 1, 2 e 3, o que supõe espaçamento igual entre categorias e atenua a
#' correlação por causa da discretização grosseira. A policórica supõe, em
#' troca, normal latente por trás de cada item, e estima a correlação entre
#' as latentes (ver `correlacao_policorica()`). Nenhuma das duas é neutra: a
#' primeira compra espaçamento igual, a segunda compra normalidade.
#'
#' Reportar as duas deixa a escolha visível em vez de embutida, e a distância
#' entre elas é informativa por si: ela mede quanto da confiabilidade estava
#' escondida pela discretização. O alfa sobre a matriz policórica é o alfa
#' ordinal de Zumbo, Gadermann e Zeisser (2007), que é a forma recomendada
#' para item de poucas categorias.
#'
#' @param avaliacoes tibble de avaliações.
#' @param etapa Etapa a analisar.
#' @return Lista com as medidas em versão de Pearson e policórica, mais
#'   `matriz` e as duas matrizes de correlação.
medir_dimensionalidade_dos_itens <- function(avaliacoes, etapa = "gestor_final") {
  largo <- avaliacoes |>
    dplyr::filter(etapa == !!etapa) |>
    dplyr::mutate(valor = nota_numerica(nota)) |>
    dplyr::select(consultor_id, projeto_id, item, valor) |>
    tidyr::pivot_wider(names_from = item, values_from = valor)

  matriz <- as.matrix(largo[, setdiff(names(largo), c("consultor_id", "projeto_id"))])
  matriz <- matriz[stats::complete.cases(matriz), , drop = FALSE]

  n_itens <- ncol(matriz)
  correlacoes <- stats::cor(matriz)
  policoricas <- matriz_policorica(matriz)

  variancia <- eigen(correlacoes, symmetric = TRUE, only.values = TRUE)$values
  variancia <- variancia / sum(variancia)
  variancia_policorica <- eigen(policoricas, symmetric = TRUE, only.values = TRUE)$values
  variancia_policorica <- variancia_policorica / sum(variancia_policorica)

  # Alfa de Cronbach na forma bruta: quanto da variância do escore somado vem
  # de covariância entre itens, e não da variância de cada um isolado.
  variancia_itens <- sum(apply(matriz, 2, stats::var))
  variancia_total <- stats::var(rowSums(matriz))
  alfa <- (n_itens / (n_itens - 1)) * (1 - variancia_itens / variancia_total)

  list(
    n_itens = n_itens,
    n_avaliacoes = nrow(matriz),
    variancia_primeiro_componente = variancia[1],
    variancia_dois_primeiros = sum(variancia[1:2]),
    correlacao_media_itens = mean(correlacoes[upper.tri(correlacoes)]),
    alfa_cronbach = alfa,
    variancia_primeiro_componente_policorica = variancia_policorica[1],
    correlacao_media_policorica = mean(policoricas[upper.tri(policoricas)]),
    alfa_ordinal = alfa_de_matriz(policoricas),
    matriz = matriz,
    correlacoes = correlacoes,
    policoricas = policoricas,
    variancia_componentes = variancia,
    variancia_componentes_policorica = variancia_policorica
  )
}

#' Decompõe a variância do escore entre consultores e dentro de cada um
#'
#' É a pergunta que decide se o rótulo tem sinal **sobre a pessoa**, que é o
#' que um recomendador precisa aprender. Um rótulo cuja variância é toda
#' dentro do consultor, isto é, a mesma pessoa recebendo notas muito
#' diferentes em projetos diferentes, não permite dizer que alguém é um bom
#' candidato: permite dizer, no máximo, que aquela alocação foi bem.
#'
#' Não é um julgamento sobre qual das duas deveria dominar. As duas contam
#' uma história legítima: variância entre pessoas é aptidão, variância dentro
#' da pessoa é adequação ao projeto, que é justamente o que a alocação tenta
#' otimizar. O que importa é saber quanto há de cada uma.
#'
#' @param avaliacoes tibble de avaliações.
#' @param etapa Etapa a analisar.
#' @return Lista com as variâncias e a fração entre consultores.
decompor_variancia_do_escore <- function(avaliacoes, etapa = "gestor_final") {
  escores <- avaliacoes |>
    dplyr::filter(etapa == !!etapa) |>
    dplyr::summarise(escore = mean(nota_numerica(nota)), .by = c(consultor_id, projeto_id))

  media_por_consultor <- escores |>
    dplyr::summarise(media = mean(escore), avaliacoes = dplyr::n(), .by = consultor_id)

  variancia_total <- stats::var(escores$escore)
  variancia_entre <- stats::var(media_por_consultor$media)

  list(
    escores = escores,
    media_por_consultor = media_por_consultor,
    variancia_total = variancia_total,
    variancia_entre_consultores = variancia_entre,
    fracao_entre_consultores = variancia_entre / variancia_total,
    consultores_com_mais_de_uma = sum(media_por_consultor$avaliacoes > 1)
  )
}

#' Mede se a margem pode ser atribuída a uma alocação individual
#'
#' A pergunta que separa os candidatos (b) dos candidatos (c) e (d) da
#' construção do rótulo. `margem_frente_mes` não tem `consultor_id`, e o
#' usuário confirmou que frente e mês é a única granularidade que existe na
#' Peers. Atribuir margem a uma pessoa dentro de um time exigiria regra de
#' rateio, que seria estimativa e não captura.
#'
#' **O candidato (c) não cai mais por custo zerado, e sim por não existir.** A
#' primeira versão deste bloco lia os 44,7% de zeros de
#' `peers_custointerns_vlr` como incompletude de apontamento de custo, e
#' concluía que a margem por mês era inutilizável. A segunda consulta às
#' bases comerciais mostrou que aquela fonte é **receita** decomposta por
#' origem, e que o zero é ausência de estagiário no mês. Não há custo nem
#' margem naquela granularidade: a despesa mensal vive em outra tabela, de
#' cobertura parcial, que o sintético ainda não reproduz.
#'
#' Sobra para o candidato (d) a plausibilidade dos valores, que agora tem
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
  # bloco declarou o candidato (d) viável olhando só a estrutura dos zeros, e
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
    # Identidade da fonte: a receita total é a soma das duas origens Peers.
    identidade_receita = max(abs(
      receita_mes$receita_total - receita_mes$receita_nf - receita_mes$receita_interns
    ))
  )
}

#' Avalia qual dos quatro candidatos de rótulo do dicionário se sustenta
#'
#' Mesma forma de `avaliar_formas_de_hibrido()` e
#' `avaliar_estimadores_de_propensao()`: veredito calculado a partir das
#' grandezas medidas, e não declarado, para não envelhecer em silêncio.
#'
#' Os quatro candidatos são os de `docs/dicionario_de_dados.md`, seção
#' "Construção do rótulo".
#'
#' @param margem_atribuivel Se a margem existe por alocação individual.
#' @param fracao_projetos_com_equipe Fração de projetos com mais de uma
#'   pessoa, que é onde o rateio seria necessário.
#' @param tem_custo_mensal Se existe custo na mesma granularidade da receita
#'   mensal, sem o qual não há margem mensal.
#' @param custo_zero_explicavel_estrutural Se o custo zero da margem
#'   estrutural coincide com projeto sem alocação.
#' @param alfa_cronbach Confiabilidade da escala de itens.
#' @param fracao_margem_negativa Fração de projetos com margem estrutural
#'   negativa, que é o teste de plausibilidade do candidato (d).
#' @param limite_alfa Piso de aceitação do alfa. Convenção declarada.
#' @param limite_margem_negativa Fração de margem negativa acima da qual o
#'   candidato (d) é tratado como não calibrado. Convenção declarada: alguns
#'   projetos de consultoria de fato dão prejuízo, um terço deles não.
#' Separa **dois eixos** que colapsar num veredito só confundiria. `viavel`
#' diz se o mecanismo existe e está correto: se a grandeza é construível a
#' partir do que a Peers tem. `calibrado` diz se o sintético reproduz a
#' escala dela de forma crível. O candidato (d) é o caso que obriga a
#' distinção: calcular margem de custo por horas contra contrato é a forma
#' certa, e ainda assim a margem que sai do sintético é implausível, porque
#' três grandezas individualmente calibradas não fecham em conjunto.
#' Reprovar (d) por isso diria que o mecanismo está errado, que não é o caso;
#' aprová-lo em silêncio esconderia que a wave 4 não pode usar o nível dessa
#' margem para nada.
#'
#' @return tibble com `candidato`, `descricao`, `exige`, `viavel`,
#'   `calibrado` e `justificativa`.
avaliar_candidatos_de_rotulo <- function(margem_atribuivel,
                                          fracao_projetos_com_equipe,
                                          tem_custo_mensal,
                                          custo_zero_explicavel_estrutural,
                                          alfa_cronbach,
                                          fracao_margem_negativa,
                                          limite_alfa = 0.7,
                                          limite_margem_negativa = 0.15) {
  escala_confiavel <- alfa_cronbach >= limite_alfa

  tibble::tribble(
    ~candidato, ~descricao, ~exige, ~viavel, ~calibrado, ~justificativa,

    "(a)", "só desempenho, normalizado por avaliador",
    "escala de itens confiável e uma etapa declarada como rótulo",
    escala_confiavel, FALSE,
    paste0(
      "a média dos itens é escala confiável (alfa de ",
      formatar_decimal(alfa_cronbach, 2), "), e a normalização por avaliador ",
      "é obrigatória e não opcional, porque as etapas divergem em nível"
    ),

    "(b)", "média ponderada ou produto de desempenho por margem",
    "margem atribuível a uma alocação individual",
    margem_atribuivel, FALSE,
    paste0(
      "a margem não existe por alocação em lugar nenhum da Peers, e ratear a de ",
      "frente e mês entre os membros do time seria necessário em ",
      formatar_pct(fracao_projetos_com_equipe),
      " dos projetos alocados; o rateio é estimativa, não captura"
    ),

    "(c)", "margem como segundo objetivo, na granularidade de projeto e mês",
    "custo na mesma granularidade da receita, para haver margem mensal",
    tem_custo_mensal, FALSE,
    paste0(
      "a fonte mensal é receita decomposta por origem, não custo: a segunda ",
      "consulta comercial provou a identidade contábil e derrubou a leitura ",
      "anterior, que tomava os zeros por incompletude de apontamento. A ",
      "despesa mensal existe em outra tabela, de cobertura parcial e também ",
      "truncada, que o sintético ainda não reproduz"
    ),

    "(d)", "margem estrutural, de custo carregado por horas contra valor de contrato",
    "custo e contrato disponíveis para todo projeto com alocação",
    custo_zero_explicavel_estrutural,
    fracao_margem_negativa < limite_margem_negativa,
    paste0(
      "mecanismo correto: não depende do apontamento de custo, que derruba o ",
      "candidato (c), e só é zero onde não há ninguém alocado, que não é ",
      "célula de `R`. Mas a escala não fecha no sintético, com ",
      formatar_pct(fracao_margem_negativa),
      " dos projetos em margem negativa, então o nível não é utilizável"
    )
  )
}

#' Constrói o rótulo escalar de uma alocação, numa das formas candidatas
#'
#' O candidato (a) do dicionário é "só desempenho, **normalizado por
#' avaliador**", e a normalização não é adorno: sem ela, a nota de uma
#' alocação depende de quem avaliou tanto quanto de quem foi avaliado. Esta
#' função constrói as três formas para que a afirmação possa ser testada em
#' vez de repetida.
#'
#' * `"bruto"`: média dos itens da etapa adotada, como a nota sai.
#' * `"normalizado"`: nota padronizada **dentro de cada avaliador** antes de
#'   agregar, o que remove a severidade de quem avaliou e preserva a ordem.
#'
#' A padronização dentro do avaliador é a forma mais simples de normalização
#' e a adequada aqui: remove a severidade, que é deslocamento de nível, sem
#' tocar na ordem, que é o que um recomendador usa. Formas mais elaboradas,
#' como modelo de limiar por avaliador, exigiriam mais dado por avaliador do
#' que há.
#'
#' Opera sobre uma etapa por vez, e não sobre a média das três, porque o
#' rótulo é uma nota e não um consenso: a etapa adotada é parte da definição
#' (ver achado II.5.1).
#'
#' @param avaliacoes tibble de avaliações, com `avaliador_id`.
#' @param forma Uma de `"normalizado"` ou `"bruto"`.
#' @param etapa Etapa adotada como rótulo.
#' @return tibble com `consultor_id`, `projeto_id`, `avaliador_id` e
#'   `rotulo`.
construir_rotulo_de_desempenho <- function(avaliacoes,
                                            forma = c("normalizado", "bruto"),
                                            etapa = "gestor_final") {
  forma <- match.arg(forma)

  base <- avaliacoes |>
    dplyr::filter(etapa == !!etapa) |>
    dplyr::mutate(valor = nota_numerica(nota))

  if (forma == "normalizado") {
    # Avaliador com uma avaliação só não tem dispersão para padronizar, e
    # dividir por zero o expulsaria do rótulo. Fica com o desvio da
    # população, que é o mais conservador: não inventa precisão que não há.
    desvio_geral <- stats::sd(base$valor)
    base <- dplyr::mutate(
      base,
      valor = (valor - mean(valor)) /
        dplyr::if_else(dplyr::n() > 1 & stats::sd(valor) > 0, stats::sd(valor), desvio_geral),
      .by = avaliador_id
    )
  }

  dplyr::summarise(
    base, rotulo = mean(valor), .by = c(consultor_id, projeto_id, avaliador_id)
  )
}

#' Compara as formas de construir o rótulo, contra o avaliador registrado
#'
#' Mede duas coisas em cada forma. A **contaminação pelo avaliador** é a
#' fração da variância do rótulo explicada por qual gestor avaliou aquela
#' alocação, e deveria ser zero: é variação que não diz nada sobre a pessoa
#' avaliada. O **sinal sobre a pessoa** é a fração da variância que fica
#' entre consultores, e é o que o recomendador tem para aprender.
#'
#' Uma forma boa tem contaminação perto de zero sem perder sinal. É por isso
#' que as duas precisam ser medidas juntas: zerar a contaminação destruindo o
#' sinal não resolveria nada.
#'
#' **Mede sobre o avaliador registrado, e não sobre um cenário simulado.** A
#' primeira versão desta função sorteava uma etapa por alocação para
#' reproduzir o regime de avaliadores distintos, porque `avaliador_id` não
#' existia no sintético. A consulta ao dono do dado de RH confirmou que a
#' avaliação é por projeto e tem gestor identificado, o gerador passou a
#' emitir `avaliador_id`, e a demonstração deixou de depender de cenário.
#'
#' @param avaliacoes tibble de avaliações, com `avaliador_id`.
#' @param etapa Etapa adotada como rótulo.
#' @return tibble com uma linha por forma.
comparar_construcoes_de_rotulo <- function(avaliacoes, etapa = "gestor_final") {
  medir <- function(rotulos, nome) {
    variancia_total <- stats::var(rotulos$rotulo)
    media_geral <- mean(rotulos$rotulo)

    por_avaliador <- dplyr::summarise(
      rotulos, media = mean(rotulo), n = dplyr::n(), .by = avaliador_id
    )
    por_consultor <- dplyr::summarise(
      rotulos, media = mean(rotulo), .by = consultor_id
    )

    # Fração da variância explicada pelo avaliador: a variância das médias
    # por gestor, ponderada pelo número de avaliações de cada um, sobre a
    # variância total.
    variancia_entre_avaliadores <- sum(
      por_avaliador$n * (por_avaliador$media - media_geral)^2
    ) / (nrow(rotulos) - 1)

    tibble::tibble(
      forma = nome,
      avaliadores = nrow(por_avaliador),
      contaminacao_avaliador = variancia_entre_avaliadores / variancia_total,
      sinal_entre_consultores = stats::var(por_consultor$media) / variancia_total
    )
  }

  dplyr::bind_rows(
    medir(construir_rotulo_de_desempenho(avaliacoes, "bruto", etapa), "Nota bruta"),
    medir(
      construir_rotulo_de_desempenho(avaliacoes, "normalizado", etapa),
      "Normalizada por avaliador"
    )
  )
}

#' Diagnostica por que a escala da margem estrutural não fecha
#'
#' O bloco II.5 encontrou margem estrutural implausível e levantou duas
#' hipóteses. A primeira, de que `valor_contrato` teria grão de alocação e
#' pessoa e não de projeto, **foi descartada**: o usuário confirmou que em
#' `actarnet_alocacao_pessoas` o valor é por projeto. A segunda é a que esta
#' função mede.
#'
#' **A hipótese medida: a taxa usada como custo é preço, não custo.** A fonte
#' de `custo_carregado` é `vw_gtac_fin_taxa_hora`, que a Quinta rodada
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
#' 2,0 no Brasil, e o teto medido fica bem abaixo. Ou a margem que o PP
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
