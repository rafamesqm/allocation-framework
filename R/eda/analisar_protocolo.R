#' Divide as interações em treino e teste, numa das formas candidatas
#'
#' As quatro formas respondem a perguntas diferentes, e é por isso que
#' escolher entre elas não é detalhe de implementação.
#'
#' * `"temporal"`: corta por data de início. É a única que respeita a ordem
#'   em que a informação chegaria em produção, e a única honesta sobre
#'   deriva. Não separa pessoas: o mesmo consultor aparece dos dois lados.
#' * `"aleatoria"`: sorteia alocações. Serve de referência para medir quanto
#'   a ordem temporal custa, e nada mais: em produção não se prevê o passado.
#' * `"por_consultor"`: validação cruzada em grupos de consultores, em que
#'   cada consultor fica no teste uma vez. É a única que mede o regime de cold
#'   start de pessoa, e por construção torna o colaborativo incapaz de pontuar
#'   os retidos. Ver `dividir_por_consultor_em_grupos()`.
#' * `"por_projeto"`: o espelho do sorteio por consultor, para o cold start de
#'   projeto. Serve só de referência no bloco II.7.
#'
#' @param alocacoes tibble com `consultor_id`, `projeto_id` e `data_inicio`.
#' @param forma Uma das quatro acima.
#' @param fracao_teste Fração retida, nas formas que sorteiam. Não se aplica a
#'   `"por_consultor"`, em que cada consultor fica no teste uma vez.
#' @param semente Inteiro para reprodutibilidade.
#' @param n_grupos Número de grupos da validação cruzada por consultor.
#' @param universo Vetor de `consultor_id` a distribuir entre os grupos, na
#'   forma `"por_consultor"`. `NULL` usa os consultores de `alocacoes`.
#' @return Lista com `treino`, `teste` e `forma`; na forma `"por_consultor"`,
#'   a de `dividir_por_consultor_em_grupos()`.
dividir_interacoes <- function(alocacoes,
                                forma = c("temporal", "aleatoria", "por_consultor", "por_projeto"),
                                fracao_teste = 0.3,
                                semente = 1,
                                n_grupos = 5,
                                universo = NULL) {
  forma <- match.arg(forma)
  if (forma == "por_consultor") {
    return(dividir_por_consultor_em_grupos(alocacoes, n_grupos, semente, universo))
  }
  set.seed(semente)

  retido <- switch(
    forma,
    temporal = alocacoes$data_inicio >=
      stats::quantile(alocacoes$data_inicio, 1 - fracao_teste, type = 1),
    aleatoria = seq_len(nrow(alocacoes)) %in%
      sample.int(nrow(alocacoes), round(nrow(alocacoes) * fracao_teste)),
    por_projeto = alocacoes$projeto_id %in% sample(
      unique(alocacoes$projeto_id),
      round(dplyr::n_distinct(alocacoes$projeto_id) * fracao_teste)
    )
  )

  list(treino = alocacoes[!retido, ], teste = alocacoes[retido, ], forma = forma)
}

#' Divide as interações em grupos de consultores, para validação cruzada
#'
#' Substitui o sorteio único de 30% dos consultores. Aquele sorteio dependia
#' da ordem em que cada consultor aparecia pela primeira vez nas linhas de
#' `alocacoes`, e tirar quatro linhas do arquivo trocava um sétimo dos
#' retidos e mudava o piso do conteúdo em mais de um ponto percentual. Aqui
#' o grupo de cada consultor depende só do `consultor_id`, em ordem, e da
#' semente, e cada consultor fica no teste exatamente uma vez: as predições de
#' todos os grupos são reunidas antes de qualquer comparação.
#'
#' Os grupos são balanceados em número de consultores: a semente embaralha
#' uma sequência `1, 2, ..., n_grupos, 1, 2, ...` do tamanho do universo.
#'
#' @param alocacoes tibble com `consultor_id`.
#' @param n_grupos Número de grupos.
#' @param semente Inteiro para reprodutibilidade.
#' @param universo Vetor de `consultor_id` a distribuir. Passar o quadro
#'   inteiro deixa a atribuição imune a mudanças nas alocações; `NULL` usa os
#'   consultores de `alocacoes`.
#' @return Lista com `forma`, `grupo_do_consultor` (tibble), `grupos` (uma
#'   lista de `treino` e `teste` por grupo) e `teste`, a união dos testes com
#'   a coluna `grupo`.
dividir_por_consultor_em_grupos <- function(alocacoes, n_grupos = 5, semente = 1,
                                            universo = NULL) {
  ids <- sort(unique(if (is.null(universo)) alocacoes$consultor_id else universo))
  if (!all(alocacoes$consultor_id %in% ids)) {
    stop("Há consultor nas alocações fora do universo da divisão.", call. = FALSE)
  }
  grupo <- withr::with_seed(semente, sample(rep_len(seq_len(n_grupos), length(ids))))
  grupo_do_consultor <- tibble::tibble(consultor_id = ids, grupo = grupo)

  grupo_da_linha <- grupo[match(alocacoes$consultor_id, ids)]
  grupos <- purrr::map(seq_len(n_grupos), function(g) {
    list(treino = alocacoes[grupo_da_linha != g, ], teste = alocacoes[grupo_da_linha == g, ])
  })

  list(
    forma = "por_consultor",
    grupo_do_consultor = grupo_do_consultor,
    grupos = grupos,
    teste = dplyr::mutate(alocacoes, grupo = grupo_da_linha)
  )
}

#' Mede o que cada forma de divisão permite e o que ela vaza
#'
#' Duas propriedades decidem a escolha, e elas se opõem.
#'
#' **Vazamento** é a fração de entidades do teste que o treino já viu. Para um
#' atributo com chave de entidade, como o perfil de skills de um consultor,
#' vazamento significa que o modelo já encontrou aquela pessoa e a avaliação
#' mede em parte memorização. O bloco II.6 mostrou o efeito: o perfil médio
#' perde toda a separação quando o vazamento sai, e cai para o acaso.
#'
#' **Avaliabilidade** é a fração de pares de teste que cada componente do
#' híbrido consegue pontuar. O colaborativo precisa ter visto as **duas**
#' pontas no treino; sem isso ele não tem vetor para nenhuma delas. O
#' conteúdo precisa que o consultor tenha atributo declarado, e não que ele
#' tenha sido visto.
#'
#' Uma divisão que zera o vazamento zera também a avaliabilidade do
#' colaborativo, e não por defeito da medida: é a definição de cold start. É
#' essa oposição que impede um protocolo único.
#'
#' Na validação cruzada por consultor, cada par de teste é comparado com o
#' treino do seu próprio grupo, e as medidas são sobre a união dos testes.
#' `n_treino` é então o tamanho médio do treino entre os grupos.
#'
#' @param divisao Lista de `dividir_interacoes()`.
#' @param com_atributo Vetor de `consultor_id` com atributo de conteúdo.
#' @return tibble de uma linha.
medir_divisao <- function(divisao, com_atributo) {
  partes <- if (is.null(divisao$grupos)) list(divisao) else divisao$grupos

  pares <- purrr::map(partes, function(parte) {
    tibble::tibble(
      consultor_visto = parte$teste$consultor_id %in% parte$treino$consultor_id,
      projeto_visto = parte$teste$projeto_id %in% parte$treino$projeto_id,
      com_atributo = parte$teste$consultor_id %in% com_atributo
    )
  }) |>
    purrr::list_rbind()

  tibble::tibble(
    forma = divisao$forma,
    n_treino = mean(vapply(partes, \(parte) nrow(parte$treino), integer(1))),
    n_teste = nrow(pares),
    vazamento_consultor = mean(pares$consultor_visto),
    vazamento_projeto = mean(pares$projeto_visto),
    avaliavel_colaborativo = mean(pares$consultor_visto & pares$projeto_visto),
    avaliavel_conteudo = mean(pares$com_atributo)
  )
}

#' Linha de base de popularidade, sob um protocolo declarado
#'
#' Refaz a linha de base do bloco II.1 do jeito que o achado II.1.4 pediu. A
#' versão de lá tinha dois vícios que a tornavam limite superior, e nenhum
#' deles é evidente sem nomear.
#'
#' O primeiro era **usar o mesmo dado para contar popularidade e para
#' avaliar**, com a alocação focal descontada. O segundo era **desempatar a
#' favor do consultor avaliado**: contar quantos estão estritamente acima dele
#' dá ao focal a melhor posição possível dentro do seu empate, e com contagens
#' inteiras os empates são muitos.
#'
#' Aqui a popularidade vem só do treino, e o empate é resolvido pelo **acerto
#' esperado** sob desempate aleatório: a fração do bloco de empatados que cabe
#' na lista. Uma versão anterior comparava a posição média do bloco com o
#' tamanho da lista, o que dá zero ou um justamente quando o bloco atravessa
#' o corte.
#'
#' @param treino,teste tibbles de alocações.
#' @param consultores tibble que define o quadro.
#' @param tamanhos Tamanhos de lista a avaliar.
#' @return tibble com `n_recomendados`, `acertos`, `tentativas` e `recall`.
medir_linha_base_sob_protocolo <- function(treino, teste, consultores,
                                            tamanhos = c(5, 10, 20, 50)) {
  popularidade <- as.integer(table(
    factor(treino$consultor_id, levels = consultores$consultor_id)
  ))
  indice <- match(teste$consultor_id, consultores$consultor_id)
  contagem_focal <- popularidade[indice]

  acima <- vapply(contagem_focal, function(k) sum(popularidade > k), integer(1))
  empatados <- vapply(contagem_focal, function(k) sum(popularidade == k), integer(1))

  # **Acerto esperado, e não acerto da posição esperada.** Sob desempate
  # aleatório o focal ocupa com a mesma chance cada uma das posições do seu
  # bloco de empatados, de `acima + 1` a `acima + empatados`, então a
  # probabilidade de ele entrar numa lista de `n` é a fração do bloco que cabe
  # nela. A versão anterior comparava a posição média com `n`, o que dá zero
  # ou um justamente quando o bloco atravessa o corte, que é o caso em que o
  # desempate importa.
  purrr::map(tamanhos, function(n) {
    acerto <- pmin(1, pmax(0, (n - acima) / empatados))
    tibble::tibble(
      n_recomendados = n,
      acertos = sum(acerto),
      tentativas = length(acerto),
      recall = mean(acerto)
    )
  }) |>
    purrr::list_rbind()
}

#' Avalia qual protocolo serve a cada componente do híbrido
#'
#' Mesma forma de `avaliar_formas_de_hibrido()` e das outras tabelas de
#' decisão do capítulo: o veredito é calculado a partir das medidas, e não
#' declarado.
#'
#' @param divisoes tibble de `medir_divisao()`, uma linha por forma.
#' @param limite_vazamento Acima desta fração de entidades já vistas, a forma
#'   é tratada como inadequada para avaliar atributo de entidade.
#' @param limite_avaliavel Abaixo desta fração de pares pontuáveis, a forma é
#'   tratada como inadequada para avaliar o colaborativo.
#' @return tibble com uma linha por forma e o veredito de cada componente.
avaliar_protocolos <- function(divisoes, limite_vazamento = 0.5,
                                limite_avaliavel = 0.5) {
  divisoes |>
    dplyr::mutate(
      serve_colaborativo = avaliavel_colaborativo >= limite_avaliavel,
      serve_conteudo = vazamento_consultor <= limite_vazamento,
      respeita_ordem_temporal = forma == "temporal"
    )
}

#' Mede o piso do componente de conteúdo: filtro de setor e acaso dentro dele
#'
#' Sob divisão por consultor, que é o protocolo do conteúdo, nenhum
#' consultor de teste tem alocação no treino. A popularidade contada no
#' treino dá zero a todos eles, que ficam abaixo de todo consultor visto e
#' nunca entram numa lista curta. O piso de popularidade, ali, é zero por
#' construção da divisão, e qualquer modelo o supera.
#'
#' O piso que faz sentido é o que o bloco II.6 já decidiu que o conteúdo é:
#' o filtro de setor, sem nada que ordene dentro dele. Quem passa o filtro
#' entra na lista com probabilidade igual à fração do conjunto filtrado que
#' cabe nela; quem não passa só entra se o filtro deixar sobra. É o patamar
#' contra o qual se pergunta se alguma coisa ordena dentro do filtro melhor
#' que o acaso, que é a pergunta que o achado II.6.6 deixou aberta.
#'
#' @param teste tibble de alocações de teste, com `consultor_id` e
#'   `projeto_id`.
#' @param consultores tibble que define o quadro.
#' @param projetos tibble com `projeto_id` e `setor`.
#' @param experiencia_setorial Experiência observada, de onde sai o setor do
#'   consultor.
#' @param n_recomendados Tamanho da lista.
#' @return Lista com `recall`, `fracao_passa` e `n`.
medir_piso_filtro_e_acaso <- function(teste, consultores, projetos,
                                      experiencia_setorial, n_recomendados = 20) {
  setor <- setor_predominante_observado(experiencia_setorial)
  setor_do_consultor <- setor$segmento[match(consultores$consultor_id, setor$consultor_id)]
  tamanho_do_setor <- table(setor_do_consultor)
  n_quadro <- nrow(consultores)

  setor_do_projeto <- projetos$setor[match(teste$projeto_id, projetos$projeto_id)]
  setor_do_focal <- setor_do_consultor[match(teste$consultor_id, consultores$consultor_id)]
  filtrados <- as.numeric(tamanho_do_setor[setor_do_projeto])
  filtrados[is.na(filtrados)] <- 0
  passa <- !is.na(setor_do_focal) & setor_do_focal == setor_do_projeto

  acerto <- ifelse(
    passa,
    pmin(1, n_recomendados / filtrados),
    pmax(0, (n_recomendados - filtrados) / (n_quadro - filtrados))
  )

  list(recall = mean(acerto), fracao_passa = mean(passa), n = nrow(teste))
}

#' Calcula o menor ganho distinguível do piso num teste pareado de McNemar
#'
#' Modelo e piso são avaliados sobre os **mesmos** pares de teste, então a
#' comparação é pareada, e o que decide o poder são os pares discordantes, em
#' que um acerta e o outro não. Tratar como duas proporções independentes
#' ignoraria isso e superestimaria o ganho necessário.
#'
#' O poder é o do **teste pré-registrado**, o McNemar exato bilateral, e não
#' o da aproximação normal de Connor (1987) que uma versão anterior usava.
#' Com poucos pares discordantes, que é o caso do colaborativo, a aproximação
#' prometia enxergar ganhos que o teste exato não enxerga. Ver
#' `calcular_poder_mcnemar_exato()`.
#'
#' O poder depende da fração de pares discordantes, que só se conhece com o
#' modelo pronto, e por isso o cálculo vem em dois cenários que cercam a
#' verdade:
#'
#' * `dominancia`: o modelo acerta tudo o que o piso acerta e mais um pouco.
#'   É o mínimo de discordância possível e o cenário mais otimista.
#' * `independencia`: os acertos do modelo não guardam relação com os do
#'   piso. Um recomendador real tende a aprender parte da popularidade e a
#'   acertar onde ela acerta, o que reduz a discordância, então este cenário é
#'   o conservador.
#'
#' O poder exato é contínuo e crescente no ganho, e a raiz é achada por
#' bisseção entre zero e o maior ganho possível, `1 - piso`.
#'
#' @param n Número de pares de teste avaliáveis.
#' @param piso Acerto do piso, entre zero e um.
#' @param cenario `"independencia"` ou `"dominancia"`.
#' @param alfa Nível do teste, bilateral.
#' @param poder Poder desejado.
#' @return O ganho mínimo em proporção, ou `NA` se nem um modelo perfeito
#'   seria distinguível com este `n`.
calcular_ganho_minimo_detectavel <- function(n, piso,
                                             cenario = c("independencia", "dominancia"),
                                             alfa = 0.05, poder = 0.8) {
  cenario <- match.arg(cenario)
  if (n <= 0 || piso >= 1) return(NA_real_)

  poder_em <- function(ganho) {
    calcular_poder_mcnemar_exato(n, piso, ganho, cenario = cenario, alfa = alfa)
  }
  teto <- 1 - piso
  if (poder_em(teto) < poder) return(NA_real_)

  stats::uniroot(
    function(ganho) poder_em(ganho) - poder, c(1e-9, teto), tol = 1e-8
  )$root
}

#' Menor contagem a favor do modelo que rejeita no McNemar exato bilateral
#'
#' Condicionado a `d` pares discordantes, o número `b` dos que favorecem o
#' modelo segue uma binomial com `d` tentativas e probabilidade 1/2 sob a
#' hipótese nula. O valor-p bilateral exato, para `b` acima de `d / 2`, é o
#' dobro da cauda superior, e é o mesmo de `binom.test(b, d)`. Só conta a
#' rejeição na direção do modelo, porque é ela que decide superioridade.
#'
#' Rejeitar com `b` exige `P(B >= b) <= alfa / 2`, e o menor `b` assim é o
#' quantil `1 - alfa / 2` da binomial mais um. A forma fechada evita varrer
#' todas as contagens a cada passo da busca do ganho mínimo, que dentro das
#' réplicas custava minutos; o resultado é conferido contra `binom.test()` nos
#' testes.
#'
#' @param discordantes Vetor de contagens de pares discordantes.
#' @param alfa Nível do teste, bilateral.
#' @return Vetor com o menor `b` que rejeita, ou `Inf` quando nenhum rejeita.
calcular_regiao_critica_mcnemar_exato <- function(discordantes, alfa = 0.05) {
  critico <- stats::qbinom(1 - alfa / 2, discordantes, 0.5) + 1
  # O quantil pode cair um acima por arredondamento; desce enquanto a
  # contagem anterior ainda rejeitar.
  anterior_rejeita <- critico - 1 > discordantes / 2 &
    stats::pbinom(critico - 2, discordantes, 0.5, lower.tail = FALSE) <= alfa / 2
  critico <- critico - anterior_rejeita
  ifelse(discordantes == 0 | critico > discordantes, Inf, critico)
}

#' Poder exato do McNemar bilateral para um ganho sobre o piso
#'
#' Soma, sobre o número de pares discordantes, a probabilidade de esse número
#' ocorrer vezes a de a contagem a favor do modelo cair na região crítica.
#' Com fração discordante `psi` e ganho `g`, um par discordante favorece o
#' modelo com probabilidade `(psi + g) / (2 psi)`.
#'
#' @param n Número de pares.
#' @param piso Acerto do piso.
#' @param ganho Diferença de acerto entre modelo e piso, positiva.
#' @param cenario Como em `calcular_ganho_minimo_detectavel()`.
#' @param alfa Nível do teste, bilateral.
#' @return O poder, entre zero e um.
calcular_poder_mcnemar_exato <- function(n, piso, ganho,
                                         cenario = c("independencia", "dominancia"),
                                         alfa = 0.05) {
  cenario <- match.arg(cenario)
  psi <- if (cenario == "dominancia") {
    ganho
  } else {
    acerto_modelo <- piso + ganho
    acerto_modelo * (1 - piso) + (1 - acerto_modelo) * piso
  }
  a_favor <- min(1, (psi + ganho) / (2 * psi))

  d <- 0:n
  critico <- calcular_regiao_critica_mcnemar_exato(d, alfa)
  rejeita_dado_d <- ifelse(
    is.finite(critico), stats::pbinom(critico - 1, d, a_favor, lower.tail = FALSE), 0
  )
  sum(stats::dbinom(d, n, psi) * rejeita_dado_d)
}

#' Planeja o poder de cada componente sob o protocolo que lhe cabe
#'
#' O bloco II.7 decidiu dois protocolos, um por componente, e cada um tem o
#' seu número de pares avaliáveis e o seu piso. Esta função junta as duas
#' coisas e devolve, para cada combinação, o menor ganho que um teste pareado
#' conseguiria distinguir do piso.
#'
#' Entram três pisos para o conteúdo, de propósito. O de popularidade aparece
#' para tornar visível que ele é degenerado sob divisão por consultor. O de
#' filtro mais acaso é o critério. O de acaso puro é a verificação de que o
#' próprio filtro carrega sinal.
#'
#' O conteúdo é avaliado na validação cruzada por consultor: cada respondente
#' entra no teste uma vez, e o piso de popularidade de cada grupo é contado
#' no treino do próprio grupo antes de os acertos serem reunidos.
#'
#' @param alocacoes,consultores,projetos Dados da realização.
#' @param experiencia_setorial Experiência observada.
#' @param fracao_teste Fração retida pela divisão temporal.
#' @param n_recomendados Tamanho da lista.
#' @param alfa,poder Repassados a `calcular_ganho_minimo_detectavel()`.
#' @param n_grupos Grupos da validação cruzada por consultor.
#' @param semente Semente das divisões.
#' @return tibble com uma linha por componente e piso.
planejar_poder_por_componente <- function(alocacoes, consultores, projetos,
                                          experiencia_setorial, fracao_teste = 0.3,
                                          n_recomendados = 20, alfa = 0.05,
                                          poder = 0.8, n_grupos = 5, semente = 1) {
  com_atributo <- unique(experiencia_setorial$consultor_id)

  temporal <- dividir_interacoes(
    alocacoes, "temporal", fracao_teste = fracao_teste, semente = semente
  )
  avaliavel <- temporal$teste$consultor_id %in% temporal$treino$consultor_id &
    temporal$teste$projeto_id %in% temporal$treino$projeto_id
  teste_colaborativo <- temporal$teste[avaliavel, ]

  por_consultor <- dividir_interacoes(
    alocacoes, "por_consultor", semente = semente, n_grupos = n_grupos,
    universo = consultores$consultor_id
  )
  teste_conteudo <- por_consultor$teste[por_consultor$teste$consultor_id %in% com_atributo, ]

  piso_de_popularidade <- function(treino, teste) {
    medir_linha_base_sob_protocolo(treino, teste, consultores, n_recomendados)$acertos
  }
  acertos_popularidade_conteudo <- sum(vapply(por_consultor$grupos, function(grupo) {
    teste <- grupo$teste[grupo$teste$consultor_id %in% com_atributo, ]
    if (nrow(teste) == 0) return(0)
    piso_de_popularidade(grupo$treino, teste)
  }, numeric(1)))
  filtro <- medir_piso_filtro_e_acaso(
    teste_conteudo, consultores, projetos, experiencia_setorial, n_recomendados
  )

  tibble::tribble(
    ~componente, ~protocolo, ~piso, ~n_avaliavel, ~valor_piso,
    "Colaborativo", "Temporal", "Popularidade no treino",
    nrow(teste_colaborativo),
    piso_de_popularidade(temporal$treino, teste_colaborativo) / nrow(teste_colaborativo),
    "Conteúdo", "Por consultor", "Popularidade no treino",
    nrow(teste_conteudo), acertos_popularidade_conteudo / nrow(teste_conteudo),
    "Conteúdo", "Por consultor", "Filtro de setor e acaso",
    nrow(teste_conteudo), filtro$recall,
    "Conteúdo", "Por consultor", "Acaso",
    nrow(teste_conteudo), n_recomendados / nrow(consultores)
  ) |>
    dplyr::mutate(
      ganho_dominancia = purrr::map2_dbl(
        .data$n_avaliavel, .data$valor_piso, calcular_ganho_minimo_detectavel,
        cenario = "dominancia", alfa = alfa, poder = poder
      ),
      ganho_independencia = purrr::map2_dbl(
        .data$n_avaliavel, .data$valor_piso, calcular_ganho_minimo_detectavel,
        cenario = "independencia", alfa = alfa, poder = poder
      ),
      fracao_passa_filtro = dplyr::if_else(
        .data$piso == "Filtro de setor e acaso", filtro$fracao_passa, NA_real_
      )
    )
}
