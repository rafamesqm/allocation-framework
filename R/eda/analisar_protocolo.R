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
#' * `"por_consultor"`: retém todas as alocações de um conjunto de
#'   consultores. É a única que mede o regime de cold start de pessoa, e por
#'   construção torna o colaborativo incapaz de pontuar os retidos.
#' * `"por_projeto"`: o espelho da anterior, para o cold start de projeto.
#'
#' @param alocacoes tibble com `consultor_id`, `projeto_id` e `data_inicio`.
#' @param forma Uma das quatro acima.
#' @param fracao_teste Fração retida, nas formas que sorteiam.
#' @param semente Inteiro para reprodutibilidade.
#' @return Lista com `treino`, `teste` e `forma`.
dividir_interacoes <- function(alocacoes,
                                forma = c("temporal", "aleatoria", "por_consultor", "por_projeto"),
                                fracao_teste = 0.3,
                                semente = 1) {
  forma <- match.arg(forma)
  set.seed(semente)

  retido <- switch(
    forma,
    temporal = alocacoes$data_inicio >=
      stats::quantile(alocacoes$data_inicio, 1 - fracao_teste, type = 1),
    aleatoria = seq_len(nrow(alocacoes)) %in%
      sample.int(nrow(alocacoes), round(nrow(alocacoes) * fracao_teste)),
    por_consultor = alocacoes$consultor_id %in% sample(
      unique(alocacoes$consultor_id),
      round(dplyr::n_distinct(alocacoes$consultor_id) * fracao_teste)
    ),
    por_projeto = alocacoes$projeto_id %in% sample(
      unique(alocacoes$projeto_id),
      round(dplyr::n_distinct(alocacoes$projeto_id) * fracao_teste)
    )
  )

  list(treino = alocacoes[!retido, ], teste = alocacoes[retido, ], forma = forma)
}

#' Mede o que cada forma de divisão permite e o que ela vaza
#'
#' Duas propriedades decidem a escolha, e elas se opõem.
#'
#' **Vazamento** é a fração de entidades do teste que o treino já viu. Para um
#' atributo com chave de entidade, como o perfil de skills de um consultor,
#' vazamento significa que o modelo já encontrou aquela pessoa e a avaliação
#' mede em parte memorização. O bloco II.6 mostrou o efeito: o perfil médio
#' media AUC de 0,62 com vazamento e 0,49 sem.
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
#' @param divisao Lista de `dividir_interacoes()`.
#' @param com_atributo Vetor de `consultor_id` com atributo de conteúdo.
#' @return tibble de uma linha.
medir_divisao <- function(divisao, com_atributo) {
  treino <- divisao$treino
  teste <- divisao$teste

  consultor_visto <- teste$consultor_id %in% treino$consultor_id
  projeto_visto <- teste$projeto_id %in% treino$projeto_id

  tibble::tibble(
    forma = divisao$forma,
    n_treino = nrow(treino),
    n_teste = nrow(teste),
    vazamento_consultor = mean(consultor_visto),
    vazamento_projeto = mean(projeto_visto),
    avaliavel_colaborativo = mean(consultor_visto & projeto_visto),
    avaliavel_conteudo = mean(teste$consultor_id %in% com_atributo)
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
#' A fórmula é a de Connor (1987) para o tamanho de amostra do McNemar,
#' resolvida para o ganho. Ela depende da fração de pares discordantes, que só
#' se conhece com o modelo pronto, e por isso o cálculo vem em dois cenários
#' que cercam a verdade:
#'
#' * `dominancia`: o modelo acerta tudo o que o piso acerta e mais um pouco.
#'   É o mínimo de discordância possível e o cenário mais otimista.
#' * `independencia`: os acertos do modelo não guardam relação com os do
#'   piso. Um recomendador real tende a aprender parte da popularidade e a
#'   acertar onde ela acerta, o que reduz a discordância, então este cenário é
#'   o conservador.
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

  z_alfa <- stats::qnorm(1 - alfa / 2)
  z_poder <- stats::qnorm(poder)

  discordancia <- function(ganho) {
    if (cenario == "dominancia") return(ganho)
    acerto_modelo <- piso + ganho
    acerto_modelo * (1 - piso) + (1 - acerto_modelo) * piso
  }
  n_necessario <- function(ganho) {
    psi <- discordancia(ganho)
    (z_alfa * sqrt(psi) + z_poder * sqrt(psi - ganho^2))^2 / ganho^2
  }

  teto <- 1 - piso - 1e-9
  if (n_necessario(teto) > n) return(NA_real_)

  stats::uniroot(
    function(ganho) n_necessario(ganho) - n, c(1e-6, teto), tol = 1e-10
  )$root
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
#' @param alocacoes,consultores,projetos Dados da realização.
#' @param experiencia_setorial Experiência observada.
#' @param fracao_teste Fração retida pelas divisões.
#' @param n_recomendados Tamanho da lista.
#' @param alfa,poder Repassados a `calcular_ganho_minimo_detectavel()`.
#' @return tibble com uma linha por componente e piso.
planejar_poder_por_componente <- function(alocacoes, consultores, projetos,
                                          experiencia_setorial, fracao_teste = 0.3,
                                          n_recomendados = 20, alfa = 0.05,
                                          poder = 0.8) {
  com_atributo <- unique(experiencia_setorial$consultor_id)

  temporal <- dividir_interacoes(alocacoes, "temporal", fracao_teste = fracao_teste)
  avaliavel <- temporal$teste$consultor_id %in% temporal$treino$consultor_id &
    temporal$teste$projeto_id %in% temporal$treino$projeto_id
  teste_colaborativo <- temporal$teste[avaliavel, ]

  por_consultor <- dividir_interacoes(alocacoes, "por_consultor", fracao_teste = fracao_teste)
  teste_conteudo <- por_consultor$teste[por_consultor$teste$consultor_id %in% com_atributo, ]

  piso_de_popularidade <- function(divisao, teste) {
    medir_linha_base_sob_protocolo(divisao$treino, teste, consultores, n_recomendados)$recall
  }
  filtro <- medir_piso_filtro_e_acaso(
    teste_conteudo, consultores, projetos, experiencia_setorial, n_recomendados
  )

  tibble::tribble(
    ~componente, ~protocolo, ~piso, ~n_avaliavel, ~valor_piso,
    "Colaborativo", "Temporal", "Popularidade no treino",
    nrow(teste_colaborativo), piso_de_popularidade(temporal, teste_colaborativo),
    "Conteúdo", "Por consultor", "Popularidade no treino",
    nrow(teste_conteudo), piso_de_popularidade(por_consultor, teste_conteudo),
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
