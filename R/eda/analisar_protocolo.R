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
#' Aqui a popularidade vem só do treino, e o empate é resolvido pela posição
#' **esperada** sob desempate aleatório, que é a medida correta: o focal fica,
#' em média, no meio do bloco de empatados.
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

  # Posição esperada sob desempate aleatório: os estritamente acima, mais
  # metade do bloco de empatados descontando o próprio focal.
  posicao <- acima + (empatados - 1) / 2 + 1

  purrr::map(tamanhos, function(n) {
    tibble::tibble(
      n_recomendados = n,
      acertos = sum(posicao <= n),
      tentativas = length(posicao),
      recall = sum(posicao <= n) / length(posicao)
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
