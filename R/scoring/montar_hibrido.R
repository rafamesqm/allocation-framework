#' Os quatro quadrantes da comutação, na ordem de leitura
#'
#' @return Vetor de texto.
listar_quadrantes <- function() {
  c("histórico e formulário", "só histórico", "só formulário", "zona morta")
}

#' Consolida o que as Etapas 2 e 3 adotaram para cada ramo
#'
#' O híbrido só monta o que os testes adotaram. Com os dois testes sem
#' rejeição, o colaborativo ordena pela popularidade na estimação e o
#' conteúdo deixa a ordem ao acaso dentro do filtro. Outra adoção exigiria
#' levar ao híbrido os escores da fatoração ou do mapeamento, o que não foi
#' implementado porque não foi adotado; a função para em vez de montar uma
#' ordem que os testes não sustentaram.
#'
#' @param avaliacao_conteudo Lista de `avaliar_componente_de_conteudo()`.
#' @param avaliacao_colaborativo Lista de `avaliar_componente_colaborativo()`.
#' @return Lista com `colaborativo` e `conteudo`.
consolidar_adocao <- function(avaliacao_conteudo, avaliacao_colaborativo) {
  adocao <- list(
    colaborativo = if (avaliacao_colaborativo$h1$supera) "fatoração" else "popularidade",
    conteudo = if (avaliacao_conteudo$teste$supera) "mapeamento" else "acaso"
  )
  if (adocao$colaborativo != "popularidade" || adocao$conteudo != "acaso") {
    stop(
      "O híbrido só implementa a popularidade no colaborativo e o acaso no conteúdo, ",
      "que é o que os testes adotaram. Adotado agora: ", adocao$colaborativo, " e ",
      adocao$conteudo, ".", call. = FALSE
    )
  }
  adocao
}

#' Monta as listas do híbrido para um conjunto de projetos
#'
#' Comutação pelo quadrante do consultor diante de cada projeto:
#'
#' * **histórico e formulário**: entra se o setor declarado é o do projeto, e
#'   é ordenado pelo colaborativo adotado;
#' * **só histórico**: entra sem filtro, ordenado pelo colaborativo adotado;
#' * **só formulário**: entra se o setor declarado é o do projeto, e é
#'   ordenado pelo conteúdo adotado;
#' * **zona morta**: não entra; o modelo não a recomenda.
#'
#' Quem tem histórico vem antes de quem só tem formulário, que é a ordem que
#' o piso composto produz: a popularidade é pelo menos 1 para quem tem
#' histórico e 0 para quem não tem. Dentro de cada grupo, a ordem é a do
#' ramo, e os empates são desfeitos pela chave de desempate do projeto.
#'
#' O histórico pode variar por projeto: `popularidade` é uma matriz consultor
#' por projeto, e quem tem contagem zero diante de um projeto não tem
#' histórico para ele. É o que permite contar a popularidade sem o próprio
#' projeto quando `ŝ` é calculado sobre o histórico inteiro.
#'
#' @param projetos Vetor de `projeto_id`.
#' @param contexto Lista de `montar_contexto_do_hibrido()`.
#' @param popularidade Matriz consultor por projeto com a contagem de
#'   alocações que vale para cada projeto.
#' @return tibble com uma linha por candidato e projeto: `projeto_id`,
#'   `consultor_id`, `quadrante`, `ramo`, `passa_filtro` (`NA` quando o
#'   filtro não se aplica, para quem não declarou setor), `posicao` e
#'   `n_candidatos`.
montar_listas_do_hibrido <- function(projetos, contexto, popularidade) {
  declarou <- !is.na(contexto$setor_do_consultor)

  purrr::map(projetos, function(projeto) {
    j <- match(projeto, contexto$projetos)
    contagem <- popularidade[, j]
    historico <- contagem > 0
    casa_setor <- declarou & contexto$setor_do_consultor == contexto$setor_do_projeto[j]
    casa_setor[is.na(casa_setor)] <- FALSE
    candidato <- casa_setor | (historico & !declarou)

    indice <- which(candidato)
    ordem <- order(-historico[indice], -contagem[indice], contexto$chave[indice, j])
    escolhidos <- indice[ordem]

    tibble::tibble(
      projeto_id = projeto,
      consultor_id = contexto$consultores[escolhidos],
      quadrante = classificar_quadrante(historico[escolhidos], declarou[escolhidos]),
      ramo = dplyr::if_else(historico[escolhidos], "colaborativo", "conteúdo"),
      passa_filtro = dplyr::if_else(declarou[escolhidos], TRUE, NA),
      posicao = seq_along(escolhidos),
      n_candidatos = length(escolhidos)
    )
  }) |>
    purrr::list_rbind()
}

#' Classifica o quadrante de cada consultor diante de um projeto
#'
#' @param historico,formulario Vetores lógicos.
#' @return Vetor de texto com um dos `listar_quadrantes()`.
classificar_quadrante <- function(historico, formulario) {
  dplyr::case_when(
    historico & formulario ~ "histórico e formulário",
    historico ~ "só histórico",
    formulario ~ "só formulário",
    .default = "zona morta"
  )
}

#' Converte a posição na lista em `ŝ`, o percentil do consultor
#'
#' `ŝ = 1 - (posição - 1) / (n - 1)`: o primeiro da lista vale 1 e o último
#' vale 0. Lista de um candidato só dá 1.
#'
#' @param posicao,n_candidatos Vetores inteiros.
#' @return Vetor numérico em [0, 1].
calcular_percentil_na_lista <- function(posicao, n_candidatos) {
  # Com um candidato só, a posição é 1 e o denominador vira 1: dá 1.
  1 - (posicao - 1) / pmax(n_candidatos - 1, 1)
}

#' Reúne o que o híbrido usa e não muda entre projetos
#'
#' @param dados Lista com o histórico da janela.
#' @param registro Lista de `ler_pre_registro()`.
#' @return Lista com `consultores`, `projetos`, `setor_do_consultor`,
#'   `setor_do_projeto`, `chave` e `lista`.
montar_contexto_do_hibrido <- function(dados, registro) {
  consultores <- dados$consultores$consultor_id
  projetos <- dados$projetos$projeto_id
  setor <- setor_predominante_observado(dados$experiencia_setorial)

  list(
    consultores = consultores,
    projetos = projetos,
    setor_do_consultor = setor$segmento[match(consultores, setor$consultor_id)],
    setor_do_projeto = dados$projetos$setor,
    chave = withr::with_seed(registro$sementes$desempate, matrix(
      stats::runif(length(consultores) * length(projetos)),
      length(consultores), length(projetos), dimnames = list(consultores, projetos)
    )),
    lista = registro$metrica$lista_decisoria
  )
}

#' Mede o híbrido montado contra o piso composto, por quadrante
#'
#' Medida descritiva do pré-registro, sem teste. Sob a separação temporal,
#' com os quadrantes definidos na estimação, cada par do teste conta como
#' acerto se o consultor realizado está entre os primeiros da lista do
#' híbrido. Quem não está na lista, como a zona morta e quem declarou outro
#' setor, conta como erro.
#'
#' O piso composto sai de outro caminho, `acertar_na_lista()`, sobre o mesmo
#' conjunto de candidatos: popularidade dentro do filtro para quem tem
#' histórico e formulário, popularidade para quem só tem histórico, acaso
#' dentro do filtro para quem só tem formulário. Com as adoções das Etapas 2
#' e 3, os dois coincidem por construção, e o teste que confere isso acusa
#' qualquer divergência entre as duas implementações.
#'
#' @param dados Lista nomeada de tibbles.
#' @param registro Lista de `ler_pre_registro()`.
#' @param adocao Lista de `consolidar_adocao()`.
#' @return Lista com `pares` (um por par do teste, com o quadrante do
#'   realizado e os dois acertos) e `listas` (as listas dos projetos de
#'   teste).
avaliar_hibrido <- function(dados, registro, adocao) {
  historico <- com_historico_da_janela(dados, janela_do_pre_registro(registro))
  divisao <- dividir_interacoes(
    historico$alocacoes, "temporal",
    fracao_teste = registro$divisoes$temporal$fracao_teste, semente = registro$sementes$divisao
  )
  contexto <- montar_contexto_do_hibrido(historico, registro)
  contagem <- as.numeric(table(factor(divisao$treino$consultor_id, levels = contexto$consultores)))
  popularidade <- matrix(
    contagem, length(contexto$consultores), length(contexto$projetos),
    dimnames = list(contexto$consultores, contexto$projetos)
  )

  teste <- divisao$teste |> dplyr::select(consultor_id, projeto_id)
  listas <- montar_listas_do_hibrido(unique(teste$projeto_id), contexto, popularidade)

  declarou <- !is.na(contexto$setor_do_consultor)
  i <- match(teste$consultor_id, contexto$consultores)
  pares <- teste |>
    dplyr::mutate(quadrante = classificar_quadrante(contagem[i] > 0, declarou[i])) |>
    dplyr::left_join(listas |> dplyr::select(projeto_id, consultor_id, posicao), by = c("projeto_id", "consultor_id")) |>
    dplyr::mutate(acerto_hibrido = as.integer(!is.na(posicao) & posicao <= contexto$lista))

  # Piso composto pelo caminho dos componentes: o filtro é o conjunto de
  # candidatos do híbrido, e o escore é a popularidade, zero para quem só
  # tem formulário. Quem não é candidato conta como erro, como no híbrido.
  candidatos <- matrix(FALSE, length(contexto$consultores), length(contexto$projetos),
                       dimnames = dimnames(popularidade))
  candidatos[cbind(match(listas$consultor_id, contexto$consultores), match(listas$projeto_id, contexto$projetos))] <- TRUE
  contexto_do_piso <- list(
    consultores = contexto$consultores, projetos = contexto$projetos,
    filtro = candidatos, chave = contexto$chave, lista = contexto$lista
  )
  e_candidato <- candidatos[cbind(i, match(teste$projeto_id, contexto$projetos))]
  pares$acerto_piso_composto <- acertar_na_lista(teste, contexto_do_piso, popularidade) * e_candidato

  list(pares = pares, listas = listas, adocao = adocao)
}

#' Calcula `ŝ` para todo par candidato, sobre o histórico inteiro da janela
#'
#' É o insumo da wave 4, e não entra em nenhuma avaliação desta wave. A
#' popularidade de cada consultor diante de um projeto é contada sem as
#' alocações dele naquele projeto: sem isso, `ŝ` de um projeto histórico
#' carregaria a própria alocação que a wave 4 vai tentar reproduzir ou
#' contestar. Quem tem histórico só naquele projeto passa, diante dele, ao
#' quadrante de quem não tem histórico.
#'
#' Par fora do filtro não tem linha. A zona morta tem linha, com `ŝ`
#' ausente: o modelo não a recomenda, e o ausente não é convertido em zero.
#'
#' @param dados Lista nomeada de tibbles.
#' @param registro Lista de `ler_pre_registro()`.
#' @param adocao Lista de `consolidar_adocao()`.
#' @return tibble com `projeto_id`, `consultor_id`, `quadrante`, `ramo`,
#'   `passa_filtro`, `posicao`, `n_candidatos` e `s_hat`.
calcular_s_hat <- function(dados, registro, adocao) {
  historico <- com_historico_da_janela(dados, janela_do_pre_registro(registro))
  contexto <- montar_contexto_do_hibrido(historico, registro)
  ocorrencia <- as.matrix(construir_matriz_r(
    dplyr::distinct(historico$alocacoes, consultor_id, projeto_id),
    historico$consultores, historico$projetos
  ))
  contagem <- rowSums(ocorrencia)
  popularidade <- matrix(contagem, nrow(ocorrencia), ncol(ocorrencia), dimnames = dimnames(ocorrencia)) -
    ocorrencia

  listas <- montar_listas_do_hibrido(contexto$projetos, contexto, popularidade) |>
    dplyr::mutate(s_hat = calcular_percentil_na_lista(posicao, n_candidatos))

  declarou <- !is.na(contexto$setor_do_consultor)
  sem_historico <- popularidade == 0
  morta <- which(sem_historico & !declarou, arr.ind = TRUE)
  zona_morta <- tibble::tibble(
    projeto_id = contexto$projetos[morta[, 2]],
    consultor_id = contexto$consultores[morta[, 1]],
    quadrante = "zona morta",
    ramo = NA_character_,
    passa_filtro = NA,
    posicao = NA_integer_,
    n_candidatos = NA_integer_,
    s_hat = NA_real_
  )

  dplyr::bind_rows(listas, zona_morta) |>
    dplyr::arrange(projeto_id, is.na(posicao), posicao)
}
