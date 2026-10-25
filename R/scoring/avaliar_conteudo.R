#' Constrói a matriz de atributos declarados de cada respondente
#'
#' São os atributos que o pré-registro admite no mapeamento: o setor
#' predominante declarado, em codificação indicadora, e as competências
#' declaradas, uma coluna indicadora por skill, sem peso de proficiência.
#' Nada aqui vem das alocações. A recência fica de fora porque as datas das
#' ondas de formulário são suposição declarada (decisão I.4.3).
#'
#' @param experiencia_setorial Experiência observada, de quem respondeu.
#' @param skills_consultor Competências declaradas.
#' @param espaco_skills Catálogo de skills, que fixa as colunas.
#' @return Matriz numérica, uma linha por respondente, nomeada pelo
#'   `consultor_id`.
construir_atributos_declarados <- function(experiencia_setorial, skills_consultor, espaco_skills) {
  setor <- setor_predominante_observado(experiencia_setorial)
  ids <- sort(setor$consultor_id)
  segmentos <- sort(unique(experiencia_setorial$segmento))
  skills <- sort(unique(espaco_skills$skill_id))

  matriz <- matrix(
    0, length(ids), length(segmentos) + length(skills),
    dimnames = list(ids, c(paste0("setor:", segmentos), paste0("skill:", skills)))
  )
  matriz[cbind(match(setor$consultor_id, ids), match(paste0("setor:", setor$segmento), colnames(matriz)))] <- 1

  declaradas <- skills_consultor[skills_consultor$consultor_id %in% ids, ]
  matriz[cbind(
    match(declaradas$consultor_id, ids),
    match(paste0("skill:", declaradas$skill_id), colnames(matriz))
  )] <- 1
  matriz
}

#' Ajusta o mapa dos atributos declarados para os fatores dos consultores
#'
#' Gantner et al. (2010): uma regressão ridge, coluna a coluna, dos fatores
#' latentes sobre os atributos, ajustada só em quem tem fator, ou seja,
#' histórico na estimação, e atributo declarado. Atributos e fatores são
#' centrados, e o intercepto não é penalizado.
#'
#' @param atributos Matriz de `construir_atributos_declarados()`.
#' @param fatores Matriz de fatores dos consultores, nomeada por linha.
#' @param com_historico Vetor de `consultor_id` com alocação na estimação.
#' @param lambda Regularização do mapa.
#' @return Lista com `media_atributos`, `media_fatores` e `coeficientes`.
ajustar_mapeamento <- function(atributos, fatores, com_historico, lambda) {
  ids <- intersect(rownames(atributos), intersect(rownames(fatores), com_historico))
  if (length(ids) < 2) {
    stop("O mapa precisa de ao menos dois consultores com histórico e atributo.", call. = FALSE)
  }
  z <- atributos[ids, , drop = FALSE]
  u <- fatores[ids, , drop = FALSE]
  media_z <- colMeans(z)
  media_u <- colMeans(u)
  zc <- sweep(z, 2, media_z)
  uc <- sweep(u, 2, media_u)

  list(
    media_atributos = media_z,
    media_fatores = media_u,
    coeficientes = solve(crossprod(zc) + diag(lambda, ncol(zc)), crossprod(zc, uc))
  )
}

#' Prevê os fatores de consultores a partir dos atributos declarados
#'
#' @param mapa Lista de `ajustar_mapeamento()`.
#' @param atributos Matriz de atributos dos consultores a prever.
#' @return Matriz de fatores previstos, uma linha por consultor.
prever_fatores <- function(mapa, atributos) {
  previsto <- sweep(atributos, 2, mapa$media_atributos) %*% mapa$coeficientes
  sweep(previsto, 2, mapa$media_fatores, "+")
}

#' Reúne o que toda avaliação do conteúdo usa e não muda entre ajustes
#'
#' O filtro é o setor predominante declarado contra o setor do projeto, só
#' para quem declarou. A chave de desempate é uma ordem aleatória por
#' projeto, sorteada uma vez com a semente de desempate do pré-registro, e é
#' a mesma para o modelo e para o piso: fora do filtro e nos empates, os dois
#' ordenam igual, e só a ordem dentro do filtro os separa.
#'
#' @param dados Lista com o histórico da janela, de `com_historico_da_janela()`.
#' @param registro Lista de `ler_pre_registro()`.
#' @return Lista com `consultores`, `projetos`, `filtro` (matriz lógica
#'   consultor por projeto), `chave` (matriz de desempate), `atributos`,
#'   `com_atributo` e `lista`.
montar_contexto_do_conteudo <- function(dados, registro) {
  consultores <- dados$consultores$consultor_id
  projetos <- dados$projetos$projeto_id
  setor <- setor_predominante_observado(dados$experiencia_setorial)
  setor_do_consultor <- setor$segmento[match(consultores, setor$consultor_id)]
  setor_do_projeto <- dados$projetos$setor

  filtro <- outer(setor_do_consultor, setor_do_projeto, `==`)
  filtro[is.na(filtro)] <- FALSE
  dimnames(filtro) <- list(consultores, projetos)

  chave <- withr::with_seed(registro$sementes$desempate, matrix(
    stats::runif(length(consultores) * length(projetos)),
    length(consultores), length(projetos), dimnames = list(consultores, projetos)
  ))

  list(
    dados = dados,
    consultores = consultores,
    projetos = projetos,
    filtro = filtro,
    chave = chave,
    atributos = construir_atributos_declarados(
      dados$experiencia_setorial, dados$skills_consultor, dados$espaco_skills
    ),
    com_atributo = setor$consultor_id,
    lista = registro$metrica$lista_decisoria
  )
}

#' Diz, para cada par, se o consultor realizado entra na lista
#'
#' A lista do projeto é o conjunto filtrado, ordenado pelo escore e, nos
#' empates, pela chave de desempate, seguido do resto do quadro, ordenado só
#' pela chave. Quem passa o filtro entra se tiver menos de `lista`
#' candidatos filtrados à frente; quem não passa só entra se o filtro deixar
#' vaga e a chave o puser dentro dela. Com `escores = NULL`, todos os
#' filtrados empatam e a ordem é ao acaso: é o piso do conteúdo.
#'
#' @param pares tibble com `consultor_id` e `projeto_id`.
#' @param contexto Lista de `montar_contexto_do_conteudo()`.
#' @param escores Matriz consultor por projeto, com as linhas e colunas do
#'   contexto, ou `NULL` para o piso.
#' @return Vetor inteiro de zeros e uns, um por par.
acertar_na_lista <- function(pares, contexto, escores = NULL) {
  i <- match(pares$consultor_id, contexto$consultores)
  j <- match(pares$projeto_id, contexto$projetos)
  if (is.null(escores)) {
    escores <- matrix(0, length(contexto$consultores), length(contexto$projetos))
  }

  filtro <- contexto$filtro[, j, drop = FALSE]
  chave <- contexto$chave[, j, drop = FALSE]
  escore <- escores[, j, drop = FALSE]
  focal <- cbind(i, seq_along(i))
  escore_focal <- escore[focal]
  chave_focal <- chave[focal]
  passa <- filtro[focal]

  a_frente <- sweep(escore, 2, escore_focal, ">") |
    (sweep(escore, 2, escore_focal, "==") & sweep(chave, 2, chave_focal, "<"))
  filtrados_a_frente <- colSums(filtro & a_frente)

  vagas_fora <- contexto$lista - colSums(filtro)
  fora_a_frente <- colSums(!filtro & sweep(chave, 2, chave_focal, "<"))

  as.integer(ifelse(passa, filtrados_a_frente < contexto$lista, fora_a_frente < vagas_fora))
}

#' Pontua todos os respondentes pelo fator previsto dos atributos
#'
#' Todo respondente é pontuado pelo fator previsto, inclusive os que têm
#' histórico: a ordem dentro do filtro é a do conteúdo, e o conteúdo só
#' enxerga atributo. Quem não declarou fica com zero, mas nunca passa o
#' filtro, então o valor não pesa.
#'
#' @param estado Elemento de `ajustar_als_implicito()`, com `x` e `y`.
#' @param contexto Lista de `montar_contexto_do_conteudo()`.
#' @param com_historico Consultores com alocação na estimação.
#' @param lambda_mapeamento Regularização do mapa.
#' @return Matriz consultor por projeto, com as dimensões do contexto.
escorar_mapeamento <- function(estado, contexto, com_historico, lambda_mapeamento) {
  respondentes <- contexto$atributos
  mapa <- ajustar_mapeamento(respondentes, estado$x, com_historico, lambda_mapeamento)
  previstos <- prever_fatores(mapa, respondentes)

  escores <- matrix(
    0, length(contexto$consultores), length(contexto$projetos),
    dimnames = list(contexto$consultores, contexto$projetos)
  )
  escores[rownames(previstos), ] <- previstos %*% t(estado$y)
  escores
}

#' Mede o acerto de toda configuração da grade sobre um par treino e validação
#'
#' A fatoração binária é ajustada uma vez por número de fatores e
#' regularização, e passa pelo estado de 10 iterações a caminho do de 20; o
#' mapa é refeito para cada regularização. Os escores de cada configuração
#' são avaliados e descartados em seguida, para não guardar a grade inteira
#' em memória.
#'
#' @param treino Alocações de estimação.
#' @param validacao Pares a avaliar.
#' @param contexto Lista de `montar_contexto_do_conteudo()`.
#' @param grade Lista com `fatores`, `lambda`, `iteracoes` e
#'   `lambda_mapeamento`.
#' @param semente Semente da fatoração.
#' @return tibble com uma linha por configuração, com `acertos` e `pares`.
medir_grade_do_mapeamento <- function(treino, validacao, contexto, grade, semente) {
  dados <- contexto$dados
  r <- construir_matriz_r(treino, dados$consultores, dados$projetos)
  com_historico <- unique(treino$consultor_id)

  linhas <- list()
  for (k in grade$fatores) {
    for (lambda in grade$lambda) {
      estados <- ajustar_als_implicito(
        r, peso = NULL, fatores = k, lambda = lambda,
        iteracoes = max(grade$iteracoes), semente = semente,
        registrar_em = grade$iteracoes
      )
      for (iteracoes in grade$iteracoes) {
        for (lambda_mapa in grade$lambda_mapeamento) {
          escores <- escorar_mapeamento(
            estados[[as.character(iteracoes)]], contexto, com_historico, lambda_mapa
          )
          linhas[[length(linhas) + 1]] <- tibble::tibble(
            fatores = k, lambda = lambda, iteracoes = iteracoes,
            lambda_mapeamento = lambda_mapa,
            acertos = sum(acertar_na_lista(validacao, contexto, escores)),
            pares = nrow(validacao)
          )
        }
      }
    }
  }
  purrr::list_rbind(linhas)
}

#' Validação interna do conteúdo dentro de um grupo externo
#'
#' Os quatro grupos de estimação fazem, cada um, a vez de validação contra
#' os outros três. O acerto de cada configuração é medido na união das
#' quatro validações, só nos pares de respondentes, e o grupo externo nunca
#' é tocado.
#'
#' @param contexto Lista de `montar_contexto_do_conteudo()`.
#' @param divisao Lista de `dividir_por_consultor_em_grupos()`.
#' @param grupo_externo Grupo reservado para o teste.
#' @param registro Lista de `ler_pre_registro()`.
#' @return tibble com uma linha por configuração: os hiperparâmetros,
#'   `acertos`, `pares` e `acerto`.
validar_conteudo_no_grupo <- function(contexto, divisao, grupo_externo, registro) {
  grade <- registro$conteudo$ordenacao$grade
  alocacoes <- contexto$dados$alocacoes
  grupo_da_linha <- divisao$grupo_do_consultor$grupo[
    match(alocacoes$consultor_id, divisao$grupo_do_consultor$consultor_id)
  ]
  internos <- setdiff(sort(unique(divisao$grupo_do_consultor$grupo)), grupo_externo)

  purrr::map(internos, function(h) {
    treino <- alocacoes[!grupo_da_linha %in% c(grupo_externo, h), ]
    validacao <- alocacoes[grupo_da_linha == h & alocacoes$consultor_id %in% contexto$com_atributo, ]
    medir_grade_do_mapeamento(treino, validacao, contexto, grade, registro$sementes$fatoracao)
  }) |>
    purrr::list_rbind() |>
    dplyr::summarise(
      acertos = sum(acertos), pares = sum(pares),
      .by = c(fatores, lambda, iteracoes, lambda_mapeamento)
    ) |>
    dplyr::mutate(acerto = acertos / pares)
}

#' Escolhe a configuração de maior acerto, com o desempate pré-registrado
#'
#' Empate no acerto vai para o modelo mais parcimonioso: menos fatores, maior
#' regularização da fatoração, maior regularização do mapa, menos iterações.
#'
#' @param validacao tibble de `validar_conteudo_no_grupo()`.
#' @return A linha escolhida.
escolher_configuracao_do_conteudo <- function(validacao) {
  validacao |>
    dplyr::arrange(
      dplyr::desc(acerto), fatores, dplyr::desc(lambda),
      dplyr::desc(lambda_mapeamento), iteracoes
    ) |>
    dplyr::slice(1)
}

#' Escolhe os hiperparâmetros do conteúdo em cada grupo da validação cruzada
#'
#' Só usa os grupos de estimação de cada partição. O teste, que é a
#' comparação com o piso nos grupos reservados, fica para depois de a escolha
#' ser revista.
#'
#' @param dados Lista nomeada de tibbles, de `carregar_dados()`.
#' @param registro Lista de `ler_pre_registro()`.
#' @return Lista com `grade` (o acerto de toda configuração em cada grupo) e
#'   `escolhidos` (uma linha por grupo).
selecionar_hiperparametros_do_conteudo <- function(dados, registro) {
  historico <- com_historico_da_janela(dados, janela_do_pre_registro(registro))
  contexto <- montar_contexto_do_conteudo(historico, registro)
  divisao <- dividir_por_consultor_em_grupos(
    historico$alocacoes, n_grupos = registro$divisoes$por_consultor$grupos,
    semente = registro$sementes$divisao, universo = contexto$consultores
  )

  grade <- purrr::map(seq_len(registro$divisoes$por_consultor$grupos), function(g) {
    validar_conteudo_no_grupo(contexto, divisao, g, registro) |>
      dplyr::mutate(grupo = g, .before = 1)
  }) |>
    purrr::list_rbind()

  list(
    grade = grade,
    escolhidos = grade |>
      dplyr::group_by(grupo) |>
      dplyr::group_modify(\(validacao, chave) escolher_configuracao_do_conteudo(validacao)) |>
      dplyr::ungroup()
  )
}

#' Monta a tabela dos hiperparâmetros escolhidos em cada grupo
#'
#' Reporta, ao lado da escolha, quantas configurações empataram no maior
#' acerto: quando são muitas, a escolha é do desempate por parcimônia, e não
#' da validação.
#'
#' @param selecao Lista de `selecionar_hiperparametros_do_conteudo()`.
#' @return tibble formatada para `salvar_tabela()`.
tabelar_hiperparametros_do_conteudo <- function(selecao) {
  empatadas <- selecao$grade |>
    dplyr::summarise(empatadas = sum(acerto == max(acerto)), configuracoes = dplyr::n(), .by = grupo)

  selecao$escolhidos |>
    dplyr::left_join(empatadas, by = "grupo") |>
    dplyr::transmute(
      Grupo = grupo,
      Fatores = fatores,
      `Lambda da fatoração` = formatar_decimal(lambda, 1),
      `Iterações` = iteracoes,
      `Lambda do mapa` = formatar_decimal(lambda_mapeamento, 1),
      `Acerto na validação interna` = formatar_pct(acerto),
      `Pares de validação` = formatar_inteiro(pares),
      `Configurações empatadas no máximo` = paste0(empatadas, " de ", configuracoes)
    )
}
