#' Mede quem ocupa as listas do híbrido, sobre todas as listas de `ŝ`
#'
#' Duas medidas, sobre as listas dos projetos da janela inteira, e não só
#' as dos projetos de teste, que são as do achado W3.4.2:
#'
#' * a fração das primeiras `lista` posições ocupada por cada quadrante,
#'   com o quadrante do consultor diante de cada projeto;
#' * o número médio de listas em que entra um consultor de cada quadrante,
#'   com o quadrante global: ter alguma alocação na janela e ter declarado
#'   setor. Quem não declarou setor entra sem filtro, em toda lista para a
#'   qual tem histórico; quem declarou entra só nas listas do próprio setor.
#'
#' @param s_hat tibble de `calcular_s_hat()`.
#' @param dados Lista nomeada de tibbles.
#' @param registro Lista de `ler_pre_registro()`.
#' @return Lista com `topo` (fração das primeiras posições por quadrante),
#'   `listas_por_consultor` (média por quadrante global) e `projetos`.
medir_ocupacao_das_listas <- function(s_hat, dados, registro) {
  historico <- com_historico_da_janela(dados, janela_do_pre_registro(registro))
  lista <- registro$metrica$lista_decisoria
  candidatos <- s_hat[!is.na(s_hat$posicao), ]
  projetos <- dplyr::n_distinct(s_hat$projeto_id)

  topo <- candidatos |>
    dplyr::filter(posicao <= lista) |>
    dplyr::count(quadrante) |>
    dplyr::mutate(fracao = n / sum(n))

  consultores <- historico$consultores$consultor_id
  quadrante_global <- classificar_quadrante(
    consultores %in% historico$alocacoes$consultor_id,
    consultores %in% historico$experiencia_setorial$consultor_id
  )
  entradas <- table(factor(candidatos$consultor_id, levels = consultores))
  listas_por_consultor <- tibble::tibble(
    quadrante = quadrante_global, listas = as.numeric(entradas)
  ) |>
    dplyr::summarise(consultores = dplyr::n(), listas_media = mean(listas), .by = quadrante) |>
    dplyr::mutate(fracao_das_listas = listas_media / projetos)

  list(topo = topo, listas_por_consultor = listas_por_consultor, projetos = projetos)
}

#' Mede a distribuição do tamanho do filtro nos pares de teste do conteúdo
#'
#' Para cada par de respondente que passa o filtro, o tamanho do filtro é o
#' número de respondentes que declararam o setor do projeto. Lista de 20 só
#' ordena alguma coisa quando o filtro tem mais de 20 candidatos: abaixo
#' disso, quem passa o filtro entra na lista qualquer que seja a ordem.
#'
#' @param dados Lista nomeada de tibbles.
#' @param registro Lista de `ler_pre_registro()`.
#' @return Lista com `maior_setor`, `pares_que_passam`, `fracao_ate_lista`,
#'   `fracao_acima_da_lista` e `distribuicao` (pares por tamanho do filtro).
medir_tamanho_do_filtro <- function(dados, registro) {
  historico <- com_historico_da_janela(dados, janela_do_pre_registro(registro))
  contexto <- montar_contexto_do_conteudo(historico, registro)
  lista <- registro$metrica$lista_decisoria
  pares <- historico$alocacoes[historico$alocacoes$consultor_id %in% contexto$com_atributo, ]
  i <- match(pares$consultor_id, contexto$consultores)
  j <- match(pares$projeto_id, contexto$projetos)
  passa <- contexto$filtro[cbind(i, j)]
  tamanho <- colSums(contexto$filtro)[j][passa]

  list(
    maior_setor = max(colSums(contexto$filtro)),
    pares_que_passam = length(tamanho),
    fracao_ate_lista = mean(tamanho <= lista),
    fracao_acima_da_lista = mean(tamanho > lista),
    distribuicao = tibble::tibble(tamanho_do_filtro = tamanho) |> dplyr::count(tamanho_do_filtro, name = "pares")
  )
}
