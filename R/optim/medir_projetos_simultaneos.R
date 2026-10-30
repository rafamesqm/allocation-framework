#' Projetos simultâneos de cada consultor em cada mês de teste
#'
#' Conta, por consultor e mês, os projetos das alocações fixas com algum dia
#' no mês e os assentos escolhidos ativos no mês. Um consultor ocupa no máximo
#' um assento por projeto, e não recebe assento num projeto em que já tem
#' carga fixa, então a soma conta projetos distintos. Entram todos os
#' consultores e meses de teste, inclusive os sem projeto. Medida descritiva,
#' acrescentada depois dos resultados das emendas 2 e 3.
#'
#' @param problema Lista de `montar_problema_da_otimizacao()`.
#' @param insumos Lista de `preparar_insumos_da_otimizacao()`.
#' @param colunas Matriz de colunas, uma solução por linha.
#' @return Matriz de contagens, um consultor e mês por linha e uma solução por
#'   coluna.
contar_projetos_simultaneos <- function(problema, insumos, colunas) {
  meses <- insumos$capacidade |> dplyr::select(consultor_id, ano_mes)
  chave <- paste(meses$consultor_id, meses$ano_mes)

  fixas <- insumos$carga_fixa$alocacoes
  fixos <- expandir_carga_em_meses(fixas, unique(meses$ano_mes)) |>
    dplyr::transmute(consultor_id = fixas$consultor_id[linha], projeto_id = fixas$projeto_id[linha], ano_mes) |>
    dplyr::summarise(n = dplyr::n_distinct(projeto_id), .by = c(consultor_id, ano_mes))
  dos_fixos <- numeric(length(chave))
  dos_fixos[match(paste(fixos$consultor_id, fixos$ano_mes), chave)] <- fixos$n

  # Cada par fica ativo nos meses do seu assento, na linha do seu consultor.
  ativos <- problema$pares |>
    dplyr::mutate(par = dplyr::row_number()) |>
    dplyr::select(par, assento_id, consultor_id) |>
    dplyr::inner_join(insumos$meses_dos_assentos |> dplyr::select(assento_id, ano_mes), by = "assento_id", relationship = "many-to-many") |>
    dplyr::mutate(linha = match(paste(consultor_id, ano_mes), chave))
  stopifnot(!anyNA(ativos$linha))
  presenca <- Matrix::sparseMatrix(
    i = ativos$linha, j = ativos$par, x = 1, dims = c(length(chave), nrow(problema$pares))
  )
  escolhas <- Matrix::sparseMatrix(
    i = as.vector(t(colunas)), j = rep(seq_len(nrow(colunas)), each = ncol(colunas)),
    x = 1, dims = c(nrow(problema$pares), nrow(colunas))
  )
  dos_fixos + as.matrix(presenca %*% escolhas)
}

#' Resumo dos projetos simultâneos de cada solução
#'
#' O máximo, o percentil 95 e o número de consultores e meses com mais de três
#' projetos, sobre todos os consultores e meses de teste.
#'
#' @inheritParams contar_projetos_simultaneos
#' @return tibble com `maximo`, `percentil_95` e `acima_de_3`, uma linha por
#'   solução.
resumir_projetos_simultaneos <- function(problema, insumos, colunas) {
  contagem <- contar_projetos_simultaneos(problema, insumos, colunas)
  tibble::tibble(
    maximo = apply(contagem, 2, max),
    percentil_95 = apply(contagem, 2, stats::quantile, probs = 0.95, names = FALSE),
    acima_de_3 = colSums(contagem > 3)
  )
}

#' Projetos simultâneos nas soluções das emendas 2 e 3
#'
#' A realizada, as linhas da tabela de payoff e os três problemas do item 10.c
#' com e sem o limite individual, o item 10.c de afinidade com o L da
#' sensibilidade e os pontos da fronteira registrada. Nada é resolvido de novo.
#'
#' @param execucao Lista de `executar_nsga2_registrado()` da semente principal.
#' @param problema Lista de `montar_problema_da_otimizacao()`.
#' @param insumos Lista de `preparar_insumos_da_otimizacao()`.
#' @param payoff,limitados,exatos_da_realizada,sensibilidade Soluções exatas
#'   sem limite, com o limite calibrado e com o L da sensibilidade.
#' @param limite O L calibrado.
#' @return Lista com `referencias` (`solucao`, `limite` e o resumo; `limite`
#'   é `NA` sem limite individual) e `pontos` (o resumo de cada ponto da
#'   fronteira).
medir_projetos_simultaneos_da_avaliacao <- function(execucao, problema, insumos, payoff, exatos_da_realizada,
                                                    limitados, sensibilidade, limite) {
  com_nomes <- function(solucoes, prefixo) stats::setNames(solucoes, paste(prefixo, names(solucoes)))
  solucoes <- c(
    list(realizada = solucao_realizada(problema)),
    com_nomes(payoff$solucoes, "payoff,"),
    com_nomes(exatos_da_realizada$solucoes, "item 10.c,"),
    com_nomes(limitados$payoff$solucoes, "payoff,"),
    com_nomes(limitados$realizada$solucoes, "item 10.c,"),
    list("item 10.c, afinidade" = sensibilidade$x)
  )
  limites <- c(
    NA, rep(NA, length(payoff$solucoes) + length(exatos_da_realizada$solucoes)),
    rep(limite, length(limitados$payoff$solucoes) + length(limitados$realizada$solucoes)),
    sensibilidade$limite
  )
  com_solucao <- !purrr::map_lgl(solucoes, is.null)
  referencias <- dplyr::bind_cols(
    tibble::tibble(solucao = names(solucoes)[com_solucao], limite = limites[com_solucao]),
    resumir_projetos_simultaneos(problema, insumos, colunas_das_solucoes(problema, solucoes[com_solucao]))
  )

  fronteira <- extrair_fronteira(execucao)
  colunas <- colunas_dos_genes(preparar_codificacao(problema), fronteira$genes)
  list(referencias = referencias, pontos = resumir_projetos_simultaneos(problema, insumos, colunas))
}
