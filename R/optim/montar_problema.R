#' Monta o problema inteiro misto da otimização, sem resolvê-lo
#'
#' As decisões são binárias por par de assento e candidato: `y[a, c] = 1`
#' quando o consultor `c` ocupa o assento `a` em todos os meses dele. A
#' formulação continua em trios de consultor, projeto e mês, porque é por
#' consultor e mês que a carga se soma: cada restrição de capacidade junta os
#' assentos ativos naquele mês.
#'
#' Restrições:
#'
#' * cada assento tem exatamente um ocupante;
#' * cada consultor ocupa no máximo um assento por projeto;
#' * para cada consultor e mês em que ele é candidato a algum assento ativo,
#'   `carga fixa + soma da carga dos assentos - sobrecarga <= capacidade`, com
#'   a sobrecarga contínua e não negativa. Minimizada, ela vale
#'   `max(0, carga - capacidade)`.
#'
#' Onde o consultor não é candidato a nada no mês, a sobrecarga da carga fixa
#' é constante e fica fora das variáveis, em `constante_sobrecarga`.
#'
#' @param insumos Lista com `assentos`, `meses_dos_assentos`, `carga_fixa`
#'   (com `por_mes`), `capacidade` e `elegibilidade`, de
#'   `preparar_insumos_da_otimizacao()`.
#' @param medida `"percentual"` ou `"horas"`, a medida de carga.
#' @return Lista com `pares`, `sobrecarga`, `restricoes` (matriz esparsa),
#'   `tipo_da_restricao`, `sentido`, `lado_direito`, `binaria`, `objetivos`,
#'   `sentido_dos_objetivos`, `constante_sobrecarga`,
#'   `sobrecarga_da_carga_fixa` e `medida`.
montar_problema_da_otimizacao <- function(insumos, medida = c("percentual", "horas")) {
  medida <- match.arg(medida)
  coluna_carga <- if (medida == "percentual") "carga_pct" else "carga_horas"
  coluna_capacidade <- if (medida == "percentual") "capacidade_pct" else "capacidade_horas"

  pares <- insumos$elegibilidade |>
    dplyr::mutate(coluna = dplyr::row_number())
  n_pares <- nrow(pares)

  # Carga de cada par em cada mês ativo do assento.
  carga_dos_pares <- pares |>
    dplyr::select(coluna, assento_id, consultor_id) |>
    dplyr::inner_join(
      insumos$meses_dos_assentos |> dplyr::select(assento_id, ano_mes, carga = dplyr::all_of(coluna_carga)),
      by = "assento_id", relationship = "many-to-many"
    )

  fixa_e_capacidade <- insumos$capacidade |>
    dplyr::select(consultor_id, ano_mes, capacidade = dplyr::all_of(coluna_capacidade)) |>
    dplyr::left_join(
      insumos$carga_fixa$por_mes |> dplyr::select(consultor_id, ano_mes, fixa = dplyr::all_of(coluna_carga)),
      by = c("consultor_id", "ano_mes")
    ) |>
    dplyr::mutate(fixa = dplyr::coalesce(fixa, 0))

  sobrecarga <- carga_dos_pares |>
    dplyr::distinct(consultor_id, ano_mes) |>
    dplyr::arrange(consultor_id, ano_mes) |>
    dplyr::inner_join(fixa_e_capacidade, by = c("consultor_id", "ano_mes")) |>
    dplyr::mutate(coluna = n_pares + dplyr::row_number())
  sem_variavel <- fixa_e_capacidade |>
    dplyr::anti_join(sobrecarga, by = c("consultor_id", "ano_mes"))

  # Linhas: assentos, um por projeto, capacidade.
  assentos <- unique(pares$assento_id)
  linha_assento <- match(pares$assento_id, assentos)

  repetidos <- pares |>
    dplyr::add_count(consultor_id, projeto_id) |>
    dplyr::filter(n > 1) |>
    dplyr::mutate(grupo = paste(consultor_id, projeto_id))
  grupos <- unique(repetidos$grupo)
  linha_projeto <- length(assentos) + match(repetidos$grupo, grupos)

  base_capacidade <- length(assentos) + length(grupos)
  chave_sobrecarga <- paste(sobrecarga$consultor_id, sobrecarga$ano_mes)
  linha_capacidade <- base_capacidade + match(paste(carga_dos_pares$consultor_id, carga_dos_pares$ano_mes), chave_sobrecarga)

  n_linhas <- base_capacidade + nrow(sobrecarga)
  restricoes <- Matrix::sparseMatrix(
    i = c(linha_assento, linha_projeto, linha_capacidade, base_capacidade + seq_len(nrow(sobrecarga))),
    j = c(pares$coluna, repetidos$coluna, carga_dos_pares$coluna, sobrecarga$coluna),
    x = c(rep(1, n_pares), rep(1, nrow(repetidos)), carga_dos_pares$carga, rep(-1, nrow(sobrecarga))),
    dims = c(n_linhas, n_pares + nrow(sobrecarga))
  )

  # O custo do assento é o custo por hora vezes as horas dele nos meses de
  # teste, pela mesma expansão da carga.
  horas_do_assento <- tapply(insumos$meses_dos_assentos$carga_horas, insumos$meses_dos_assentos$assento_id, sum)
  zeros_sobrecarga <- rep(0, nrow(sobrecarga))
  list(
    pares = pares,
    sobrecarga = sobrecarga,
    restricoes = restricoes,
    tipo_da_restricao = rep(
      c("um ocupante por assento", "um assento por consultor e projeto", "capacidade por consultor e mês"),
      c(length(assentos), length(grupos), nrow(sobrecarga))
    ),
    sentido = c(rep("==", length(assentos)), rep("<=", length(grupos) + nrow(sobrecarga))),
    lado_direito = c(rep(1, length(assentos) + length(grupos)), sobrecarga$capacidade - sobrecarga$fixa),
    binaria = c(rep(TRUE, n_pares), rep(FALSE, nrow(sobrecarga))),
    objetivos = list(
      afinidade = c(pares$s_hat, zeros_sobrecarga),
      custo = c(pares$custo_hora * horas_do_assento[pares$assento_id], zeros_sobrecarga),
      sobrecarga = c(rep(0, n_pares), rep(1, nrow(sobrecarga)))
    ),
    sentido_dos_objetivos = c(afinidade = "max", custo = "min", sobrecarga = "min"),
    constante_sobrecarga = sum(pmax(sem_variavel$fixa - sem_variavel$capacidade, 0)),
    sobrecarga_da_carga_fixa = sum(pmax(fixa_e_capacidade$fixa - fixa_e_capacidade$capacidade, 0)),
    medida = medida
  )
}

#' Completa uma escolha de ocupantes com a sobrecarga mínima que ela implica
#'
#' @param problema Lista de `montar_problema_da_otimizacao()`.
#' @param escolhidos Vetor lógico ou 0/1 com um valor por par.
#' @return Vetor numérico com um valor por variável do problema.
completar_solucao <- function(problema, escolhidos) {
  n_pares <- nrow(problema$pares)
  stopifnot(length(escolhidos) == n_pares)
  y <- as.numeric(escolhidos)
  linhas <- which(problema$tipo_da_restricao == "capacidade por consultor e mês")
  carga <- as.vector(problema$restricoes[linhas, seq_len(n_pares), drop = FALSE] %*% y)
  c(y, pmax(carga - problema$lado_direito[linhas], 0))
}

#' A alocação realizada, como solução do problema
#'
#' @param problema Lista de `montar_problema_da_otimizacao()`.
#' @return Vetor numérico com um valor por variável.
solucao_realizada <- function(problema) {
  completar_solucao(problema, as.numeric(problema$pares$realizado))
}

#' Avalia uma solução: viabilidade e os três objetivos
#'
#' A sobrecarga total soma a constante dos consultores e meses sem variável;
#' a atribuível aos assentos desconta dela a sobrecarga que a carga fixa
#' sozinha já produz, e é a que vale zero sob o limite rígido.
#'
#' @param problema Lista de `montar_problema_da_otimizacao()`.
#' @param x Vetor numérico de `completar_solucao()`.
#' @param tolerancia Folga numérica admitida nas restrições.
#' @return Lista com `viavel`, `violacoes`, `afinidade`, `custo`,
#'   `sobrecarga` e `sobrecarga_atribuivel`.
avaliar_solucao <- function(problema, x, tolerancia = 1e-9) {
  lado_esquerdo <- as.vector(problema$restricoes %*% x)
  violada <- ifelse(
    problema$sentido == "==",
    abs(lado_esquerdo - problema$lado_direito) > tolerancia,
    lado_esquerdo > problema$lado_direito + tolerancia
  )
  binarias <- x[problema$binaria]
  inteira <- all(abs(binarias - round(binarias)) <= tolerancia & binarias >= -tolerancia & binarias <= 1 + tolerancia)
  nao_negativa <- all(x[!problema$binaria] >= -tolerancia)

  sobrecarga <- sum(problema$objetivos$sobrecarga * x) + problema$constante_sobrecarga
  list(
    viavel = !any(violada) && inteira && nao_negativa,
    violacoes = table(problema$tipo_da_restricao[violada]),
    afinidade = sum(problema$objetivos$afinidade * x),
    custo = sum(problema$objetivos$custo * x),
    sobrecarga = sobrecarga,
    sobrecarga_atribuivel = sobrecarga - problema$sobrecarga_da_carga_fixa
  )
}
