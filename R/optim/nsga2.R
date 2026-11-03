#' Prepara a codificação do NSGA-II a partir do problema montado
#'
#' Um gene por assento, na ordem da montagem; o valor do gene é a posição do
#' ocupante na lista de candidatos do assento, também na ordem da montagem.
#' Como os pares do problema estão ordenados por assento, a coluna do par
#' escolhido é o início do assento mais o gene menos um.
#'
#' A avaliação usa as próprias linhas de capacidade da matriz montada, e por
#' isso é exata: é a mesma conta de `avaliar_solucao()`, feita para a
#' população inteira de uma vez.
#'
#' @param problema Lista de `montar_problema_da_otimizacao()`.
#' @return Lista com `n_assentos`, `inicio`, `n_candidatos`, `projeto`,
#'   `consultor`, `assentos_do_projeto`, `capacidade`, `limite`, `s_hat`,
#'   `custo`, `ajuste_da_sobrecarga` e `gene_realizado`.
preparar_codificacao <- function(problema) {
  pares <- problema$pares
  assentos <- unique(pares$assento_id)
  if (!identical(pares$coluna, seq_len(nrow(pares))) || is.unsorted(match(pares$assento_id, assentos))) {
    stop("Os pares precisam estar em ordem de assento para a codificação.", call. = FALSE)
  }
  inicio <- match(assentos, pares$assento_id)
  n_candidatos <- as.integer(table(factor(pares$assento_id, levels = assentos)))
  projeto <- match(pares$projeto_id[inicio], unique(pares$projeto_id[inicio]))
  linhas <- which(problema$tipo_da_restricao == "capacidade por consultor e mês")
  realizado <- pares$coluna[pares$realizado]

  list(
    n_assentos = length(assentos),
    inicio = inicio,
    n_candidatos = n_candidatos,
    projeto = projeto,
    consultor = match(pares$consultor_id, unique(pares$consultor_id)),
    assentos_do_projeto = split(seq_along(assentos), projeto),
    capacidade = problema$restricoes[linhas, seq_len(nrow(pares)), drop = FALSE],
    limite = problema$lado_direito[linhas],
    s_hat = pares$s_hat,
    custo = problema$objetivos$custo[seq_len(nrow(pares))],
    # Sobrecarga atribuível = soma das folgas + constante - carga fixa sozinha.
    ajuste_da_sobrecarga = problema$constante_sobrecarga - problema$sobrecarga_da_carga_fixa,
    gene_realizado = realizado[match(assentos, pares$assento_id[realizado])] - inicio + 1L
  )
}

#' Colunas dos pares escolhidos por cada indivíduo
#'
#' @param codificacao Lista de `preparar_codificacao()`.
#' @param genes Matriz inteira, um indivíduo por linha, um assento por coluna.
#' @return Matriz de mesmas dimensões com a coluna do par.
colunas_dos_genes <- function(codificacao, genes) {
  genes + matrix(codificacao$inicio - 1L, nrow(genes), ncol(genes), byrow = TRUE)
}

#' Avalia os três objetivos e a violação de uma população
#'
#' @param codificacao Lista de `preparar_codificacao()`.
#' @param genes Matriz inteira, um indivíduo por linha.
#' @return Lista com `objetivos` (matriz com `afinidade`, `custo` e
#'   `sobrecarga`, a atribuível aos assentos, nos sentidos originais) e
#'   `violacao` (número de duplicidades de consultor e projeto).
avaliar_populacao <- function(codificacao, genes) {
  n <- nrow(genes)
  colunas <- colunas_dos_genes(codificacao, genes)
  escolha <- Matrix::sparseMatrix(
    i = as.vector(colunas), j = rep(seq_len(n), ncol(genes)), x = 1,
    dims = c(length(codificacao$s_hat), n)
  )
  carga <- as.matrix(codificacao$capacidade %*% escolha)
  sobrecarga <- colSums(pmax(carga - codificacao$limite, 0)) + codificacao$ajuste_da_sobrecarga

  chave <- matrix(codificacao$consultor[colunas], n) * (max(codificacao$projeto) + 1L) +
    matrix(codificacao$projeto, n, ncol(genes), byrow = TRUE)
  list(
    objetivos = cbind(
      afinidade = rowSums(matrix(codificacao$s_hat[colunas], n)),
      custo = rowSums(matrix(codificacao$custo[colunas], n)),
      sobrecarga = sobrecarga
    ),
    violacao = apply(chave, 1, \(k) sum(duplicated(k)))
  )
}

#' Repara as duplicidades de consultor e projeto de um indivíduo
#'
#' Percorre os assentos na ordem da montagem: num par repetido de consultor e
#' projeto, o primeiro assento mantém o ocupante, e cada seguinte recebe um
#' candidato sorteado entre os do assento ainda não usados no projeto. Sem
#' candidato livre, o gene fica como está e a violação permanece.
#'
#' @param codificacao Lista de `preparar_codificacao()`.
#' @param genes Vetor inteiro de um indivíduo.
#' @return `genes` reparado.
reparar_individuo <- function(codificacao, genes) {
  colunas <- genes + codificacao$inicio - 1L
  ocupante <- codificacao$consultor[colunas]
  repetidos <- which(duplicated(cbind(ocupante, codificacao$projeto)))
  for (assento in repetidos) {
    do_projeto <- codificacao$assentos_do_projeto[[codificacao$projeto[assento]]]
    usados <- ocupante[setdiff(do_projeto, assento)]
    candidatos <- codificacao$consultor[codificacao$inicio[assento] + seq_len(codificacao$n_candidatos[assento]) - 1L]
    livres <- which(!candidatos %in% usados)
    if (length(livres) > 0) {
      genes[assento] <- livres[sample.int(length(livres), 1)]
      ocupante[assento] <- candidatos[genes[assento]]
    }
  }
  genes
}

#' Matriz de dominação com restrições (Deb et al., 2002)
#'
#' `i` domina `j` se tem violação menor; com as duas viáveis, se não é pior em
#' nenhum objetivo e é melhor em algum. Os objetivos são minimizados.
#'
#' @param objetivos Matriz a minimizar, um indivíduo por linha.
#' @param violacao Vetor de violações.
#' @return Matriz lógica `n x n`, `TRUE` em `[i, j]` quando `i` domina `j`.
calcular_dominacao <- function(objetivos, violacao) {
  nao_pior <- TRUE
  melhor <- FALSE
  for (k in seq_len(ncol(objetivos))) {
    nao_pior <- nao_pior & outer(objetivos[, k], objetivos[, k], "<=")
    melhor <- melhor | outer(objetivos[, k], objetivos[, k], "<")
  }
  viaveis <- outer(violacao == 0, violacao == 0, "&")
  outer(violacao, violacao, "<") | (viaveis & nao_pior & melhor)
}

#' Ordenação não dominada rápida (Deb et al., 2002)
#'
#' @param dominacao Matriz de `calcular_dominacao()`.
#' @return Vetor inteiro com a fronteira de cada indivíduo, a partir de 1.
ordenar_nao_dominados <- function(dominacao) {
  dominado_por <- colSums(dominacao)
  fronteira <- integer(nrow(dominacao))
  atual <- which(dominado_por == 0)
  k <- 1L
  while (length(atual) > 0) {
    fronteira[atual] <- k
    dominado_por <- dominado_por - colSums(dominacao[atual, , drop = FALSE])
    dominado_por[fronteira > 0] <- -1L
    atual <- which(dominado_por == 0)
    k <- k + 1L
  }
  fronteira
}

#' Distância de aglomeração dentro de uma fronteira (Deb et al., 2002)
#'
#' Os extremos de cada objetivo recebem distância infinita; os demais, a soma
#' das distâncias normalizadas aos vizinhos.
#'
#' @param objetivos Matriz dos indivíduos de uma fronteira.
#' @return Vetor numérico.
calcular_aglomeracao <- function(objetivos) {
  n <- nrow(objetivos)
  distancia <- numeric(n)
  if (n <= 2) return(rep(Inf, n))
  for (k in seq_len(ncol(objetivos))) {
    ordem <- order(objetivos[, k])
    valores <- objetivos[ordem, k]
    amplitude <- valores[n] - valores[1]
    distancia[ordem[c(1, n)]] <- Inf
    if (amplitude > 0) {
      interior <- ordem[2:(n - 1)]
      distancia[interior] <- distancia[interior] + (valores[3:n] - valores[1:(n - 2)]) / amplitude
    }
  }
  distancia
}

#' Fronteira e distância de aglomeração de uma população
#'
#' @param objetivos Matriz a minimizar.
#' @param violacao Vetor de violações.
#' @return Lista com `fronteira` e `aglomeracao`.
classificar_populacao <- function(objetivos, violacao) {
  fronteira <- ordenar_nao_dominados(calcular_dominacao(objetivos, violacao))
  aglomeracao <- numeric(length(fronteira))
  for (k in unique(fronteira)) {
    membros <- which(fronteira == k)
    aglomeracao[membros] <- calcular_aglomeracao(objetivos[membros, , drop = FALSE])
  }
  list(fronteira = fronteira, aglomeracao = aglomeracao)
}

#' Converte os objetivos para minimização: afinidade de sinal trocado
#'
#' @param objetivos Matriz com `afinidade`, `custo` e `sobrecarga`.
#' @return Matriz a minimizar.
para_minimizacao <- function(objetivos) {
  objetivos[, "afinidade"] <- -objetivos[, "afinidade"]
  objetivos
}

#' Executa o NSGA-II sobre o problema montado
#'
#' Fiel a Deb et al. (2002): torneio binário pela fronteira e pela distância de
#' aglomeração, cruzamento uniforme por assento, mutação por gene, reparo das
#' duplicidades e sobrevivência elitista de pais e filhos reunidos. Os
#' detalhes que o artigo não fixa estão na emenda 1 do pré-registro da
#' otimização. Todo sorteio sai do gerador do R iniciado uma vez pela semente.
#'
#' @param problema Lista de `montar_problema_da_otimizacao()`.
#' @param tamanho Tamanho da população, par.
#' @param geracoes Número de gerações.
#' @param probabilidade_de_cruzamento Probabilidade de cruzar um par.
#' @param probabilidade_de_mutacao Probabilidade de mutação por gene.
#' @param semente Semente da execução.
#' @param registrar_a_cada Intervalo de gerações para guardar a primeira
#'   fronteira, como histórico de convergência.
#' @return Lista com `genes`, `objetivos`, `violacao` e `fronteira` da
#'   população final, `historico` (a primeira fronteira viável a cada
#'   intervalo) e `segundos`.
executar_nsga2 <- function(problema, tamanho, geracoes, probabilidade_de_cruzamento,
                           probabilidade_de_mutacao, semente, registrar_a_cada = 100) {
  stopifnot(tamanho %% 2 == 0)
  inicio <- proc.time()[["elapsed"]]
  codificacao <- preparar_codificacao(problema)
  n_assentos <- codificacao$n_assentos
  sortear_genes <- function(assentos) ceiling(stats::runif(length(assentos)) * codificacao$n_candidatos[assentos])
  reparar <- function(genes) {
    for (i in seq_len(nrow(genes))) genes[i, ] <- reparar_individuo(codificacao, genes[i, ])
    genes
  }

  withr::with_seed(semente, {
    # População inicial: a alocação realizada e o restante ao acaso, reparado.
    sorteados <- matrix(sortear_genes(rep(seq_len(n_assentos), each = tamanho - 1)), tamanho - 1)
    genes <- rbind(codificacao$gene_realizado, reparar(sorteados))
    avaliacao <- avaliar_populacao(codificacao, genes)
    classe <- classificar_populacao(para_minimizacao(avaliacao$objetivos), avaliacao$violacao)
    historico <- list()

    for (geracao in seq_len(geracoes)) {
      # Torneio binário pelo operador de comparação de aglomeração.
      a <- sample.int(tamanho, tamanho, replace = TRUE)
      b <- sample.int(tamanho, tamanho, replace = TRUE)
      a_vence <- classe$fronteira[a] < classe$fronteira[b] |
        (classe$fronteira[a] == classe$fronteira[b] & classe$aglomeracao[a] >= classe$aglomeracao[b])
      pais <- genes[ifelse(a_vence, a, b), , drop = FALSE]

      # Cruzamento uniforme em pares consecutivos.
      filhos <- pais
      primeiro <- seq(1, tamanho, by = 2)
      cruza <- stats::runif(length(primeiro)) < probabilidade_de_cruzamento
      troca <- matrix(stats::runif(length(primeiro) * n_assentos) < 0.5, length(primeiro)) & cruza
      filhos[primeiro, ][troca] <- pais[primeiro + 1, ][troca]
      filhos[primeiro + 1, ][troca] <- pais[primeiro, ][troca]

      # Mutação: cada gene é sorteado de novo na lista do assento.
      muta <- which(matrix(stats::runif(tamanho * n_assentos) < probabilidade_de_mutacao, tamanho))
      filhos[muta] <- sortear_genes(col(filhos)[muta])
      filhos <- reparar(filhos)

      # Sobrevivência elitista sobre pais e filhos reunidos.
      reunidos <- rbind(genes, filhos)
      avaliacao_filhos <- avaliar_populacao(codificacao, filhos)
      objetivos <- rbind(avaliacao$objetivos, avaliacao_filhos$objetivos)
      violacao <- c(avaliacao$violacao, avaliacao_filhos$violacao)
      classe_reunida <- classificar_populacao(para_minimizacao(objetivos), violacao)
      ficam <- order(classe_reunida$fronteira, -classe_reunida$aglomeracao)[seq_len(tamanho)]

      genes <- reunidos[ficam, , drop = FALSE]
      avaliacao <- list(objetivos = objetivos[ficam, , drop = FALSE], violacao = violacao[ficam])
      # O torneio seguinte usa a fronteira e a aglomeração da sobrevivência.
      classe <- list(fronteira = classe_reunida$fronteira[ficam], aglomeracao = classe_reunida$aglomeracao[ficam])

      if (geracao %% registrar_a_cada == 0) {
        historico[[length(historico) + 1]] <- list(
          geracao = geracao,
          objetivos = avaliacao$objetivos[classe$fronteira == 1 & avaliacao$violacao == 0, , drop = FALSE]
        )
      }
    }
  })

  list(
    genes = genes, objetivos = avaliacao$objetivos, violacao = avaliacao$violacao,
    fronteira = classe$fronteira, historico = historico,
    segundos = proc.time()[["elapsed"]] - inicio
  )
}

#' Fronteira final do NSGA-II: não dominados, viáveis e distintos
#'
#' @param execucao Lista de `executar_nsga2()`.
#' @return Lista com `objetivos` (tibble) e `genes` dos pontos distintos.
extrair_fronteira <- function(execucao) {
  primeira <- which(execucao$fronteira == 1 & execucao$violacao == 0)
  distintos <- primeira[!duplicated(round(execucao$objetivos[primeira, , drop = FALSE], 9))]
  list(
    objetivos = tibble::as_tibble(execucao$objetivos[distintos, , drop = FALSE]),
    genes = execucao$genes[distintos, , drop = FALSE]
  )
}

#' Executa o NSGA-II com os parâmetros da emenda 1
#'
#' @param problema Lista de `montar_problema_da_otimizacao()`.
#' @param emenda Emenda 1 do pré-registro da otimização.
#' @param semente Semente da execução, uma das registradas na emenda.
#' @return Lista de `executar_nsga2()` com `semente`.
executar_nsga2_registrado <- function(problema, emenda, semente) {
  registradas <- c(emenda$sementes$principal, emenda$sementes$variabilidade)
  if (!semente %in% registradas) {
    stop("Semente ", semente, " fora das registradas na emenda.", call. = FALSE)
  }
  n_assentos <- nrow(problema$pares[problema$pares$realizado, ])
  if (emenda$operadores$probabilidade_de_mutacao_por_gene != paste0("1/", n_assentos)) {
    stop("A mutação registrada não corresponde ao número de assentos.", call. = FALSE)
  }
  execucao <- executar_nsga2(
    problema,
    tamanho = emenda$populacao$tamanho,
    geracoes = emenda$orcamento$geracoes,
    probabilidade_de_cruzamento = emenda$operadores$probabilidade_de_cruzamento,
    probabilidade_de_mutacao = 1 / n_assentos,
    semente = semente
  )
  execucao$semente <- semente
  execucao
}
