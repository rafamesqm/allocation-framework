#' Remove arestas duplicadas de um pareamento de configuration model via
#' double edge swap, preservando exatamente a sequência de graus
#'
#' O stub-matching bipartido (Newman, 2010, "Networks: An Introduction", cap.
#' 13; van der Hofstad, 2016, "Random Graphs and Complex Networks", vol. 1)
#' é, por construção, um multigrafo: pode gerar o mesmo par
#' (consultor, projeto) mais de uma vez. Uma linha de alocação repetida não
#' faz sentido de negócio, mas simplesmente descartar a cópia extra
#' distorceria o grau de quem tomou a duplicata.
#'
#' A técnica padrão da literatura de grafos aleatórios para eliminar
#' duplicatas sem alterar o grau de ninguém é o double edge swap: toma duas
#' arestas (c1, p1) e (c2, p2) e troca as pontas para (c1, p2) e (c2, p1).
#' Como cada nó envolvido perde uma aresta e ganha outra, seu grau não muda.
#' É a mesma lógica por trás do algoritmo de swap para matrizes binárias com
#' margem fixa (Verhelst, 2008, "An efficient MCMC algorithm to sample
#' binary matrices with fixed marginals", Psychometrika) e do "curveball"
#' popularizado depois na literatura de redes ecológicas (Strona et al.,
#' 2014, "A fast and unbiased procedure to randomize ecological binary
#' matrices with fixed row and column totals", Nature Communications). Ver
#' também Fosdick, Larremore, Nishimura & Ugander (2018, "Configuring Random
#' Graph Models with Fixed Degree Sequences", SIAM Review) para a comparação
#' entre stub-matching bruto, rejeição e reparo por swap.
#'
#' A troca só é aceita se (1) os quatro nós envolvidos são de fato distintos
#' onde precisam ser (`c1 != c2`, `p1 != p2`), e (2) nenhuma das duas arestas
#' novas já existe no grafo (senão a troca trocaria uma duplicata por
#' outra). Quando não se acha troca válida em `max_tentativas`, a duplicata é
#' descartada e o grau realizado fica ligeiramente abaixo do sorteado para os
#' nós envolvidos: evita loop infinito nos casos raros (grafo pequeno ou
#' muito denso) em que nenhuma troca válida existe.
#'
#' A checagem de aresta repetida é O(n) por tentativa; para as poucas
#' dezenas de duplicatas esperadas na escala deste projeto (milhares de
#' arestas), o custo total é irrelevante. Não otimizado com estrutura de
#' dados mais sofisticada: corretude e clareza antes de velocidade.
#'
#' @param arestas tibble com `consultor_id`, `projeto_id`, uma linha por
#'   aresta, pode ter pares repetidos.
#' @param max_tentativas Tentativas de troca por duplicata antes de desistir
#'   e descartar a cópia excedente.
#' @return tibble de arestas sem pares repetidos, com o mesmo grau por nó
#'   (exceto o desvio raro documentado acima).
remover_duplicatas_por_double_edge_swap <- function(arestas, max_tentativas = 50) {
  chave <- paste(arestas$consultor_id, arestas$projeto_id, sep = "|||")
  indices_duplicados <- which(duplicated(chave))

  for (i in indices_duplicados) {
    if (sum(chave == chave[i]) <= 1) next # já resolvido como efeito colateral de outra troca

    resolvido <- FALSE
    tentativa <- 0

    while (!resolvido && tentativa < max_tentativas) {
      tentativa <- tentativa + 1

      pool <- setdiff(seq_along(chave), i)
      j <- if (length(pool) == 1) pool else sample(pool, size = 1)

      c1 <- arestas$consultor_id[i]
      p1 <- arestas$projeto_id[i]
      c2 <- arestas$consultor_id[j]
      p2 <- arestas$projeto_id[j]

      swap_valido <- c1 != c2 && p1 != p2 &&
        !any(chave == paste(c1, p2, sep = "|||")) &&
        !any(chave == paste(c2, p1, sep = "|||"))

      if (swap_valido) {
        arestas$projeto_id[i] <- p2
        arestas$projeto_id[j] <- p1
        chave[i] <- paste(c1, p2, sep = "|||")
        chave[j] <- paste(c2, p1, sep = "|||")
        resolvido <- TRUE
      }
    }
  }

  arestas[!duplicated(chave), ]
}
