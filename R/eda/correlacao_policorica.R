#' Probabilidade acumulada da normal bivariada padrão
#'
#' Implementada aqui, e não trazida de pacote, por uma razão de
#' reprodutibilidade e não de gosto: `mvtnorm` está instalado nesta máquina
#' mas **não está no `renv.lock`**, e a regra 2 do CLAUDE.md exige que o
#' auditor rode a pipeline depois de um `renv::restore()`. Acrescentar
#' dependência ao lock por uma função de vinte linhas seria pior negócio que
#' escrevê-la.
#'
#' Usa a identidade clássica que escreve a acumulada bivariada como a
#' univariada mais uma integral em `rho`, que é suave e bem comportada em
#' todo o intervalo e por isso rende bem com a quadratura adaptativa de
#' `stats::integrate()`:
#'
#' `Phi2(h, k, rho) = Phi(h) Phi(k) + integral de 0 a rho de
#' exp(-(h^2 - 2 t h k + k^2) / (2 (1 - t^2))) / (2 pi sqrt(1 - t^2)) dt`
#'
#' @param h,k Limites superiores, na escala normal padrão.
#' @param rho Correlação, entre -1 e 1 exclusive.
#' @return Probabilidade acumulada conjunta.
pnorm_bivariada <- function(h, k, rho) {
  if (is.infinite(h) || is.infinite(k)) {
    if (h == -Inf || k == -Inf) return(0)
    if (h == Inf) return(stats::pnorm(k))
    if (k == Inf) return(stats::pnorm(h))
  }

  if (abs(rho) < 1e-10) {
    return(stats::pnorm(h) * stats::pnorm(k))
  }

  integrando <- function(t) {
    exp(-(h^2 - 2 * t * h * k + k^2) / (2 * (1 - t^2))) / (2 * pi * sqrt(1 - t^2))
  }

  stats::pnorm(h) * stats::pnorm(k) +
    stats::integrate(integrando, lower = 0, upper = rho, rel.tol = 1e-8)$value
}

#' Correlação policórica entre duas variáveis ordinais
#'
#' **Por que não a de Pearson.** Tratar uma nota ordinal de três níveis como
#' se fosse 1, 2 e 3 supõe que a distância de ABAIXO para DENTRO é igual à de
#' DENTRO para ACIMA, o que nada no dado sustenta, e atenua a correlação para
#' baixo por causa da discretização grosseira. A policórica supõe, em vez
#' disso, que cada item é o corte de uma variável contínua normal latente, e
#' estima a correlação **entre as latentes**. Para item de três categorias a
#' diferença entre as duas não é decorativa.
#'
#' A suposição de normalidade latente é forte e fica declarada: ela é a
#' contrapartida de não supor espaçamento igual, e é a escolha padrão na
#' literatura de psicometria justamente para escalas de poucas categorias.
#'
#' Estima em dois passos, que é o procedimento usual: os limiares de cada
#' variável saem das marginais, por `qnorm` das proporções acumuladas, e
#' então só `rho` é otimizado, maximizando a verossimilhança multinomial da
#' tabela de contingência.
#'
#' @param x,y Vetores ordinais de mesmo comprimento.
#' @return Correlação policórica estimada.
correlacao_policorica <- function(x, y) {
  tabela <- table(x, y)
  if (nrow(tabela) < 2 || ncol(tabela) < 2) return(NA_real_)

  limiares <- function(margem) {
    acumulado <- cumsum(margem) / sum(margem)
    c(-Inf, stats::qnorm(utils::head(acumulado, -1)), Inf)
  }
  a <- limiares(rowSums(tabela))
  b <- limiares(colSums(tabela))

  log_verossimilhanca <- function(rho) {
    total <- 0
    for (i in seq_len(nrow(tabela))) {
      for (j in seq_len(ncol(tabela))) {
        if (tabela[i, j] == 0) next
        # Probabilidade do retângulo, por inclusão e exclusão nos cantos.
        p <- pnorm_bivariada(a[i + 1], b[j + 1], rho) -
          pnorm_bivariada(a[i], b[j + 1], rho) -
          pnorm_bivariada(a[i + 1], b[j], rho) +
          pnorm_bivariada(a[i], b[j], rho)
        total <- total + tabela[i, j] * log(max(p, 1e-12))
      }
    }
    total
  }

  stats::optimize(
    log_verossimilhanca, interval = c(-0.999, 0.999), maximum = TRUE
  )$maximum
}

#' Matriz de correlações policóricas entre as colunas de uma matriz ordinal
#'
#' @param m Matriz numérica com categorias ordinais em cada coluna.
#' @return Matriz de correlações, com 1 na diagonal.
matriz_policorica <- function(m) {
  n <- ncol(m)
  saida <- diag(n)
  dimnames(saida) <- list(colnames(m), colnames(m))

  for (i in seq_len(n - 1)) {
    for (j in seq(i + 1, n)) {
      r <- correlacao_policorica(m[, i], m[, j])
      saida[i, j] <- r
      saida[j, i] <- r
    }
  }

  saida
}

#' Alfa padronizado a partir de uma matriz de correlações
#'
#' Com a matriz policórica na entrada, é o **alfa ordinal** de Zumbo,
#' Gadermann e Zeisser (2007), que é a forma recomendada para itens de poucas
#' categorias. Com a de Pearson, é o alfa padronizado usual, que sobre
#' ordinal de três níveis é um limite inferior.
#'
#' Reportar os dois lado a lado é deliberado: a distância entre eles mede
#' quanto da confiabilidade estava sendo escondida pela discretização, e não
#' obriga o leitor a aceitar a suposição de normalidade latente para
#' acompanhar o argumento.
#'
#' @param correlacoes Matriz de correlações entre itens.
#' @return Alfa padronizado.
alfa_de_matriz <- function(correlacoes) {
  n <- ncol(correlacoes)
  media <- mean(correlacoes[upper.tri(correlacoes)])

  (n * media) / (1 + (n - 1) * media)
}
