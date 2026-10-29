#' Intervalo de escore de Wilson para uma proporção
#'
#' @param sucessos,n Contagens.
#' @param z Quantil da normal do nível desejado.
#' @return Vetor com o limite inferior e o superior.
calcular_intervalo_wilson <- function(sucessos, n, z = stats::qnorm(0.975)) {
  p <- sucessos / n
  centro <- (p + z^2 / (2 * n)) / (1 + z^2 / n)
  meia_largura <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / (1 + z^2 / n)
  c(centro - meia_largura, centro + meia_largura)
}

#' Intervalo de escore híbrido de Newcombe para a diferença pareada
#'
#' Método 10 de Newcombe (1998), para a diferença entre duas proporções
#' medidas nos mesmos pares, como o acerto do modelo e o do piso. Combina os
#' intervalos de Wilson de cada proporção com a correlação entre os acertos,
#' o que respeita o pareamento sem a aproximação normal da diferença. É
#' descritivo: o teste que decide é o McNemar exato.
#'
#' A tabela pareada tem `a` pares em que os dois acertam, `b` em que só o
#' primeiro acerta, `c` em que só o segundo acerta e `d` em que nenhum
#' acerta. A diferença é `(b - c) / n`.
#'
#' @param a,b,c,d Contagens da tabela pareada.
#' @param nivel Nível de confiança.
#' @return Lista com `diferenca`, `inferior` e `superior`.
calcular_intervalo_newcombe_pareado <- function(a, b, c, d, nivel = 0.95) {
  # Em numérico: o produto das quatro margens estoura o inteiro do R já com
  # algumas centenas de pares.
  a <- as.numeric(a)
  b <- as.numeric(b)
  c <- as.numeric(c)
  d <- as.numeric(d)
  n <- a + b + c + d
  z <- stats::qnorm(1 - (1 - nivel) / 2)
  p1 <- (a + b) / n
  p2 <- (a + c) / n
  w1 <- calcular_intervalo_wilson(a + b, n, z)
  w2 <- calcular_intervalo_wilson(a + c, n, z)

  denominador <- sqrt((a + b) * (c + d) * (a + c) * (b + d))
  phi <- if (denominador == 0) 0 else (a * d - b * c) / denominador

  diferenca <- p1 - p2
  inferior <- diferenca - sqrt(
    (p1 - w1[1])^2 - 2 * phi * (p1 - w1[1]) * (w2[2] - p2) + (w2[2] - p2)^2
  )
  superior <- diferenca + sqrt(
    (w1[2] - p1)^2 - 2 * phi * (w1[2] - p1) * (p2 - w2[1]) + (p2 - w2[1])^2
  )
  list(diferenca = diferenca, inferior = inferior, superior = superior)
}

#' Intervalo de Newcombe a partir dos acertos pareados
#'
#' @param acerto_modelo,acerto_piso Vetores de zeros e uns, pareados.
#' @param nivel Nível de confiança.
#' @return Lista de `calcular_intervalo_newcombe_pareado()`.
intervalo_da_diferenca_pareada <- function(acerto_modelo, acerto_piso, nivel = 0.95) {
  calcular_intervalo_newcombe_pareado(
    a = sum(acerto_modelo == 1 & acerto_piso == 1),
    b = sum(acerto_modelo == 1 & acerto_piso == 0),
    c = sum(acerto_modelo == 0 & acerto_piso == 1),
    d = sum(acerto_modelo == 0 & acerto_piso == 0),
    nivel = nivel
  )
}
