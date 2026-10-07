#' Sorteia início, duração e fim de cada alocação, dentro da janela do projeto
#'
#' Sem eixo temporal por alocação não existe `calendario_alocacao` nem
#' `capacidade_periodo`, e o vetor `k` da Camada 3 (capacidade por pessoa e
#' período) não é construível: a soma de `pct_alocacao` por consultor vira um
#' total sem denominador, que aparenta superalocação generalizada. A fonte
#' real tem as duas datas por alocação (`vw_gtac_cltv_alocacao`, campos
#' `Data de início` e `Data de fim`), então a ausência era lacuna do gerador,
#' não do dado.
#'
#' **A duração é uma fração da janela do projeto**, `Beta(a, b)`, o que
#' mantém toda alocação dentro do projeto em que está. Os dois parâmetros são
#' calibrados contra os quartis reais da duração de alocação, 4, 18 e 60 dias
#' (`docs/mapeamento_schema.md`, Sexta rodada), e não contra a duração do
#' projeto: é a duração da alocação que tem âncora própria.
#'
#' **A calibração anterior quebrou em silêncio, e é por isso que esta existe.**
#' Ela usava `Beta(3, 1)`, com cobertura média de 75% do projeto, acertada
#' numa época em que a duração de projeto vinha do span das alocações, com
#' mediana de 18 dias. Quando a segunda consulta comercial passou a duração
#' de projeto para a contratual, com mediana de 81 dias, a fração de 75%
#' ficou igual e a duração das alocações subiu junto: mediana de 56 dias
#' contra 18 reais, o que inflava a vigência simultânea e a ocupação. Os
#' parâmetros abaixo saem de `calibrar_cobertura_alocacao()` sobre as
#' durações de projeto atuais, e um teste falha se a duração das alocações
#' voltar a se afastar das âncoras.
#'
#' Fora da calibração, a média (51,1 contra 51,6 reais), o desvio (92,1
#' contra 86,8) e a assimetria (4,03 contra 3,97) também batem, o que é a
#' evidência de que a forma, e não só os três pontos, está certa.
#'
#' @param data_inicio_projeto Vetor de `Date`, início do projeto de cada
#'   alocação (uma posição por alocação, já repetido).
#' @param duracao_projeto_dias Vetor inteiro, duração do projeto de cada
#'   alocação, em dias. Nunca menor que 1 (ver `gerar_projetos()`).
#' @param forma_a,forma_b Parâmetros da `Beta(a, b)` que sorteia a fração da
#'   janela do projeto coberta pela alocação.
#' @return tibble com `data_inicio`, `duracao_dias` e `data_fim`, uma linha
#'   por alocação, na mesma ordem da entrada.
gerar_periodo_alocacao <- function(data_inicio_projeto,
                                    duracao_projeto_dias,
                                    forma_a = 0.426,
                                    forma_b = 0.689) {
  n <- length(data_inicio_projeto)

  fracao_coberta <- stats::rbeta(n, forma_a, forma_b)
  duracao_dias <- pmax(round(duracao_projeto_dias * fracao_coberta), 1)

  folga_dias <- duracao_projeto_dias - duracao_dias
  data_inicio <- data_inicio_projeto + floor(stats::runif(n) * (folga_dias + 1))

  tibble::tibble(
    data_inicio = data_inicio,
    duracao_dias = duracao_dias,
    data_fim = data_inicio + duracao_dias
  )
}

#' Calibra a fração de cobertura contra os quartis reais da duração de alocação
#'
#' A duração de uma alocação é `d * X`, com `d` a duração do projeto e
#' `X ~ Beta(a, b)`. Sobre um conjunto de projetos, a duração das alocações
#' é a mistura dessas distribuições, e o quantil dela sai da CDF média, que é
#' suave nos parâmetros. Os dois parâmetros são ajustados aos três quartis
#' por mínimos quadrados na escala logarítmica, partindo de uma grade para
#' não depender de um único ponto inicial.
#'
#' Não roda na geração, porque leva segundos e as réplicas de estabilidade a
#' chamariam cem vezes. É a função que produz os parâmetros padrão de
#' `gerar_periodo_alocacao()`, e fica aqui para eles serem auditáveis.
#'
#' @param duracao_projeto_dias Duração do projeto de cada alocação.
#' @param quartis_alvo Quartis reais da duração de alocação, em dias.
#' @return Lista com `forma_a`, `forma_b` e o erro do ajuste.
calibrar_cobertura_alocacao <- function(duracao_projeto_dias,
                                        quartis_alvo = c(4, 18, 60)) {
  probabilidades <- c(0.25, 0.5, 0.75)

  quantil_da_mistura <- function(p, a, b) {
    cdf_menos_p <- function(x) {
      mean(stats::pbeta(pmin(x / duracao_projeto_dias, 1), a, b)) - p
    }
    stats::uniroot(cdf_menos_p, c(0, max(duracao_projeto_dias)), tol = 1e-6)$root
  }

  erro <- function(parametros) {
    a <- exp(parametros[1])
    b <- exp(parametros[2])
    quantis <- vapply(probabilidades, quantil_da_mistura, numeric(1), a = a, b = b)
    sum(log(quantis / quartis_alvo)^2)
  }

  inicios <- expand.grid(a = log(c(0.3, 0.7, 1.5)), b = log(c(1, 3, 8)))
  ajustes <- lapply(seq_len(nrow(inicios)), function(i) {
    stats::optim(unlist(inicios[i, ]), erro, control = list(reltol = 1e-10))
  })
  melhor <- ajustes[[which.min(vapply(ajustes, `[[`, numeric(1), "value"))]]

  list(
    forma_a = exp(melhor$par[[1]]),
    forma_b = exp(melhor$par[[2]]),
    erro = melhor$value
  )
}
