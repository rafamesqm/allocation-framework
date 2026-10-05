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
#' **A duração é uma fração da janela do projeto, não um sorteio
#' independente.** Na fonte real o span do projeto é a união das alocações
#' (ver docs/mapeamento_schema.md, Sexta rodada), então as duas durações não
#' são independentes: as medianas reais coincidem (18 dias dos dois lados) e
#' as médias ficam na razão 51,6/68,8 = 0,75, ou seja, uma alocação típica
#' cobre a maior parte do projeto em que está. Sortear a duração de alocação
#' por conta própria e truncar pelo projeto ignora isso e encolhe a duração
#' de forma grosseira (testado: mediana cai para 5 dias contra 18 do alvo).
#'
#' A fração de cobertura é `Beta(concentracao_cobertura, 1)`, equivalente a
#' `runif()^(1/a)`: massa crescente em direção à cobertura total, um
#' parâmetro só. O default 3 foi escolhido por reproduzir exatamente a razão
#' real de médias (0,75), que é o alvo mais confiável aqui: a razão sobrevive
#' à distorção de cauda que a própria duração de projeto carrega (ver
#' `gerar_projetos()`, casamento de quantis com teto), enquanto comparar
#' médias absolutas não sobreviveria. Com esse default o P75 de duração de
#' alocação sai em 59 dias contra 60 reais.
#'
#' Limitação documentada: mediana (13 contra 18) e P25 (3 contra 4) saem
#' abaixo do alvo. O resto do desvio vem do arredondamento em projetos
#' curtos, onde a fração multiplica uma janela de poucos dias e o piso de 1
#' dia domina, não do mecanismo de cobertura.
#'
#' @param data_inicio_projeto Vetor de `Date`, início do projeto de cada
#'   alocação (uma posição por alocação, já repetido).
#' @param duracao_projeto_dias Vetor inteiro, duração do projeto de cada
#'   alocação, em dias. Nunca menor que 1 (ver `gerar_projetos()`).
#' @param concentracao_cobertura Parâmetro `a` da `Beta(a, 1)` que sorteia a
#'   fração da janela do projeto coberta pela alocação. Quanto maior, mais a
#'   alocação tende a cobrir o projeto inteiro.
#' @return tibble com `data_inicio`, `duracao_dias` e `data_fim`, uma linha
#'   por alocação, na mesma ordem da entrada.
gerar_periodo_alocacao <- function(data_inicio_projeto,
                                    duracao_projeto_dias,
                                    concentracao_cobertura = 3) {
  n <- length(data_inicio_projeto)

  fracao_coberta <- stats::runif(n)^(1 / concentracao_cobertura)
  duracao_dias <- pmax(round(duracao_projeto_dias * fracao_coberta), 1)

  folga_dias <- duracao_projeto_dias - duracao_dias
  data_inicio <- data_inicio_projeto + floor(stats::runif(n) * (folga_dias + 1))

  tibble::tibble(
    data_inicio = data_inicio,
    duracao_dias = duracao_dias,
    data_fim = data_inicio + duracao_dias
  )
}
