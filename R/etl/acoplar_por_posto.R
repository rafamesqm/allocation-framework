#' Acopla uma variável a outra por posto, preservando a marginal exatamente
#'
#' Existe para resolver um conflito que aparece toda vez que um gerador tem
#' peça calibrada: a marginal de `valor_contrato` está casada por quantis
#' contra o real e não pode ser mexida, mas a variável precisa passar a
#' depender do esforço efetivamente alocado, com o qual hoje não tem relação
#' nenhuma.
#'
#' A saída é uma **permutação** dos valores de entrada, então a distribuição
#' marginal sai idêntica, valor a valor, e nenhuma calibração se perde. O que
#' muda é só quem recebe qual valor. É a ideia de Iman e Conover (1982) para
#' induzir correlação de posto entre variáveis cujas marginais estão fixadas.
#'
#' **O defeito que motivou.** O bloco II.5 mediu, na Camada 3, correlação de
#' 0,05 entre o esforço estimado de um projeto e as horas de fato alocadas
#' nele, e de 0,01 entre custo e valor de contrato em log. Tudo era sorteado
#' de forma independente, então um projeto de contrato pequeno podia receber
#' uma equipe grande por muito tempo, e a margem estrutural do candidato (d)
#' saía sem sentido: 32% dos projetos com margem negativa e décimo percentil
#' em menos doze vezes o valor do contrato.
#'
#' Nada disso é plausível. Contrato maior compra mais esforço, e essa é a
#' relação mais elementar do negócio de consultoria. A **existência** do
#' acoplamento é estrutura reproduzida; a `forca` é suposição declarada, e não
#' há agregado real para calibrá-la porque o cruzamento entre contrato e horas
#' alocadas nunca foi extraído enquanto havia acesso.
#'
#' `forca` igual a 1 ordena os valores exatamente pela referência, o que seria
#' determinismo implausível. Abaixo de 1, a ordenação é feita sobre a
#' referência perturbada por ruído, o que produz uma relação forte e ainda
#' assim ruidosa, que é o que se observa na prática.
#'
#' @param valores Vetor a permutar, cuja marginal deve ser preservada.
#' @param referencia Vetor com o qual `valores` deve passar a correlacionar.
#' @param forca Entre 0 e 1. Peso do posto da referência contra ruído.
#' @return `valores` permutado, com a mesma marginal.
acoplar_por_posto <- function(valores, referencia, forca = 0.7) {
  stopifnot(length(valores) == length(referencia))
  stopifnot(forca >= 0, forca <= 1)

  n <- length(valores)
  if (n < 2) return(valores)

  # Postos na escala 0 a 1 dos dois lados, para a mistura com o ruído ficar
  # na mesma unidade qualquer que seja a escala original da referência.
  posto_referencia <- rank(referencia, ties.method = "random") / n
  ruido <- stats::runif(n)

  ordem_desejada <- order(forca * posto_referencia + (1 - forca) * ruido)

  saida <- numeric(n)
  saida[ordem_desejada] <- sort(valores)
  saida
}
#' Acopla o valor de contrato ao esforço alocado e deriva o esforço estimado
#'
#' Roda depois de `gerar_alocacoes()` de propósito, e não dentro de
#' `gerar_projetos()`: o esforço alocado só existe depois que a alocação foi
#' gerada. Não há ciclo, porque `gerar_alocacoes()` usa do projeto apenas
#' setor e janela de datas, nunca contrato nem esforço.
#'
#' **O esforço estimado é derivado, e não sorteado.** Ele é a soma das horas
#' mensais das alocações do projeto, a necessidade de capacidade por mês que
#' a Camada 3 confronta com a capacidade `k`. A versão anterior o sorteava
#' de uma Normal calibrada por âncoras que eram, na verdade, de `horas_mes`
#' por pessoa, com mediana 120 e desvio 66,9: uma grandeza por consultor
#' tomada como se fosse por projeto. Além do erro de unidade, o truncamento
#' em zero deixava 3,6% dos projetos com esforço exatamente nulo, um atributo
#' que nenhum projeto real tem. Não há âncora real de esforço por projeto, e
#' derivar é a forma de ele ser coerente com as alocações por construção, em
#' vez de por um acoplamento de força arbitrária.
#'
#' **Projeto sem alocação recebe esforço sorteado entre os dos projetos
#' alocados.** Ele não tem alocação de onde derivar, mas é justamente o
#' projeto que o modelo de otimização precisa equipar, e por isso não pode
#' ficar sem necessidade de capacidade. O sorteio reproduz a distribuição dos
#' alocados, que é a estimativa disponível antes do equipamento.
#'
#' **O contrato continua acoplado por posto ao total de horas alocadas.**
#' Acoplar o projeto sem alocação pelo posto o colocaria no piso da
#' distribuição de contrato, o que diria que projeto ainda não equipado é
#' projeto pequeno. Não é: é projeto sobre o qual não há informação de
#' esforço, e eles ficam com valores sorteados entre os que sobraram,
#' preservando a marginal do conjunto todo. A força é suposição declarada.
#'
#' @param projetos tibble de `gerar_projetos()`.
#' @param alocacoes tibble de `gerar_alocacoes()`.
#' @param forca_contrato Intensidade do acoplamento do contrato.
#' @param dias_por_mes Divisor que converte duração em dias para meses.
#' @return `projetos` com `valor_contrato` permutado e `esforco_estimado`
#'   acrescentado logo depois dele.
acoplar_projetos_ao_esforco <- function(projetos, alocacoes,
                                         forca_contrato = 0.7,
                                         dias_por_mes = 365.25 / 12) {
  esforco_por_projeto <- alocacoes |>
    dplyr::mutate(horas = horas_mes * duracao_dias / dias_por_mes) |>
    dplyr::summarise(
      horas_alocadas = sum(horas),
      horas_mes_equipe = sum(horas_mes),
      .by = projeto_id
    )

  posicao <- match(projetos$projeto_id, esforco_por_projeto$projeto_id)
  com_horas <- !is.na(posicao)
  horas <- esforco_por_projeto$horas_alocadas[posicao[com_horas]]

  valor_contrato <- projetos$valor_contrato
  valor_contrato[com_horas] <- acoplar_por_posto(
    valor_contrato[com_horas], horas, forca_contrato
  )
  valor_contrato[!com_horas] <- sample(valor_contrato[!com_horas])

  esforco_estimado <- esforco_por_projeto$horas_mes_equipe[posicao]
  esforco_estimado[!com_horas] <- sample(
    esforco_estimado[com_horas], sum(!com_horas), replace = TRUE
  )

  projetos |>
    dplyr::mutate(
      valor_contrato = .env$valor_contrato,
      esforco_estimado = .env$esforco_estimado,
      .after = valor_contrato
    )
}
