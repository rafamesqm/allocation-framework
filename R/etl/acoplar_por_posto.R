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

#' Acopla valor de contrato e esforço estimado ao esforço de fato alocado
#'
#' Roda depois de `gerar_alocacoes()` de propósito, e não dentro de
#' `gerar_projetos()`: o esforço alocado só existe depois que a alocação foi
#' gerada. Não há ciclo, porque `gerar_alocacoes()` usa do projeto apenas
#' setor e janela de datas, nunca contrato nem esforço.
#'
#' **Projeto sem alocação fica de fora do acoplamento.** Acoplar pelo posto
#' colocaria todos eles no piso da distribuição de contrato, o que diria que
#' projeto ainda não equipado é projeto pequeno. Não é: ele é projeto sobre o
#' qual não há informação de esforço, e o que corresponde à ausência de
#' informação é ausência de acoplamento. Eles ficam com valores sorteados
#' entre os que sobraram, preservando a marginal do conjunto todo.
#'
#' O esforço estimado acopla mais forte que o contrato porque é uma
#' estimativa da mesma grandeza, enquanto o preço tem margem de negociação
#' por cima do esforço. As duas forças são suposição declarada.
#'
#' @param projetos tibble de `gerar_projetos()`.
#' @param alocacoes tibble de `gerar_alocacoes()`.
#' @param forca_contrato,forca_esforco Intensidade de cada acoplamento.
#' @param dias_por_mes Divisor que converte duração em dias para meses.
#' @return `projetos` com `valor_contrato` e `esforco_estimado` permutados.
acoplar_projetos_ao_esforco <- function(projetos, alocacoes,
                                         forca_contrato = 0.7,
                                         forca_esforco = 0.85,
                                         dias_por_mes = 365.25 / 12) {
  horas_por_projeto <- alocacoes |>
    dplyr::mutate(horas = horas_mes * duracao_dias / dias_por_mes) |>
    dplyr::summarise(horas_alocadas = sum(horas), .by = projeto_id)

  com_horas <- projetos$projeto_id %in% horas_por_projeto$projeto_id
  horas <- horas_por_projeto$horas_alocadas[
    match(projetos$projeto_id[com_horas], horas_por_projeto$projeto_id)
  ]

  permutar <- function(valores, forca) {
    saida <- valores
    saida[com_horas] <- acoplar_por_posto(valores[com_horas], horas, forca)
    saida[!com_horas] <- sample(valores[!com_horas])
    saida
  }

  projetos |>
    dplyr::mutate(
      valor_contrato = permutar(valor_contrato, forca_contrato),
      esforco_estimado = permutar(esforco_estimado, forca_esforco)
    )
}
