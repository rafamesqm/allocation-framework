#' Gera avaliações de desempenho sintéticas para um subconjunto de alocações
#'
#' Nem toda alocação passa por um ciclo de avaliação (a estrutura real é
#' semestral, ver docs/mapeamento_schema.md), por isso sorteia um
#' subconjunto de `alocacoes` para receber avaliação. Cada avaliação
#' selecionada replica a estrutura real reconstruída sem acesso a banco: 3
#' etapas (individual, gestor às cegas, gestor final) sobre os mesmos 14
#' itens (9 subcompetências + 5 itens de Performance, ver
#' docs/dicionario_de_dados.md), cada item com nota ordinal de 3 níveis
#' (ABAIXO/DENTRO/ACIMA) e um comentário de texto livre de tamanho variável
#' (ver `gerar_comentario_avaliacao()`).
#'
#' Viés de leniência plantado deliberadamente: a autoavaliação (etapa
#' individual) usa pesos bem mais inclinados para ACIMA do que a avaliação
#' às cegas do gestor, refletindo o único exemplo real visto (8 de 9
#' subcompetências "acima do esperado" na autoavaliação, ver
#' docs/mapeamento_schema.md) e o *self-other rating bias* documentado na
#' literatura de avaliação de desempenho. A etapa gestor final não é
#' independente: parte da nota às cegas e é calibrada em direção à
#' autoavaliação (ver `calibrar_nota_final()`), simulando o efeito de ver os
#' dois lado a lado antes de definir a nota final.
#'
#' O texto real da rubrica, que muda por `nivel_senioridade` do avaliado
#' (ver docs/mapeamento_schema.md), não é gerado: só a nota ordinal e o
#' comentário são modelados, a rubrica em si é um artefato de interface, não
#' um dado a reproduzir. Por isso a nota não é deslocada por nível de
#' senioridade: não há evidência real de que a distribuição de nota (e não
#' só o texto da rubrica) varie por nível.
#'
#' @param alocacoes tibble de `gerar_alocacoes()`.
#' @param fracao_com_avaliacao Fração das alocações que recebem avaliação.
#' @param pesos_individual,pesos_gestor_cegas Pesos ABAIXO/DENTRO/ACIMA para
#'   a autoavaliação e para a avaliação às cegas do gestor.
#' @param prob_calibracao_puxa_individual Probabilidade de a etapa gestor
#'   final se aproximar um nível da autoavaliação (ver
#'   `calibrar_nota_final()`).
#' @param semente Inteiro para reprodutibilidade.
#' @return tibble longo: `consultor_id`, `projeto_id`, `item`,
#'   `categoria_item`, `etapa`, `nota`, `comentario`.
gerar_avaliacoes_desempenho <- function(alocacoes,
                                         fracao_com_avaliacao = 0.5,
                                         pesos_individual = c(ABAIXO = 0.02, DENTRO = 0.20, ACIMA = 0.78),
                                         pesos_gestor_cegas = c(ABAIXO = 0.15, DENTRO = 0.60, ACIMA = 0.25),
                                         prob_calibracao_puxa_individual = 0.3,
                                         semente = 1) {
  set.seed(semente)

  subcompetencias <- obter_subcompetencias_avaliacao()
  itens_performance <- c(
    "Resultado do Projeto", "Qualidade da Entrega",
    "Relacionamento com Cliente", "Comercial", "Valor para Empresa"
  )
  itens <- tibble::tibble(
    item = c(subcompetencias, itens_performance),
    categoria_item = c(
      rep("subcompetencia", length(subcompetencias)),
      rep("performance", length(itens_performance))
    )
  )
  n_itens <- nrow(itens)

  n_avaliacoes <- round(nrow(alocacoes) * fracao_com_avaliacao)
  selecionadas <- alocacoes[sample.int(nrow(alocacoes), size = n_avaliacoes), c("consultor_id", "projeto_id")]

  base <- dplyr::bind_cols(
    selecionadas[rep(seq_len(n_avaliacoes), each = n_itens), ],
    itens[rep(seq_len(n_itens), times = n_avaliacoes), ]
  )

  niveis_nota <- c("ABAIXO", "DENTRO", "ACIMA")
  n_total <- nrow(base)

  nota_individual <- sample(niveis_nota, n_total, replace = TRUE, prob = pesos_individual)
  nota_gestor_cegas <- sample(niveis_nota, n_total, replace = TRUE, prob = pesos_gestor_cegas)
  nota_gestor_final <- calibrar_nota_final(nota_gestor_cegas, nota_individual, prob_calibracao_puxa_individual)

  dplyr::bind_rows(
    dplyr::mutate(base, etapa = "individual", nota = nota_individual),
    dplyr::mutate(base, etapa = "gestor_cegas", nota = nota_gestor_cegas),
    dplyr::mutate(base, etapa = "gestor_final", nota = nota_gestor_final)
  ) |>
    dplyr::mutate(comentario = gerar_comentario_avaliacao(dplyr::n()))
}
