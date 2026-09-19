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
#' **As notas derivam de uma qualidade latente compartilhada**, e não são
#' sorteadas item a item. A versão anterior sorteava cada item de cada etapa
#' de forma independente, e o bloco II.5 mostrou o que isso produzia: o
#' primeiro componente principal explicava 8,5% da variância entre os 14
#' itens, a correlação média entre eles era -0,002 e o alfa de Cronbach dava
#' negativo. O rótulo não tinha sinal nenhum sobre a pessoa, e nenhum modelo
#' colaborativo teria o que aprender dele. Ver
#' `construir_qualidade_latente()` para o que nessa decomposição é estrutura
#' reproduzida e o que é suposição declarada.
#'
#' O ruído do avaliador é bem maior na autoavaliação que na avaliação às
#' cegas, de propósito: é o que reproduz a concordância baixa entre
#' autoavaliação e gestor que as meta-análises de *self-other rating
#' agreement* reportam, sem precisar torná-las independentes, que era o
#' defeito anterior. Duas pessoas avaliando a mesma alocação discordam muito,
#' mas não observam realidades diferentes.
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
#'   a autoavaliação e para a avaliação às cegas do gestor. As marginais
#'   saem exatamente nestes pesos, porque o corte do latente é por quantil.
#' @param prob_calibracao_puxa_individual Probabilidade de a etapa gestor
#'   final se aproximar um nível da autoavaliação (ver
#'   `calibrar_nota_final()`).
#' @param sd_ruido_individual,sd_ruido_gestor Desvio do ruído de cada
#'   avaliador sobre a qualidade latente. O da autoavaliação é maior porque
#'   é o que produz a concordância baixa entre as duas etapas.
#' @param com_comentario Se `FALSE`, omite a coluna `comentario`. O texto
#'   livre responde por praticamente todo o tempo de geração desta tabela e
#'   não entra em nenhuma grandeza medida, então a simulação de Monte Carlo
#'   do bloco I.0 o dispensa. A tabela versionada continua trazendo o
#'   comentário, que é parte do esquema real.
#' @param semente Inteiro para reprodutibilidade.
#' @return tibble longo: `consultor_id`, `projeto_id`, `item`,
#'   `categoria_item`, `etapa`, `nota` e, se pedido, `comentario`.
gerar_avaliacoes_desempenho <- function(alocacoes,
                                         fracao_com_avaliacao = 0.5,
                                         pesos_individual = c(ABAIXO = 0.02, DENTRO = 0.20, ACIMA = 0.78),
                                         pesos_gestor_cegas = c(ABAIXO = 0.15, DENTRO = 0.60, ACIMA = 0.25),
                                         prob_calibracao_puxa_individual = 0.3,
                                         sd_ruido_individual = 1.4,
                                         sd_ruido_gestor = 0.5,
                                         com_comentario = TRUE,
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

  n_total <- nrow(base)

  # A qualidade latente é a mesma para as duas etapas independentes: os dois
  # avaliadores observam a mesma alocação. O que os separa é o ruído de cada
  # um, e é só isso que os faz discordar.
  qualidade <- construir_qualidade_latente(
    consultor_id = base$consultor_id,
    alocacao_id = paste(base$consultor_id, base$projeto_id),
    item = base$item
  )

  nota_individual <- cortar_latente_em_nota(
    qualidade + stats::rnorm(n_total, sd = sd_ruido_individual), pesos_individual
  )
  nota_gestor_cegas <- cortar_latente_em_nota(
    qualidade + stats::rnorm(n_total, sd = sd_ruido_gestor), pesos_gestor_cegas
  )
  nota_gestor_final <- calibrar_nota_final(nota_gestor_cegas, nota_individual, prob_calibracao_puxa_individual)

  avaliacoes <- dplyr::bind_rows(
    dplyr::mutate(base, etapa = "individual", nota = nota_individual),
    dplyr::mutate(base, etapa = "gestor_cegas", nota = nota_gestor_cegas),
    dplyr::mutate(base, etapa = "gestor_final", nota = nota_gestor_final)
  )

  if (!com_comentario) {
    return(avaliacoes)
  }

  dplyr::mutate(avaliacoes, comentario = gerar_comentario_avaliacao(dplyr::n()))
}
