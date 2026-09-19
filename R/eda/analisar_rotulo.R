#' Converte a nota ordinal de três níveis em número
#'
#' Ponto único da conversão, porque tratar ordinal como número é escolha, não
#' evidência, e escolha espalhada por vários lugares diverge. A escala é
#' igualmente espaçada, que é a suposição mais simples e a que a literatura
#' de avaliação costuma adotar; ela não é neutra, porque supõe que a distância
#' de ABAIXO para DENTRO é igual à de DENTRO para ACIMA, e nada no dado
#' sustenta isso. Fica declarada aqui, em um lugar só.
#'
#' @param nota Vetor de texto com os três níveis.
#' @return Vetor numérico de 1 a 3.
nota_numerica <- function(nota) {
  escala <- c(ABAIXO = 1, DENTRO = 2, ACIMA = 3)
  unname(escala[nota])
}

#' Mede a cobertura do rótulo explícito sobre a matriz de interações
#'
#' O rótulo explícito é a avaliação de desempenho, e ele só existe onde houve
#' ciclo de avaliação. A cobertura decide quanto de `R` tem valor de célula
#' informativo e quanto teria de cair para feedback implícito.
#'
#' @param dados Lista nomeada de tibbles, de `carregar_dados()`.
#' @return Lista com contagens e frações dos três denominadores.
medir_cobertura_do_rotulo <- function(dados) {
  pares_avaliados <- dplyr::n_distinct(
    paste(dados$avaliacoes_desempenho$consultor_id,
          dados$avaliacoes_desempenho$projeto_id)
  )

  list(
    pares_avaliados = pares_avaliados,
    pares_totais = nrow(dados$alocacoes),
    cobertura_pares = pares_avaliados / nrow(dados$alocacoes),
    consultores_avaliados = dplyr::n_distinct(dados$avaliacoes_desempenho$consultor_id),
    cobertura_consultores = dplyr::n_distinct(dados$avaliacoes_desempenho$consultor_id) /
      nrow(dados$consultores),
    projetos_avaliados = dplyr::n_distinct(dados$avaliacoes_desempenho$projeto_id),
    cobertura_projetos = dplyr::n_distinct(dados$avaliacoes_desempenho$projeto_id) /
      nrow(dados$projetos)
  )
}

#' Mede a divergência entre as etapas de avaliação
#'
#' As três etapas avaliam a **mesma** pessoa na **mesma** alocação sobre os
#' **mesmos** itens, então a comparação entre elas é pareada e direta. Se
#' divergem muito, dizer "o rótulo é a avaliação de desempenho" é ambíguo, e
#' a etapa adotada precisa ser declarada como parte da definição do rótulo.
#'
#' Reporta nível médio e concordância separadamente porque são coisas
#' distintas: dois avaliadores podem concordar na ordem e discordar no nível,
#' que é exatamente o caso da leniência da autoavaliação, e é a ordem que
#' importa para um recomendador.
#'
#' @param avaliacoes tibble de avaliações, longa por item e etapa.
#' @return Lista com `por_etapa`, `pares` e a tabela larga usada.
medir_divergencia_entre_avaliadores <- function(avaliacoes) {
  largo <- avaliacoes |>
    dplyr::select(consultor_id, projeto_id, item, etapa, nota) |>
    tidyr::pivot_wider(names_from = etapa, values_from = nota)

  etapas <- setdiff(names(largo), c("consultor_id", "projeto_id", "item"))

  por_etapa <- avaliacoes |>
    dplyr::summarise(
      nivel_medio = mean(nota_numerica(nota)),
      fracao_acima = mean(nota == "ACIMA"),
      fracao_abaixo = mean(nota == "ABAIXO"),
      .by = etapa
    ) |>
    dplyr::arrange(dplyr::desc(nivel_medio))

  combinacoes <- utils::combn(etapas, 2, simplify = FALSE)
  pares <- purrr::map(combinacoes, function(par) {
    a <- largo[[par[1]]]
    b <- largo[[par[2]]]

    tibble::tibble(
      etapa_a = par[1],
      etapa_b = par[2],
      concordancia = mean(a == b),
      correlacao = stats::cor(nota_numerica(a), nota_numerica(b))
    )
  }) |>
    purrr::list_rbind() |>
    dplyr::arrange(correlacao)

  list(por_etapa = por_etapa, pares = pares, largo = largo)
}

#' Mede a dimensionalidade dos itens de uma etapa de avaliação
#'
#' A pergunta é se os 14 itens medem uma coisa ou várias. Se medem uma, o
#' rótulo escalar é a média deles e não perde nada. Se medem várias coisas
#' independentes, colapsar em escalar destrói informação, e o rótulo precisa
#' declarar qual dimensão adota.
#'
#' Três medidas da mesma propriedade, porque nenhuma sozinha convence. A
#' variância do primeiro componente diz quanto de estrutura comum existe. A
#' correlação média entre itens diz o mesmo sem depender de decomposição. O
#' alfa de Cronbach diz se a média dos itens é uma escala confiável, e tem a
#' vantagem de ter faixa de referência consolidada na literatura, com 0,7
#' como piso usual de aceitação.
#'
#' Alfa negativo, que é o que a primeira versão do gerador produzia, não é um
#' valor baixo: é sinal de que os itens não compartilham nada e que a média
#' deles não é escala nenhuma.
#'
#' @param avaliacoes tibble de avaliações.
#' @param etapa Etapa a analisar.
#' @return Lista com `n_itens`, `n_avaliacoes`, `variancia_primeiro_componente`,
#'   `correlacao_media_itens`, `alfa_cronbach` e `matriz`.
medir_dimensionalidade_dos_itens <- function(avaliacoes, etapa = "gestor_final") {
  largo <- avaliacoes |>
    dplyr::filter(etapa == !!etapa) |>
    dplyr::mutate(valor = nota_numerica(nota)) |>
    dplyr::select(consultor_id, projeto_id, item, valor) |>
    tidyr::pivot_wider(names_from = item, values_from = valor)

  matriz <- as.matrix(largo[, setdiff(names(largo), c("consultor_id", "projeto_id"))])
  matriz <- matriz[stats::complete.cases(matriz), , drop = FALSE]

  n_itens <- ncol(matriz)
  correlacoes <- stats::cor(matriz)
  decomposicao <- stats::prcomp(matriz, scale. = TRUE)
  variancia <- decomposicao$sdev^2 / sum(decomposicao$sdev^2)

  # Alfa de Cronbach na forma padrão: quanto da variância do escore somado
  # vem de covariância entre itens, e não da variância de cada um isolado.
  variancia_itens <- sum(apply(matriz, 2, stats::var))
  variancia_total <- stats::var(rowSums(matriz))
  alfa <- (n_itens / (n_itens - 1)) * (1 - variancia_itens / variancia_total)

  list(
    n_itens = n_itens,
    n_avaliacoes = nrow(matriz),
    variancia_primeiro_componente = variancia[1],
    variancia_dois_primeiros = sum(variancia[1:2]),
    correlacao_media_itens = mean(correlacoes[upper.tri(correlacoes)]),
    alfa_cronbach = alfa,
    matriz = matriz,
    variancia_componentes = variancia
  )
}

#' Decompõe a variância do escore entre consultores e dentro de cada um
#'
#' É a pergunta que decide se o rótulo tem sinal **sobre a pessoa**, que é o
#' que um recomendador precisa aprender. Um rótulo cuja variância é toda
#' dentro do consultor, isto é, a mesma pessoa recebendo notas muito
#' diferentes em projetos diferentes, não permite dizer que alguém é um bom
#' candidato: permite dizer, no máximo, que aquela alocação foi bem.
#'
#' Não é um julgamento sobre qual das duas deveria dominar. As duas contam
#' uma história legítima: variância entre pessoas é aptidão, variância dentro
#' da pessoa é adequação ao projeto, que é justamente o que a alocação tenta
#' otimizar. O que importa é saber quanto há de cada uma.
#'
#' @param avaliacoes tibble de avaliações.
#' @param etapa Etapa a analisar.
#' @return Lista com as variâncias e a fração entre consultores.
decompor_variancia_do_escore <- function(avaliacoes, etapa = "gestor_final") {
  escores <- avaliacoes |>
    dplyr::filter(etapa == !!etapa) |>
    dplyr::summarise(escore = mean(nota_numerica(nota)), .by = c(consultor_id, projeto_id))

  media_por_consultor <- escores |>
    dplyr::summarise(media = mean(escore), avaliacoes = dplyr::n(), .by = consultor_id)

  variancia_total <- stats::var(escores$escore)
  variancia_entre <- stats::var(media_por_consultor$media)

  list(
    escores = escores,
    media_por_consultor = media_por_consultor,
    variancia_total = variancia_total,
    variancia_entre_consultores = variancia_entre,
    fracao_entre_consultores = variancia_entre / variancia_total,
    consultores_com_mais_de_uma = sum(media_por_consultor$avaliacoes > 1)
  )
}

#' Mede se a margem pode ser atribuída a uma alocação individual
#'
#' A pergunta que separa os candidatos (b) dos candidatos (c) e (d) da
#' construção do rótulo. `margem_frente_mes` não tem `consultor_id`, e o
#' usuário confirmou que frente e mês é a única granularidade que existe na
#' Peers. Atribuir margem a uma pessoa dentro de um time exigiria regra de
#' rateio, que seria estimativa e não captura.
#'
#' Mede também a fração de linhas com custo zero nas duas fontes de margem,
#' porque margem igual à receita não é ponderação econômica utilizável. A
#' distinção entre as duas fontes é o achado: em `margem_frente_mes` o custo
#' zero é incompletude de apontamento, calibrada contra os 44,7% reais de
#' `peers_custointerns_vlr`, e aparece independentemente de haver gente
#' alocada. Em `margem_estrutural` o custo zero só aparece onde não há
#' ninguém alocado, que é explicável e não atrapalha, porque projeto sem
#' alocação não é célula de `R`.
#'
#' @param dados Lista nomeada de tibbles.
#' @return Lista com a atribuibilidade e a qualidade das duas fontes.
medir_atribuibilidade_da_margem <- function(dados) {
  equipe <- dplyr::count(dados$alocacoes, projeto_id, name = "consultores")

  frente_mes <- dados$margem_frente_mes
  estrutural <- dados$margem_estrutural
  sem_alocacao <- !estrutural$projeto_id %in% dados$alocacoes$projeto_id

  com_receita <- frente_mes$receita > 0
  margem_relativa <- frente_mes$margem[com_receita] / frente_mes$receita[com_receita]

  list(
    tem_consultor_na_margem = "consultor_id" %in% names(frente_mes),
    equipe = equipe,
    mediana_equipe = stats::median(equipe$consultores),
    maior_equipe = max(equipe$consultores),
    fracao_projetos_com_equipe = mean(equipe$consultores > 1),
    custo_zero_frente_mes = mean(frente_mes$custo == 0),
    custo_zero_estrutural = mean(estrutural$custo_total == 0),
    custo_zero_estrutural_sem_alocacao = all(
      (estrutural$custo_total == 0) == sem_alocacao
    ),
    margem_relativa_mediana = stats::median(margem_relativa),
    fracao_margem_relativa_integral = mean(margem_relativa == 1)
  )
}

#' Avalia qual dos quatro candidatos de rótulo do dicionário se sustenta
#'
#' Mesma forma de `avaliar_formas_de_hibrido()` e
#' `avaliar_estimadores_de_propensao()`: veredito calculado a partir das
#' grandezas medidas, e não declarado, para não envelhecer em silêncio.
#'
#' Os quatro candidatos são os de `docs/dicionario_de_dados.md`, seção
#' "Construção do rótulo".
#'
#' @param margem_atribuivel Se a margem existe por alocação individual.
#' @param fracao_projetos_com_equipe Fração de projetos com mais de uma
#'   pessoa, que é onde o rateio seria necessário.
#' @param custo_zero_frente_mes Fração de linhas de margem por frente e mês
#'   sem custo apontado.
#' @param custo_zero_explicavel_estrutural Se o custo zero da margem
#'   estrutural coincide com projeto sem alocação.
#' @param alfa_cronbach Confiabilidade da escala de itens.
#' @param limite_alfa Piso de aceitação do alfa. Convenção declarada.
#' @return tibble com `candidato`, `descricao`, `exige`, `viavel` e
#'   `justificativa`.
avaliar_candidatos_de_rotulo <- function(margem_atribuivel,
                                          fracao_projetos_com_equipe,
                                          custo_zero_frente_mes,
                                          custo_zero_explicavel_estrutural,
                                          alfa_cronbach,
                                          limite_alfa = 0.7) {
  escala_confiavel <- alfa_cronbach >= limite_alfa

  tibble::tribble(
    ~candidato, ~descricao, ~exige, ~viavel, ~justificativa,

    "(a)", "só desempenho, normalizado por avaliador",
    "escala de itens confiável e uma etapa declarada como rótulo",
    escala_confiavel,
    paste0(
      "a média dos itens é escala confiável (alfa de ",
      formatar_decimal(alfa_cronbach, 2), "), e a normalização por avaliador ",
      "é obrigatória e não opcional, porque as etapas divergem em nível"
    ),

    "(b)", "média ponderada ou produto de desempenho por margem",
    "margem atribuível a uma alocação individual",
    margem_atribuivel,
    paste0(
      "a margem não existe por alocação em lugar nenhum da Peers, e ratear a de ",
      "frente e mês entre os membros do time seria necessário em ",
      formatar_pct(fracao_projetos_com_equipe),
      " dos projetos alocados; o rateio é estimativa, não captura"
    ),

    "(c)", "margem como segundo objetivo, na granularidade de frente e mês",
    "margem utilizável como ponderação econômica na granularidade em que existe",
    custo_zero_frente_mes < 0.1,
    paste0(
      "a granularidade serve, mas ", formatar_pct(custo_zero_frente_mes),
      " das linhas não têm custo apontado, e margem igual à receita não pondera ",
      "nada; o número é âncora real, calibrado contra os 44,7% de ",
      "`peers_custointerns_vlr`"
    ),

    "(d)", "margem estrutural, de custo carregado por horas contra valor de contrato",
    "custo e contrato disponíveis para todo projeto com alocação",
    custo_zero_explicavel_estrutural,
    paste0(
      "não depende do apontamento de custo, que é a fonte do problema do ",
      "candidato (c): o custo é calculado das horas alocadas e da tabela de ",
      "custo carregado, e só é zero onde não há ninguém alocado, que não é ",
      "célula de `R`"
    )
  )
}
