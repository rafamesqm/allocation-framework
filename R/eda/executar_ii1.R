#' Executa o bloco II.1 da EDA, esparsidade, cauda longa e linha de base
#'
#' Primeiro bloco da frente direcionada. Mesma estrutura de `executar_i1()`,
#' mas o propósito muda: aqui não se descreve o dado, mede-se a propriedade
#' que discrimina entre famílias de método.
#'
#' **Âncora na literatura.** Cremonesi, Koren e Turrin (2010) mostram que um
#' recomendador que apenas devolve os itens mais populares é linha de base
#' surpreendentemente forte, e que avaliação sem ela faz modelo fraco parecer
#' bom. Steck (2011) trata a distribuição de popularidade como propriedade
#' que condiciona tanto o modelo quanto a métrica.
#'
#' **Propriedade medida.** Esparsidade da matriz de interações, forma da
#' sequência de grau dos dois lados, concentração da interação nos nós mais
#' ativos, e o acerto que a popularidade sozinha alcança.
#'
#' **Decisão que discrimina.** Sobredispersão alta significa que a vizinhança
#' dos nós de grau baixo é rala ou dominada pelos poucos nós populares, o que
#' degrada método baseado em vizinhança e pesa a favor de fator latente. A
#' linha de base de popularidade fixa o piso que o recomendador da wave 3
#' precisa superar para justificar sua existência.
#'
#' @param dados Lista nomeada de tibbles, de `carregar_dados()`.
#' @param estabilidade tibble de `resumir_estabilidade()`, opcional.
#' @return Lista com `objetos`, `tabelas`, `figuras` e `achados`.
executar_ii1 <- function(dados, estabilidade = NULL) {
  esparsidade <- medir_esparsidade(dados$alocacoes, dados$consultores, dados$projetos)

  grau_consultor <- dplyr::count(dados$alocacoes, consultor_id)$n
  grau_projeto <- dplyr::count(dados$alocacoes, projeto_id)$n

  ajuste_consultor <- ajustar_modelo_de_grau(grau_consultor)
  ajuste_projeto <- ajustar_modelo_de_grau(grau_projeto)

  lorenz_consultor <- calcular_lorenz(grau_consultor)
  lorenz_projeto <- calcular_lorenz(grau_projeto)

  linha_base <- medir_linha_base_popularidade(dados$alocacoes, dados$consultores)

  fracao_do_topo <- function(graus, fracao) {
    ordenados <- sort(graus, decreasing = TRUE)
    sum(ordenados[seq_len(ceiling(length(ordenados) * fracao))]) / sum(ordenados)
  }
  concentracao_top10_projeto <- fracao_do_topo(grau_projeto, 0.10)
  concentracao_top10_consultor <- fracao_do_topo(grau_consultor, 0.10)

  recall_20 <- linha_base$recall[linha_base$n_recomendados == 20]
  recall_50 <- linha_base$recall[linha_base$n_recomendados == 50]
  fracao_lista_50 <- 50 / nrow(dados$consultores)

  curvas <- dplyr::bind_rows(
    dplyr::mutate(lorenz_consultor$curva, lado = "Consultores"),
    dplyr::mutate(lorenz_projeto$curva, lado = "Projetos")
  )

  ajustes <- dplyr::bind_rows(
    dplyr::mutate(ajuste_consultor, lado = "Consultores", .before = 1),
    dplyr::mutate(ajuste_projeto, lado = "Projetos", .before = 1)
  )

  objetos <- list(
    esparsidade = esparsidade,
    ajuste_consultor = ajuste_consultor,
    ajuste_projeto = ajuste_projeto,
    ajustes = ajustes,
    lorenz_consultor = lorenz_consultor,
    lorenz_projeto = lorenz_projeto,
    curvas = curvas,
    linha_base = linha_base,
    recall_20 = recall_20,
    recall_50 = recall_50,
    fracao_lista_50 = fracao_lista_50,
    recall_20_intervalo = formatar_intervalo(estabilidade, "recall_popularidade_20"),
    recall_20_posicao = posicao_no_intervalo(
      estabilidade, "recall_popularidade_20", recall_20
    ),
    concentracao_top10_projeto = concentracao_top10_projeto,
    concentracao_top10_consultor = concentracao_top10_consultor,
    grau_consultor = grau_consultor,
    grau_projeto = grau_projeto
  )

  tabelas <- list(
    ii1_esparsidade = esparsidade |>
      dplyr::transmute(
        Consultores = consultores, Projetos = projetos,
        `Células possíveis` = formatar_inteiro(celulas),
        `Observações` = formatar_inteiro(observacoes),
        Densidade = formatar_decimal(densidade, 5),
        Preenchimento = formatar_pct(densidade, 2)
      ),
    ii1_ajuste_grau = ajustes |>
      dplyr::transmute(
        Lado = lado, n = formatar_inteiro(n),
        `Média` = formatar_decimal(media, 2),
        `Variância` = formatar_decimal(variancia, 2),
        `Índice de dispersão` = formatar_decimal(indice_dispersao, 2),
        `AIC Poisson` = formatar_decimal(aic_poisson, 0),
        `AIC Bin. Negativa` = formatar_decimal(aic_binomial_negativa, 0),
        Ganho = formatar_decimal(ganho_aic, 0)
      ),
    ii1_linha_base = linha_base |>
      dplyr::transmute(
        `Tamanho da lista` = n_recomendados,
        `Fração do quadro` = formatar_pct(n_recomendados / nrow(dados$consultores)),
        Acertos = formatar_inteiro(acertos),
        Tentativas = formatar_inteiro(tentativas),
        Recall = formatar_pct(recall, 2)
      )
  )

  figuras <- list(
    ii1_lorenz = figura_lorenz(
      curvas,
      titulo = "Concentração da interação, curva de Lorenz",
      subtitulo = paste0(
        "Gini ", formatar_decimal(lorenz_consultor$gini, 3),
        " do lado do consultor e ",
        formatar_decimal(lorenz_projeto$gini, 3), " do lado do projeto"
      )
    ),
    ii1_linha_base = figura_linha_base_popularidade(
      linha_base, nrow(dados$consultores),
      titulo = "Linha de base: recomendar apenas por popularidade",
      subtitulo = "Recall com deixa-um-de-fora. Diagonal: acerto ao acaso"
    )
  )

  achados <- registrar_achados("II.1", com_sintese(tibble::tribble(
    ~codigo, ~etiqueta, ~enunciado, ~implicacao,

    "II.1.1", "âncora real",
    paste0(
      "A matriz de interações tem densidade de ",
      formatar_pct(esparsidade$densidade, 3),
      if (is.null(estabilidade)) {
        ""
      } else {
        paste0(
          ", ou ",
          formatar_intervalo(estabilidade, "densidade_R", function(x) formatar_pct(x, 3)),
          " sobre as realizações"
        )
      },
      ": ", formatar_inteiro(esparsidade$observacoes), " observações em ",
      formatar_inteiro(esparsidade$celulas), " células possíveis."
    ),
    paste0(
      "A ordem de grandeza reproduz a densidade real estimada de 0,5%, com a ",
      "ressalva de cobertura de join já registrada. Esparsidade nesse nível é ",
      "o regime em que método de vizinhança tem pouca sobreposição para ",
      "estimar similaridade, e é o primeiro argumento a favor de fator ",
      "latente sobre vizinhança."
    ),

    "II.1.2", "consequência do gerador",
    paste0(
      "A sequência de grau é fortemente sobredispersa dos dois lados: índice ",
      "de dispersão ", formatar_decimal(ajuste_consultor$indice_dispersao, 1),
      " do lado do consultor e ",
      formatar_decimal(ajuste_projeto$indice_dispersao, 1),
      " do lado do projeto, contra 1 sob Poisson. A Binomial Negativa vence a ",
      "Poisson por ", formatar_decimal(ajuste_consultor$ganho_aic, 0), " e ",
      formatar_decimal(ajuste_projeto$ganho_aic, 0), " de AIC."
    ),
    paste0(
      "O gerador foi calibrado com Binomial Negativa justamente por isso, ",
      "então reproduzir a sobredispersão é conferência, não descoberta. O que ",
      "importa é a consequência: com grau tão desigual, a vizinhança dos nós ",
      "pouco alocados é rala, e similaridade estimada sobre poucos vizinhos ",
      "comuns é instável."
    ),

    "II.1.3", "instrumento",
    paste0(
      "A interação é concentrada: Gini de ",
      formatar_decimal(lorenz_projeto$gini, 3), " no grau do projeto e ",
      formatar_decimal(lorenz_consultor$gini, 3), " no do consultor, com os ",
      "10% maiores projetos concentrando ",
      formatar_pct(concentracao_top10_projeto), " das alocações e os 10% ",
      "maiores consultores concentrando ",
      formatar_pct(concentracao_top10_consultor), "."
    ),
    paste0(
      "É a concentração que torna popularidade uma linha de base forte ",
      "(Cremonesi et al., 2010), e é ela que obriga a métrica da wave 3 a ",
      "controlar popularidade, sob pena de premiar um modelo que apenas ",
      "aprendeu a devolver quem já é muito alocado."
    ),

    "II.1.4", "instrumento",
    paste0(
      "Recomendar apenas os consultores mais alocados, sem olhar o projeto, ",
      "acerta ", formatar_pct(recall_20, 1), " das alocações com lista de 20 ",
      "e ", formatar_pct(recall_50, 1), " com lista de 50, esta última ",
      "cobrindo ", formatar_pct(fracao_lista_50), " do quadro",
      if (is.null(estabilidade)) {
        "."
      } else {
        paste0(
          ". Sobre as realizações, ",
          formatar_intervalo(estabilidade, "recall_popularidade_20"),
          " em lista de 20 e ",
          formatar_intervalo(estabilidade, "recall_popularidade_50"),
          " em lista de 50",
          descrever_ponto_fora(c(
            "lista de 20" = posicao_no_intervalo(
              estabilidade, "recall_popularidade_20", recall_20
            ),
            "lista de 50" = posicao_no_intervalo(
              estabilidade, "recall_popularidade_50", recall_50
            )
          )),
          "."
        )
      }
    ),
    paste0(
      "É o piso que o recomendador da wave 3 precisa superar para justificar ",
      "sua existência: modelo que não bata isso com folga não está usando ",
      "informação de afinidade, apenas redescobrindo popularidade. O número é ",
      "limite superior, por desempate favorável ao consultor avaliado, e ",
      "precisa ser refeito sob o protocolo do bloco II.7 antes de virar ",
      "critério formal."
    )
  ), tibble::tribble(
    ~codigo, ~decisao, ~limitacao, ~proveniencia,

    "II.1.1",
    paste0(
      "O método de vizinhança fica descartado em favor de fator latente: ",
      "neste nível de esparsidade não há sobreposição para estimar ",
      "similaridade entre pares."
    ),
    NA,
    "fato real",

    "II.1.2", NA,
    paste0(
      "O gerador foi calibrado com Binomial Negativa, então reproduzir a ",
      "sobredispersão é conferência e não descoberta, e a magnitude do índice ",
      "não fala sobre a Peers."
    ),
    "depende do gerador",

    "II.1.3",
    paste0(
      "A métrica da wave 3 controla popularidade, sob pena de premiar um ",
      "modelo que apenas aprendeu a devolver quem já é muito alocado."
    ),
    NA,
    "estrutural",

    "II.1.4",
    paste0(
      "A linha de base de popularidade é reportada ao lado de toda comparação ",
      "da wave 3, como piso que o recomendador precisa superar para justificar ",
      "sua existência."
    ),
    paste0(
      "O número medido aqui é limite superior, por desempate favorável ao ",
      "consultor avaliado, e só vira critério formal depois de refeito sob o ",
      "protocolo do bloco II.7."
    ),
    "estrutural"
  )))

  list(objetos = objetos, tabelas = tabelas, figuras = figuras, achados = achados)
}
