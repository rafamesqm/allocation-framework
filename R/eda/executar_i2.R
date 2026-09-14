#' Executa o bloco I.2 da EDA, univariada por camada
#'
#' Mesmo contrato de `executar_i1()`: calcula, formata, desenha e conclui num
#' só lugar, e o documento apenas narra o resultado. Ver o roxygen daquela
#' função para o porquê da concentração e para a regra de coerência com o
#' gerador.
#'
#' @param dados Lista nomeada de tibbles, de `carregar_dados()`.
#' @return Lista com `objetos`, `tabelas`, `figuras` e `achados`.
executar_i2 <- function(dados) {
  resumo_consultores <- dplyr::bind_rows(
    resumir_numericas(dados$consultores),
    resumir_numericas(dados$custo_carregado, "custo_carregado")
  )
  categoricas_consultores <- resumir_categoricas(dados$consultores) |>
    dplyr::filter(variavel != "consultor_id")

  resumo_projetos <- resumir_numericas(dados$projetos)
  resumo_alocacoes <- resumir_numericas(dados$alocacoes)
  resumo_margem <- resumir_numericas(dados$margem_frente_mes, c("receita", "custo", "margem"))
  resumo_ocupacao <- resumir_numericas(dados$ocupacao_consultor, c("pct_ocupacao", "horas_mes"))
  resumo_estrutural <- resumir_numericas(
    dados$margem_estrutural, c("custo_total", "margem_estrutural")
  )

  confronto_projetos <- confrontar_com_real(resumo_projetos, "projetos")
  confronto_alocacoes <- confrontar_com_real(resumo_alocacoes, "alocacoes")
  confronto_margem <- confrontar_com_real(resumo_margem, "margem_frente_mes")
  comparacao_nivel <- confrontar_proporcoes(dados$consultores, "consultores", "nivel_senioridade")

  pega <- function(confronto, v, e) {
    linha <- confronto[confronto$variavel == v & confronto$estatistica == e, ]
    list(real = linha$valor_real, sint = linha$valor_sintetico, dif = linha$diferenca_relativa)
  }
  vc_mediana <- pega(confronto_projetos, "valor_contrato", "mediana_pos")
  vc_assim <- pega(confronto_projetos, "valor_contrato", "assimetria_pos")
  dur_media <- pega(confronto_projetos, "duracao_dias", "media")

  local <- categoricas_consultores[categoricas_consultores$variavel == "localizacao", ]
  horas <- resumo_alocacoes[resumo_alocacoes$variavel == "horas_mes", ]
  ocupacao <- resumo_ocupacao[resumo_ocupacao$variavel == "pct_ocupacao", ]
  acima_capacidade <- mean(dados$ocupacao_consultor$pct_ocupacao > 1)

  objetos <- list(
    resumo_consultores = resumo_consultores,
    categoricas_consultores = categoricas_consultores,
    resumo_projetos = resumo_projetos,
    resumo_alocacoes = resumo_alocacoes,
    resumo_operacao = dplyr::bind_rows(
      dplyr::mutate(resumo_margem, tabela = "margem_frente_mes"),
      dplyr::mutate(resumo_ocupacao, tabela = "ocupacao_consultor"),
      dplyr::mutate(resumo_estrutural, tabela = "margem_estrutural")
    ),
    confronto_projetos = confronto_projetos,
    confronto_alocacoes = confronto_alocacoes,
    confronto_margem = confronto_margem,
    comparacao_nivel = comparacao_nivel,
    acima_capacidade = acima_capacidade,
    estrutural_negativa = mean(dados$margem_estrutural$margem_estrutural < 0),
    horas_zero = sum(dados$alocacoes$horas_mes == 0),
    n_alocacoes = nrow(dados$alocacoes)
  )

  formatar_confronto <- function(confronto) {
    confronto |>
      dplyr::transmute(
        Variável = variavel, Estatística = estatistica,
        Real = formatar_decimal(valor_real),
        Sintético = formatar_decimal(valor_sintetico),
        `Dif. relativa` = formatar_diferenca(diferenca_relativa),
        Rodada = rodada
      )
  }

  tabelas <- list(
    i2_consultores_numericas = resumo_consultores |>
      dplyr::transmute(
        Variável = variavel, n = n,
        Mín. = round(minimo, 2), P25 = round(p25, 2), Mediana = round(mediana, 2),
        P75 = round(p75, 2), Máx. = round(maximo, 2),
        Média = round(media, 2), Desvio = round(desvio, 2),
        Assimetria = round(assimetria, 2)
      ),
    i2_consultores_categoricas = categoricas_consultores |>
      dplyr::transmute(
        Variável = variavel, Categorias = n_categorias, Moda = moda,
        `Fração da moda` = formatar_pct(fracao_moda),
        Equilíbrio = round(equilibrio, 3)
      ),
    i2_piramide_senioridade = comparacao_nivel |>
      dplyr::mutate(dplyr::across(dplyr::where(is.numeric), \(x) round(x, 4))),
    i2_projetos_numericas = resumo_projetos |>
      dplyr::transmute(
        Variável = variavel, `Fração zero` = formatar_pct(fracao_zero),
        P25 = round(p25, 1), Mediana = round(mediana, 1), P75 = round(p75, 1),
        Máx. = round(maximo, 1), Média = round(media, 1), Desvio = round(desvio, 1),
        Assimetria = round(assimetria, 2), `Mediana pos.` = round(mediana_pos, 1)
      ),
    i2_projetos_confronto_real = formatar_confronto(confronto_projetos),
    i2_alocacoes_numericas = resumo_alocacoes |>
      dplyr::transmute(
        Variável = variavel, `Fração zero` = formatar_pct(fracao_zero),
        Mín. = round(minimo, 2), P25 = round(p25, 2), Mediana = round(mediana, 2),
        P75 = round(p75, 2), Máx. = round(maximo, 2), Média = round(media, 2),
        Desvio = round(desvio, 2), Assimetria = round(assimetria, 2)
      ),
    i2_alocacoes_confronto_real = formatar_confronto(confronto_alocacoes),
    i2_operacao_numericas = objetos$resumo_operacao |>
      dplyr::transmute(
        Tabela = tabela, Variável = variavel,
        `Fração zero` = formatar_pct(fracao_zero),
        P25 = round(p25, 1), Mediana = round(mediana, 1), P75 = round(p75, 1),
        Média = round(media, 1), Desvio = round(desvio, 1),
        Assimetria = round(assimetria, 2)
      ),
    i2_margem_confronto_real = formatar_confronto(confronto_margem)
  )

  figuras <- list(
    i2_piramide_senioridade = figura_proporcoes_comparadas(
      comparacao_nivel,
      titulo = "Pirâmide de senioridade, sintético contra real",
      subtitulo = "Proporção de consultores por nível, da base (N4) ao topo (N0)",
      # Ordinal: a ordem da carreira é a informação da variável, e ordenar
      # por frequência a destruiria. Base embaixo, como se lê uma pirâmide.
      ordem = rev(c("N0", "N1", "N2", "N3", "N4"))
    ),
    i2_projetos_distribuicoes = figura_distribuicoes(
      dados$projetos,
      colunas = c("valor_contrato", "esforco_estimado", "duracao_dias"),
      titulo = "Distribuição das numéricas de projeto",
      subtitulo = paste(
        "Escala log para acomodar a cauda.",
        "Painéis não comparáveis entre si em magnitude"
      ),
      escala_log = TRUE
    )
  )

  achados <- registrar_achados("I.2", tibble::tribble(
    ~codigo, ~etiqueta, ~enunciado, ~implicacao,

    "I.2.1", "consequência do gerador",
    paste0(
      "Duas categóricas de consultor são problemáticas para compor C. `fuso` ",
      "é constante na população, com equilíbrio zero. `localizacao` tem ",
      local$n_categorias, " categorias com ", formatar_pct(local$fracao_moda),
      " concentrados na moda."
    ),
    paste0(
      "`fuso` tem poder discriminante nulo e não deve ocupar coluna na matriz ",
      "de atributos. A cauda de estados raros de `localizacao` exige ",
      "agrupamento antes da codificação one-hot."
    ),

    "I.2.2", "âncora real",
    paste0(
      "A calibração por casamento de quantis acerta onde mira e não controla ",
      "o resto. Em valor_contrato a mediana dos positivos sai a ",
      formatar_diferenca(vc_mediana$dif), " do real, enquanto a assimetria ",
      "dos positivos fica em ", formatar_decimal(vc_assim$sint), " contra ",
      formatar_decimal(vc_assim$real), ". Em duracao_dias os três quantis ",
      "batem e a média sai ", formatar_diferenca(dur_media$dif), "."
    ),
    paste0(
      "Análise que dependa de quantil da distribuição sintética é defensável; ",
      "análise que dependa de média, desvio ou de comportamento de cauda ",
      "extrema não é."
    ),

    "I.2.3", "consequência do gerador",
    paste0(
      "horas_mes é exatamente zero em ", formatar_pct(horas$fracao_zero),
      " das alocações, o que descreve alocação sem dedicação nenhuma e não ",
      "tem contraparte no dado real, onde a mediana é 120 horas e nenhuma ",
      "massa em zero é reportada."
    ),
    paste0(
      "Origem no piso em zero de `gerar_variavel_por_nivel()`. Afeta qualquer ",
      "ponderação da interação por horas, que é candidato de rótulo em ",
      "aberto, e deve ser corrigido antes da wave 3."
    ),

    "I.2.4", "instrumento",
    paste0(
      "Com o eixo temporal, pct_ocupacao vira grandeza interpretável: mediana ",
      formatar_decimal(ocupacao$mediana), " de capacidade usada, com ",
      formatar_pct(acima_capacidade), " dos pares consultor-mês acima da ",
      "capacidade integral."
    ),
    paste0(
      "O histórico observado viola a restrição de capacidade que a wave 4 ",
      "pretende impor. Sem agregado real para confrontar, o valor está no ",
      "diagnóstico existir, não na magnitude sintética."
    )
  ))

  list(objetos = objetos, tabelas = tabelas, figuras = figuras, achados = achados)
}
