#' Executa o bloco I.4 da EDA, estrutura temporal
#'
#' Mesmo contrato de `executar_i1()`: calcula, formata, desenha e conclui num
#' só lugar, e o documento apenas narra.
#'
#' @param dados Lista nomeada de tibbles, de `carregar_dados()`.
#' @param data_referencia Data de referência usada na geração, contra a qual
#'   se mede o quanto a janela se estende para o futuro.
#' @param especificacao Contrato de esquema, de `especificacao_tabelas()`.
#' @return Lista com `objetos`, `tabelas`, `figuras` e `achados`.
executar_i4 <- function(dados,
                         data_referencia = as.Date("2026-09-01"),
                         especificacao = especificacao_tabelas()) {
  janela <- resumir_janela_temporal(dados, especificacao)

  # Janela de análise declarada, com censura à direita na data de referência.
  #
  # A cauda de duração de projeto não é corrigível no gerador: os quantis
  # reais são log-simétricos (81/18 = 18/4 = 4,5), então a log-normal ajusta o
  # corpo com perfeição, mas essa mesma log-normal implica média de 216 dias
  # contra 68,8 reais. Nenhuma log-normal de dois parâmetros, com ou sem teto,
  # bate quantis, média e máximo ao mesmo tempo: a distribuição real não é
  # log-normal, e descobrir qual é exigiria dado que não existe mais.
  #
  # A saída é de protocolo, não de modelagem: declarar a janela em que a
  # análise vale e censurar o resto, que é prática padrão em análise de
  # sobrevivência. O período com entrada de projetos vai do primeiro início
  # até a data de referência; além dela só há projeção da cauda.
  inicio_janela <- min(dados$projetos$data_inicio)
  janela_analise <- c(inicio_janela, data_referencia)

  dentro_da_janela <- dados$alocacoes$data_inicio <= data_referencia
  fracao_alem_da_janela <- mean(!dentro_da_janela)

  inicios_projeto <- serie_mensal_eventos(dados$projetos$data_inicio)
  vigencias <- serie_vigencias_mensais(
    dados$alocacoes$data_inicio, dados$alocacoes$data_fim
  )
  sazonalidade <- testar_sazonalidade_mensal(dados$projetos$data_inicio)

  ocupacao_mensal <- dados$ocupacao_consultor |>
    dplyr::summarise(
      pct_ocupacao_medio = mean(pct_ocupacao),
      consultores = dplyr::n(),
      .by = ano_mes
    ) |>
    dplyr::mutate(mes = as.Date(paste0(ano_mes, "-01"))) |>
    dplyr::arrange(mes)

  recencia <- resumir_numericas(dados$skills_consultor, "recencia_dias")

  fim_mais_distante <- max(dados$projetos$data_fim)
  dias_alem <- as.integer(fim_mais_distante - data_referencia)
  anos_alem <- dias_alem / 365.25
  inicio_alocacao_mais_tardio <- max(dados$alocacoes$data_inicio)

  serie_comparada <- dplyr::bind_rows(
    dplyr::transmute(inicios_projeto, mes, valor = n, serie = "Projetos iniciados"),
    dplyr::transmute(vigencias, mes, valor = n_vigentes, serie = "Alocações vigentes")
  )

  objetos <- list(
    janela = janela,
    inicios_projeto = inicios_projeto,
    vigencias = vigencias,
    sazonalidade = sazonalidade,
    ocupacao_mensal = ocupacao_mensal,
    recencia = recencia,
    recencia_valores = dados$skills_consultor$recencia_dias,
    fim_mais_distante = fim_mais_distante,
    dias_alem = dias_alem,
    anos_alem = anos_alem,
    inicio_alocacao_mais_tardio = inicio_alocacao_mais_tardio,
    data_referencia = data_referencia,
    janela_analise = janela_analise,
    fracao_alem_da_janela = fracao_alem_da_janela
  )

  tabelas <- list(
    i4_janela_temporal = janela |>
      dplyr::transmute(
        Tabela = tabela, Campo = coluna,
        Início = format(inicio), Fim = format(fim),
        `Amplitude (dias)` = formatar_inteiro(amplitude_dias),
        n = formatar_inteiro(n)
      ),
    i4_sazonalidade = sazonalidade$contagens |>
      dplyr::transmute(
        `Mês do ano` = mes_do_ano,
        Observado = observado,
        Esperado = formatar_decimal(esperado, 1),
        `Razão` = formatar_decimal(razao, 3)
      )
  )

  figuras <- list(
    i4_serie_mensal = figura_serie_mensal(
      serie_comparada, "mes", "valor", "serie",
      titulo = "Projetos iniciados e alocações vigentes por mês",
      subtitulo = paste(
        "Início é evento pontual; vigência conta a alocação em todo mês",
        "em que ela está ativa"
      ),
      eixo_y = "Contagem no mês",
      legenda = "Série"
    ),
    i4_sazonalidade = figura_sazonalidade(
      sazonalidade$contagens,
      titulo = "Início de projeto por mês do ano",
      subtitulo = paste(
        "Barras: observado. Pontos: esperado sob ocorrência uniforme,",
        "corrigido pelos dias de cada mês na janela"
      )
    ),
    i4_ocupacao_mensal = figura_serie_mensal(
      ocupacao_mensal, "mes", "pct_ocupacao_medio",
      titulo = "Ocupação média por mês",
      subtitulo = "Média de pct_ocupacao entre os consultores com alocação no mês",
      eixo_y = "Fração da capacidade"
    )
  )

  achados <- registrar_achados("I.4", tibble::tribble(
    ~codigo, ~etiqueta, ~enunciado, ~implicacao,

    "I.4.1", "consequência do gerador",
    paste0(
      "A janela dos dados se estende muito além do horizonte da geração: o ",
      "fim de projeto mais distante é ", format(fim_mais_distante), ", ",
      formatar_decimal(anos_alem, 1), " anos após a data de referência de ",
      format(data_referencia), ". Há alocação começando em ",
      format(inicio_alocacao_mais_tardio), "."
    ),
    paste0(
      "Não é corrigível no gerador: os quantis reais são log-simétricos, ",
      "então a log-normal ajusta o corpo e ainda assim estica a cauda muito ",
      "além do que a fonte comporta. A distribuição real não é log-normal e ",
      "não há mais dado para descobrir qual é, porque o acesso encerrou. ",
      "Fica a saída de protocolo: **a janela de análise ",
      "declarada vai de ", format(janela_analise[1]), " a ",
      format(janela_analise[2]), ", com censura à direita**, e as ",
      formatar_pct(fracao_alem_da_janela),
      " das alocações que começam depois disso ficam fora de qualquer ",
      "conclusão temporal."
    ),

    "I.4.2", "instrumento",
    paste0(
      "Não há evidência de sazonalidade nos inícios de projeto: qui-quadrado ",
      formatar_decimal(sazonalidade$qui_quadrado, 2), " com ",
      sazonalidade$gl, " graus de liberdade, p = ",
      formatar_decimal(sazonalidade$valor_p, 3), "."
    ),
    paste0(
      "Resultado esperado por construção, já que o gerador sorteia a data de ",
      "início de forma uniforme, e por isso não é achado sobre a Peers. O ",
      "valor está no teste existir com o esperado corrigido pelo comprimento ",
      "de cada mês, pronto para o dado real, onde sazonalidade de venda é ",
      "plausível."
    ),

    "I.4.3", "consequência do gerador",
    paste0(
      "A recência de skill é constante dentro de cada consultor e agrupada em ",
      "poucas ondas: ", formatar_inteiro(recencia$n), " registros assumem só ",
      dplyr::n_distinct(dados$skills_consultor$recencia_dias),
      " valores distintos, concentrados entre ",
      formatar_inteiro(recencia$minimo), " e ",
      formatar_inteiro(recencia$maximo), " dias."
    ),
    paste0(
      "Este bloco mediu recência uniforme entre 1 e 730 dias na primeira ",
      "versão, com valor independente por skill, e isso motivou a correção. ",
      "As duas propriedades agora reproduzidas são estruturais: na fonte real ",
      "as skills de uma pessoa vêm de uma resposta de formulário só, e são ",
      "três formulários aplicados em campanha. **As datas das ondas são ",
      "suposição declarada, não calibração**, porque as planilhas saíram da ",
      "máquina antes de qualquer data ser agregada. O que se pode afirmar é a ",
      "forma, e ela importa: o decaimento por recência se comporta de modo ",
      "descontínuo, não suave, e a wave 3 precisa tratá-lo assim."
    ),

    "I.4.4", "instrumento",
    paste0(
      "A vigência mensal de alocações vai de ", min(vigencias$n_vigentes),
      " a ", max(vigencias$n_vigentes), " alocações simultâneas ao longo de ",
      nrow(vigencias), " meses, com mediana ",
      formatar_inteiro(stats::median(vigencias$n_vigentes)), "."
    ),
    paste0(
      "A distinção entre evento pontual e vigência é o que torna a carga de ",
      "trabalho legível ao longo do tempo, e é a base da restrição de ",
      "capacidade da wave 4. Não existia antes de a alocação ganhar eixo ",
      "temporal."
    )
  ))

  list(objetos = objetos, tabelas = tabelas, figuras = figuras, achados = achados)
}
