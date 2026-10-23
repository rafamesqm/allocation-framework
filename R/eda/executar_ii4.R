#' Executa o bloco II.4 da EDA, viés de seleção, propensão e positividade
#'
#' **Âncora na literatura.** Marlin e Zemel (2009) mostram que dado de
#' recomendação é ausente de forma não aleatória e que ignorar isso enviesa
#' qualquer avaliação. Steck (2010) trata a propensão por popularidade como
#' caso base. Schnabel, Swaminathan, Singh, Chandak e Joachims (2016)
#' formalizam a avaliação não enviesada por reponderação inversa de propensão.
#'
#' **Propriedade medida.** O viés de seleção observado, quanto dele a
#' propensão estimável captura, a positividade dos estratos e o custo em
#' variância de reponderar.
#'
#' **Decisão que discrimina.** Se estimador não enviesado por propensão é
#' aplicável, ou se a validação da wave 3 fica limitada por construção e
#' precisa declarar o viés em vez de corrigi-lo.
#'
#' **Este é o bloco de maior risco de circularidade do capítulo, e o
#' enquadramento vem antes dos números.** O viés de seleção deste conjunto foi
#' plantado por `gerar_alocacoes()` com `prob_vies = 0,7`. Um modelo de
#' propensão ajustado aqui recupera um mecanismo escrito neste projeto, e
#' reportar esse reencontro como descoberta seria devolver a entrada em forma
#' de resultado, exatamente o que o achado I.0.3 denuncia.
#'
#' O bloco é construído para não depender disso. As três conclusões que ele
#' habilita se apoiam em: a cobertura da covariável, que é âncora real de
#' 48,4%; a ausência de amostra de exposição aleatória, que é fato sobre a
#' organização e não sobre o código; e a cegueira estrutural da propensão por
#' popularidade a qualquer mecanismo de afinidade, que é propriedade do
#' estimador e valeria com qualquer magnitude de viés. A magnitude do viés
#' aparece etiquetada como consequência do gerador e não sustenta decisão
#' nenhuma.
#'
#' @param dados Lista nomeada de tibbles, de `carregar_dados()`.
#' @param estabilidade tibble de `resumir_estabilidade()`, opcional.
#' @param limite_ess Fração do tamanho efetivo de amostra abaixo da qual
#'   reponderar é tratado como inviável por variância. Convenção declarada.
#' @return Lista com `objetos`, `tabelas`, `figuras` e `achados`.
executar_ii4 <- function(dados, estabilidade = NULL, limite_ess = 0.5) {
  propensao <- estimar_propensao_por_popularidade(
    dados$alocacoes, dados$consultores, dados$projetos
  )
  positividade <- diagnosticar_positividade(
    propensao, dados$alocacoes, dados$consultores, dados$projetos
  )
  vies <- medir_vies_de_selecao(dados, propensao)
  amostras <- verificar_amostra_de_exposicao_aleatoria()

  existe_amostra_aleatoria <- any(amostras$disponivel)
  positividade_completa <- positividade$fracao_pares_sem_positividade == 0

  # "Captura o mecanismo" é uma afirmação sobre distância, e precisa de um
  # critério. O adotado: a propensão captura o mecanismo se a taxa que ela
  # prevê fica mais perto da observada do que da aleatória. É deliberadamente
  # generoso, porque o interesse é não superestimar a cegueira do modelo, e
  # ainda assim a propensão por popularidade falha nele com folga.
  distancia_ao_observado <- abs(vies$taxa_sob_popularidade - vies$taxa_observada)
  distancia_ao_acaso <- abs(vies$taxa_sob_popularidade - vies$taxa_ao_acaso)
  propensao_captura_mecanismo <- distancia_ao_observado < distancia_ao_acaso

  estimadores <- avaliar_estimadores_de_propensao(
    positividade_completa = positividade_completa,
    propensao_captura_mecanismo = propensao_captura_mecanismo,
    cobertura_covariavel = vies$cobertura_covariavel_consultores,
    ess_relativo = positividade$ess_relativo,
    existe_amostra_aleatoria = existe_amostra_aleatoria,
    limite_ess = limite_ess
  )
  estimadores_viaveis <- dplyr::filter(estimadores, viavel)

  # A realização versionada é o extremo das réplicas na positividade, e o
  # enunciado precisa dizer isso em vez de citar ponto e intervalo lado a
  # lado como se concordassem. Ver `posicao_no_intervalo()`.
  posicao_ess <- posicao_no_intervalo(
    estabilidade, "ess_relativo_ips", positividade$ess_relativo
  )
  posicao_positividade <- posicao_no_intervalo(
    estabilidade, "pares_sem_positividade",
    positividade$fracao_pares_sem_positividade
  )

  comparacao_taxas <- tibble::tibble(
    cenario = c(
      "Pareamento ao acaso",
      "Previsto pela propensão por popularidade",
      "Observado no histórico"
    ),
    taxa = c(vies$taxa_ao_acaso, vies$taxa_sob_popularidade, vies$taxa_observada),
    # A linha do acaso não é medida nem prevista por modelo: é 1 dividido pelo
    # número de setores, uma referência analítica. Pintá-la como as outras
    # duas sugeriria que as três têm o mesmo estatuto, que é justamente a
    # confusão que a figura existe para desfazer.
    origem = c(
      "Referência analítica", "Previsto pelo modelo", "Medido no dado"
    )
  )

  objetos <- list(
    propensao = propensao,
    positividade = positividade,
    vies = vies,
    amostras = amostras,
    estimadores = estimadores,
    estimadores_viaveis = estimadores_viaveis,
    comparacao_taxas = comparacao_taxas,
    positividade_completa = positividade_completa,
    propensao_captura_mecanismo = propensao_captura_mecanismo,
    posicao_ess = posicao_ess,
    posicao_positividade = posicao_positividade,
    estabilidade = estabilidade,
    existe_amostra_aleatoria = existe_amostra_aleatoria,
    limite_ess = limite_ess,
    n_pares = nrow(dados$consultores) * nrow(dados$projetos),
    n_observacoes = nrow(dados$alocacoes)
  )

  tabelas <- list(
    ii4_positividade = tibble::tibble(
      Medida = c(
        "Pares na matriz",
        "Pares com propensão zero",
        "Consultores sem nenhuma alocação",
        "Projetos sem nenhuma alocação",
        "Propensão mínima entre os observados",
        "Propensão mediana entre os observados",
        "Propensão máxima na matriz",
        "Peso IPS máximo",
        "Tamanho efetivo de amostra (Kish)",
        "Tamanho efetivo relativo"
      ),
      Valor = c(
        formatar_inteiro(objetos$n_pares),
        paste0(
          formatar_inteiro(positividade$pares_sem_positividade), " (",
          formatar_pct(positividade$fracao_pares_sem_positividade), ")"
        ),
        formatar_inteiro(positividade$consultores_grau_zero),
        formatar_inteiro(positividade$projetos_grau_zero),
        formatar_decimal(positividade$quantis_observados[["0%"]], 5),
        formatar_decimal(positividade$quantis_observados[["50%"]], 5),
        paste0(
          formatar_decimal(positividade$propensao_maxima, 3),
          if (positividade$aproximacao_valida) " (abaixo de 1)" else " (ACIMA DE 1)"
        ),
        formatar_decimal(positividade$peso_maximo, 0),
        formatar_decimal(positividade$ess, 0),
        formatar_pct(positividade$ess_relativo)
      )
    ),
    ii4_vies_selecao = tibble::tibble(
      `Cenário` = comparacao_taxas$cenario,
      `Taxa de mesmo setor` = formatar_pct(comparacao_taxas$taxa),
      `Razão sobre o acaso` = formatar_decimal(
        comparacao_taxas$taxa / vies$taxa_ao_acaso, 2
      ),
      Origem = comparacao_taxas$origem
    ),
    ii4_estimadores = estimadores |>
      dplyr::transmute(
        Estimador = estimador, Exige = exige,
        `Viável` = ifelse(viavel, "sim", "não"),
        Justificativa = justificativa
      ),
    ii4_exposicao_aleatoria = amostras |>
      dplyr::transmute(
        Fonte = fonte,
        `Disponível` = ifelse(disponivel, "sim", "não"),
        `Observação` = observacao
      )
  )

  figuras <- list(
    ii4_distribuicao_propensao = figura_distribuicao_propensao(
      positividade$propensao_observada,
      mediana = positividade$quantis_observados[["50%"]],
      titulo = "Propensão estimada dos pares efetivamente observados",
      subtitulo = paste0(
        "Fora do gráfico, ",
        formatar_pct(positividade$fracao_pares_sem_positividade),
        " da matriz tem propensão exatamente zero, e nenhum peso finito recupera esses pares"
      )
    ),
    ii4_cegueira_propensao = figura_cegueira_da_propensao(
      comparacao_taxas,
      titulo = "O que a propensão por popularidade enxerga do viés de seleção",
      subtitulo = "A propensão estimável sobre toda a matriz fica quase indistinguível do pareamento ao acaso"
    )
  )

  achados <- registrar_achados("II.4", com_sintese(tibble::tribble(
    ~codigo, ~etiqueta, ~enunciado, ~implicacao,

    "II.4.1", "consequência do gerador",
    paste0(
      "A alocação é fortemente enviesada por afinidade setorial: a taxa de ",
      "pareamento entre setor do consultor e setor do projeto é de ",
      formatar_pct(vies$taxa_observada), " contra ",
      formatar_pct(vies$taxa_ao_acaso), " sob pareamento ao acaso, uma razão ",
      "de ", formatar_decimal(vies$lift_observado, 1), " vezes",
      if (is.null(estabilidade)) {
        "."
      } else {
        # A versão observável, e não a latente: o ponto ao lado é calculado com
        # a experiência que o formulário capturou, e citar o intervalo da
        # latente, como uma versão anterior fazia, comparava duas grandezas.
        paste0(
          ". Sobre as realizações, ",
          formatar_intervalo(estabilidade, "taxa_mesmo_setor_observavel"), "."
        )
      }
    ),
    paste0(
      "**A magnitude não diz nada sobre a organização.** O viés foi plantado em ",
      "`gerar_alocacoes()` com `prob_vies = 0,7`, e qualquer modelo de ",
      "propensão ajustado aqui reencontra um mecanismo escrito neste projeto. ",
      "O número entra para tornar auditável a circularidade, não para ",
      "sustentar decisão. O que os achados seguintes usam dele é apenas que ",
      "**existe** um mecanismo de afinidade dominando a seleção, o que na ",
      "organização é fato conhecido: a alocação é centralizada e subjetiva, e é esse ",
      "o problema que motiva a monografia."
    ),

    "II.4.2", "instrumento",
    paste0(
      "A propensão estimável sobre a matriz inteira é cega ao mecanismo que ",
      "domina a seleção. A propensão por popularidade prevê taxa de ",
      "pareamento setorial de ", formatar_pct(vies$taxa_sob_popularidade),
      ", contra ", formatar_pct(vies$taxa_observada), " observados e ",
      formatar_pct(vies$taxa_ao_acaso), " ao acaso: ela fica ",
      formatar_decimal(vies$lift_da_propensao, 2),
      " vezes o acaso, quando o dado está em ",
      formatar_decimal(vies$lift_observado, 1), " vezes."
    ),
    paste0(
      "A conclusão não depende da magnitude do viés, e por isso não é ",
      "circular: propensão que só usa popularidade **não pode**, por ",
      "construção, capturar afinidade entre atributos, qualquer que seja a ",
      "força dela. Reponderar por `1 / propensão` corrigiria a concentração ",
      "em quem é muito alocado, que é uma dimensão real do viés, e deixaria ",
      "intacta a dimensão que decide quem trabalha com quem."
    ),

    "II.4.3", "âncora real",
    paste0(
      "A covariável que capturaria o mecanismo não está disponível para a ",
      "matriz inteira. O setor predominante do consultor vem de ",
      "`experiencia_setorial`, que o bloco II.3 mostrou cobrir ",
      formatar_pct(vies$cobertura_covariavel_consultores),
      " dos consultores, e por isso a taxa de pareamento só é calculável em ",
      formatar_pct(vies$cobertura_covariavel_alocacoes),
      " das alocações."
    ),
    paste0(
      "Um modelo de propensão com covariável, que enxergaria o que a ",
      "popularidade não enxerga, não tem entrada para mais da metade da ",
      "matriz. A limitação é de dado real e não do gerador: os 48,4% são ",
      "medida da organização, registrada em `docs/mapeamento_schema.md`. Fecha a ",
      "segunda saída: nem a propensão estimável serve, nem a que serviria é ",
      "estimável."
    ),

    "II.4.4", "âncora real",
    paste0(
      "Não existe amostra de exposição aleatória nem política de registro ",
      "conhecida: as ", nrow(amostras),
      " fontes que tornariam a propensão falsificável estão todas ausentes ",
      "nesta base."
    ),
    paste0(
      "É o achado decisivo do bloco, e é sobre a organização, não sobre o código. ",
      "Marlin e Zemel (2009) mostram que sem uma dessas fontes um modelo de ",
      "ausência correto e um errado explicam igualmente bem o que foi ",
      "observado, e Schnabel et al. (2016) validam propensão exatamente ",
      "contra elas. Os conjuntos públicos que permitem esse tipo de trabalho, ",
      "como o Yahoo! R3 e o coat, existem porque alguém coletou uma amostra ",
      "aleatória de propósito. Aqui não há, e não há como construir ",
      "retroativamente."
    ),

    "II.4.5", "instrumento",
    paste0(
      "Mesmo onde a positividade vale, reponderar custa quase toda a amostra: ",
      "o peso IPS máximo é ", formatar_decimal(positividade$peso_maximo, 0),
      " e o tamanho efetivo de amostra de Kish cai para ",
      formatar_decimal(positividade$ess, 0), " de ",
      formatar_inteiro(objetos$n_observacoes), " alocações, ou ",
      formatar_pct(positividade$ess_relativo),
      if (is.null(estabilidade)) {
        "."
      } else {
        paste0(
          ", contra ", formatar_intervalo(estabilidade, "ess_relativo_ips"),
          " sobre as realizações",
          if (identical(posicao_ess, "dentro")) {
            "."
          } else {
            paste0(
              ". O ponto versionado cai **", posicao_ess,
              "** do intervalo: esta realização é extrema na positividade, e ",
              "é o intervalo que deve ser citado, não ela."
            )
          }
        )
      },
      " E a positividade não vale em toda parte: ",
      formatar_pct(positividade$fracao_pares_sem_positividade),
      " dos pares têm propensão exatamente zero, vindos de ",
      positividade$consultores_grau_zero, " consultores e ",
      positividade$projetos_grau_zero, " projetos sem nenhuma alocação."
    ),
    paste0(
      "**A decisão que este bloco habilita é que a wave 3 declara o viés em ",
      "vez de corrigi-lo.** Dos ", nrow(estimadores),
      " estimadores confrontados, o único que sobrevive é: ",
      paste(estimadores_viaveis$estimador, collapse = "; "),
      ". Na prática, a métrica do bloco II.7 é calculada sobre a população ",
      "observada e a conclusão é enunciada sobre ela, sem a pretensão de valer ",
      "para os pares que nunca tiveram chance de aparecer. O lado do consultor ",
      "dessa falha de positividade é âncora real: 48 dos 285 ativos não ",
      "aparecem em nenhuma linha de alocação no dado da organização."
    )
  ), tibble::tribble(
    ~codigo, ~decisao, ~limitacao, ~proveniencia,

    "II.4.1", NA,
    paste0(
      "A magnitude do viés de afinidade setorial não diz nada sobre a organização, ",
      "porque foi plantada no gerador: o número entra para tornar auditável a ",
      "circularidade, e o que os achados seguintes usam dele é apenas que ",
      "existe um mecanismo de seleção por atributo."
    ),
    "depende do gerador",

    "II.4.2", NA,
    paste0(
      "Propensão que só usa popularidade não pode, por construção, capturar ",
      "afinidade entre atributos, qualquer que seja a força dela: reponderar ",
      "por `1 / propensão` corrigiria a concentração em quem é muito alocado e ",
      "deixaria intacta a dimensão que decide."
    ),
    "estrutural",

    "II.4.3", NA,
    paste0(
      "Um modelo de propensão com covariável não tem entrada para mais da ",
      "metade da matriz, porque o setor predominante vem de um sinal que cobre ",
      "48,4% dos consultores."
    ),
    "fato real",

    "II.4.4", NA,
    paste0(
      "Sem amostra de exposição aleatória nem política de registro conhecida, ",
      "a propensão não é falsificável nesta base: um modelo de ausência ",
      "correto e um errado explicam igualmente bem o que foi observado."
    ),
    "fato real",

    "II.4.5",
    paste0(
      "A wave 3 declara o viés em vez de corrigi-lo: a métrica é calculada ",
      "sobre a população observada e a conclusão é enunciada sobre ela, sem a ",
      "pretensão de valer para os pares que nunca tiveram chance de aparecer."
    ),
    NA,
    "fato real"
  )))

  list(objetos = objetos, tabelas = tabelas, figuras = figuras, achados = achados)
}
