#' Executa o bloco II.7 da EDA, protocolo de avaliação
#'
#' **Âncora na literatura.** Cremonesi, Koren e Turrin (2010) mostram que a
#' escolha do protocolo muda a ordem entre algoritmos, e Bellogín, Castells e
#' Cantador (2011) que a métrica precisa controlar popularidade para não
#' premiar quem só a redescobre.
#'
#' **Propriedade medida.** Quanta observação cada forma de divisão deixa para
#' cada componente do híbrido, e quanto ela vaza.
#'
#' **Decisão que discrimina.** A forma da divisão, a métrica e a linha de
#' base, fixadas **antes** de qualquer resultado de modelo. É o que o roadmap
#' registrou como razão de este bloco ficar na wave 2 e não na 3: decidir o
#' protocolo com o modelo em mãos é quando a decisão fica suscetível de ser
#' ajustada ao resultado.
#'
#' **O bloco herda três restrições duras.** Do II.4, que não há estimador não
#' enviesado aplicável, então a conclusão é enunciada sobre a população
#' observada. Do II.6, que divisão temporal separa alocações e não pessoas, e
#' atributo com chave de entidade vaza. Do II.1, que a linha de base de
#' popularidade reportada lá era limite superior e precisava ser refeita aqui.
#'
#' @param dados Lista nomeada de tibbles, de `carregar_dados()`.
#' @param estabilidade tibble de `resumir_estabilidade()`, opcional.
#' @param fracao_teste Fração retida em cada forma de divisão.
#' @param tamanhos Tamanhos de lista da linha de base.
#' @return Lista com `objetos`, `tabelas`, `figuras` e `achados`.
executar_ii7 <- function(dados, estabilidade = NULL, fracao_teste = 0.3,
                          tamanhos = c(5, 10, 20, 50)) {
  com_atributo <- unique(dados$experiencia_setorial$consultor_id)
  formas <- c("temporal", "aleatoria", "por_consultor", "por_projeto")

  divisoes <- purrr::map(formas, function(forma) {
    medir_divisao(
      dividir_interacoes(dados$alocacoes, forma, fracao_teste = fracao_teste),
      com_atributo
    )
  }) |>
    purrr::list_rbind()

  protocolos <- avaliar_protocolos(divisoes)

  temporal <- protocolos[protocolos$forma == "temporal", ]
  aleatoria <- protocolos[protocolos$forma == "aleatoria", ]
  por_consultor <- protocolos[protocolos$forma == "por_consultor", ]

  # O quanto a divisão aleatória infla a avaliabilidade do colaborativo, que
  # é o número que decide o bloco: ela só consegue pontuar porque deixa o
  # mesmo projeto dos dois lados, o que em produção não acontece.
  inflacao_aleatoria <- aleatoria$avaliavel_colaborativo /
    temporal$avaliavel_colaborativo

  divisao_temporal <- dividir_interacoes(
    dados$alocacoes, "temporal", fracao_teste = fracao_teste
  )
  linha_base <- medir_linha_base_sob_protocolo(
    divisao_temporal$treino, divisao_temporal$teste, dados$consultores, tamanhos
  )
  linha_base_otimista <- medir_linha_base_popularidade(
    dados$alocacoes, dados$consultores, tamanhos
  )

  comparacao_base <- dplyr::inner_join(
    dplyr::select(linha_base, n_recomendados, recall_protocolo = recall),
    dplyr::select(linha_base_otimista, n_recomendados, recall_otimista = recall),
    by = "n_recomendados"
  ) |>
    dplyr::mutate(diferenca = recall_otimista - recall_protocolo)

  nenhuma_serve_aos_dois <- !any(protocolos$serve_colaborativo & protocolos$serve_conteudo)

  objetos <- list(
    divisoes = divisoes,
    protocolos = protocolos,
    temporal = temporal,
    aleatoria = aleatoria,
    por_consultor = por_consultor,
    inflacao_aleatoria = inflacao_aleatoria,
    linha_base = linha_base,
    comparacao_base = comparacao_base,
    nenhuma_serve_aos_dois = nenhuma_serve_aos_dois,
    fracao_teste = fracao_teste
  )

  tabelas <- list(
    ii7_divisoes = protocolos |>
      dplyr::transmute(
        Forma = rotular_forma_de_divisao(forma),
        Teste = formatar_inteiro(n_teste),
        `Consultor já visto` = formatar_pct(vazamento_consultor),
        `Projeto já visto` = formatar_pct(vazamento_projeto),
        `Colaborativo pontua` = formatar_pct(avaliavel_colaborativo),
        `Conteúdo pontua` = formatar_pct(avaliavel_conteudo)
      ),
    ii7_vereditos = protocolos |>
      dplyr::transmute(
        Forma = rotular_forma_de_divisao(forma),
        `Serve ao colaborativo` = ifelse(serve_colaborativo, "sim", "não"),
        `Serve ao conteúdo` = ifelse(serve_conteudo, "sim", "não"),
        `Respeita a ordem temporal` = ifelse(respeita_ordem_temporal, "sim", "não")
      ),
    ii7_linha_base = comparacao_base |>
      dplyr::transmute(
        `Tamanho da lista` = n_recomendados,
        `Sob o protocolo` = formatar_pct(recall_protocolo),
        `Versão do bloco II.1` = formatar_pct(recall_otimista),
        `Diferença` = formatar_pct(diferenca)
      )
  )

  figuras <- list(
    ii7_avaliabilidade = figura_avaliabilidade(
      protocolos,
      titulo = "O que cada forma de divisão deixa para cada componente",
      subtitulo = "Vazamento alto invalida o conteúdo; avaliabilidade baixa invalida o colaborativo"
    )
  )

  achados <- registrar_achados("II.7", tibble::tribble(
    ~codigo, ~etiqueta, ~enunciado, ~implicacao,

    "II.7.1", "instrumento",
    paste0(
      "Sob divisão temporal, o colaborativo consegue pontuar apenas ",
      formatar_pct(temporal$avaliavel_colaborativo),
      " dos pares de teste, porque só ",
      formatar_pct(temporal$vazamento_projeto),
      " dos projetos do teste aparecem no treino. A divisão aleatória eleva ",
      "a avaliabilidade para ",
      formatar_pct(aleatoria$avaliavel_colaborativo), ", ou ",
      formatar_decimal(inflacao_aleatoria, 1), " vezes mais."
    ),
    paste0(
      "**A diferença inteira vem de deixar o mesmo projeto dos dois lados**, ",
      "que em produção nunca acontece: projeto de consultoria é item efêmero, ",
      "aparece uma vez e não volta. Avaliar o colaborativo com divisão ",
      "aleatória reportaria um componente que funciona e que não poderia ",
      "funcionar. Conjugado ao bloco II.2, que mostrou a fatoração não ser ",
      "identificável nesta escala, o colaborativo não é só fraco: é **quase ",
      "inavaliável** sob qualquer protocolo honesto."
    ),

    "II.7.2", "instrumento",
    paste0(
      if (nenhuma_serve_aos_dois) {
        "Nenhuma das quatro formas de divisão serve aos dois componentes ao "
      } else {
        "Alguma forma de divisão serve aos dois componentes ao "
      },
      "mesmo tempo. A temporal deixa ",
      formatar_pct(temporal$vazamento_consultor),
      " dos consultores do teste já vistos no treino, o que invalida ",
      "avaliação de atributo de entidade. A divisão por consultor zera esse ",
      "vazamento e zera junto a avaliabilidade do colaborativo."
    ),
    paste0(
      "A oposição não é defeito de medida, é definição: separar pessoas para ",
      "não vazar atributo é exatamente criar cold start, e cold start é o ",
      "regime em que o colaborativo não tem o que dizer. **A decisão ",
      "habilitada é adotar dois protocolos, um por componente**, o que casa ",
      "com o híbrido chaveado do bloco II.3: cada componente é avaliado no ",
      "regime em que ele atua. Um protocolo único obrigaria a mentir sobre um ",
      "dos dois."
    ),

    "II.7.3", "instrumento",
    paste0(
      "A linha de base de popularidade do bloco II.1 era otimista nas listas ",
      "que importam, como aquele bloco já suspeitava. Refeita com a ",
      "popularidade contada só no treino e com desempate pela posição ",
      "esperada, o acerto em lista de 20 cai de ",
      formatar_pct(comparacao_base$recall_otimista[comparacao_base$n_recomendados == 20]),
      " para ",
      formatar_pct(comparacao_base$recall_protocolo[comparacao_base$n_recomendados == 20]),
      ", e em lista de 10 cai de ",
      formatar_pct(comparacao_base$recall_otimista[comparacao_base$n_recomendados == 10]),
      " para ",
      formatar_pct(comparacao_base$recall_protocolo[comparacao_base$n_recomendados == 10]),
      ". Nos extremos a correção não morde: em lista de 5 os dois praticamente ",
      "coincidem, e em lista de 50 coincidem."
    ),
    paste0(
      "Eram dois vícios. O primeiro, contar popularidade no mesmo dado em que ",
      "se avalia. O segundo, e menos evidente, **desempatar a favor do ",
      "consultor avaliado**: com contagens inteiras os empates são grandes, e ",
      "contar só quem está estritamente acima põe o focal na melhor posição ",
      "possível dentro do empate. A posição esperada sob desempate aleatório ",
      "é a medida correta, e o efeito se concentra no meio da faixa porque é ",
      "lá que o bloco de empatados decide se o focal entra na lista: em lista ",
      "muito curta ele fica fora de qualquer jeito, e em lista longa entra de ",
      "qualquer jeito. **Este é o número que a wave 3 precisa bater**, e ",
      "ele fica fixado agora, antes de existir modelo, que é o que impede ",
      "ajustá-lo ao resultado."
    ),

    "II.7.4", "instrumento",
    paste0(
      "O protocolo fica declarado: divisão por consultor para o conteúdo, ",
      "divisão temporal para o colaborativo, acerto em lista como métrica ",
      "com a linha de base de popularidade reportada ao lado em toda ",
      "comparação, e conclusão enunciada sobre a população observada."
    ),
    paste0(
      "Cada peça responde a uma restrição herdada e não a uma preferência. A ",
      "divisão por consultor vem do bloco II.6, que mediu o vazamento de ",
      "atributo de entidade. A temporal vem de não se prever o passado. A ",
      "linha de base ao lado vem do bloco II.1, onde a concentração torna a ",
      "popularidade forte o bastante para ser confundida com aprendizado. E ",
      "enunciar sobre a população observada vem do bloco II.4, onde nenhum ",
      "estimador não enviesado sobreviveu: o modelo não fala sobre os pares ",
      "que nunca tiveram chance de aparecer."
    )
  ))

  list(objetos = objetos, tabelas = tabelas, figuras = figuras, achados = achados)
}

#' Rótulo legível de cada forma de divisão
#'
#' Os nomes internos são identificadores, e a regra 4 do projeto exige
#' português legível em tudo que aparece no texto. Ponto único da tradução,
#' para figura e tabela não divergirem.
#'
#' @param forma Vetor com os nomes internos.
#' @return Vetor com os rótulos legíveis.
rotular_forma_de_divisao <- function(forma) {
  rotulos <- c(
    temporal = "Temporal",
    aleatoria = "Aleatória por alocação",
    por_consultor = "Por consultor",
    por_projeto = "Por projeto"
  )
  unname(rotulos[forma])
}
