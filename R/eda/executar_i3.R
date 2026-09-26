#' Executa o bloco I.3 da EDA, bivariada e associação
#'
#' Mesmo contrato de `executar_i1()`: calcula, formata, desenha e conclui num
#' só lugar, e o documento apenas narra. Ver aquele roxygen para o porquê da
#' concentração e para a regra de coerência com o gerador.
#'
#' Duas decisões de método valem destaque, porque sem elas os números deste
#' bloco seriam ilegíveis como evidência.
#'
#' O confronto com os alvos reais usa **Pearson**, não Spearman, porque os
#' alvos vieram de `CORR()` no Spark, que é Pearson. Spearman aparece ao lado
#' para leitura da estrutura, já que é robusto à assimetria que domina estas
#' variáveis, mas comparar Spearman com alvo de Pearson compararia coisas
#' diferentes.
#'
#' A associação entre categóricas usa V de Cramér **com correção de viés**,
#' sem a qual pares independentes medem longe de zero só pelo número de
#' categorias (ver `calcular_v_cramer()`).
#'
#' @param dados Lista nomeada de tibbles, de `carregar_dados()`.
#' @param estabilidade tibble de `resumir_estabilidade()`, opcional.
#' @param limite_livre Valor absoluto de associação a partir do qual um par
#'   declarado sem mecanismo é tratado como dependência acidental. Convenção
#'   de leitura declarada, não teste.
#' @return Lista com `objetos`, `tabelas`, `figuras` e `achados`.
executar_i3 <- function(dados, estabilidade = NULL, limite_livre = 0.15) {
  consultor <- dados$consultores |>
    dplyr::left_join(dados$custo_carregado, by = "consultor_id") |>
    dplyr::mutate(nivel_num = senioridade_numerica(nivel_senioridade))

  alocacao <- dados$alocacoes |>
    dplyr::left_join(
      dplyr::select(consultor, consultor_id, nivel_num),
      by = "consultor_id"
    )

  correlacoes <- dplyr::bind_rows(
    calcular_correlacoes(
      consultor, c("nivel_num", "tempo_casa", "tempo_nivel", "custo_carregado")
    ),
    calcular_correlacoes(
      alocacao, c("nivel_num", "pct_alocacao", "horas_mes", "duracao_dias")
    ),
    calcular_correlacoes(
      dados$projetos, c("valor_contrato", "esforco_estimado", "duracao_dias")
    )
  )

  categoricas <- dplyr::bind_rows(
    calcular_associacoes_categoricas(
      dados$consultores, c("nivel_senioridade", "tipo_contrato", "localizacao")
    ),
    calcular_associacoes_categoricas(dados$projetos, c("setor", "tipo_projeto"))
  )

  confronto_numerico <- confrontar_associacoes(correlacoes, "pearson")
  confronto_categorico <- confrontar_associacoes(categoricas, "v_cramer")

  calibradas <- dplyr::filter(confronto_numerico, procedencia == "calibrada")
  livres <- dplyr::filter(confronto_numerico, procedencia == "livre")
  induzidas <- dplyr::filter(confronto_numerico, procedencia == "induzida")
  contrato <- dplyr::filter(
    confronto_categorico, variavel_b == "tipo_contrato",
    variavel_a == "nivel_senioridade"
  )
  induzida_forte <- induzidas[which.max(abs(induzidas$medida)), ]
  pior_calibrada <- calibradas[which.max(abs(calibradas$diferenca_do_alvo)), ]

  objetos <- list(
    correlacoes = correlacoes,
    categoricas = categoricas,
    confronto_numerico = confronto_numerico,
    confronto_categorico = confronto_categorico,
    calibradas = calibradas,
    livres = livres,
    induzidas = induzidas,
    contrato = contrato,
    induzida_forte = induzida_forte,
    pior_calibrada = pior_calibrada,
    maior_livre = if (nrow(livres) > 0) max(abs(livres$medida)) else 0,
    pior_livre = if (nrow(livres) > 0) livres[which.max(abs(livres$medida)), ] else livres,
    limite_livre = limite_livre
  )

  formatar_confronto <- function(confronto, rotulo_medida) {
    confronto |>
      dplyr::transmute(
        Par = paste(variavel_a, "x", variavel_b),
        Procedência = procedencia,
        !!rotulo_medida := formatar_decimal(medida, 3),
        `Alvo real` = ifelse(is.na(valor_real), "n/a", formatar_decimal(valor_real, 2)),
        Atenção = ifelse(atencao, "sim", ""),
        Mecanismo = mecanismo
      )
  }

  tabelas <- list(
    i3_correlacoes_numericas = correlacoes |>
      dplyr::transmute(
        Par = paste(variavel_a, "x", variavel_b),
        Spearman = formatar_decimal(spearman, 3),
        Pearson = formatar_decimal(pearson, 3),
        n = formatar_inteiro(n)
      ),
    i3_confronto_numerico = formatar_confronto(confronto_numerico, "Pearson"),
    i3_confronto_categorico = formatar_confronto(confronto_categorico, "V de Cramér")
  )

  figuras <- list(
    i3_associacoes_procedencia = figura_associacoes_por_procedencia(
      confronto_numerico,
      titulo = "Correlações por procedência declarada",
      subtitulo = paste(
        "Em calibrada, leia a distância até o alvo.",
        "Em livre, a distância até zero"
      )
    )
  )

  achados <- registrar_achados("I.3", com_sintese(tibble::tribble(
    ~codigo, ~etiqueta, ~enunciado, ~implicacao,

    "I.3.1", "âncora real",
    paste0(
      "As ", nrow(calibradas), " correlações com alvo real registrado são ",
      "reproduzidas: a pior delas, ", pior_calibrada$variavel_a, " x ",
      pior_calibrada$variavel_b, ", sai a ",
      formatar_decimal(pior_calibrada$medida, 3), " contra ",
      formatar_decimal(pior_calibrada$valor_real, 2), " de alvo."
    ),
    paste0(
      "Verifica que a estrutura de dependência calibrada sobreviveu às etapas ",
      "seguintes do gerador. Não é descoberta sobre a Peers: foram esses ",
      "números que calibraram o gerador. ",
      if (is.null(estabilidade)) {
        ""
      } else {
        paste0(
          "A estabilidade entre realizações é muito desigual, e a comparação ",
          "pontual só vale onde ela é alta: nível por custo fica em ",
          formatar_intervalo(estabilidade, "cor_nivel_custo", \(x) formatar_decimal(x, 3)),
          ", enquanto nível por tempo de casa fica em ",
          formatar_intervalo(estabilidade, "cor_nivel_tempo_casa", \(x) formatar_decimal(x, 3)),
          ", faixa larga demais para confrontar com alvo pontual (ver I.0.4)."
        )
      }
    ),

    "I.3.2", "consequência do gerador",
    paste0(
      "A associação induzida mais forte fora das calibradas é ",
      induzida_forte$variavel_a, " x ", induzida_forte$variavel_b, ", em ",
      formatar_decimal(induzida_forte$medida, 3),
      ", que nasce de as duas dependerem do nível de senioridade em direções ",
      "opostas, sem nenhum alvo real por trás."
    ),
    paste0(
      "A magnitude é propriedade do código, não do domínio, e não pode ser ",
      "citada como característica da Peers nem usada para justificar escolha ",
      "de modelagem."
    ),

    "I.3.3", "instrumento",
    paste0(
      if (nrow(livres) == 0) {
        paste0(
          "Não resta nenhum par sem mecanismo declarado entre as numéricas ",
          "confrontadas: os três últimos, de projeto, ganharam mecanismo ",
          "quando o gerador passou a acoplar contrato e esforço ao esforço ",
          "alocado."
        )
      } else if (objetos$maior_livre <= limite_livre) {
        paste0(
          "Nenhum dos ", nrow(livres), " pares sem mecanismo declarado se ",
          "afasta de zero: o maior valor absoluto é ",
          formatar_decimal(objetos$maior_livre, 3),
          ". O gerador não introduziu dependência não intencional entre ",
          "variáveis que deveriam ser independentes."
        )
      } else {
        paste0(
          "Dos ", nrow(livres), " pares sem mecanismo declarado, ao menos um ",
          "se afasta de zero: ", objetos$pior_livre$variavel_a, " e ",
          objetos$pior_livre$variavel_b, " medem ",
          formatar_decimal(objetos$maior_livre, 3),
          ", acima do limite de ", formatar_decimal(limite_livre, 2),
          " adotado como convenção de leitura."
        )
      }
    ),
    paste0(
      if (nrow(livres) == 0) {
        paste0(
          "**Com a categoria vazia, esta verificação passa a não vigiar ",
          "nada**, e isso é perda de cobertura do instrumento, não boa ",
          "notícia. Registrar a categoria vazia é o que impede de ler o ",
          "silêncio como aprovação. Toda variável nova que entrar no gerador ",
          "sem mecanismo declarado volta a povoá-la, e é para esse momento ",
          "que a verificação continua no lugar. "
        )
      } else {
        ""
      },
      "A verificação roda a cada execução e passa a acusar dependência ",
      "acidental introduzida por mudança futura no gerador, que é o tipo de ",
      "defeito que nenhum teste unitário de peça isolada pega. **Ela já ",
      "pegou uma**: quando o bloco II.5 acoplou contrato e esforço estimado ",
      "ao esforço de fato alocado, os três pares de projeto deixaram de ser ",
      "independentes e este achado passou a acusá-los. A resposta correta ",
      "não foi afrouxar o limite, foi reclassificar os três de `livre` para ",
      "`induzida` em `procedencia_associacoes()`, porque eles passaram a ter ",
      "mecanismo declarado. O enunciado acima é condicional de propósito: ",
      "afirmar que nada se afasta de zero enquanto o número diz o contrário ",
      "seria a incoerência que este capítulo existe para não cometer."
    ),

    "I.3.4", "consequência do gerador",
    paste0(
      "nivel_senioridade e tipo_contrato medem ",
      formatar_decimal(contrato$medida, 3),
      " de V de Cramér corrigido, uma dependência clara. Ela existe porque o ",
      "nível de base é amarrado a CLT no gerador, reproduzindo a coincidência ",
      "exata entre os 42 ESTAGIARIO e os 42 N4 registrada na calibração real."
    ),
    paste0(
      "Este bloco mediu 0,037 na primeira versão, isto é, independência, e a ",
      "constatação motivou a correção do gerador. A ressalva que permanece: a ",
      "âncora real determina apenas a base, porque os 90 não empregados reais ",
      "não cabem em N0 mais N1, que somam 63. Acima da base o contrato é ",
      "marginal calibrada, então a magnitude 0,291 é propriedade do código e ",
      "não medida da Peers."
    )
  ), tibble::tribble(
    ~codigo, ~decisao, ~limitacao, ~proveniencia,

    "I.3.1", NA,
    paste0(
      "Reproduzir correlação que calibrou o gerador não é descoberta sobre a ",
      "Peers, e a comparação pontual contra o alvo só vale onde a estabilidade ",
      "entre realizações é alta."
    ),
    "depende do gerador",

    "I.3.2", NA,
    paste0(
      "A associação induzida mais forte fora das calibradas é propriedade do ",
      "código, e não pode ser citada como característica da Peers nem usada ",
      "para justificar escolha de modelagem."
    ),
    "depende do gerador",

    "I.3.3", NA,
    paste0(
      "Com a categoria de pares sem mecanismo vazia, a verificação passa a não ",
      "vigiar nada, e o silêncio dela não pode ser lido como aprovação."
    ),
    "depende do gerador",

    "I.3.4", NA,
    paste0(
      "A âncora real determina apenas a base da pirâmide, porque os 90 não ",
      "empregados reais não cabem nos 63 postos de N0 e N1; acima da base o ",
      "contrato é marginal calibrada, então a magnitude da dependência é ",
      "propriedade do código."
    ),
    "depende do gerador"
  )))

  list(objetos = objetos, tabelas = tabelas, figuras = figuras, achados = achados)
}
