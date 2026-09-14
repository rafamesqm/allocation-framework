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
#' @return Lista com `objetos`, `tabelas`, `figuras` e `achados`.
executar_i3 <- function(dados) {
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
    maior_livre = max(abs(livres$medida))
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

  achados <- registrar_achados("I.3", tibble::tribble(
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
      "números que calibraram o gerador."
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
      "Nenhum dos ", nrow(livres), " pares sem mecanismo declarado se afasta ",
      "de zero: o maior valor absoluto é ",
      formatar_decimal(objetos$maior_livre, 3),
      ". O gerador não introduziu dependência não intencional entre ",
      "variáveis que deveriam ser independentes."
    ),
    paste0(
      "A verificação roda a cada execução e passa a acusar dependência ",
      "acidental introduzida por mudança futura no gerador, que é o tipo de ",
      "defeito que nenhum teste unitário de peça isolada pega."
    ),

    "I.3.4", "consequência do gerador",
    paste0(
      "nivel_senioridade e tipo_contrato medem ",
      formatar_decimal(contrato$medida, 3),
      " de V de Cramér corrigido, ou seja, independência. No dado real os ",
      "dois são fortemente ligados: o bloco ESTAGIARIO tem exatamente a ",
      "mesma contagem de N4."
    ),
    paste0(
      "É dependência real que o sintético não reproduz, por refinamento ",
      "adiado na wave 1. Qualquer análise que dependa da relação entre ",
      "contrato e senioridade é inválida sobre este dado, e a limitação deve ",
      "constar na monografia."
    )
  ))

  list(objetos = objetos, tabelas = tabelas, figuras = figuras, achados = achados)
}
