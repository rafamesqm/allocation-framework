#' Agregados reais da Peers, em forma legível por máquina
#'
#' Transcrição das estatísticas agregadas levantadas ao vivo no Unity
#' Catalog e registradas em `docs/mapeamento_schema.md`. Mesmo papel que
#' `especificacao_tabelas()` cumpre para o esquema: permite confrontar o
#' sintético com o real por código, em vez de o texto afirmar semelhança.
#'
#' **O que uma concordância aqui significa, e o que não significa.** Todos
#' estes números foram usados para calibrar o gerador. Bater com eles não é
#' descoberta sobre a Peers, é verificação de que a calibração foi de fato
#' aplicada e sobreviveu às etapas seguintes do gerador. É um teste de
#' regressão sobre a fidelidade do sintético, e é assim que o capítulo deve
#' apresentá-lo. O que seria informativo é uma divergência: ela indicaria
#' que alguma etapa posterior distorceu a distribuição calibrada, como já
#' aconteceu com a truncagem de grau na wave 1.
#'
#' Cobre apenas o que está registrado com número explícito na documentação.
#' Variável sem linha aqui não tem âncora empírica, e qualquer afirmação
#' sobre ela no capítulo é sobre o gerador, não sobre a empresa.
#'
#' A assimetria segue a definição populacional do Spark, ver
#' `calcular_assimetria()`.
#'
#' @return tibble com `tabela`, `variavel`, `estatistica`, `categoria`
#'   (preenchida só para proporções), `valor_real` e `rodada` (a rodada de
#'   calibração de onde o número veio).
referencias_reais <- function() {
  tibble::tribble(
    ~tabela, ~variavel, ~estatistica, ~categoria, ~valor_real, ~rodada,

    # Camada 1: Consultores ------------------------------------------------
    # `custo_carregado` não entra com dispersão de propósito. Os valores da
    # Segunda rodada (desvio 453,50, P25 286,58, P50 485,70, P75 944,62)
    # mediam variação histórica da tabela de taxas, isto é, reajustes da
    # mesma sigla de cargo ao longo do tempo, não variação entre pessoas num
    # mesmo momento. A Quinta rodada superou essa leitura ao descobrir que
    # cada sigla tem uma taxa vigente única. Confrontar a dispersão entre os
    # 285 consultores contra aquele número compararia populações distintas.
    "consultores", "nivel_senioridade", "prop", "N4", 42 / 285, "Primeira",
    "consultores", "nivel_senioridade", "prop", "N3", 107 / 285, "Primeira",
    "consultores", "nivel_senioridade", "prop", "N2", 71 / 285, "Primeira",
    "consultores", "nivel_senioridade", "prop", "N1", 55 / 285, "Primeira",
    "consultores", "nivel_senioridade", "prop", "N0", 8 / 285, "Primeira",

    "consultores", "tipo_contrato", "prop", "CLT", 0.684, "Sexta",

    "consultores", "localizacao", "prop", "SAO PAULO", 182 / 285, "Quarta",
    "consultores", "localizacao", "prop", "MINAS GERAIS", 70 / 285, "Quarta",
    "consultores", "localizacao", "prop", "RIO DE JANEIRO", 13 / 285, "Quarta",

    "consultores", "disposicao_viagem_nacional", "prop", "TRUE", 175 / 285, "Quarta",
    "consultores", "disposicao_viagem_internacional", "prop", "TRUE", 185 / 285, "Quarta",

    # Camada 1: Projetos ---------------------------------------------------
    # Quantis de valor_contrato são dos positivos, e a assimetria também:
    # a fonte real os reporta sob o rótulo "só positivos".
    "projetos", "valor_contrato", "fracao_zero", NA_character_, 0.147, "Segunda",
    "projetos", "valor_contrato", "p25_pos", NA_character_, 34814, "Segunda",
    "projetos", "valor_contrato", "mediana_pos", NA_character_, 108300, "Segunda",
    "projetos", "valor_contrato", "p75_pos", NA_character_, 375054, "Segunda",
    "projetos", "valor_contrato", "assimetria_pos", NA_character_, 12.68, "Segunda",

    "projetos", "esforco_estimado", "desvio", NA_character_, 66.90, "Segunda",
    "projetos", "esforco_estimado", "p25", NA_character_, 32, "Segunda",
    "projetos", "esforco_estimado", "mediana", NA_character_, 120, "Segunda",
    "projetos", "esforco_estimado", "p75", NA_character_, 160, "Segunda",
    "projetos", "esforco_estimado", "assimetria", NA_character_, -0.05, "Segunda",

    "projetos", "duracao_dias", "media", NA_character_, 68.8, "Sexta",
    "projetos", "duracao_dias", "desvio", NA_character_, 119.7, "Sexta",
    "projetos", "duracao_dias", "p25", NA_character_, 4, "Sexta",
    "projetos", "duracao_dias", "mediana", NA_character_, 18, "Sexta",
    "projetos", "duracao_dias", "p75", NA_character_, 81, "Sexta",
    "projetos", "duracao_dias", "assimetria", NA_character_, 3.6, "Sexta",
    "projetos", "duracao_dias", "maximo", NA_character_, 1184, "Sexta",

    "projetos", "setor", "prop", "Varejo", 1097 / 4086, "Terceira",
    "projetos", "setor", "prop", "Serviços", 773 / 4086, "Terceira",
    "projetos", "setor", "prop", "Indústria", 510 / 4086, "Terceira",
    "projetos", "setor", "prop", "Banking", 384 / 4086, "Terceira",
    "projetos", "setor", "prop", "Educação", 348 / 4086, "Terceira",
    "projetos", "setor", "prop", "Health", 315 / 4086, "Terceira",
    "projetos", "setor", "prop", "Insurance", 240 / 4086, "Terceira",
    "projetos", "setor", "prop", "Agronegócio", 182 / 4086, "Terceira",
    "projetos", "setor", "prop", "Alimentos e Bebidas", 165 / 4086, "Terceira",
    "projetos", "setor", "prop", "Private Equity", 72 / 4086, "Terceira",

    "projetos", "tipo_projeto", "prop", "Experience & Efficiency", 991 / 4214, "Terceira",
    "projetos", "tipo_projeto", "prop", "Strategy and M&A", 642 / 4214, "Terceira",
    "projetos", "tipo_projeto", "prop",
    "Data Analytics & Artificial Intelligence", 530 / 4214, "Terceira",
    "projetos", "tipo_projeto", "prop", "Supply Chain", 524 / 4214, "Terceira",
    "projetos", "tipo_projeto", "prop", "Tech Consulting", 427 / 4214, "Terceira",
    "projetos", "tipo_projeto", "prop", "Finance", 424 / 4214, "Terceira",

    # Camada 2: Alocações --------------------------------------------------
    "alocacoes", "duracao_dias", "media", NA_character_, 51.6, "Sexta",
    "alocacoes", "duracao_dias", "desvio", NA_character_, 86.8, "Sexta",
    "alocacoes", "duracao_dias", "p25", NA_character_, 4, "Sexta",
    "alocacoes", "duracao_dias", "mediana", NA_character_, 18, "Sexta",
    "alocacoes", "duracao_dias", "p75", NA_character_, 60, "Sexta",
    "alocacoes", "duracao_dias", "assimetria", NA_character_, 3.97, "Sexta",

    "alocacoes", "horas_mes", "desvio", NA_character_, 66.90, "Segunda",
    "alocacoes", "horas_mes", "mediana", NA_character_, 120, "Segunda",

    # Camada 3: Operação ---------------------------------------------------
    # Na fonte real, fração de zero, média, desvio e assimetria de receita e
    # custo são gerais; só os quantis são dos positivos.
    "margem_frente_mes", "receita", "fracao_zero", NA_character_, 0.159, "Quinta",
    "margem_frente_mes", "receita", "media", NA_character_, 192076.84, "Quinta",
    "margem_frente_mes", "receita", "desvio", NA_character_, 202058.84, "Quinta",
    "margem_frente_mes", "receita", "assimetria", NA_character_, 1.85, "Quinta",
    "margem_frente_mes", "receita", "p25_pos", NA_character_, 46105.50, "Quinta",
    "margem_frente_mes", "receita", "mediana_pos", NA_character_, 136901.46, "Quinta",
    "margem_frente_mes", "receita", "p75_pos", NA_character_, 268683.84, "Quinta",

    "margem_frente_mes", "custo", "fracao_zero", NA_character_, 0.447, "Quinta",
    "margem_frente_mes", "custo", "media", NA_character_, 14315.61, "Quinta",
    "margem_frente_mes", "custo", "desvio", NA_character_, 17980.01, "Quinta",
    "margem_frente_mes", "custo", "p25_pos", NA_character_, 16000, "Quinta",
    "margem_frente_mes", "custo", "mediana_pos", NA_character_, 21000, "Quinta",
    "margem_frente_mes", "custo", "p75_pos", NA_character_, 26250, "Quinta"
  )
}

#' Confronta o resumo univariado de uma tabela com os agregados reais
#'
#' Junta o resultado de `resumir_numericas()` com `referencias_reais()` e
#' calcula a diferença relativa. Variável sem âncora registrada não aparece
#' no resultado, o que é intencional: o silêncio aqui é a informação de que
#' aquela variável não tem contraparte real para comparar.
#'
#' A diferença relativa não é calculada quando o valor real é zero, para não
#' produzir infinito onde a leitura correta é diferença absoluta.
#'
#' @param resumo tibble de `resumir_numericas()`.
#' @param nome_tabela Nome da tabela, como aparece em `referencias_reais()`.
#' @param referencias tibble de `referencias_reais()`.
#' @return tibble com `variavel`, `estatistica`, `valor_real`,
#'   `valor_sintetico`, `diferenca_relativa` e `rodada`.
confrontar_com_real <- function(resumo, nome_tabela, referencias = referencias_reais()) {
  alvos <- referencias |>
    dplyr::filter(tabela == nome_tabela, estatistica != "prop")

  resumo |>
    tidyr::pivot_longer(
      cols = -c(variavel, n, n_ausente),
      names_to = "estatistica",
      values_to = "valor_sintetico"
    ) |>
    dplyr::inner_join(alvos, by = c("variavel", "estatistica")) |>
    dplyr::mutate(
      diferenca_relativa = dplyr::if_else(
        valor_real == 0, NA_real_, (valor_sintetico - valor_real) / valor_real
      )
    ) |>
    dplyr::select(
      variavel, estatistica, valor_real, valor_sintetico, diferenca_relativa, rodada
    ) |>
    dplyr::arrange(variavel, estatistica)
}

#' Confronta a composição de uma variável categórica com as proporções reais
#'
#' Contraparte de `confrontar_com_real()` para o lado categórico. Devolve uma
#' linha por categoria com âncora registrada, mais a proporção observada no
#' sintético.
#'
#' Categoria real sem correspondente no sintético aparece com proporção
#' sintética zero, em vez de sumir do resultado: categoria calibrada que o
#' gerador nunca produziu é exatamente o tipo de falha que precisa ficar
#' visível.
#'
#' @param tabela Data frame ou tibble com a variável.
#' @param nome_tabela Nome da tabela, como em `referencias_reais()`.
#' @param nome_variavel Nome da coluna categórica.
#' @param referencias tibble de `referencias_reais()`.
#' @return tibble com `categoria`, `proporcao_real`, `proporcao_sintetica`,
#'   `diferenca` (em pontos de proporção) e `rodada`.
confrontar_proporcoes <- function(tabela, nome_tabela, nome_variavel,
                                   referencias = referencias_reais()) {
  alvos <- referencias |>
    dplyr::filter(tabela == nome_tabela, variavel == nome_variavel, estatistica == "prop")

  observado <- tabela[[nome_variavel]]
  proporcoes <- tibble::tibble(categoria = as.character(observado)) |>
    dplyr::count(categoria) |>
    dplyr::mutate(proporcao_sintetica = n / sum(n)) |>
    dplyr::select(categoria, proporcao_sintetica)

  alvos |>
    dplyr::select(categoria, proporcao_real = valor_real, rodada) |>
    dplyr::left_join(proporcoes, by = "categoria") |>
    dplyr::mutate(
      proporcao_sintetica = dplyr::coalesce(proporcao_sintetica, 0),
      diferenca = proporcao_sintetica - proporcao_real
    ) |>
    dplyr::select(categoria, proporcao_real, proporcao_sintetica, diferenca, rodada) |>
    dplyr::arrange(dplyr::desc(proporcao_real))
}

#' Tabelas cujo sinal a fonte real deriva da campanha de formulários de skills
#'
#' Declaração, não inferência. A lista vem do crosswalk de
#' `docs/mapeamento_schema.md`, que aponta `skills_hard[]`,
#' `experiencia_setorial[]`, `expertise_funcional` e `soft_skills[]` para os
#' arquivos de `data/raw/skills/`, com `certificacoes[]` como campo opcional
#' dentro dos mesmos formulários.
#'
#' Precisa ser declarada porque não dá para reconhecer essas tabelas pela
#' cobertura medida: `alocacoes` e `ocupacao_consultor` também cobrem parte
#' dos consultores, por motivo inteiramente diferente, e um filtro por
#' cobertura parcial as arrastaria junto.
#'
#' `idiomas_consultor` fica de fora: a fonte é
#' `vw_apdata_bi_entidades_globais`, base corporativa sem nulo entre os 285
#' ativos, e não o formulário.
#'
#' @param incluir_opcionais Se `TRUE`, inclui `certificacoes_consultor`, que
#'   é campo opcional dentro do formulário e por isso cobre um subconjunto
#'   dos respondentes, não todos eles.
#' @return Vetor com os nomes das tabelas.
tabelas_derivadas_de_formulario <- function(incluir_opcionais = FALSE) {
  obrigatorias <- c(
    "skills_consultor",
    "experiencia_setorial",
    "expertise_funcional",
    "soft_skills_consultor"
  )

  if (incluir_opcionais) c(obrigatorias, "certificacoes_consultor") else obrigatorias
}
