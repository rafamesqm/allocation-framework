#' Agregados reais da organização, em forma legível por máquina
#'
#' Transcrição das estatísticas agregadas levantadas ao vivo no Unity
#' Catalog e registradas em `docs/mapeamento_schema.md`. Mesmo papel que
#' `especificacao_tabelas()` cumpre para o esquema: permite confrontar o
#' sintético com o real por código, em vez de o texto afirmar semelhança.
#'
#' **O que uma concordância aqui significa, e o que não significa.** Todos
#' estes números foram usados para calibrar o gerador. Bater com eles não é
#' descoberta sobre a organização, é verificação de que a calibração foi de fato
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
    # Recalibrados pela segunda consulta comercial, da base comercial de projetos, tabela
    # completa e mesma fonte da âncora de margem. Os valores anteriores
    # vinham da fonte de alocação do sistema de gestão de projetos e davam mediana quatro vezes
    # menor; misturar as duas fontes era o que fazia a margem não fechar.
    "projetos", "valor_contrato", "fracao_zero", NA_character_, 0.147, "Segunda consulta",
    "projetos", "valor_contrato", "p25_pos", NA_character_, 190000, "Segunda consulta",
    "projetos", "valor_contrato", "mediana_pos", NA_character_, 457000, "Segunda consulta",
    "projetos", "valor_contrato", "p75_pos", NA_character_, 1100000, "Segunda consulta",

    # Idem: a duração passa a vir da base comercial de projetos, em semanas (p10/50/90 de
    # 3,6/11,6/43,6), convertidas em dias. A calibração anterior media outra
    # coisa e deixava o projeto curto demais para acumular as horas que o
    # contrato implica.
    "projetos", "duracao_dias", "p25", NA_character_, 42, "Segunda consulta",
    "projetos", "duracao_dias", "mediana", NA_character_, 81, "Segunda consulta",
    "projetos", "duracao_dias", "p75", NA_character_, 156, "Segunda consulta",
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

    "projetos", "tipo_projeto", "prop", "oferta_01", 991 / 4214, "Terceira",
    "projetos", "tipo_projeto", "prop", "oferta_02", 642 / 4214, "Terceira",
    "projetos", "tipo_projeto", "prop", "oferta_03", 530 / 4214, "Terceira",
    "projetos", "tipo_projeto", "prop", "oferta_04", 524 / 4214, "Terceira",
    "projetos", "tipo_projeto", "prop", "oferta_05", 427 / 4214, "Terceira",
    "projetos", "tipo_projeto", "prop", "oferta_06", 424 / 4214, "Terceira",

    # Camada 2: Alocações --------------------------------------------------
    "alocacoes", "duracao_dias", "media", NA_character_, 51.6, "Sexta",
    "alocacoes", "duracao_dias", "desvio", NA_character_, 86.8, "Sexta",
    "alocacoes", "duracao_dias", "p25", NA_character_, 4, "Sexta",
    "alocacoes", "duracao_dias", "mediana", NA_character_, 18, "Sexta",
    "alocacoes", "duracao_dias", "p75", NA_character_, 60, "Sexta",
    "alocacoes", "duracao_dias", "assimetria", NA_character_, 3.97, "Sexta",

    # Estes cinco ficaram registrados como `esforco_estimado` de projeto até
    # a wave 2.5, mas a origem é a fonte de alocação de pessoas por projeto, uma linha
    # por pessoa alocada: são horas mensais de consultor, não esforço de
    # projeto. O esforço de projeto não tem âncora real e passou a ser
    # derivado das alocações (ver `acoplar_projetos_ao_esforco()`).
    "alocacoes", "horas_mes", "desvio", NA_character_, 66.90, "Segunda",
    "alocacoes", "horas_mes", "p25", NA_character_, 32, "Segunda",
    "alocacoes", "horas_mes", "mediana", NA_character_, 120, "Segunda",
    "alocacoes", "horas_mes", "p75", NA_character_, 160, "Segunda",
    "alocacoes", "horas_mes", "assimetria", NA_character_, -0.05, "Segunda",

    # Camada 3: Operação ---------------------------------------------------
    # Os quantis da receita mensal vêm da segunda consulta às bases
    # comerciais, sobre meses com receita positiva. Substituem os da Quinta
    # rodada, que liam a mesma fonte como receita e custo quando ela é
    # receita decomposta por origem. A fração de zero segue sendo a das
    # linhas totalmente zeradas.
    "receita_projeto_mes", "receita_total", "fracao_zero", NA_character_, 0.159, "Segunda consulta",
    "receita_projeto_mes", "receita_total", "p25_pos", NA_character_, 90000, "Segunda consulta",
    "receita_projeto_mes", "receita_total", "mediana_pos", NA_character_, 172000, "Segunda consulta",
    "receita_projeto_mes", "receita_total", "p75_pos", NA_character_, 304000, "Segunda consulta"
  )
}

#' Âncoras de margem no nível do projeto, da segunda consulta comercial
#'
#' Separadas de `referencias_reais()` porque não são resumo univariado de
#' coluna de tabela sintética: são a distribuição da margem do projeto
#' inteiro, que o gerador não emite como coluna e o bloco II.5 calcula.
#'
#' Vêm da base comercial de projetos, tabela **completa** e sem teto de extração, última
#' versão por CRM, 556 projetos. A consulta mediu que tamanho, duração,
#' margem e desconto são estáveis entre os anos cobertos e não cobertos pelo
#' truncamento, então esta âncora não sofre do viés que afeta as grandezas
#' mensais.
#'
#' É a régua que faltava ao bloco II.5: antes dela, a plausibilidade da
#' margem estrutural era julgamento meu.
#'
#' @return tibble com `estatistica` e `valor_real`.
referencias_margem_projeto <- function() {
  tibble::tribble(
    ~estatistica, ~valor_real,
    "p05", 0.175,
    "p25", 0.272,
    "mediana", 0.320,
    "p75", 0.345,
    "p95", 0.401,
    "fracao_prejuizo", 0.02
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
#' a base corporativa de colaboradores, sem nulo entre os 285
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
