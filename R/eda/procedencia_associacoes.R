#' Procedência declarada de cada associação entre variáveis
#'
#' Sem este registro, a matriz de correlação do sintético é ilegível como
#' evidência: toda associação que ela mostra foi posta ali por alguém, e o
#' leitor não tem como distinguir a que reproduz o dado real da que é efeito
#' colateral do código. O registro declara, antes de olhar o número, o que
#' cada par deveria ser.
#'
#' Quatro procedências:
#'
#' * `calibrada`: o gerador induz a associação **e** existe valor real
#'   registrado em `docs/mapeamento_schema.md` para confrontar. Reproduzir o
#'   alvo é verificação de que a calibração sobreviveu, não descoberta.
#' * `induzida`: o gerador cria a associação por construção, mas não há alvo
#'   real. A magnitude é propriedade do código, não da Peers, e não deve ser
#'   citada como característica do domínio.
#' * `livre`: nenhum mecanismo liga as duas variáveis, que são sorteadas de
#'   forma independente. **A expectativa é associação nula**, e um valor
#'   materialmente diferente de zero indica dependência não intencional no
#'   gerador, que é achado de verdade.
#' * `ausente`: existe dependência documentada no dado real que o gerador
#'   **não** reproduz. A expectativa é associação nula no sintético, e isso é
#'   uma limitação a declarar, não um acerto.
#'
#' @return tibble com `variavel_a`, `variavel_b`, `procedencia`,
#'   `valor_real`, `rodada` e `mecanismo`.
procedencia_associacoes <- function() {
  tibble::tribble(
    ~variavel_a, ~variavel_b, ~procedencia, ~valor_real, ~rodada, ~mecanismo,

    # Consultor -------------------------------------------------------------
    "nivel_num", "custo_carregado", "calibrada", 0.90, "Segunda",
    "custo reamostrado de uma banda de cargos por nível",
    "nivel_num", "tempo_casa", "calibrada", 0.34, "Segunda",
    "modelo linear em nível mais ruído, calibrado por momentos",
    "tempo_casa", "tempo_nivel", "induzida", NA_real_, NA_character_,
    "tempo no nível é fração do tempo de casa, logo limitado por ele",
    "nivel_num", "tempo_nivel", "induzida", NA_real_, NA_character_,
    "herdada por tempo_nivel derivar de tempo_casa, que depende do nível",
    "tempo_casa", "custo_carregado", "induzida", NA_real_, NA_character_,
    "indireta: as duas dependem do nível de senioridade",
    "tempo_nivel", "custo_carregado", "induzida", NA_real_, NA_character_,
    "indireta pela mesma cadeia, mais fraca",

    # Alocação --------------------------------------------------------------
    "nivel_num", "pct_alocacao", "calibrada", 0.52, "Quarta",
    "amostragem condicional ao nível por casamento de momentos",
    "nivel_num", "horas_mes", "calibrada", -0.55, "Quarta",
    "idem, com alvo negativo: mais sênior dedica menos hora de execução",
    "pct_alocacao", "horas_mes", "induzida", NA_real_, NA_character_,
    "indireta e de sinal oposto: as duas dependem do nível em direções contrárias",
    "duracao_dias", "pct_alocacao", "livre", NA_real_, NA_character_,
    "janela da alocação é sorteada sem olhar para intensidade",
    "duracao_dias", "horas_mes", "livre", NA_real_, NA_character_,
    "idem",

    # Projeto ---------------------------------------------------------------
    # As três reclassificadas de `livre` para `induzida` quando o bloco II.5
    # mostrou que a independência entre porte financeiro e esforço tornava a
    # margem estrutural sem sentido (ver `acoplar_projetos_ao_esforco()`).
    # O contrato passa a ser permutado pelo posto das horas de fato alocadas,
    # o que o liga ao esforço e à duração. A existência do vínculo é
    # estrutura reproduzida; a força do acoplamento é suposição declarada,
    # sem alvo real. Esforço e duração voltaram a `livre` na wave 2.5: o
    # esforço passou a ser a soma das horas mensais da equipe, que não cresce
    # com a duração do projeto, e o par mediu perto de zero.
    "valor_contrato", "esforco_estimado", "induzida", NA_real_, NA_character_,
    "o contrato é ordenado pelas horas alocadas, das quais o esforço mensal é um fator",
    "valor_contrato", "duracao_dias", "induzida", NA_real_, NA_character_,
    "alocação mais longa acumula mais horas, e as horas ordenam o contrato",
    "esforco_estimado", "duracao_dias", "livre", NA_real_, NA_character_,
    "o esforço soma as horas mensais da equipe, e o tamanho da equipe é sorteado sem olhar para a duração",

    # Categóricas -----------------------------------------------------------
    # Reclassificada de `ausente` para `induzida` quando o gerador passou a
    # amarrar contrato ao nível (ver `gerar_tipo_contrato()`). A âncora real
    # é a coincidência exata entre os 42 ESTAGIARIO e os 42 N4, que determina
    # a base; acima dela não há de-para documentado, então a magnitude da
    # associação é propriedade do código e não há alvo real para confrontar.
    "nivel_senioridade", "tipo_contrato", "induzida", NA_real_, "Sexta",
    "nível de base amarrado a CLT pela coincidência ESTAGIARIO igual a N4; marginal calibrada acima dela",
    "nivel_senioridade", "localizacao", "livre", NA_real_, NA_character_,
    "sorteios independentes",
    "setor", "tipo_projeto", "livre", NA_real_, NA_character_,
    "Segmento e Offering sorteados de forma independente no gerador",
    "localizacao", "tipo_contrato", "livre", NA_real_, NA_character_,
    "sorteios independentes"
  )
}

#' Confronta as associações medidas com a procedência declarada
#'
#' Junta a medida com o registro e marca cada par como conforme ou digno de
#' atenção. O critério depende da procedência, e é isso que torna a tabela
#' interpretável: para `calibrada` o que importa é a distância do alvo real;
#' para `livre` e `ausente`, o que importa é a distância de zero.
#'
#' `limite_livre` é o quanto uma associação sem mecanismo pode se afastar de
#' zero antes de merecer investigação. O default 0,10 é uma convenção de
#' leitura, não um teste: com 285 consultores, correlação amostral em torno
#' de 0,06 é o que se espera por acaso sob independência, e 0,10 dá alguma
#' folga sobre isso sem deixar passar dependência real.
#'
#' @param medidas tibble com `variavel_a`, `variavel_b` e a coluna de medida.
#' @param coluna_medida Nome da coluna com o valor medido.
#' @param procedencia tibble de `procedencia_associacoes()`.
#' @param limite_livre Distância de zero tolerada em par `livre` ou `ausente`.
#' @return tibble com a medida, a procedência, o alvo real quando houver, e
#'   `atencao`, verdadeiro quando o par foge do esperado.
confrontar_associacoes <- function(medidas, coluna_medida,
                                    procedencia = procedencia_associacoes(),
                                    limite_livre = 0.10) {
  medidas |>
    dplyr::rename(medida = dplyr::all_of(coluna_medida)) |>
    dplyr::inner_join(procedencia, by = c("variavel_a", "variavel_b")) |>
    dplyr::mutate(
      diferenca_do_alvo = dplyr::if_else(
        procedencia == "calibrada", medida - valor_real, NA_real_
      ),
      atencao = dplyr::case_when(
        procedencia == "calibrada" ~ abs(medida - valor_real) > 0.10,
        procedencia %in% c("livre", "ausente") ~ abs(medida) > limite_livre,
        .default = FALSE
      )
    ) |>
    dplyr::select(
      variavel_a, variavel_b, procedencia, medida, valor_real,
      diferenca_do_alvo, atencao, mecanismo, rodada
    ) |>
    dplyr::arrange(procedencia, dplyr::desc(abs(medida)))
}
