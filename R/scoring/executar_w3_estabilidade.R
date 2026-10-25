#' Grandezas centrais da wave 3, com o rótulo de cada uma para o texto
#'
#' São as que o roteiro da Etapa 6 pede: acertos, pisos, diferenças, ganhos
#' mínimos e proporções de dominância, mais a avaliabilidade do colaborativo
#' e o híbrido. Os nomes levam o prefixo da fonte, para não colidirem.
#'
#' @return tibble com `medida`, `etapa` e `rotulo`.
listar_grandezas_centrais_w3 <- function() {
  tibble::tribble(
    ~medida, ~etapa, ~rotulo,
    "conteudo_acerto_mapeamento", "Conteúdo, emenda 2", "Acerto do mapeamento",
    "conteudo_acerto_piso_filtro", "Conteúdo, emenda 2", "Piso de filtro com acaso",
    "conteudo_diferenca_mapeamento", "Conteúdo, emenda 2", "Mapeamento menos piso",
    "conteudo_ganho_minimo_dominancia", "Conteúdo, emenda 2", "Ganho mínimo, otimista",
    "conteudo_ganho_minimo_independencia", "Conteúdo, emenda 2", "Ganho mínimo, conservador",
    "conteudo_supera_piso", "Conteúdo, emenda 2", "Rejeição a favor do mapeamento",
    "colaborativo_fracao_avaliavel", "Colaborativo", "Fração avaliável",
    "colaborativo_acerto_popularidade_esperado", "Colaborativo", "Piso, acerto esperado",
    "colaborativo_acerto_popularidade", "Colaborativo", "Piso, empates sorteados",
    "colaborativo_acerto_binario", "Colaborativo", "Acerto da fatoração binária",
    "colaborativo_diferenca_h1", "Colaborativo", "H1: binária menos piso",
    "colaborativo_diferenca_h2", "Colaborativo", "H2: confiança menos binária",
    "colaborativo_ganho_minimo_h1_dominancia", "Colaborativo", "Ganho mínimo de H1, otimista",
    "colaborativo_ganho_minimo_h1_independencia", "Colaborativo", "Ganho mínimo de H1, conservador",
    "colaborativo_supera_h1", "Colaborativo", "Rejeição em H1",
    "hibrido_acerto", "Híbrido", "Acerto do híbrido",
    "hibrido_menos_piso_composto", "Híbrido", "Híbrido menos piso composto",
    "hibrido_acerto_historico_e_formulario", "Híbrido", "Acerto, histórico e formulário",
    "hibrido_acerto_so_historico", "Híbrido", "Acerto, só histórico",
    "conteudo_recomendado_melhor", "Segundo nível", "Conteúdo: recomendado melhor",
    "conteudo_recomendado_pior", "Segundo nível", "Conteúdo: recomendado pior",
    "conteudo_incomparaveis", "Segundo nível", "Conteúdo: incomparáveis",
    "colaborativo_recomendado_melhor", "Segundo nível", "Colaborativo: recomendado melhor",
    "colaborativo_recomendado_pior", "Segundo nível", "Colaborativo: recomendado pior",
    "colaborativo_incomparaveis", "Segundo nível", "Colaborativo: incomparáveis",
    "colaborativo_recomendado_mais_frequente", "Segundo nível", "Colaborativo: peso do recomendado mais frequente",
    "colaborativo_realizado_com_mais_folga", "Segundo nível", "Colaborativo: realizado com mais folga"
  )
}

#' Junta as realizações das três fontes numa tabela só
#'
#' @param conteudo,colaborativo,segundo_nivel tibbles de realizações, com
#'   `semente`.
#' @return tibble com `semente` e as colunas prefixadas pela fonte; as do
#'   segundo nível já vêm com o prefixo do ramo.
juntar_realizacoes_w3 <- function(conteudo, colaborativo, segundo_nivel) {
  prefixar <- function(tabela, prefixo) {
    dplyr::rename_with(tabela, \(nome) paste0(prefixo, nome), -semente)
  }
  prefixar(conteudo, "conteudo_") |>
    dplyr::inner_join(prefixar(colaborativo, "colaborativo_"), by = "semente") |>
    dplyr::inner_join(segundo_nivel, by = "semente")
}

#' Executa o bloco da Etapa 6, a estabilidade das grandezas centrais
#'
#' Cada grandeza tem a mediana e o intervalo entre realizações, e a posição
#' da realização versionada nele. Ponto fora não é defeito: um sorteio em
#' cada dez cai fora de um intervalo de 90% por definição. O que não pode é
#' o texto citar o ponto sem dizer que ele caiu fora.
#'
#' @param versionado tibble de uma linha, com as mesmas colunas de
#'   `juntar_realizacoes_w3()` menos `semente`.
#' @param replicas tibble de `juntar_realizacoes_w3()`.
#' @param registro Lista de `ler_pre_registro()`.
#' @return Lista com `objetos`, `tabelas`, `figuras` e `achados`.
executar_w3_estabilidade <- function(versionado, replicas, registro) {
  centrais <- listar_grandezas_centrais_w3()
  estabilidade <- resumir_estabilidade(replicas[, c("semente", centrais$medida)], nivel = registro$estabilidade$nivel)
  v <- as.list(versionado)

  situacao <- centrais |>
    dplyr::left_join(estabilidade, by = "medida") |>
    dplyr::mutate(
      versionado = purrr::map_dbl(medida, \(m) v[[m]]),
      posicao = purrr::map2_chr(medida, versionado, \(m, x) posicao_no_intervalo(estabilidade, m, x))
    )
  fora <- situacao[situacao$posicao != "dentro", ]
  pct <- \(m) formatar_intervalo(estabilidade, m)
  pontos <- \(x) formatar_decimal(100 * x, 1)
  ponto_de <- \(m) formatar_pct(v[[m]])

  objetos <- list(estabilidade = estabilidade, situacao = situacao, fora = fora)

  tabelas <- list(
    w3_estabilidade = situacao |>
      dplyr::transmute(
        Etapa = etapa, Grandeza = rotulo,
        `Realização versionada` = formatar_decimal(versionado, 4),
        Mediana = formatar_decimal(mediana, 4),
        `Intervalo de 90%` = paste0(formatar_decimal(inferior, 4), " a ", formatar_decimal(superior, 4)),
        `Posição da versionada` = posicao,
        `Réplicas` = n_replicas
      )
  )

  diferenca_maxima <- max(abs(replicas$hibrido_menos_piso_composto))
  achados <- registrar_achados("W3.6", com_sintese(tibble::tribble(
    ~codigo, ~etiqueta, ~enunciado, ~implicacao,

    "W3.6.1", "instrumento",
    paste0(
      "Das ", nrow(situacao), " grandezas centrais da wave 3, a realização versionada cai ",
      "fora do intervalo de 90% entre ", nrow(replicas), " realizações em ", nrow(fora),
      if (nrow(fora) == 0) {
        "."
      } else {
        paste0(
          ": ", paste(paste0(fora$rotulo, " (", fora$etapa, ", ", fora$posicao, ")"), collapse = "; "), "."
        )
      }
    ),
    paste0(
      "Um sorteio em cada dez cai fora de um intervalo de 90% por definição, e grandezas ",
      "correlacionadas caem fora juntas, como o bloco I.0 mostrou. Onde a versionada cai ",
      "fora, o texto cita o intervalo, e não o ponto, como o valor típico."
    ),

    "W3.6.2", "instrumento",
    paste0(
      "Entre realizações, no conteúdo o recomendado é melhor por dominância em ",
      pct("conteudo_recomendado_melhor"), " das discordâncias e pior em ",
      pct("conteudo_recomendado_pior"), ". No colaborativo, é melhor em ",
      pct("colaborativo_recomendado_melhor"), " e pior em ",
      pct("colaborativo_recomendado_pior"), "; o recomendado mais frequente responde por ",
      pct("colaborativo_recomendado_mais_frequente"), " das comparações, e o realizado tem ",
      "mais folga em ", pct("colaborativo_realizado_com_mais_folga"), "."
    ),
    paste0(
      "As duas leituras do segundo nível se sustentam entre realizações, e não só na ",
      "versionada: no colaborativo, reproduzir a popularidade é recomendar quase sempre ",
      "a mesma pessoa, já ocupada; no conteúdo, o sorteio dentro do filtro ganha ",
      "sobretudo no setor. As magnitudes são do gerador."
    ),

    "W3.6.3", "instrumento",
    paste0(
      "O acerto do híbrido em lista de 20 fica em ", pct("hibrido_acerto"),
      " entre realizações, contra ", ponto_de("hibrido_acerto"), " na versionada; para quem ",
      "tem histórico e formulário, em ", pct("hibrido_acerto_historico_e_formulario"),
      ", e para quem só tem histórico, em ", pct("hibrido_acerto_so_historico"),
      descrever_ponto_fora(c(
        "acerto do híbrido" = posicao_no_intervalo(estabilidade, "hibrido_acerto", v$hibrido_acerto)
      )),
      ". A maior diferença entre o híbrido e o piso composto, em todas as realizações, é ",
      "de ", pontos(diferenca_maxima), " pontos."
    ),
    paste0(
      if (diferenca_maxima == 0) {
        "**A igualdade entre híbrido e piso composto vale em todas as realizações**, e não só na versionada, como a construção garante com estas adoções. "
      } else {
        "**O híbrido diverge do piso composto em alguma realização**, o que com estas adoções só pode ser defeito de implementação. "
      },
      "A variação do acerto entre realizações é a da popularidade filtrada, e não de ",
      "algum sinal do modelo.",
      if (identical(posicao_no_intervalo(estabilidade, "hibrido_acerto", v$hibrido_acerto), "dentro")) {
        ""
      } else {
        paste0(
          " O acerto que o achado W3.4.1 cita para a versionada não é o típico: o valor ",
          "a citar como acerto do híbrido é o intervalo entre realizações."
        )
      }
    ),

    "W3.6.4", "instrumento",
    paste0(
      "A adoção é a da realização versionada, popularidade no colaborativo e acaso no ",
      "conteúdo, e não é decidida de novo em cada realização. A decisão teria mudado em ",
      formatar_pct(mean(replicas$conteudo_supera_piso), 0), " das realizações no conteúdo e em ",
      formatar_pct(mean(replicas$colaborativo_supera_h1), 0), " no colaborativo."
    ),
    paste0(
      "Os intervalos do segundo nível e do híbrido medem a variabilidade do modelo ",
      "adotado, e não a da decisão. A estabilidade da decisão é a das frequências de ",
      "rejeição, já reportadas nos achados W3.2.7 e W3.3.2."
    )
  ), tibble::tribble(
    ~codigo, ~decisao, ~limitacao, ~proveniencia,

    "W3.6.1", NA,
    paste0(
      "Os intervalos medem quanto cada número se moveria se a população sintética fosse ",
      "sorteada de novo, e não incerteza sobre a organização (I.0.1)."
    ),
    "estrutural",

    "W3.6.4", NA,
    paste0(
      "Os intervalos do segundo nível e do híbrido supõem a adoção da realização ",
      "versionada; numa realização em que o teste rejeitasse, o híbrido seria outro."
    ),
    "estrutural"
  )))

  list(objetos = objetos, tabelas = tabelas, figuras = list(), achados = achados)
}
