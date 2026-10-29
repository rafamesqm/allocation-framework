#' Resume o híbrido e o piso composto por quadrante do consultor realizado
#'
#' @param pares Elemento `pares` de `avaliar_hibrido()`.
#' @return tibble com uma linha por quadrante e uma de total: `quadrante`,
#'   `pares`, `fracao`, `acerto_hibrido` e `acerto_piso`.
resumir_hibrido_por_quadrante <- function(pares) {
  por_quadrante <- pares |>
    dplyr::summarise(
      pares = dplyr::n(), acerto_hibrido = mean(acerto_hibrido),
      acerto_piso = mean(acerto_piso_composto), .by = quadrante
    ) |>
    dplyr::mutate(quadrante = factor(quadrante, levels = listar_quadrantes())) |>
    tidyr::complete(quadrante, fill = list(pares = 0L)) |>
    dplyr::arrange(quadrante) |>
    dplyr::mutate(quadrante = as.character(quadrante))

  total <- nrow(pares)
  total_hibrido <- mean(pares$acerto_hibrido)
  total_piso <- mean(pares$acerto_piso_composto)
  dplyr::bind_rows(
    por_quadrante,
    tibble::tibble(quadrante = "todos", pares = total, acerto_hibrido = total_hibrido, acerto_piso = total_piso)
  ) |>
    dplyr::mutate(fracao = pares / total, .after = pares)
}

#' Executa o bloco da Etapa 4, a montagem do híbrido e `ŝ`
#'
#' @param avaliacao Lista de `avaliar_hibrido()`.
#' @param s_hat tibble de `calcular_s_hat()`.
#' @param registro Lista de `ler_pre_registro()`.
#' @return Lista com `objetos`, `tabelas`, `figuras` e `achados`.
executar_w3_hibrido <- function(avaliacao, s_hat, registro) {
  lista <- registro$metrica$lista_decisoria
  resumo <- resumir_hibrido_por_quadrante(avaliacao$pares)
  q <- \(nome) resumo[resumo$quadrante == nome, ]
  todos <- q("todos")
  coincide <- with(resumo[resumo$pares > 0, ], all(abs(acerto_hibrido - acerto_piso) < 1e-12))

  topo <- avaliacao$listas[avaliacao$listas$posicao <= lista, ]
  fracao_topo_formulario <- mean(topo$ramo == "conteúdo")
  so_formulario <- q("só formulário")

  candidatos <- s_hat[!is.na(s_hat$s_hat), ]
  morta <- s_hat[s_hat$quadrante == "zona morta", ]

  objetos <- list(resumo = resumo, coincide = coincide, fracao_topo_formulario = fracao_topo_formulario)

  pct_ou_traco <- \(x) ifelse(is.na(x), "--", formatar_pct(x))
  tabelas <- list(
    w3_hibrido_quadrantes = resumo |>
      dplyr::transmute(
        `Quadrante do consultor realizado` = quadrante,
        Pares = formatar_inteiro(pares),
        `Fração dos pares` = formatar_pct(fracao),
        `Acerto do híbrido` = pct_ou_traco(acerto_hibrido),
        `Acerto do piso composto` = pct_ou_traco(acerto_piso),
        `Piso do quadrante` = c(
          "popularidade dentro do filtro", "popularidade", "acaso dentro do filtro",
          "fora da lista, conta como erro", "composto"
        )[match(quadrante, c(listar_quadrantes(), "todos"))]
      ),
    w3_s_hat_resumo = s_hat |>
      dplyr::summarise(
        pares = dplyr::n(), projetos = dplyr::n_distinct(projeto_id),
        mediana = stats::median(s_hat), ausentes = sum(is.na(s_hat)), .by = quadrante
      ) |>
      dplyr::mutate(quadrante = factor(quadrante, levels = listar_quadrantes())) |>
      dplyr::arrange(quadrante) |>
      dplyr::transmute(
        Quadrante = as.character(quadrante),
        `Pares consultor e projeto` = formatar_inteiro(pares),
        Projetos = formatar_inteiro(projetos),
        `Mediana de ŝ` = ifelse(is.na(mediana), "--", formatar_decimal(mediana, 2)),
        `ŝ ausente` = formatar_inteiro(ausentes)
      )
  )

  achados <- registrar_achados("W3.4", com_sintese(tibble::tribble(
    ~codigo, ~etiqueta, ~enunciado, ~implicacao,

    "W3.4.1", "instrumento",
    paste0(
      "Montado com as adoções das Etapas 2 e 3, o híbrido acerta ",
      formatar_pct(todos$acerto_hibrido), " dos ", formatar_inteiro(todos$pares),
      " pares do teste temporal em lista de ", lista, ", e o piso composto acerta ",
      formatar_pct(todos$acerto_piso), ". Por quadrante do consultor realizado: ",
      "histórico e formulário, ", formatar_pct(q("histórico e formulário")$acerto_hibrido),
      " contra ", formatar_pct(q("histórico e formulário")$acerto_piso), " em ",
      formatar_inteiro(q("histórico e formulário")$pares), " pares; só histórico, ",
      formatar_pct(q("só histórico")$acerto_hibrido), " contra ",
      formatar_pct(q("só histórico")$acerto_piso), " em ",
      formatar_inteiro(q("só histórico")$pares), "; só formulário, ",
      formatar_pct(so_formulario$acerto_hibrido), " contra ",
      formatar_pct(so_formulario$acerto_piso), " em ", formatar_inteiro(so_formulario$pares),
      ". A zona morta responde por ", formatar_pct(q("zona morta")$fracao),
      " dos pares, que contam como erro porque o modelo não a recomenda."
    ),
    paste0(
      if (coincide) {
        "**O híbrido coincide com o piso composto em todos os quadrantes.** "
      } else {
        "**O híbrido diverge do piso composto em algum quadrante**, o que, com estas adoções, só pode ser defeito de implementação. "
      },
      "Com os dois testes sem rejeição, o colaborativo ordena pela popularidade e o ",
      "conteúdo deixa a ordem ao acaso dentro do filtro, e o híbrido se reduz, por ",
      "construção, à popularidade filtrada para quem tem histórico e ao acaso dentro ",
      "do filtro para quem não tem. É o desfecho que o pré-registro aceitou e que a ",
      "síntese da EDA anteviu (III.4): não é falha a corrigir com ajuste, é a medida ",
      "de quanto sinal estes dados carregam para ordenar consultores."
    ),

    "W3.4.2", "instrumento",
    paste0(
      "Quem só respondeu o formulário ocupa ", formatar_pct(fracao_topo_formulario),
      " das ", lista, " primeiras posições das listas dos projetos de teste, e dos ",
      formatar_inteiro(so_formulario$pares), " pares cujo consultor realizado está nesse ",
      "quadrante, ", formatar_pct(so_formulario$acerto_hibrido), " entram na lista de ", lista, ". ",
      "A medida é sobre as listas do teste temporal; sobre todas as listas de `ŝ`, a ",
      "ocupação está no achado W3.4.4."
    ),
    paste0(
      "É a consequência da ordem da lista, que põe quem tem histórico antes de quem só ",
      "tem formulário: sem histórico, a popularidade é zero, e o conteúdo, que só filtra, ",
      "não tem como trazer essas pessoas para o topo."
    ),

    "W3.4.3", "instrumento",
    paste0(
      "`ŝ` sai para ", formatar_inteiro(nrow(candidatos)), " pares de candidato e projeto, ",
      "em ", formatar_inteiro(dplyr::n_distinct(candidatos$projeto_id)), " projetos, ",
      "como o percentil do consultor na lista do projeto, com a popularidade contada sem ",
      "o próprio projeto. A zona morta tem ", formatar_inteiro(nrow(morta)), " pares com ",
      "`ŝ` ausente, e os pares fora do filtro não têm linha. Cada linha leva o ramo que ",
      "a ordenou, o quadrante e se o par passou no filtro."
    ),
    paste0(
      "`ŝ` é o insumo da wave 4, e esta wave não o usa para nada além da avaliação. ",
      "Contar a popularidade sem o próprio projeto impede que `ŝ` de um projeto ",
      "histórico carregue a alocação que a wave 4 vai reproduzir ou contestar. A ",
      "explicação de cada valor vem da arquitetura: o ramo, o filtro e a posição na ",
      "popularidade."
    )
  ), tibble::tribble(
    ~codigo, ~decisao, ~limitacao, ~proveniencia,

    "W3.4.1", NA,
    paste0(
      "A medida é descritiva e vale para a população observada: um híbrido que ",
      "reproduz a popularidade filtrada adere ao processo atual de alocação, com o ",
      "viés de seleção que ele carrega (II.4.5), e não mostra adequação."
    ),
    "estrutural",

    "W3.4.2", NA,
    paste0(
      "Com quem tem histórico antes de quem só tem formulário, estes raramente entram ",
      "na lista de ", lista, ", e a inclusão deles depende da regra de exposição ",
      "definida fora do modelo, como na zona morta."
    ),
    "estrutural",

    "W3.4.3",
    paste0(
      "`ŝ`, insumo da wave 4, é o percentil do consultor na lista do projeto, ",
      "ausente para a zona morta e sem valor para pares fora do filtro, com a ",
      "popularidade contada sem o próprio projeto."
    ),
    paste0(
      "Para quem tem histórico, `ŝ` é popularidade, e para quem só tem formulário, ",
      "acaso dentro do filtro: na wave 4 ele carrega aderência ao processo atual, e ",
      "não adequação da pessoa ao projeto. A popularidade é contada sobre o histórico ",
      "inteiro da janela, inclusive alocações posteriores ao início do projeto; ",
      "contá-la só até a data de início é decisão da wave 4."
    ),
    "estrutural"
  )))

  list(objetos = objetos, tabelas = tabelas, figuras = list(), achados = achados)
}
