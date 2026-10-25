#' Executa o bloco da revisão da wave 3: dependência, listas e ocupação
#'
#' Tudo aqui é descritivo e nada muda decisão. Reúne:
#'
#' * a dependência entre pares do mesmo consultor, pelo McNemar agrupado de
#'   Durkalski et al. (2003), análise posterior ao resultado;
#' * o acerto nas listas descritivas do pré-registro, no conteúdo, no
#'   colaborativo e no híbrido;
#' * a distribuição do tamanho do filtro e a ocupação das listas do híbrido.
#'
#' @param conteudo_e1,conteudo_e2,colaborativo Listas com `avaliacao` e
#'   `replicas` de cada componente.
#' @param segundo_nivel Lista com `versionado` e `replicas` de
#'   `medir_segundo_nivel()`.
#' @param tamanho_do_filtro Lista de `medir_tamanho_do_filtro()`.
#' @param ocupacao Lista de `medir_ocupacao_das_listas()`.
#' @param registro Lista de `ler_pre_registro()`.
#' @return Lista com `objetos`, `tabelas`, `figuras` e `achados`.
executar_w3_revisao <- function(conteudo_e1, conteudo_e2, colaborativo, segundo_nivel,
                                tamanho_do_filtro, ocupacao, registro) {
  versionado <- list(
    `conteúdo, emenda 1` = resumir_avaliacao_do_conteudo(conteudo_e1$avaliacao, registro),
    `conteúdo, emenda 2` = resumir_avaliacao_do_conteudo_cruzado(conteudo_e2$avaliacao, registro),
    colaborativo = resumir_avaliacao_do_colaborativo(colaborativo$avaliacao, registro)
  )
  replicas <- list(
    `conteúdo, emenda 1` = conteudo_e1$replicas,
    `conteúdo, emenda 2` = conteudo_e2$replicas,
    colaborativo = colaborativo$replicas
  )
  valor_p_comum <- c(
    `conteúdo, emenda 1` = versionado[[1]]$valor_p_mcnemar,
    `conteúdo, emenda 2` = versionado[[2]]$valor_p_mcnemar,
    colaborativo = versionado[[3]]$valor_p_h1
  )

  dependencia <- purrr::imap(replicas, function(r, componente) {
    v <- versionado[[componente]]
    tibble::tibble(
      componente = componente,
      valor_p_comum = valor_p_comum[[componente]],
      valor_p_agrupado = v$agrupado_valor_p,
      comum_a_favor = mean(r$comum_rejeita_a_favor),
      comum_contra = mean(r$comum_rejeita_contra),
      agrupado_a_favor = mean(r$agrupado_rejeita_a_favor),
      agrupado_contra = mean(r$agrupado_rejeita_contra),
      variancia_padronizada = stats::var(r$mcnemar_padronizado),
      consultores_de_teste = v$consultores_de_teste
    )
  }) |>
    purrr::list_rbind()
  dep <- \(componente) dependencia[dependencia$componente == componente, ]

  # Listas descritivas e a decisória, lado a lado, para cada componente.
  tamanhos <- sort(c(registro$metrica$listas_descritivas, registro$metrica$lista_decisoria))
  coluna_da_lista <- function(prefixo, n, decisoria) {
    if (n == registro$metrica$lista_decisoria) decisoria else paste0("lista_", prefixo, "_", n)
  }
  extrair <- function(tabela, prefixo, n, decisoria) tabela[[coluna_da_lista(prefixo, n, decisoria)]]
  segundo_versionado <- dplyr::mutate(segundo_nivel$versionado, hibrido_piso = hibrido_acerto - hibrido_menos_piso_composto)
  segundo_replicas <- dplyr::mutate(segundo_nivel$replicas, hibrido_piso = hibrido_acerto - hibrido_menos_piso_composto)
  especificacao <- tibble::tribble(
    ~componente, ~fonte, ~prefixo_modelo, ~decisoria_modelo, ~prefixo_piso, ~decisoria_piso,
    "Conteúdo, emenda 1", "conteúdo, emenda 1", "acerto_modelo", "acerto_mapeamento", "acerto_piso", "acerto_piso_filtro",
    "Conteúdo, emenda 2", "conteúdo, emenda 2", "acerto_modelo", "acerto_mapeamento", "acerto_piso", "acerto_piso_filtro",
    "Colaborativo", "colaborativo", "acerto_binario", "acerto_binario", "acerto_popularidade", "acerto_popularidade",
    "Híbrido", "híbrido", "acerto_hibrido", "hibrido_acerto", "acerto_piso_composto", "hibrido_piso"
  )
  listas <- purrr::pmap(especificacao, function(componente, fonte, prefixo_modelo, decisoria_modelo, prefixo_piso, decisoria_piso) {
    v <- if (fonte == "híbrido") segundo_versionado else versionado[[fonte]]
    r <- if (fonte == "híbrido") segundo_replicas else replicas[[fonte]]
    purrr::map(tamanhos, function(n) {
      modelo_r <- extrair(r, prefixo_modelo, n, decisoria_modelo)
      piso_r <- extrair(r, prefixo_piso, n, decisoria_piso)
      faixa <- \(x) paste0(
        formatar_pct(stats::median(x)), " (", formatar_pct(stats::quantile(x, 0.05)), " a ",
        formatar_pct(stats::quantile(x, 0.95)), ")"
      )
      tibble::tibble(
        componente = componente, lista = n,
        modelo = extrair(v, prefixo_modelo, n, decisoria_modelo),
        piso = extrair(v, prefixo_piso, n, decisoria_piso),
        modelo_entre_realizacoes = faixa(modelo_r),
        piso_entre_realizacoes = faixa(piso_r),
        diferenca_mediana = stats::median(modelo_r - piso_r)
      )
    }) |>
      purrr::list_rbind()
  }) |>
    purrr::list_rbind()
  resumir_listas <- function(componente) {
    linhas <- listas[listas$componente == componente, ]
    paste0(
      "em lista de ", linhas$lista, ", ", formatar_pct(linhas$modelo), " contra ",
      formatar_pct(linhas$piso), " na versionada, com diferença mediana de ",
      formatar_pontos(linhas$diferenca_mediana), " entre realizações",
      collapse = "; "
    )
  }
  # A maior diferença de cada componente, na versionada e na mediana entre
  # realizações, só nas listas que o filtro não esgota.
  maior_diferenca <- function(componente, coluna) {
    linhas <- listas[listas$componente == componente & listas$lista < tamanho_do_filtro$maior_setor, ]
    if (coluna == "versionada") max(abs(linhas$modelo - linhas$piso)) else max(abs(linhas$diferenca_mediana))
  }

  topo <- ocupacao$topo
  por_consultor <- ocupacao$listas_por_consultor
  topo_de <- \(q) {
    x <- topo$fracao[topo$quadrante == q]
    if (length(x) == 0) 0 else x
  }
  listas_de <- \(q) por_consultor[por_consultor$quadrante == q, ]

  objetos <- list(dependencia = dependencia, listas = listas, ocupacao = ocupacao, tamanho_do_filtro = tamanho_do_filtro)

  tabelas <- list(
    w3_dependencia_dos_pares = dependencia |>
      dplyr::transmute(
        Componente = componente,
        `Valor-p do McNemar comum, versionada` = formatar_decimal(valor_p_comum, 3),
        `Valor-p do McNemar agrupado, versionada` = formatar_decimal(valor_p_agrupado, 3),
        `Comum: rejeita a favor / contra` = paste0(formatar_pct(comum_a_favor, 0), " / ", formatar_pct(comum_contra, 0)),
        `Agrupado: rejeita a favor / contra` = paste0(formatar_pct(agrupado_a_favor, 0), " / ", formatar_pct(agrupado_contra, 0)),
        `Variância da estatística padronizada do comum` = formatar_decimal(variancia_padronizada, 2),
        `Consultores de teste, versionada` = formatar_inteiro(consultores_de_teste)
      ),
    w3_listas_descritivas = listas |>
      dplyr::transmute(
        Componente = componente, Lista = lista,
        `Modelo, versionada` = formatar_pct(modelo), `Piso, versionada` = formatar_pct(piso),
        `Modelo entre realizações` = modelo_entre_realizacoes,
        `Piso entre realizações` = piso_entre_realizacoes,
        `Diferença mediana entre realizações` = formatar_pontos(diferenca_mediana)
      ),
    w3_tamanho_do_filtro = tamanho_do_filtro$distribuicao |>
      dplyr::mutate(fracao = pares / sum(pares)) |>
      dplyr::transmute(
        `Tamanho do filtro` = tamanho_do_filtro, `Pares que passam o filtro` = pares,
        `Fração` = formatar_pct(fracao)
      ),
    w3_hibrido_ocupacao = dplyr::full_join(
      topo |> dplyr::select(quadrante, fracao_topo = fracao),
      por_consultor, by = "quadrante"
    ) |>
      dplyr::mutate(quadrante = factor(quadrante, levels = listar_quadrantes())) |>
      dplyr::arrange(quadrante) |>
      dplyr::transmute(
        Quadrante = as.character(quadrante),
        `Fração das 20 primeiras posições` = formatar_pct(dplyr::coalesce(fracao_topo, 0)),
        Consultores = formatar_inteiro(consultores),
        `Listas em que entra, em média` = formatar_decimal(listas_media, 1),
        `Fração das listas` = formatar_pct(fracao_das_listas)
      )
  )

  achados_conteudo <- registrar_achados("W3.2", com_sintese(tibble::tribble(
    ~codigo, ~etiqueta, ~enunciado, ~implicacao,

    "W3.2.10", "instrumento",
    paste0(
      "Análise descritiva, posterior ao resultado: os pares de teste se agrupam por ",
      "consultor, e o McNemar comum os trata como independentes. Entre realizações, o ",
      "McNemar comum rejeita a favor do mapeamento em ", formatar_pct(dep("conteúdo, emenda 1")$comum_a_favor, 0),
      " e contra em ", formatar_pct(dep("conteúdo, emenda 1")$comum_contra, 0), " sob a emenda 1, e em ",
      formatar_pct(dep("conteúdo, emenda 2")$comum_a_favor, 0), " e ",
      formatar_pct(dep("conteúdo, emenda 2")$comum_contra, 0), " sob a emenda 2; no colaborativo, em ",
      formatar_pct(dep("colaborativo")$comum_a_favor, 0), " e ", formatar_pct(dep("colaborativo")$comum_contra, 0),
      ". Sem efeito, cada direção rejeitaria em até 2,5%. O McNemar agrupado por consultor, de ",
      "Durkalski et al. (2003), rejeita a favor e contra em ",
      formatar_pct(dep("conteúdo, emenda 1")$agrupado_a_favor, 0), " e ",
      formatar_pct(dep("conteúdo, emenda 1")$agrupado_contra, 0), " sob a emenda 1, ",
      formatar_pct(dep("conteúdo, emenda 2")$agrupado_a_favor, 0), " e ",
      formatar_pct(dep("conteúdo, emenda 2")$agrupado_contra, 0), " sob a emenda 2, e ",
      formatar_pct(dep("colaborativo")$agrupado_a_favor, 0), " e ", formatar_pct(dep("colaborativo")$agrupado_contra, 0),
      " no colaborativo. A variância da estatística padronizada do McNemar comum entre ",
      "realizações é de ", formatar_decimal(dep("conteúdo, emenda 1")$variancia_padronizada, 2), " e ",
      formatar_decimal(dep("conteúdo, emenda 2")$variancia_padronizada, 2), " no conteúdo e de ",
      formatar_decimal(dep("colaborativo")$variancia_padronizada, 2), " no colaborativo, contra 1 sob ",
      "pares independentes. Na realização versionada, o agrupado dá p = ",
      formatar_decimal(dep("conteúdo, emenda 2")$valor_p_agrupado, 3), " sob a emenda 2, contra ",
      formatar_decimal(dep("conteúdo, emenda 2")$valor_p_comum, 3), " do comum."
    ),
    paste0(
      "**O teste do conteúdo é anticonservador**, porque os pares se agrupam por ",
      "consultor: a rejeição nas duas direções passa do que o acaso daria, e a variância ",
      "da estatística padronizada fica acima de 1. Pelo mesmo motivo, o intervalo de ",
      "Newcombe e o ganho mínimo do conteúdo ficam otimistas, mais estreitos do que a ",
      "dependência permite. O colaborativo, com pares quase independentes, não mostra o ",
      "efeito. A decisão não muda: o teste pré-registrado não rejeitou, e o ajuste pela ",
      "dependência só tornaria a rejeição mais difícil."
    ),

    "W3.2.11", "instrumento",
    paste0(
      "O maior setor tem ", tamanho_do_filtro$maior_setor, " respondentes. Dos ",
      formatar_inteiro(tamanho_do_filtro$pares_que_passam), " pares que passam o filtro, ",
      formatar_pct(tamanho_do_filtro$fracao_ate_lista), " estão em filtros de ",
      registro$metrica$lista_decisoria, " candidatos ou menos, em que quem passa o filtro ",
      "entra na lista qualquer que seja a ordem, e ", formatar_pct(tamanho_do_filtro$fracao_acima_da_lista),
      " em filtros maiores. Nas listas descritivas, sob a emenda 2, o mapeamento contra o ",
      "filtro com acaso fica ", resumir_listas("Conteúdo, emenda 2"), "."
    ),
    paste0(
      "A lista de 20 deixa pouco espaço para ordenar dentro do filtro: só os pares em ",
      "filtros maiores que 20 podem discordar entre o mapeamento e o acaso, e a lista de 50, ",
      "maior que todo filtro, coincide com o piso por construção. Nas listas curtas, que ",
      "ordenam mais, a versionada chega a ", formatar_pontos(maior_diferenca("Conteúdo, emenda 2", "versionada")),
      " de diferença, mas a diferença mediana entre realizações não passa de ",
      formatar_pontos(maior_diferenca("Conteúdo, emenda 2", "mediana")),
      ": a vantagem da versionada é do sorteio desta realização, e a realização típica ",
      "não separa o mapeamento do piso. São listas descritivas do pré-registro, sem ",
      "teste de decisão."
    )
  ), tibble::tribble(
    ~codigo, ~decisao, ~limitacao, ~proveniencia,
    "W3.2.10", NA,
    paste0(
      "A análise é posterior ao resultado e descritiva; o intervalo de Newcombe e o ganho ",
      "mínimo do conteúdo, que supõem pares independentes, ficam otimistas."
    ),
    "estrutural",
    "W3.2.11", NA,
    paste0(
      "O tamanho do filtro é do sintético; no dado real o espaço setorial é mais fino ",
      "(II.6.1), e o espaço que a lista deixa para ordenar precisa ser recontado."
    ),
    "depende do gerador"
  )))

  achados_colaborativo <- registrar_achados("W3.3", tibble::tribble(
    ~codigo, ~etiqueta, ~enunciado, ~implicacao,
    "W3.3.6", "instrumento",
    paste0(
      "Nas listas descritivas, a fatoração binária contra a popularidade fica ",
      resumir_listas("Colaborativo"), ", nos pares avaliáveis."
    ),
    paste0(
      "Entre realizações, a diferença mediana entre a fatoração e a popularidade não passa ",
      "de ", formatar_pontos(maior_diferenca("Colaborativo", "mediana")), " em nenhum ",
      "tamanho de lista, o que repete, fora da lista de 20, o desfecho de H1 (W3.3.2); as ",
      "diferenças da versionada, de até ", formatar_pontos(maior_diferenca("Colaborativo", "versionada")),
      ", são de poucos pares avaliáveis. Listas descritivas do pré-registro, sem teste de ",
      "decisão."
    )
  ))

  achados_hibrido <- registrar_achados("W3.4", com_sintese(tibble::tribble(
    ~codigo, ~etiqueta, ~enunciado, ~implicacao,

    "W3.4.4", "instrumento",
    paste0(
      "Sobre todas as ", formatar_inteiro(ocupacao$projetos), " listas de `ŝ`, e não só as dos ",
      "projetos de teste, em que o achado W3.4.2 foi medido, as ", registro$metrica$lista_decisoria,
      " primeiras posições ficam com quem só tem histórico em ", formatar_pct(topo_de("só histórico")),
      ", com quem tem histórico e formulário em ", formatar_pct(topo_de("histórico e formulário")),
      " e com quem só tem formulário em ", formatar_pct(topo_de("só formulário")), ". Um consultor ",
      "que só tem histórico entra, em média, em ", formatar_decimal(listas_de("só histórico")$listas_media, 1),
      " listas, ou ", formatar_pct(listas_de("só histórico")$fracao_das_listas), " delas; um que tem ",
      "histórico e formulário, em ", formatar_decimal(listas_de("histórico e formulário")$listas_media, 1),
      "; um que só tem formulário, em ", formatar_decimal(listas_de("só formulário")$listas_media, 1), "."
    ),
    paste0(
      "**Quem não respondeu o formulário entra em todas as listas, sem filtro, e quem ",
      "respondeu só nas do próprio setor.** Somado à ordem por popularidade, isso põe o topo ",
      "das listas nas mãos de quem tem histórico e não declarou setor. O formulário, que ",
      "deveria abrir espaço pelo conteúdo, restringe quem o respondeu."
    ),

    "W3.4.5", "instrumento",
    paste0(
      "Nas listas descritivas, o híbrido contra o piso composto fica ", resumir_listas("Híbrido"),
      ", no teste temporal."
    ),
    paste0(
      "Em todo tamanho de lista o híbrido coincide com o piso composto, como na lista de 20 ",
      "(W3.4.1), por construção das adoções."
    )
  ), tibble::tribble(
    ~codigo, ~decisao, ~limitacao, ~proveniencia,
    "W3.4.4", NA,
    paste0(
      "A assimetria entre quem respondeu e quem não respondeu o formulário vem da ",
      "comutação decidida na EDA (II.3.3), e a inclusão de quem só tem formulário depende ",
      "da regra de exposição definida fora do modelo."
    ),
    "estrutural"
  )))

  list(
    objetos = objetos, tabelas = tabelas, figuras = list(),
    achados = dplyr::bind_rows(achados_conteudo, achados_colaborativo, achados_hibrido)
  )
}
