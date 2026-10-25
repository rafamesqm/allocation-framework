#' Conta, por ramo e critério, quem ganha a comparação
#'
#' Desdobra a classificação por dominância critério a critério, para dizer
#' de onde vêm as classes: mais folga, menor custo por hora, setor
#' compatível contra incompatível, menor distância de senioridade. Critério
#' desconhecido para qualquer dos dois fica como desconhecido.
#'
#' @param classificadas tibble de `classificar_discordancia_por_dominancia()`.
#' @return tibble com `ramo`, `criterio` e as frações de `recomendado`,
#'   `realizado`, `empate` e `desconhecido`.
desdobrar_criterios_por_ramo <- function(classificadas) {
  setor <- c("incompatível" = 0, "compatível" = 1)
  comparar <- function(rec, real) {
    dplyr::case_when(
      is.na(rec) | is.na(real) ~ "desconhecido",
      rec > real ~ "recomendado",
      rec < real ~ "realizado",
      .default = "empate"
    )
  }
  cl <- classificadas
  dplyr::bind_rows(
    tibble::tibble(ramo = cl$ramo, criterio = "folga", vence = comparar(cl$folga_recomendado, cl$folga_realizado)),
    tibble::tibble(ramo = cl$ramo, criterio = "custo", vence = comparar(-cl$custo_recomendado, -cl$custo_realizado)),
    tibble::tibble(
      ramo = cl$ramo, criterio = "setor",
      vence = comparar(unname(setor[cl$setor_recomendado]), unname(setor[cl$setor_realizado]))
    ),
    tibble::tibble(
      ramo = cl$ramo, criterio = "senioridade",
      vence = comparar(-cl$senioridade_recomendado, -cl$senioridade_realizado)
    )
  ) |>
    dplyr::count(ramo, criterio, vence) |>
    dplyr::mutate(fracao = n / sum(n), .by = c(ramo, criterio)) |>
    dplyr::select(-n) |>
    dplyr::mutate(vence = factor(vence, levels = c("recomendado", "realizado", "empate", "desconhecido"))) |>
    tidyr::complete(ramo, criterio, vence, fill = list(fracao = 0)) |>
    tidyr::pivot_wider(names_from = vence, values_from = fracao, values_fill = 0)
}

#' Executa o bloco da Etapa 5, a plausibilidade das discordâncias
#'
#' @param plausibilidade Lista de `avaliar_plausibilidade_das_discordancias()`.
#' @param registro Lista de `ler_pre_registro()`.
#' @return Lista com `objetos`, `tabelas`, `figuras` e `achados`.
executar_w3_plausibilidade <- function(plausibilidade, registro) {
  classificadas <- plausibilidade$classificadas
  classes <- c("recomendado melhor", "recomendado pior", "incomparáveis", "empate")
  ramos <- c("conteúdo", "colaborativo")

  por_ramo <- classificadas |>
    dplyr::mutate(classe = factor(classe, levels = classes), ramo = factor(ramo, levels = ramos)) |>
    dplyr::count(ramo, classe, .drop = FALSE) |>
    dplyr::mutate(fracao = n / sum(n), .by = ramo) |>
    dplyr::mutate(dplyr::across(c(ramo, classe), as.character))
  extras <- classificadas |>
    dplyr::summarise(
      comparadas = dplyr::n(), desempates = sum(desempate_por_tempo_de_casa),
      antes_de_2026 = mean(antes_das_ferias), .by = ramo
    ) |>
    dplyr::left_join(plausibilidade$sem_comparavel, by = "ramo")
  criterios <- desdobrar_criterios_por_ramo(classificadas)
  # Quantas pessoas diferentes fazem o papel de recomendado comparado: com a
  # popularidade sem filtro, a lista de todo projeto começa pela mesma pessoa.
  concentracao <- classificadas |>
    dplyr::summarise(
      distintos = dplyr::n_distinct(consultor_recomendado),
      mais_frequente = max(table(consultor_recomendado)) / dplyr::n(), .by = ramo
    )
  concentrado <- \(r) concentracao[concentracao$ramo == r, ]

  fracao <- \(r, cl) por_ramo$fracao[por_ramo$ramo == r & por_ramo$classe == cl]
  contagem <- \(r, cl) por_ramo$n[por_ramo$ramo == r & por_ramo$classe == cl]
  extra <- \(r) extras[extras$ramo == r, ]
  criterio <- \(r, cr) criterios[criterios$ramo == r & criterios$criterio == cr, ]
  desconhecido <- \(r, cr) plausibilidade$desconhecidos$pares[
    plausibilidade$desconhecidos$ramo == r & plausibilidade$desconhecidos$criterio == cr
  ]
  estratos <- plausibilidade$sorteio$estratos
  vazios <- estratos[estratos$vazio, ]
  sorteados <- sum(estratos$sorteados)
  texto_vazios <- if (nrow(vazios) == 0) {
    "nenhum estrato ficou vazio"
  } else {
    paste0(
      "o estrato ", paste(paste(vazios$ramo, vazios$classe, sep = " e "), collapse = "; "),
      " ficou vazio, porque não há discordância nele"
    )
  }

  objetos <- list(por_ramo = por_ramo, extras = extras, criterios = criterios)

  pct <- \(x) formatar_pct(x)
  tabelas <- list(
    w3_plausibilidade_classes = por_ramo |>
      dplyr::left_join(extras, by = "ramo") |>
      dplyr::transmute(
        Ramo = ramo, Classe = classe, Pares = formatar_inteiro(n), `Fração do ramo` = pct(fracao),
        `Discordâncias do ramo` = formatar_inteiro(discordancias),
        `Sem comparável, contadas à parte` = formatar_inteiro(sem_comparavel),
        `Desempates por tempo de casa no ramo` = formatar_inteiro(desempates),
        `Mês anterior a 2026, sem férias` = pct(antes_de_2026)
      ),
    w3_plausibilidade_criterios = criterios |>
      dplyr::transmute(
        Ramo = ramo, `Critério` = criterio,
        `Recomendado vence` = pct(recomendado), `Realizado vence` = pct(realizado),
        Empate = pct(empate), Desconhecido = pct(desconhecido)
      ),
    w3_plausibilidade_estratos = estratos |>
      dplyr::transmute(
        Ramo = ramo, Classe = classe, `Disponíveis` = disponiveis,
        `Sorteados` = sorteados, `Sorteados por desempate` = sorteados_por_desempate,
        Vazio = ifelse(vazio, "sim", "não")
      ),
    # A planilha da leitura sai em branco, como modelo: o avaliador é o autor.
    # A cópia preenchida vive em modelling/leitura/rubrica_preenchida.csv,
    # que a pipeline nunca escreve; preencher este arquivo seria perder os
    # vereditos no próximo tar_make().
    w3_rubrica_leitura_modelo = plausibilidade$rubrica |>
      dplyr::transmute(
        `Discordância` = discordancia_id, Projeto = projeto_id,
        `Consultor recomendado` = consultor_recomendado, `Consultor realizado` = consultor_realizado,
        Ramo = ramo, Filtro = filtro, `Posição na lista` = posicao_popularidade, Classe = classe,
        `Disponibilidade do recomendado no mês` = disponibilidade_do_recomendado,
        `Compatibilidade de setor` = compatibilidade_de_setor,
        `Justificativa observável da escolha realizada` = justificativa_observavel,
        Veredito = as.character(veredito)
      )
  )

  achados <- registrar_achados("W3.5", com_sintese(tibble::tribble(
    ~codigo, ~etiqueta, ~enunciado, ~implicacao,

    "W3.5.1", "instrumento",
    paste0(
      "Das ", extra("conteúdo")$discordancias, " discordâncias do conteúdo, ",
      formatar_pct(fracao("conteúdo", "recomendado melhor")), " têm o recomendado melhor ",
      "por dominância, ", formatar_pct(fracao("conteúdo", "recomendado pior")), " pior e ",
      formatar_pct(fracao("conteúdo", "incomparáveis")), " incomparáveis. Das ",
      extra("colaborativo")$discordancias, " do colaborativo, ",
      formatar_pct(fracao("colaborativo", "recomendado melhor")), " têm o recomendado melhor, ",
      formatar_pct(fracao("colaborativo", "recomendado pior")), " pior e ",
      formatar_pct(fracao("colaborativo", "incomparáveis")), " incomparáveis. Ficaram sem ",
      "consultor comparável ", sum(extras$sem_comparavel), " discordâncias, contadas à parte, ",
      "e ", sum(extras$desempates), " classificações vieram do desempate por tempo de casa."
    ),
    paste0(
      "O primeiro nível media aderência ao processo; este olha o que ele não vê. A ",
      "assimetria entre os ramos vem do que cada lista põe no topo. No conteúdo, o ",
      "recomendado é um sorteio dentro do filtro de setor, e ganha sobretudo no setor, ",
      "porque o realizado dessas discordâncias quase sempre declarou outro setor. No ",
      "colaborativo, o recomendado é o mais popular fora da equipe, e tem menos folga ",
      "que o realizado em ", formatar_pct(criterio("colaborativo", "folga")$realizado),
      " das comparações (W3.5.2): reproduzir a popularidade é recomendar quem já está ",
      "ocupado. A comparação é por ",
      "dominância e não pesa um critério contra outro, então incomparável não é empate: ",
      "é discordância que depende de quanto se valoriza cada critério."
    ),

    "W3.5.2", "instrumento",
    paste0(
      "Critério a critério, o recomendado do colaborativo tem mais folga que o realizado ",
      "em ", formatar_pct(criterio("colaborativo", "folga")$recomendado), " das comparações ",
      "e menos em ", formatar_pct(criterio("colaborativo", "folga")$realizado), ", e custo ",
      "por hora menor em ", formatar_pct(criterio("colaborativo", "custo")$recomendado),
      "; no setor, o realizado vence em ", formatar_pct(criterio("colaborativo", "setor")$realizado),
      ", o recomendado em ", formatar_pct(criterio("colaborativo", "setor")$recomendado),
      ", e o critério é desconhecido em ", formatar_pct(criterio("colaborativo", "setor")$desconhecido),
      ". O recomendado comparado do colaborativo é quase sempre a mesma pessoa: são ",
      concentrado("colaborativo")$distintos, " consultores distintos, e o mais frequente ",
      "responde por ", formatar_pct(concentrado("colaborativo")$mais_frequente),
      " das comparações, contra ", concentrado("conteúdo")$distintos, " distintos no ",
      "conteúdo, com o mais frequente em ", formatar_pct(concentrado("conteúdo")$mais_frequente),
      ". O recomendado do conteúdo vence no setor em ",
      formatar_pct(criterio("conteúdo", "setor")$recomendado), " das comparações e na ",
      "folga em ", formatar_pct(criterio("conteúdo", "folga")$recomendado), "."
    ),
    paste0(
      "É o desdobramento que explica as classes do W3.5.1. **No colaborativo, a ",
      "comparação por dominância é, na prática, contra uma pessoa só**: a popularidade ",
      "sem filtro põe o mesmo consultor no topo de toda lista, e ele, por ser o mais ",
      "alocado, nunca tem mais folga que o realizado. As frações do colaborativo dizem ",
      "respeito a esse consultor, e não a uma política de recomendação. A folga, o custo e a ",
      "senioridade dependem da ocupação, da tabela de custo e da pirâmide do gerador, ",
      "então a magnitude de cada fração é do sintético; a forma, a popularidade ",
      "recomendando quem já está ocupado, decorre da própria regra de ordenação."
    ),

    "W3.5.3", "instrumento",
    paste0(
      "O setor fica desconhecido em ", desconhecido("colaborativo", "setor"), " das ",
      extra("colaborativo")$comparadas, " comparações do colaborativo e em ",
      desconhecido("conteúdo", "setor"), " das ", extra("conteúdo")$comparadas, " do ",
      "conteúdo; folga, custo e senioridade são conhecidos em todas. Nos pares com setor ",
      "desconhecido, a comparação é feita sobre os três critérios restantes."
    ),
    paste0(
      "O colaborativo recomenda quem tem histórico, declarado ou não, e por isso é nele ",
      "que o setor falta: quem não respondeu o formulário fica desconhecido, e não ",
      "incompatível. No conteúdo, recomendado e realizado declararam setor por ",
      "construção da avaliação."
    ),

    "W3.5.4", "instrumento",
    paste0(
      "A leitura qualitativa sorteou ", sorteados, " casos, com a semente ",
      registro$sementes$leitura_qualitativa, " e até ", registro$segundo_nivel$casos_por_estrato,
      " por estrato de ramo e classe, sem a zona morta; ", texto_vazios, ". A planilha da ",
      "rubrica sai em branco, com o ramo, o filtro e a posição na lista já preenchidos ",
      "como explicação da arquitetura. No conteúdo, o primeiro recomendado é um sorteio ",
      "dentro do filtro, e ", formatar_pct(extra("conteúdo")$antes_de_2026), " das ",
      "comparações dele caem em mês anterior a 2026, em que a folga é calculada sem férias."
    ),
    paste0(
      "Os vereditos são do autor, fora da pipeline. Sobre dados sintéticos, a leitura ",
      "julga a coerência do gerador, e não o processo real de alocação: demonstra o ",
      "instrumento sem validar o modelo."
    )
  ), tibble::tribble(
    ~codigo, ~decisao, ~limitacao, ~proveniencia,

    "W3.5.1", NA,
    paste0(
      "A classificação por dominância depende de folga, custo e senioridade sintéticos, ",
      "e o desempate por tempo de casa favorece veteranos; as frações valem para o ",
      "gerador e para a população observada."
    ),
    "depende do gerador",

    "W3.5.4", NA,
    paste0(
      "A leitura tem um avaliador único, o autor; no ramo de conteúdo o recomendado é um ",
      "sorteio dentro do filtro; e nos meses da janela anteriores a 2026 a folga é ",
      "calculada sem férias, porque as férias sintéticas só cobrem 2026."
    ),
    "depende do gerador"
  )))

  list(objetos = objetos, tabelas = tabelas, figuras = list(), achados = achados)
}
