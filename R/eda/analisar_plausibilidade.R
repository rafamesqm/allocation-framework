# Segundo nível de avaliação do bloco II.7: plausibilidade das discordâncias
# entre a alocação recomendada e a realizada. Uma discordância é um par de
# teste em que o consultor realizado não está entre os recomendados; ela
# compara com o realizado o primeiro recomendado que não estava alocado ao
# projeto no mês da alocação realizada, escolhido por
# `selecionar_recomendado_comparado()`. Recomendar quem já está no time não é
# alternativa ao realizado, e compará-lo mediria outra coisa.
#
# A entrada das funções abaixo é uma tabela de discordâncias com as colunas
# `discordancia_id`, `projeto_id`, `data_inicio` (da alocação realizada),
# `consultor_realizado`, `consultor_recomendado`, `ramo` ("conteúdo" ou
# "colaborativo"), `na_zona_morta`, `filtro` e `posicao_popularidade`. Ela só
# existe depois que a wave 3 produzir as recomendações; até lá as funções são
# o protocolo, testado, e não um resultado.

#' Os quatro vereditos da leitura qualitativa
#'
#' @return Vetor de texto com os vereditos admitidos, na ordem da rubrica.
listar_vereditos_de_leitura <- function() {
  c("plausível", "plausível com ressalva", "implausível", "indeterminável")
}

#' Escolhe o recomendado que a discordância compara com o realizado
#'
#' É o primeiro da lista recomendada que não estava alocado ao projeto no mês
#' da alocação realizada. O realizado, que está alocado por definição, nunca é
#' escolhido.
#'
#' @param recomendados Lista com um vetor de consultores por discordância, na
#'   ordem da recomendação.
#' @param projeto_id Vetor com o projeto de cada discordância.
#' @param mes Vetor de texto com o mês da alocação realizada, "AAAA-MM".
#' @param alocacoes tibble com `consultor_id`, `projeto_id`, `data_inicio` e
#'   `data_fim`.
#' @return Vetor com o consultor escolhido, ou `NA` quando todos os
#'   recomendados já estavam no time naquele mês.
selecionar_recomendado_comparado <- function(recomendados, projeto_id, mes, alocacoes) {
  stopifnot(length(recomendados) == length(projeto_id), length(projeto_id) == length(mes))

  meses <- expandir_para_meses(alocacoes$data_inicio, alocacoes$data_fim) |>
    dplyr::mutate(
      consultor_id = alocacoes$consultor_id[linha],
      projeto_id = alocacoes$projeto_id[linha]
    )

  purrr::pmap_chr(list(recomendados, projeto_id, mes), function(lista, projeto, mes_alvo) {
    time <- meses$consultor_id[meses$projeto_id == projeto & meses$ano_mes == mes_alvo]
    fora_do_time <- setdiff(lista, time)
    if (length(fora_do_time) == 0) NA_character_ else fora_do_time[1]
  })
}

#' Separa as discordâncias que têm consultor comparável das que não têm
#'
#' Quando todos os recomendados já estavam no time no mês,
#' `selecionar_recomendado_comparado()` devolve `NA`, e o mesmo acontece com
#' lista de recomendação vazia. Essas discordâncias não têm com quem comparar
#' o realizado: ficam fora da comparação por dominância e da leitura
#' qualitativa, e são contadas à parte para o número não sumir.
#'
#' @param discordancias tibble com `consultor_recomendado`.
#' @return Lista com `comparaveis`, as discordâncias com consultor
#'   comparável, e `sem_comparavel`, quantas ficaram de fora.
separar_discordancias_comparaveis <- function(discordancias) {
  tem_comparavel <- !is.na(discordancias$consultor_recomendado)
  list(
    comparaveis = discordancias[tem_comparavel, ],
    sem_comparavel = sum(!tem_comparavel)
  )
}

#' Mede a folga de capacidade de consultores num mês, sem a alocação em análise
#'
#' A folga é a capacidade do consultor no mês menos as horas das demais
#' alocações dele ativas naquele mês. Mês parcial entra ponderado pelos dias
#' cobertos, como em `agregar_ocupacao_consultor()`. A alocação analisada
#' sai do cálculo: sem isso, o realizado pagaria em folga a própria
#' alocação que está sendo julgada, e perderia o critério por construção.
#'
#' A capacidade no mês é a capacidade mensal vezes a fração do mês fora de
#' férias. A capacidade padrão, 168 horas, é o mesmo limite de mês cheio de
#' trabalho de `medir_sobredeterminacao_economica()`. Com as férias a
#' constante deixa de deslocar recomendado e realizado por igual: ela escala
#' o desconto de férias, e quando só um dos dois está de férias no mês o
#' resultado da comparação pode depender dela.
#'
#' **As férias sintéticas cobrem só 2026.** Mês fora desse ano fica com a
#' capacidade cheia por falta de registro, e não por ausência de férias.
#'
#' @param consultor_id Vetor de consultores.
#' @param mes Vetor de texto com o mês de cada consulta, no formato "AAAA-MM"
#'   de `expandir_para_meses()`.
#' @param alocacoes tibble com `consultor_id`, `projeto_id`, `horas_mes`,
#'   `data_inicio` e `data_fim`.
#' @param projeto_excluido Vetor com o projeto da alocação analisada, para
#'   cada consulta. A alocação do consultor nesse projeto não entra.
#' @param ferias tibble com `consultor_id`, `inicio_ferias` e `fim_ferias`.
#' @param capacidade_mensal_horas Capacidade de um consultor num mês inteiro
#'   sem férias.
#' @return Vetor numérico com a folga em horas, um valor por consulta.
medir_folga_de_capacidade <- function(consultor_id, mes, alocacoes,
                                      projeto_excluido, ferias,
                                      capacidade_mensal_horas = 168) {
  stopifnot(length(consultor_id) == length(mes), length(mes) == length(projeto_excluido))

  meses_de_ferias <- expandir_para_meses(ferias$inicio_ferias, ferias$fim_ferias) |>
    dplyr::mutate(
      consultor_id = ferias$consultor_id[linha],
      fracao_de_ferias = dias_ativos / dias_no_mes
    )

  meses <- expandir_para_meses(alocacoes$data_inicio, alocacoes$data_fim)
  horas_ativas <- meses |>
    dplyr::mutate(
      consultor_id = alocacoes$consultor_id[linha],
      projeto_id = alocacoes$projeto_id[linha],
      horas = alocacoes$horas_mes[linha] * dias_ativos / dias_no_mes
    )

  purrr::pmap_dbl(
    list(consultor_id, mes, projeto_excluido),
    function(consultor, mes_alvo, projeto) {
      ocupadas <- horas_ativas$horas[
        horas_ativas$consultor_id == consultor &
          horas_ativas$ano_mes == mes_alvo &
          horas_ativas$projeto_id != projeto
      ]
      de_ferias <- sum(meses_de_ferias$fracao_de_ferias[
        meses_de_ferias$consultor_id == consultor & meses_de_ferias$ano_mes == mes_alvo
      ])
      capacidade_mensal_horas * (1 - min(de_ferias, 1)) - sum(ocupadas)
    }
  )
}

#' Mede a distância de senioridade de um consultor ao restante do time
#'
#' O nível é ordinal, e a distância é a diferença absoluta de posições na
#' escala de níveis entre o consultor e a mediana das posições do restante
#' do time. O restante do time são os consultores alocados ao projeto no
#' mesmo mês, excluídos o recomendado e o realizado da discordância.
#'
#' @param posicao Posição do consultor na escala de níveis.
#' @param posicoes_do_time Posições do restante do time no mês.
#' @return Distância, ou `NA` quando o projeto não tem outros membros no mês.
medir_distancia_de_senioridade <- function(posicao, posicoes_do_time) {
  if (length(posicoes_do_time) == 0) return(NA_real_)
  abs(posicao - stats::median(posicoes_do_time))
}

#' Compara o setor declarado do consultor com o setor do projeto
#'
#' Três valores, e não dois: consultor sem setor declarado fica
#' "desconhecido", e não "incompatível", porque a ausência de resposta ao
#' formulário não diz nada sobre a experiência da pessoa. Projeto sem setor
#' também fica "desconhecido".
#'
#' @param setor_consultor Setor predominante declarado, ou `NA`.
#' @param setor_projeto Setor do projeto.
#' @return Vetor de texto com "compatível", "incompatível" ou "desconhecido".
comparar_setor <- function(setor_consultor, setor_projeto) {
  dplyr::case_when(
    is.na(setor_consultor) | is.na(setor_projeto) ~ "desconhecido",
    setor_consultor == setor_projeto ~ "compatível",
    .default = "incompatível"
  )
}

#' Mede os quatro critérios de cada discordância, para os dois consultores
#'
#' Os critérios são a folga de capacidade no mês de início da alocação
#' realizada, já descontadas as férias, o custo por hora, a compatibilidade
#' de setor e a adequação de
#' senioridade ao restante do time no mesmo mês. Projeto sem outros membros
#' no mês dá empate no critério de senioridade: as duas distâncias saem 0.
#'
#' @param discordancias tibble descrito no cabeçalho deste arquivo.
#' @param dados Lista nomeada de tibbles, de `carregar_dados()`.
#' @param capacidade_mensal_horas Repassado a `medir_folga_de_capacidade()`.
#' @return `discordancias` com as colunas `folga_*`, `custo_*`, `setor_*`,
#'   `senioridade_*` e `tempo_casa_*`, com sufixos `recomendado` e
#'   `realizado`, e `sem_time_no_mes`.
medir_criterios_da_discordancia <- function(discordancias, dados,
                                            capacidade_mensal_horas = 168) {
  if (anyNA(discordancias$consultor_recomendado)) {
    stop(
      sum(is.na(discordancias$consultor_recomendado)),
      " discordância(s) sem consultor comparável chegaram à medição dos ",
      "critérios. Separe-as antes com separar_discordancias_comparaveis().",
      call. = FALSE
    )
  }
  mes <- format(discordancias$data_inicio, "%Y-%m")
  setor_projeto <- dados$projetos$setor[
    match(discordancias$projeto_id, dados$projetos$projeto_id)
  ]
  setor_declarado <- setor_predominante_observado(dados$experiencia_setorial)

  atributo <- function(consultor, tabela, coluna) {
    tabela[[coluna]][match(consultor, tabela$consultor_id)]
  }
  posicao <- function(consultor) {
    senioridade_numerica(atributo(consultor, dados$consultores, "nivel_senioridade"))
  }

  meses_alocacao <- expandir_para_meses(dados$alocacoes$data_inicio, dados$alocacoes$data_fim) |>
    dplyr::mutate(
      consultor_id = dados$alocacoes$consultor_id[linha],
      projeto_id = dados$alocacoes$projeto_id[linha]
    )
  time_no_mes <- purrr::pmap(
    list(discordancias$projeto_id, mes, discordancias$consultor_recomendado,
         discordancias$consultor_realizado),
    function(projeto, mes_alvo, recomendado, realizado) {
      membros <- unique(meses_alocacao$consultor_id[
        meses_alocacao$projeto_id == projeto & meses_alocacao$ano_mes == mes_alvo
      ])
      posicao(setdiff(membros, c(recomendado, realizado)))
    }
  )
  sem_time <- lengths(time_no_mes) == 0
  distancia <- function(consultor) {
    d <- purrr::map2_dbl(posicao(consultor), time_no_mes, medir_distancia_de_senioridade)
    ifelse(sem_time, 0, d)
  }

  folga <- function(consultor) {
    medir_folga_de_capacidade(
      consultor, mes, dados$alocacoes, discordancias$projeto_id,
      ferias = dados$ferias_consultor,
      capacidade_mensal_horas = capacidade_mensal_horas
    )
  }
  setor <- function(consultor) {
    comparar_setor(atributo(consultor, setor_declarado, "segmento"), setor_projeto)
  }

  rec <- discordancias$consultor_recomendado
  real <- discordancias$consultor_realizado
  discordancias |>
    dplyr::mutate(
      folga_recomendado = folga(rec),
      folga_realizado = folga(real),
      custo_recomendado = atributo(rec, dados$custo_carregado, "custo_carregado"),
      custo_realizado = atributo(real, dados$custo_carregado, "custo_carregado"),
      setor_recomendado = setor(rec),
      setor_realizado = setor(real),
      senioridade_recomendado = distancia(rec),
      senioridade_realizado = distancia(real),
      tempo_casa_recomendado = atributo(rec, dados$consultores, "tempo_casa"),
      tempo_casa_realizado = atributo(real, dados$consultores, "tempo_casa"),
      sem_time_no_mes = sem_time
    )
}

#' Classifica cada discordância por dominância entre recomendado e realizado
#'
#' Em cada critério conhecido para os dois, um consultor ganha quando é
#' estritamente melhor: mais folga, menor custo por hora, setor compatível
#' contra incompatível, menor distância de senioridade. A classe sai da
#' comparação conjunta:
#'
#' * **recomendado melhor**: não perde em nenhum critério e ganha em ao
#'   menos um;
#' * **recomendado pior**: o inverso;
#' * **incomparáveis**: cada um ganha em algum critério;
#' * **empate**: iguais em todos os critérios conhecidos.
#'
#' O empate é desfeito pelo maior tempo de casa, e só fica empate quando o
#' tempo de casa também coincide. **O desempate favorece veteranos**: entre
#' dois consultores indistinguíveis pelos quatro critérios, o mais antigo
#' vence, o que é uma escolha declarada e não uma medida de adequação.
#'
#' Critério desconhecido para qualquer dos dois consultores sai da
#' comparação daquele par, que passa a ser julgado só sobre os critérios
#' conhecidos para ambos. A coluna `criterios_desconhecidos` registra quais.
#'
#' @param criterios tibble de `medir_criterios_da_discordancia()`.
#' @return `criterios` com `classe`, `desempate_por_tempo_de_casa` e
#'   `criterios_desconhecidos`.
classificar_discordancia_por_dominancia <- function(criterios) {
  ordem_setor <- c("incompatível" = 0, "compatível" = 1)
  # Cada critério vira um escore em que maior é melhor, e NA é desconhecido.
  escores <- list(
    folga = list(criterios$folga_recomendado, criterios$folga_realizado),
    custo = list(-criterios$custo_recomendado, -criterios$custo_realizado),
    setor = list(
      unname(ordem_setor[criterios$setor_recomendado]),
      unname(ordem_setor[criterios$setor_realizado])
    ),
    senioridade = list(-criterios$senioridade_recomendado, -criterios$senioridade_realizado)
  )

  conhecido <- purrr::map(escores, \(e) !is.na(e[[1]]) & !is.na(e[[2]]))
  ganha_recomendado <- purrr::map2(escores, conhecido, \(e, k) k & e[[1]] > e[[2]])
  ganha_realizado <- purrr::map2(escores, conhecido, \(e, k) k & e[[2]] > e[[1]])

  algum <- function(lista) Reduce(`|`, lista)
  rec_ganha <- algum(ganha_recomendado)
  real_ganha <- algum(ganha_realizado)

  delta_tempo <- criterios$tempo_casa_recomendado - criterios$tempo_casa_realizado
  empate_nos_criterios <- !rec_ganha & !real_ganha

  classe <- dplyr::case_when(
    rec_ganha & real_ganha ~ "incomparáveis",
    rec_ganha ~ "recomendado melhor",
    real_ganha ~ "recomendado pior",
    delta_tempo > 0 ~ "recomendado melhor",
    delta_tempo < 0 ~ "recomendado pior",
    .default = "empate"
  )

  desconhecidos <- purrr::map(seq_len(nrow(criterios)), function(i) {
    names(conhecido)[!vapply(conhecido, \(k) k[i], logical(1))]
  })

  criterios |>
    dplyr::mutate(
      classe = classe,
      desempate_por_tempo_de_casa = empate_nos_criterios & !is.na(delta_tempo) & delta_tempo != 0,
      criterios_desconhecidos = desconhecidos
    )
}

#' Conta, por critério, quantos pares tiveram o critério desconhecido
#'
#' @param classificadas tibble de `classificar_discordancia_por_dominancia()`.
#' @return tibble com `criterio` e `pares`, com os quatro critérios mesmo
#'   quando a contagem é zero.
contar_criterios_desconhecidos <- function(classificadas) {
  criterios <- c("folga", "custo", "setor", "senioridade")
  tibble::tibble(
    criterio = criterios,
    pares = vapply(
      criterios,
      \(c) sum(vapply(classificadas$criterios_desconhecidos, \(d) c %in% d, logical(1))),
      integer(1),
      USE.NAMES = FALSE
    )
  )
}

#' Sorteia os casos da leitura qualitativa, estratificados
#'
#' O universo são as discordâncias do teste fora da zona morta. Os estratos
#' cruzam o ramo que atuou, conteúdo ou colaborativo, com a classe da
#' comparação por dominância, recomendado melhor, pior ou incomparáveis, o
#' que dá seis estratos. Cada estrato contribui com `por_estrato` casos, e
#' estrato sem discordância nenhuma é reportado como vazio em vez de
#' desaparecer. A semente é fixa e declarada aqui, para o sorteio ser
#' reproduzível por quem audita.
#'
#' @param classificadas tibble de `classificar_discordancia_por_dominancia()`.
#' @param por_estrato Casos sorteados por estrato.
#' @param semente Semente do sorteio.
#' @return Lista com `casos`, as linhas sorteadas; `estratos`, a contagem
#'   disponível e sorteada de cada um dos seis estratos, e quantas delas
#'   vieram do desempate por tempo de casa; e `desempates_por_classe`, a mesma
#'   contagem de desempates somada por classe.
sortear_casos_para_leitura <- function(classificadas, por_estrato = 2, semente = 20261022) {
  ramos <- c("conteúdo", "colaborativo")
  classes <- c("recomendado melhor", "recomendado pior", "incomparáveis")

  universo <- classificadas |>
    dplyr::filter(!na_zona_morta, classe %in% classes) |>
    dplyr::arrange(discordancia_id)

  casos <- withr::with_seed(semente, {
    universo |>
      dplyr::group_by(ramo, classe) |>
      dplyr::group_modify(\(estrato, chave) {
        estrato[sort(sample.int(nrow(estrato), min(por_estrato, nrow(estrato)))), ]
      }) |>
      dplyr::ungroup()
  })

  contar <- function(tabela, nome) {
    tabela |>
      dplyr::summarise(
        "{nome}" := dplyr::n(),
        "{nome}_por_desempate" := sum(desempate_por_tempo_de_casa),
        .by = c(ramo, classe)
      )
  }
  estratos <- tidyr::expand_grid(ramo = ramos, classe = classes) |>
    dplyr::left_join(contar(universo, "disponiveis"), by = c("ramo", "classe")) |>
    dplyr::left_join(contar(casos, "sorteados"), by = c("ramo", "classe")) |>
    dplyr::mutate(
      dplyr::across(c(disponiveis, disponiveis_por_desempate, sorteados, sorteados_por_desempate),
                    \(x) dplyr::coalesce(as.integer(x), 0L)),
      vazio = disponiveis == 0
    )
  desempates_por_classe <- estratos |>
    dplyr::summarise(
      disponiveis_por_desempate = sum(disponiveis_por_desempate),
      sorteados_por_desempate = sum(sorteados_por_desempate),
      .by = classe
    )

  list(casos = casos, estratos = estratos, desempates_por_classe = desempates_por_classe)
}

#' Monta a rubrica da leitura qualitativa para os casos sorteados
#'
#' A rubrica é fixa e tem quatro campos a preencher por caso: disponibilidade
#' do recomendado no mês, compatibilidade de setor, algo observável nos
#' dados que justifique a escolha realizada, e o veredito. A explicação de
#' cada recomendação vem da própria arquitetura, e por isso o ramo, o filtro
#' e a posição na popularidade já entram preenchidos. A leitura tem um
#' avaliador único, o autor, o que é limitação declarada.
#'
#' @param casos Elemento `casos` de `sortear_casos_para_leitura()`.
#' @return tibble com uma linha por caso e os campos da rubrica em branco.
montar_rubrica_de_leitura <- function(casos) {
  casos |>
    dplyr::transmute(
      discordancia_id, projeto_id, consultor_recomendado, consultor_realizado,
      ramo, filtro, posicao_popularidade, classe,
      disponibilidade_do_recomendado = NA_character_,
      compatibilidade_de_setor = NA_character_,
      justificativa_observavel = NA_character_,
      veredito = factor(NA_character_, levels = listar_vereditos_de_leitura())
    )
}
