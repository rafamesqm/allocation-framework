#' A janela de análise declarada no achado I.4.1
#'
#' O período vai do primeiro início de projeto até a data de referência da
#' geração. Além dela o gerador só projeta a cauda de duração, que a fonte
#' real não comporta (achado I.4.1), e uma alocação que começa depois da data
#' de referência não é histórico observado, é projeção.
#'
#' **O intervalo é semiaberto**, `[inicio, fim)`: o último dia observado é a
#' véspera de `fim`. Com o fim fechado, toda alocação vigente na data de
#' referência deixaria um dia solto em 2026-09, e a série mensal ganharia um
#' mês com ocupação quase nula que é artefato do corte.
#'
#' @return Vetor `Date` nomeado com `inicio` e `fim`.
janela_de_analise <- function() {
  c(inicio = as.Date("2024-09-01"), fim = as.Date("2026-09-01"))
}

#' Restringe intervalos de datas à janela, com censura à direita
#'
#' Fica só o que começa dentro da janela. O que começa dentro e termina
#' depois é censurado no último dia observado, e a duração é recalculada a
#' partir da data censurada, na mesma convenção do gerador
#' (`data_fim = data_inicio + duracao_dias`).
#'
#' @param tabela tibble com `data_inicio`, `data_fim` e `duracao_dias`.
#' @param janela Vetor de `janela_de_analise()`.
#' @return `tabela` filtrada e censurada.
restringir_a_janela <- function(tabela, janela = janela_de_analise()) {
  ultimo_dia <- janela[["fim"]] - 1

  tabela |>
    dplyr::filter(data_inicio >= janela[["inicio"]], data_inicio < janela[["fim"]]) |>
    dplyr::mutate(
      data_fim = pmin(data_fim, ultimo_dia),
      duracao_dias = as.integer(data_fim - data_inicio)
    )
}

#' Acrescenta aos dados as versões restritas à janela de análise
#'
#' A janela vale para toda leitura de **tempo de calendário**: séries
#' mensais, ocupação e capacidade por mês, divisões temporais, meses ativos
#' do problema e horas observadas por par. As tabelas originais ficam
#' intactas ao lado, porque as leituras estruturais e as distribuições
#' confrontadas com quantis reais continuam sobre o conjunto inteiro: a
#' duração de projeto, por exemplo, é confrontada com a duração real, e
#' censurá-la mediria o corte e não o gerador.
#'
#' `ocupacao_consultor` é recalculada a partir das alocações censuradas, e não
#' filtrada por mês, para o último mês da janela não carregar horas de dias
#' que já estão fora dela.
#'
#' A função é idempotente: recalcula sempre a partir de `alocacoes` e
#' `projetos`, então chamá-la duas vezes dá o mesmo resultado.
#'
#' @param dados Lista nomeada de tibbles, de `carregar_dados()`.
#' @param janela Vetor de `janela_de_analise()`.
#' @return `dados` com `alocacoes_janela`, `projetos_janela`,
#'   `ocupacao_janela` e `janela`.
aplicar_janela_de_analise <- function(dados, janela = janela_de_analise()) {
  dados$alocacoes_janela <- restringir_a_janela(dados$alocacoes, janela)
  dados$projetos_janela <- restringir_a_janela(dados$projetos, janela)
  dados$ocupacao_janela <- agregar_ocupacao_consultor(dados$alocacoes_janela)
  dados$janela <- janela
  dados
}

#' Mede quanto as leituras estruturais mudariam se respeitassem a janela
#'
#' As leituras estruturais ficam sobre o conjunto inteiro, que é a escala em
#' que o gerador foi calibrado contra os agregados reais, enquanto o modelo
#' da wave 3 é estimado sobre o histórico da janela. Esta tabela mede a
#' distância entre as duas matrizes, para a escolha ficar declarada com o
#' seu custo e não implícita.
#'
#' @param dados Lista nomeada de tibbles, com `alocacoes`, `consultores`,
#'   `projetos` e `experiencia_setorial`.
#' @param janela Vetor de `janela_de_analise()`.
#' @return tibble com `medida`, `conjunto_inteiro` e `janela`.
comparar_leituras_estruturais_na_janela <- function(dados, janela = janela_de_analise()) {
  medir <- function(alocacoes) {
    sem_interacao <- !dados$consultores$consultor_id %in% alocacoes$consultor_id
    sem_setor <- !dados$consultores$consultor_id %in% dados$experiencia_setorial$consultor_id
    c(
      nrow(alocacoes),
      nrow(alocacoes) / (nrow(dados$consultores) * nrow(dados$projetos)),
      1 - dplyr::n_distinct(alocacoes$consultor_id) / nrow(dados$consultores),
      1 - dplyr::n_distinct(alocacoes$projeto_id) / nrow(dados$projetos),
      mean(sem_interacao & sem_setor),
      calcular_lorenz(dplyr::count(alocacoes, consultor_id)$n)$gini,
      medir_identificabilidade(
        alocacoes, dados$consultores, dados$projetos, fatores = 10
      )$obs_por_parametro
    )
  }

  tibble::tibble(
    medida = c(
      "Alocações", "Densidade de R", "Consultores sem alocação",
      "Projetos sem alocação", "Zona morta", "Gini do grau do consultor",
      "Observações por parâmetro, 10 fatores"
    ),
    conjunto_inteiro = medir(dados$alocacoes),
    janela = medir(restringir_a_janela(dados$alocacoes, janela))
  )
}

#' Devolve os dados com as alocações e a ocupação trocadas pelas da janela
#'
#' Atalho para as funções que leem `dados$alocacoes` por dentro, como as
#' divisões temporais, e precisam receber o histórico já restrito sem que
#' cada uma aprenda a procurar outro nome de tabela.
#'
#' @param dados Lista nomeada de tibbles.
#' @param janela Vetor de `janela_de_analise()`.
#' @return `dados` com `alocacoes` e `ocupacao_consultor` restritas à janela.
com_historico_da_janela <- function(dados, janela = janela_de_analise()) {
  dados <- aplicar_janela_de_analise(dados, janela)
  dados$alocacoes <- dados$alocacoes_janela
  dados$ocupacao_consultor <- dados$ocupacao_janela
  dados
}
