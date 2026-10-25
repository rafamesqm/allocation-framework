#' Mede os pisos de cada componente com os parâmetros do pré-registro
#'
#' É a Etapa 1 da wave 3: refazer, na pipeline da modelagem, os pisos que o
#' bloco II.7 fixou e que o pré-registro guardou como valores de
#' verificação. Usa as mesmas funções do protocolo da EDA, alimentadas só
#' pelo pré-registro, para que piso e modelo saiam da mesma definição.
#'
#' @param dados Lista nomeada de tibbles, de `carregar_dados()`.
#' @param registro Lista de `ler_pre_registro()`.
#' @return tibble de `planejar_poder_por_componente()`.
medir_pisos_pre_registrados <- function(dados, registro) {
  historico <- com_historico_da_janela(dados, janela_do_pre_registro(registro))
  planejar_poder_por_componente(
    historico$alocacoes, historico$consultores, historico$projetos,
    historico$experiencia_setorial,
    fracao_teste = registro$divisoes$temporal$fracao_teste,
    n_recomendados = registro$metrica$lista_decisoria,
    alfa = registro$teste$nivel,
    poder = registro$teste$ganho_minimo$poder,
    n_grupos = registro$divisoes$por_consultor$grupos,
    semente = registro$sementes$divisao
  )
}

#' Confere os pisos medidos contra os valores de verificação registrados
#'
#' Para a pipeline se algum piso não reproduzir. Seguir com um piso diferente
#' do registrado seria comparar o modelo com outra coisa que não a combinada
#' antes do resultado.
#'
#' @param pisos tibble de `medir_pisos_pre_registrados()`.
#' @param registro Lista de `ler_pre_registro()`.
#' @param tolerancia Diferença absoluta admitida, pelo arredondamento do
#'   registro a quatro casas.
#' @return tibble com cada valor medido, o registrado e se confere. Erro se
#'   algum não conferir.
conferir_pisos <- function(pisos, registro, tolerancia = 1e-4) {
  linha <- function(componente, piso) pisos[pisos$componente == componente & pisos$piso == piso, ]
  colaborativo <- linha("Colaborativo", "Popularidade no treino")
  conteudo <- linha("Conteúdo", "Filtro de setor e acaso")
  acaso <- linha("Conteúdo", "Acaso")
  v <- registro$verificacao

  conferencia <- tibble::tribble(
    ~grandeza, ~medido, ~registrado,
    "Pares avaliáveis do colaborativo", colaborativo$n_avaliavel, v$colaborativo$pares_avaliaveis,
    "Piso do colaborativo", colaborativo$valor_piso, v$colaborativo$piso,
    "Ganho mínimo do colaborativo, otimista", colaborativo$ganho_dominancia, v$colaborativo$ganho_minimo_dominancia,
    "Ganho mínimo do colaborativo, conservador", colaborativo$ganho_independencia, v$colaborativo$ganho_minimo_independencia,
    "Pares avaliáveis do conteúdo", conteudo$n_avaliavel, v$conteudo$pares_avaliaveis,
    "Piso do conteúdo, filtro com acaso", conteudo$valor_piso, v$conteudo$piso_filtro_acaso,
    "Pares que passam o filtro", conteudo$fracao_passa_filtro, v$conteudo$fracao_passa_filtro,
    "Acaso puro", acaso$valor_piso, v$conteudo$acaso_puro,
    "Ganho mínimo do conteúdo, otimista", conteudo$ganho_dominancia, v$conteudo$ganho_minimo_dominancia,
    "Ganho mínimo do conteúdo, conservador", conteudo$ganho_independencia, v$conteudo$ganho_minimo_independencia
  ) |>
    dplyr::mutate(confere = abs(medido - registrado) <= tolerancia + 5e-5)

  if (!all(conferencia$confere)) {
    stop(
      "Piso fora do pré-registrado: ",
      paste(conferencia$grandeza[!conferencia$confere], collapse = "; "),
      ". A wave 3 não segue com piso diferente do registrado.",
      call. = FALSE
    )
  }
  conferencia
}

#' Monta a tabela dos pisos para o texto, com o intervalo entre realizações
#'
#' @param pisos tibble de `medir_pisos_pre_registrados()`.
#' @param estabilidade tibble de `resumir_estabilidade()`.
#' @return tibble formatada para `salvar_tabela()`.
tabelar_pisos <- function(pisos, estabilidade) {
  medidas <- c(
    "Colaborativo|Popularidade no treino" = "piso_colaborativo",
    "Conteúdo|Filtro de setor e acaso" = "piso_conteudo_filtro"
  )
  pares <- c(
    "Colaborativo|Popularidade no treino" = "n_avaliavel_colaborativo",
    "Conteúdo|Filtro de setor e acaso" = "n_avaliavel_conteudo"
  )
  chave <- paste(pisos$componente, pisos$piso, sep = "|")
  pontos <- \(x) ifelse(is.na(x), "--", formatar_decimal(100 * x, 1))

  pisos |>
    dplyr::transmute(
      Componente = componente,
      `Divisão` = dplyr::if_else(protocolo == "Por consultor", "Por consultor, validação cruzada", protocolo),
      Piso = piso,
      `Pares avaliáveis` = formatar_inteiro(n_avaliavel),
      `Pares entre realizações` = dplyr::coalesce(
        purrr::map_chr(pares[chave], \(m) if (is.na(m)) NA_character_ else formatar_intervalo(estabilidade, m, formatar_inteiro)),
        "--"
      ),
      `Acerto do piso` = formatar_pct(valor_piso),
      `Piso entre realizações` = dplyr::coalesce(
        purrr::map_chr(medidas[chave], \(m) if (is.na(m)) NA_character_ else formatar_intervalo(estabilidade, m)),
        "--"
      ),
      `Ganho mínimo, otimista` = pontos(ganho_dominancia),
      `Ganho mínimo, conservador` = pontos(ganho_independencia)
    )
}
