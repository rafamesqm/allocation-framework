#' Mede a cobertura de cada sinal disponível para compor o modelo
#'
#' Cobertura agregada esconde o que decide a arquitetura do híbrido. Os
#' sinais de conteúdo deste projeto têm cobertura radicalmente diferente
#' entre si, e tratá-los como um bloco único levaria à conclusão errada sobre
#' quais formas de híbrido são viáveis.
#'
#' @param dados Lista nomeada de tibbles, de `carregar_dados()`.
#' @return tibble com `sinal`, `tipo`, `entidades_cobertas`, `total` e
#'   `cobertura`, ordenada da menor cobertura para a maior.
medir_cobertura_de_sinais <- function(dados) {
  n_consultores <- nrow(dados$consultores)

  sinais_consultor <- tibble::tribble(
    ~sinal, ~tabela, ~tipo,
    "skills técnicas", "skills_consultor", "conteúdo",
    "certificações", "certificacoes_consultor", "conteúdo",
    "experiência setorial", "experiencia_setorial", "conteúdo",
    "expertise funcional", "expertise_funcional", "conteúdo",
    "idiomas", "idiomas_consultor", "conteúdo",
    "soft skills", "soft_skills_consultor", "conteúdo",
    "histórico de alocação", "alocacoes", "interação"
  )

  cobertura_consultor <- purrr::pmap(
    list(sinais_consultor$sinal, sinais_consultor$tabela, sinais_consultor$tipo),
    function(nome_sinal, nome_tabela, tipo_sinal) {
      cobertos <- sum(
        dados$consultores$consultor_id %in% dados[[nome_tabela]]$consultor_id
      )

      tibble::tibble(
        sinal = nome_sinal,
        lado = "Consultor",
        tipo = tipo_sinal,
        entidades_cobertas = cobertos,
        total = n_consultores,
        cobertura = cobertos / n_consultores
      )
    }
  ) |>
    purrr::list_rbind()

  n_projetos <- nrow(dados$projetos)
  com_categoria <- sum(!is.na(dados$projetos$setor) & !is.na(dados$projetos$tipo_projeto))
  com_alocacao <- sum(dados$projetos$projeto_id %in% dados$alocacoes$projeto_id)

  cobertura_projeto <- tibble::tibble(
    sinal = c("setor e offering", "histórico de alocação"),
    lado = "Projeto",
    tipo = c("conteúdo", "interação"),
    entidades_cobertas = c(com_categoria, com_alocacao),
    total = n_projetos,
    cobertura = c(com_categoria, com_alocacao) / n_projetos
  )

  dplyr::bind_rows(cobertura_consultor, cobertura_projeto) |>
    dplyr::arrange(cobertura)
}

#' Cruza cold start de interação com cold start de atributo
#'
#' O cruzamento é o que define a zona morta: a entidade sem histórico **e**
#' sem o atributo que o casamento usa é aquela para a qual nenhum componente
#' do híbrido tem o que dizer. Medir as duas dimensões em separado esconde
#' exatamente esse caso.
#'
#' O resultado depende de qual sinal de conteúdo se considera, e é por isso
#' que `sinal_conteudo` é argumento: a zona morta com skills é grande e com
#' experiência setorial é vazia, o que muda a conclusão sobre a arquitetura.
#'
#' Reporta também o esperado sob independência entre as duas ausências, para
#' o excesso observado poder ser lido contra ele. O gerador sorteia as duas
#' de forma independente, então excesso aqui é ruído amostral, não estrutura.
#'
#' @param dados Lista nomeada de tibbles.
#' @param sinal_conteudo Nome da tabela que fornece o atributo de conteúdo.
#' @return Lista com `quadrantes` (tibble), `zona_morta` e
#'   `zona_morta_esperada`.
cruzar_cold_start <- function(dados, sinal_conteudo = "skills_consultor") {
  tem_interacao <- dados$consultores$consultor_id %in% dados$alocacoes$consultor_id
  tem_conteudo <- dados$consultores$consultor_id %in% dados[[sinal_conteudo]]$consultor_id
  n_consultores <- nrow(dados$consultores)

  zona_morta <- sum(!tem_interacao & !tem_conteudo)
  esperado <- n_consultores * mean(!tem_interacao) * mean(!tem_conteudo)

  quadrantes <- tibble::tibble(
    interacao = c(TRUE, TRUE, FALSE, FALSE),
    conteudo = c(TRUE, FALSE, TRUE, FALSE),
    situacao = c(
      "Ambos os componentes atuam",
      "Só o colaborativo atua",
      "Só o conteúdo atua",
      "Nenhum componente atua"
    ),
    consultores = c(
      sum(tem_interacao & tem_conteudo),
      sum(tem_interacao & !tem_conteudo),
      sum(!tem_interacao & tem_conteudo),
      zona_morta
    )
  ) |>
    dplyr::mutate(fracao = consultores / n_consultores)

  list(
    quadrantes = quadrantes,
    zona_morta = zona_morta,
    zona_morta_esperada = esperado,
    sinal_conteudo = sinal_conteudo
  )
}

#' Avalia a viabilidade de cada forma de híbrido da taxonomia de Burke
#'
#' Burke (2002) organiza os híbridos de recomendação em formas com exigências
#' distintas sobre os componentes. Nem toda forma é viável sob qualquer
#' padrão de cobertura, e é o padrão medido que decide, não a preferência.
#'
#' A avaliação é declarativa e não calculada: ela registra, para cada forma,
#' a exigência que a literatura lhe atribui e o veredito diante do que os
#' blocos II.1 a II.3 mediram. Fica como dado versionado, e não como prosa,
#' para a decisão poder ser auditada e revista quando a medida mudar.
#'
#' @return tibble com `forma`, `exige`, `viavel` e `justificativa`.
avaliar_formas_de_hibrido <- function() {
  tibble::tribble(
    ~forma, ~exige, ~viavel, ~justificativa,

    "Ponderado", "os dois componentes produzem escore para todo par", FALSE,
    "o colaborativo não produz escore para entidade sem histórico, e são 17% dos consultores e 21% dos projetos; ponderar exigiria um valor de reserva que na prática é o próprio conteúdo",

    "Chaveado", "critério observável para escolher o componente por caso", TRUE,
    "a presença de histórico é critério observável e direto; degenera para conteúdo na maioria dos casos, dado o bloco II.2",

    "Cascata", "um componente ordena e o outro refina", TRUE,
    "o conteúdo tem cobertura universal no espaço setorial e serve para gerar o conjunto candidato; o colaborativo reordena onde houver histórico suficiente",

    "Aumento de atributo", "saída do colaborativo entra como atributo do conteúdo", FALSE,
    "exige que a saída do colaborativo seja confiável, e o bloco II.2 mostra que ela não é identificável nesta escala",

    "Meta-nível", "modelo aprendido por um componente alimenta o outro", FALSE,
    "mesma exigência do aumento de atributo, agravada por depender do modelo inteiro e não de um escore"
  )
}
