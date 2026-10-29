#' Mede a cobertura de cada sinal disponível para compor o modelo
#'
#' Cobertura agregada esconde o que decide a arquitetura do híbrido. Os
#' sinais de conteúdo deste projeto têm cobertura radicalmente diferente
#' entre si, e tratá-los como um bloco único levaria à conclusão errada sobre
#' quais formas de híbrido são viáveis.
#'
#' @param dados Lista nomeada de tibbles, de `carregar_dados()`.
#' Carrega também o nome da `tabela` de origem, para quem consome poder
#' selecionar sinais pela fonte declarada em `tabelas_derivadas_de_formulario()`
#' em vez de casar o rótulo por texto, que quebra em silêncio se o rótulo mudar.
#'
#' @return tibble com `sinal`, `tabela`, `tipo`, `entidades_cobertas`, `total`
#'   e `cobertura`, ordenada da menor cobertura para a maior.
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
        tabela = nome_tabela,
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
    tabela = c("projetos", "alocacoes"),
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
#' `sinal_conteudo` segue como argumento para a afirmação poder ser
#' verificada sinal a sinal, e não porque a resposta varie: os sinais
#' derivados da campanha de formulários cobrem exatamente o mesmo conjunto de
#' consultores (ver `verificar_coincidencia_de_sinais()`), então trocar um
#' pelo outro devolve a mesma zona morta. Uma versão anterior deste bloco
#' concluiu o contrário, que a zona morta desaparecia ao trocar skills por
#' experiência setorial, mas isso media um artefato: os geradores dos dois
#' sinais sorteavam cobertura de forma independente, quando na fonte real é a
#' mesma pessoa respondendo a mesma campanha.
#'
#' Reporta também o esperado sob independência entre as duas ausências, para
#' o excesso observado poder ser lido contra ele. O gerador sorteia a ausência
#' de histórico independentemente da ausência de resposta, então excesso aqui
#' é ruído amostral, não estrutura. Que essa independência valha na organização é
#' suposição não verificada, e das que importam: se quem está alocado responde
#' menos formulário, ou mais, a zona morta real não é a medida aqui. O acesso
#' que permitiria checar está encerrado, então fica como limitação declarada.
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
#' @param cobertura_conteudo Fração de consultores com atributo de conteúdo
#'   discriminante declarado.
#' @param cobertura_interacao_consultor,cobertura_interacao_projeto Frações
#'   com histórico de alocação, de `medir_cobertura_de_sinais()`.
#' @param colaborativo_identificavel Veredito do bloco II.2 sobre a fatoração
#'   latente ser determinada pelos dados nesta escala.
#' @return tibble com `forma`, `exige`, `viavel` e `justificativa`.
avaliar_formas_de_hibrido <- function(cobertura_conteudo,
                                       cobertura_interacao_consultor,
                                       cobertura_interacao_projeto,
                                       colaborativo_identificavel = FALSE) {
  conteudo_universal <- isTRUE(all.equal(cobertura_conteudo, 1))
  interacao_universal <- isTRUE(all.equal(cobertura_interacao_consultor, 1)) &&
    isTRUE(all.equal(cobertura_interacao_projeto, 1))

  # Um componente só serve para gerar o conjunto candidato de uma cascata se
  # cobrir todas as entidades e ordenar de forma confiável. O conteúdo cobre
  # menos da metade dos consultores; o colaborativo cobre mais, mas o bloco
  # II.2 nega a confiabilidade. A cascata cai pelos dois lados, cada um por
  # um motivo diferente, e é por isso que os dois são avaliados em separado.
  cascata_por_conteudo <- conteudo_universal
  cascata_por_colaborativo <- interacao_universal && colaborativo_identificavel

  tibble::tribble(
    ~forma, ~exige, ~viavel, ~justificativa,

    "Ponderado", "os dois componentes produzem escore para todo par",
    conteudo_universal && interacao_universal,
    paste0(
      "nenhum dos dois cobre todo par: o conteúdo alcança ",
      formatar_pct(cobertura_conteudo), " dos consultores e o colaborativo, ",
      formatar_pct(cobertura_interacao_consultor), " dos consultores e ",
      formatar_pct(cobertura_interacao_projeto),
      " dos projetos; ponderar exigiria um valor de reserva para as duas ausências"
    ),

    "Chaveado", "critério observável para escolher o componente por caso", TRUE,
    "ter histórico e ter respondido o formulário são observáveis diretamente no dado, e é o par delas que define qual componente tem o que dizer em cada caso",

    "Cascata", "um componente cobre tudo e ordena de forma confiável, o outro refina",
    cascata_por_conteudo || cascata_por_colaborativo,
    paste0(
      "o conteúdo não pode gerar o conjunto candidato porque cobre ",
      formatar_pct(cobertura_conteudo),
      " dos consultores, e quem ficasse de fora nunca entraria na lista; o colaborativo cobre mais (",
      formatar_pct(cobertura_interacao_consultor),
      ") mas o bloco II.2 mostra que sua ordenação não é identificável nesta escala"
    ),

    "Aumento de atributo", "saída do colaborativo entra como atributo do conteúdo",
    colaborativo_identificavel,
    "exige que a saída do colaborativo seja confiável, e o bloco II.2 mostra que ela não é identificável nesta escala",

    "Meta-nível", "modelo aprendido por um componente alimenta o outro",
    colaborativo_identificavel,
    "mesma exigência do aumento de atributo, agravada por depender do modelo inteiro e não de um escore"
  )
}

#' Verifica se os sinais de conteúdo cobrem o mesmo conjunto de consultores
#'
#' A pergunta não é decorativa. Se os sinais cobrem conjuntos diferentes,
#' trocar um pelo outro recupera consultores e a cobertura do conteúdo é a
#' união, maior que a de cada parte. Se cobrem o mesmo conjunto, a união é
#' igual à parte e não há o que trocar: o cold start de conteúdo passa a ser
#' um bloco único de pessoas.
#'
#' Na fonte real são o mesmo formulário (docs/mapeamento_schema.md), então a
#' resposta esperada é coincidência. A função existe para isso ser verificado
#' no dado a cada execução em vez de acreditado, porque foi exatamente uma
#' versão do gerador que violava essa coincidência, sorteando cobertura
#' independente por sinal, que produziu a conclusão errada da primeira versão
#' deste bloco.
#'
#' @param dados Lista nomeada de tibbles.
#' @param tabelas Nomes das tabelas derivadas da campanha de formulários.
#' @return Lista com `coincidem`, `tamanhos`, `uniao` e `intersecao`.
verificar_coincidencia_de_sinais <- function(dados,
                                              tabelas = tabelas_derivadas_de_formulario()) {
  conjuntos <- lapply(tabelas, function(nome) sort(unique(dados[[nome]]$consultor_id)))
  names(conjuntos) <- tabelas

  uniao <- sort(unique(unlist(conjuntos, use.names = FALSE)))
  intersecao <- Reduce(intersect, conjuntos)

  list(
    coincidem = length(uniao) == length(intersecao),
    tamanhos = vapply(conjuntos, length, integer(1)),
    uniao = uniao,
    intersecao = intersecao
  )
}

#' Mede o poder discriminante de um sinal, e não só sua cobertura
#'
#' Cobertura e discriminação são coisas diferentes, e confundi-las é o erro
#' mais fácil de cometer aqui. Um sinal presente em 100% dos consultores mas
#' que os divide em pouquíssimas classes não serve para ordenar candidatos:
#' ele responde a mesma coisa para todo mundo dentro da classe majoritária, e
#' a alocação precisa justamente separar pessoas dentro dela.
#'
#' Reporta o número de perfis distintos e a fração na maior classe, que é o
#' tamanho do grupo que o sinal reconhecidamente **não** separa.
#'
#' @param tabela tibble longa com `consultor_id` e a coluna do sinal.
#' @param coluna Nome da coluna que compõe o perfil.
#' @param n_consultores Total de consultores, para as frações.
#' @return Lista com `perfis`, `n_perfis`, `n_maior_classe` e
#'   `fracao_maior_classe`.
medir_discriminacao_de_sinal <- function(tabela, coluna, n_consultores) {
  perfis <- tabela |>
    dplyr::group_by(consultor_id) |>
    dplyr::summarise(
      perfil = paste(sort(.data[[coluna]]), collapse = " + "), .groups = "drop"
    )

  contagem <- perfis |>
    dplyr::count(perfil, sort = TRUE) |>
    dplyr::mutate(fracao = n / n_consultores)

  list(
    perfis = contagem,
    n_perfis = nrow(contagem),
    n_maior_classe = contagem$n[1],
    fracao_maior_classe = contagem$fracao[1]
  )
}
