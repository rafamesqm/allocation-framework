#' Executa o bloco II.2 da EDA, identificabilidade da fatoração latente
#'
#' **Âncora na literatura.** Koren, Bell e Volinsky (2009) consolidam a
#' fatoração latente como método de referência para recomendação, sobre o
#' Netflix Prize. É contra o porte daquele problema, e dos conjuntos
#' MovieLens sobre os quais a literatura foi validada, que o porte deste
#' precisa ser lido.
#'
#' **Propriedade medida.** Quantidade de evidência por parâmetro livre, na
#' razão global e no corte por entidade.
#'
#' **Decisão que discrimina.** Se a fatoração latente é estimável nesta
#' escala. É o bloco de maior consequência da Parte II, porque pode eliminar
#' uma família inteira antes de qualquer experimento, e o faz com um
#' argumento que vale igualmente para o dado real, cuja escala é a mesma.
#'
#' O resultado conversa diretamente com o bloco II.1: onde a entidade não tem
#' evidência para determinar seu vetor, a regularização encolhe a estimativa
#' em direção ao valor a priori, e a predição tende aos termos de viés, que
#' são essencialmente popularidade. Ou seja, a família ou não é identificável
#' ou, onde regularizada, degenera na linha de base que precisava superar.
#'
#' @param dados Lista nomeada de tibbles, de `carregar_dados()`.
#' @param estabilidade tibble de `resumir_estabilidade()`, opcional.
#' @param fatores Valores de `k` avaliados.
#' @return Lista com `objetos`, `tabelas`, `figuras` e `achados`.
executar_ii2 <- function(dados, estabilidade = NULL,
                          fatores = c(1, 2, 3, 5, 10, 20)) {
  identificabilidade <- medir_identificabilidade(
    dados$alocacoes, dados$consultores, dados$projetos, fatores
  )

  comparacao_k10 <- comparar_porte_com_referencias(
    dados$alocacoes, dados$consultores, dados$projetos, fatores = 10
  )

  # A curva desenha a escala real, e não a sintética, porque é a real que
  # sustenta o argumento do bloco. As duas coincidem em 0,02% na razão por
  # parâmetro, então plotar ambas daria duas linhas sobrepostas, o que o
  # leitor lê como defeito de renderização e não como concordância. A tabela
  # mostra as duas lado a lado, que é onde a coincidência fica visível.
  curva_comparacao <- purrr::map(fatores, function(k) {
    comparar_porte_com_referencias(
      dados$alocacoes, dados$consultores, dados$projetos, fatores = k
    ) |>
      dplyr::filter(conjunto != "Alocação de consultores (sintético)")
  }) |>
    purrr::list_rbind()

  em <- function(k) identificabilidade[identificabilidade$fatores == k, ]
  k1 <- em(1)
  k2 <- em(2)
  k5 <- em(5)
  k10 <- em(10)

  este <- comparacao_k10[comparacao_k10$conjunto == comparacao_k10$conjunto[1], ]
  real <- comparacao_k10[comparacao_k10$conjunto == "Alocação de consultores (real)", ]
  netflix <- comparacao_k10[comparacao_k10$conjunto == "Netflix Prize", ]
  movielens_pequeno <- comparacao_k10[comparacao_k10$conjunto == "MovieLens 100K", ]
  razao_netflix <- netflix$obs_por_parametro / este$obs_por_parametro
  razao_movielens <- movielens_pequeno$obs_por_parametro / este$obs_por_parametro

  # A distância relativa entre a razão do sintético e a dos agregados reais.
  # É o que autoriza dizer que a conclusão do bloco não depende do gerador.
  desvio_real <- abs(este$obs_por_parametro - real$obs_por_parametro) /
    real$obs_por_parametro

  objetos <- list(
    identificabilidade = identificabilidade,
    comparacao_k10 = comparacao_k10,
    curva_comparacao = curva_comparacao,
    k1 = k1, k2 = k2, k5 = k5, k10 = k10,
    este = este,
    real = real,
    desvio_real = desvio_real,
    razao_netflix = razao_netflix,
    razao_movielens = razao_movielens,
    fatores = fatores
  )

  tabelas <- list(
    ii2_identificabilidade = identificabilidade |>
      dplyr::transmute(
        `Fatores` = fatores,
        `Parâmetros livres` = formatar_inteiro(parametros),
        `Observações` = formatar_inteiro(observacoes),
        `Obs. por parâmetro` = formatar_decimal(obs_por_parametro, 3),
        `Consultores sem evidência` = formatar_pct(fracao_consultores_subdeterminados),
        `Projetos sem evidência` = formatar_pct(fracao_projetos_subdeterminados)
      ),
    ii2_comparacao_porte = comparacao_k10 |>
      dplyr::transmute(
        Conjunto = conjunto,
        `Interações` = formatar_inteiro(interacoes),
        `Usuários` = formatar_inteiro(usuarios),
        Itens = formatar_inteiro(itens),
        `Parâmetros (k=10)` = formatar_inteiro(parametros),
        `Obs. por parâmetro` = formatar_decimal(obs_por_parametro, 2)
      )
  )

  figuras <- list(
    ii2_evidencia = figura_evidencia_por_parametro(
      curva_comparacao,
      titulo = "Evidência por parâmetro, contra conjuntos públicos de referência",
      subtitulo = "Escala real da Peers, medida no Unity Catalog; o sintético coincide com ela em 0,02%. Linha tracejada: um parâmetro livre por observação"
    ),
    ii2_subdeterminacao = figura_subdeterminacao(
      identificabilidade,
      titulo = "Entidades sem evidência para determinar o próprio vetor",
      subtitulo = "Fração com menos interações observadas que fatores a estimar"
    )
  )

  achados <- registrar_achados("II.2", com_sintese(tibble::tribble(
    ~codigo, ~etiqueta, ~enunciado, ~implicacao,

    "II.2.1", "instrumento",
    paste0(
      "Mesmo com dois fatores, a fatoração latente tem ",
      formatar_inteiro(k2$parametros), " parâmetros livres para ",
      formatar_inteiro(k2$observacoes), " observações, ou ",
      formatar_decimal(k2$obs_por_parametro, 2),
      " observação por parâmetro. Em cinco fatores cai para ",
      formatar_decimal(k5$obs_por_parametro, 2), "."
    ),
    paste0(
      "Abaixo de uma observação por parâmetro não há evidência suficiente ",
      "para estimar o modelo sem que a regularização faça o trabalho. A ",
      "conclusão não depende do gerador: refeita direto sobre os agregados ",
      "reais documentados, ", formatar_inteiro(real$interacoes),
      " pares distintos entre ", formatar_inteiro(real$usuarios),
      " colaboradores e ", formatar_inteiro(real$itens),
      " códigos de alocação, a razão com dez fatores dá ",
      formatar_decimal(real$obs_por_parametro, 3), ", contra ",
      formatar_decimal(este$obs_por_parametro, 3),
      " no sintético, uma diferença de ", formatar_pct(desvio_real), "."
    ),

    "II.2.2", "instrumento",
    paste0(
      "Contra os conjuntos sobre os quais a literatura de fatoração foi ",
      "construída, a diferença é de ordem de grandeza: com dez fatores, este ",
      "problema tem ", formatar_decimal(este$obs_por_parametro, 2),
      " observação por parâmetro, contra ",
      formatar_decimal(movielens_pequeno$obs_por_parametro, 1),
      " do MovieLens 100K e ",
      formatar_decimal(netflix$obs_por_parametro, 1),
      " do Netflix Prize, isto é, ",
      formatar_decimal(razao_movielens, 0), " e ",
      formatar_decimal(razao_netflix, 0), " vezes menos evidência."
    ),
    paste0(
      "O MovieLens 100K é o menor conjunto de uso corrente na literatura, e ",
      "ainda assim tem uma ordem de grandeza mais de evidência por parâmetro. ",
      "Transportar para cá resultado obtido naquela escala, sem ressalva, é ",
      "erro de leitura."
    ),

    "II.2.3", "âncora real",
    paste0(
      "O corte por entidade é mais severo que a razão global, porque o vetor ",
      "de cada entidade é estimado só com as interações dela. Já em dois ",
      "fatores, ", formatar_pct(k2$fracao_projetos_subdeterminados),
      " dos projetos e ", formatar_pct(k2$fracao_consultores_subdeterminados),
      " dos consultores têm menos interações que fatores",
      if (is.null(estabilidade)) {
        "."
      } else {
        paste0(
          ". Em cinco fatores, os projetos sem evidência ficam em ",
          formatar_intervalo(estabilidade, "projetos_subdeterminados_k5"), "."
        )
      }
    ),
    paste0(
      "A esparsidade extrema do lado do projeto, com boa parte deles tendo ",
      "uma única pessoa alocada, é característica do problema e não artefato ",
      "do gerador: projeto de consultoria com equipe pequena é a norma. Isso ",
      "torna o lado do projeto estruturalmente hostil a representação latente."
    ),

    "II.2.4", "instrumento",
    paste0(
      "Onde a entidade não tem evidência para determinar seu vetor, a ",
      "regularização não cria informação: ela encolhe a estimativa para o ",
      "valor a priori, e a predição tende aos termos de viés do modelo, que ",
      "são essencialmente popularidade."
    ),
    paste0(
      "Conjugado ao bloco II.1, o argumento fecha: a fatoração latente pura ",
      "ou não é identificável nesta escala ou, onde regularizada o bastante ",
      "para ser, degenera na linha de base de popularidade que ela precisava ",
      "superar. **A decisão que este bloco habilita é que o colaborativo não ",
      "pode ser o componente principal**, e que o peso precisa recair sobre o ",
      "conteúdo e sobre restrição, não sobre o histórico de interação. Com a ",
      "ressalva, que é do bloco II.3 e não deste, de que o conteúdo também ",
      "não cobre todo mundo: ele alcança menos consultores que o próprio ",
      "colaborativo, e transferir o peso para ele não resolve o caso de quem ",
      "não tem nenhum dos dois. A forma do híbrido é decisão do bloco II.3."
    )
  ), tibble::tribble(
    ~codigo, ~decisao, ~limitacao, ~proveniencia,

    "II.2.1", NA,
    paste0(
      "A fatoração latente pura não é estimável nesta escala sem que a ",
      "regularização faça o trabalho, e a conta não depende do gerador: ",
      "refeita direto sobre os agregados reais, 4.598 pares distintos entre ",
      "511 colaboradores e 1.833 códigos de alocação, a razão com dez fatores ",
      "dá o mesmo valor que no sintético."
    ),
    "fato real",

    "II.2.2", NA,
    paste0(
      "Transportar para cá, sem ressalva, resultado obtido na escala do ",
      "MovieLens ou do Netflix Prize é erro de leitura: há entre 19 e 103 ",
      "vezes menos evidência por parâmetro."
    ),
    "fato real",

    "II.2.3", NA,
    paste0(
      "O lado do projeto é estruturalmente hostil a representação latente, e ",
      "isso é característica do problema e não artefato do gerador: projeto de ",
      "consultoria com equipe pequena é a norma."
    ),
    "fato real",

    "II.2.4",
    paste0(
      "O colaborativo não pode ser o componente principal do híbrido, e o peso ",
      "precisa recair sobre conteúdo e restrição, não sobre o histórico de ",
      "interação."
    ),
    NA,
    "fato real"
  )))

  list(objetos = objetos, tabelas = tabelas, figuras = figuras, achados = achados)
}
