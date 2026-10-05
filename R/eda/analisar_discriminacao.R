#' Mede a resolução de um espaço de categorias usado como proxy de `P`
#'
#' O candidato mais simples para estimar `P` é o *one-hot* das categorias do
#' projeto: o consultor casa se a categoria dele bate com a do projeto. A
#' pergunta que decide se isso serve não é se o casamento existe, é quantas
#' pessoas ele deixa **empatadas**.
#'
#' Um espaço grosso, como só o setor, põe dezenas de consultores na mesma
#' classe, e dentro dela o mecanismo não ordena ninguém: a recomendação fica
#' indiferente entre pessoas que a alocação precisa distinguir. Um espaço
#' fino, como setor cruzado com offering, tem muitas classes e poucos
#' consultores em cada, e aí não há massa para estimar nada.
#'
#' **Os dois lados do compromisso são medidos juntos de propósito**, porque
#' olhar só um deles leva à conclusão errada. `empate_medio` é a fração do
#' quadro que fica indistinguível de um consultor típico, e mede o primeiro
#' problema. `classes_ralas` é a fração de classes com menos consultores que
#' `minimo_por_classe`, e mede o segundo.
#'
#' @param rotulo_consultor Vetor com a classe de cada consultor.
#' @param minimo_por_classe Abaixo disto a classe é tratada como rala.
#' @return Lista com `n_classes`, `mediana_por_classe`, `empate_medio`,
#'   `classes_ralas` e a contagem por classe.
medir_resolucao_de_categorias <- function(rotulo_consultor, minimo_por_classe = 5) {
  presentes <- rotulo_consultor[!is.na(rotulo_consultor)]
  contagem <- as.integer(table(presentes))
  n <- length(presentes)

  list(
    n_classes = length(contagem),
    n_consultores = n,
    mediana_por_classe = stats::median(contagem),
    # Probabilidade de dois consultores sorteados caírem na mesma classe, que
    # é o tamanho esperado do empate visto por um consultor típico.
    empate_medio = sum(contagem * (contagem - 1)) / (n * (n - 1)),
    classes_ralas = mean(contagem < minimo_por_classe),
    contagem = contagem
  )
}

#' Constrói a matriz consultor por skill, densa e normalizada
#'
#' O perfil de um consultor é o vetor de proficiência dele sobre o espaço de
#' skills, com zero onde a skill não foi declarada. A normalização para norma
#' um é o que permite comparar perfis por cosseno sem que quem declarou mais
#' skills domine a similaridade só por ter vetor maior.
#'
#' @param skills_consultor tibble longo com `consultor_id`, `skill_id` e
#'   `proficiencia`.
#' @param espaco_skills tibble com o catálogo de `skill_id`.
#' @param escala Proficiências na ordem ordinal.
#' @return Matriz de consultores por skills, com linhas de norma um.
construir_matriz_de_perfis <- function(skills_consultor, espaco_skills,
                                        escala = c("Iniciante", "Básico", "Médio", "Avançado")) {
  consultores <- sort(unique(skills_consultor$consultor_id))
  skills <- sort(unique(espaco_skills$skill_id))

  matriz <- matrix(
    0, nrow = length(consultores), ncol = length(skills),
    dimnames = list(consultores, skills)
  )
  posicao <- cbind(
    match(skills_consultor$consultor_id, consultores),
    match(skills_consultor$skill_id, skills)
  )
  matriz[posicao] <- match(skills_consultor$proficiencia, escala)

  normas <- sqrt(rowSums(matriz^2))
  normas[normas == 0] <- 1
  matriz / normas
}

#' Estima o perfil de skills que cada categoria de projeto exige
#'
#' É o mecanismo (a) do obstáculo de `skills_requeridas[]`: como o requisito
#' não é capturado em processo nenhum, o perfil exigido é aproximado pelo
#' perfil médio de quem já atuou em projetos parecidos.
#'
#' **Estimado só sobre o conjunto de treino**, e a separação é o que impede a
#' medida de ser circular: avaliar o perfil sobre as mesmas alocações que o
#' formaram diria apenas que a média de um conjunto se parece com ele mesmo.
#'
#' **Ponderar por alocação ou por pessoa muda o que o mecanismo mede.** Se a
#' média corre sobre as alocações, quem é muito alocado entra várias vezes e
#' o perfil da categoria tende ao perfil dos consultores populares. O escore
#' resultante recupera popularidade disfarçada de conteúdo, e passa a competir
#' com a linha de base do bloco II.1 usando a informação dela. Por pessoa
#' distinta, cada consultor que atuou na categoria pesa igual, e o que sobra é
#' conteúdo. O argumento `por_pessoa` existe para a diferença poder ser
#' medida em vez de suposta.
#'
#' @param alocacoes_treino tibble de alocações do treino.
#' @param projetos tibble com a categoria de cada projeto.
#' @param perfis Matriz de `construir_matriz_de_perfis()`.
#' @param categoria Nome da coluna de categoria do projeto.
#' @param por_pessoa Se `TRUE`, cada consultor pesa uma vez na média da
#'   categoria, qualquer que seja o número de alocações dele.
#' @return Matriz de categorias por skills, com linhas de norma um.
estimar_perfil_por_categoria <- function(alocacoes_treino, projetos, perfis,
                                          categoria = "setor",
                                          por_pessoa = TRUE) {
  categoria_do_projeto <- projetos[[categoria]][
    match(alocacoes_treino$projeto_id, projetos$projeto_id)
  ]
  linha <- match(alocacoes_treino$consultor_id, rownames(perfis))

  usaveis <- !is.na(linha) & !is.na(categoria_do_projeto)
  linha <- linha[usaveis]
  categoria_do_projeto <- categoria_do_projeto[usaveis]

  categorias <- sort(unique(categoria_do_projeto))
  saida <- matrix(
    0, nrow = length(categorias), ncol = ncol(perfis),
    dimnames = list(categorias, colnames(perfis))
  )
  for (cat in categorias) {
    linhas_da_categoria <- linha[categoria_do_projeto == cat]
    if (por_pessoa) {
      linhas_da_categoria <- unique(linhas_da_categoria)
    }
    saida[cat, ] <- colMeans(perfis[linhas_da_categoria, , drop = FALSE])
  }

  normas <- sqrt(rowSums(saida^2))
  normas[normas == 0] <- 1
  saida / normas
}

#' Mede quanto um escore separa quem foi alocado de quem não foi
#'
#' A área sob a curva ROC, calculada pela estatística de Mann e Whitney, que
#' é a forma exata e não precisa de pacote: a AUC é a probabilidade de um par
#' sorteado ter o alocado com escore maior que o não alocado. Meio ponto para
#' empate é o tratamento correto, e é justamente o que penaliza mecanismo que
#' empata muita gente.
#'
#' @param escore Vetor de escores.
#' @param alocado Vetor lógico, verdadeiro onde houve alocação.
#' @return AUC entre 0 e 1, ou `NA` se faltar um dos dois grupos.
calcular_auc <- function(escore, alocado) {
  if (!any(alocado) || all(alocado)) return(NA_real_)

  postos <- rank(escore, ties.method = "average")
  n_pos <- sum(alocado)
  n_neg <- sum(!alocado)

  (sum(postos[alocado]) - n_pos * (n_pos + 1) / 2) / (n_pos * n_neg)
}

#' Compara os mecanismos de estimação de `P` sobre alocações retidas
#'
#' Confronta os dois candidatos que o obstáculo de `skills_requeridas[]`
#' deixou em aberto, mais a linha de base de popularidade do bloco II.1, que
#' é o piso que qualquer mecanismo precisa bater para justificar existir.
#'
#' * **one-hot**: o consultor pontua 1 se o setor predominante dele é o do
#'   projeto, e 0 caso contrário. Escore binário, logo cheio de empates.
#' * **perfil médio**: similaridade de cosseno entre o perfil de skills do
#'   consultor e o perfil médio de quem atuou na categoria, estimado no
#'   treino. Escore contínuo, sem empates.
#' * **popularidade**: o grau do consultor no treino, sem olhar o projeto.
#'
#' **A magnitude da separação é consequência do gerador**, que planta
#' afinidade setorial com `prob_vies`. O que a comparação mede sem depender
#' disso é a **capacidade de desempate**: um escore binário não ordena dentro
#' da classe, qualquer que seja a força da afinidade.
#'
#' @param dados Lista nomeada de tibbles.
#' @param data_corte Alocações que começam a partir daqui vão para o teste.
#' @param amostra_negativos Consultores não alocados sorteados por projeto de
#'   teste, para a AUC não custar a matriz inteira.
#' @param semente Inteiro para reprodutibilidade.
#' @return Lista com `resultados`, `n_teste` e `data_corte`.
comparar_mecanismos_de_p <- function(dados, data_corte = NULL,
                                      amostra_negativos = 30, semente = 1) {
  set.seed(semente)

  if (is.null(data_corte)) {
    data_corte <- stats::median(dados$alocacoes$data_inicio)
  }
  treino <- dados$alocacoes[dados$alocacoes$data_inicio < data_corte, ]
  teste <- dados$alocacoes[dados$alocacoes$data_inicio >= data_corte, ]

  perfis <- construir_matriz_de_perfis(dados$skills_consultor, dados$espaco_skills)
  perfil_categoria <- estimar_perfil_por_categoria(
    treino, dados$projetos, perfis, por_pessoa = TRUE
  )
  perfil_por_alocacao <- estimar_perfil_por_categoria(
    treino, dados$projetos, perfis, por_pessoa = FALSE
  )

  # Soma e contagem por categoria, para poder tirar um consultor da média sem
  # recalculá-la. É o que permite pontuar sem vazamento: se o consultor
  # avaliado ajudou a formar o perfil da categoria, o cosseno dele contra
  # aquele perfil mede em parte a semelhança dele consigo mesmo.
  pertence <- membros_por_categoria(treino, dados$projetos, perfis)

  setor_consultor <- setor_predominante_observado(dados$experiencia_setorial)
  grau_treino <- table(factor(treino$consultor_id, levels = dados$consultores$consultor_id))

  candidatos <- rownames(perfis)
  avaliacoes <- purrr::map(seq_len(nrow(teste)), function(i) {
    projeto <- teste$projeto_id[i]
    setor_projeto <- dados$projetos$setor[dados$projetos$projeto_id == projeto]
    positivo <- teste$consultor_id[i]
    if (!positivo %in% candidatos || !setor_projeto %in% rownames(perfil_categoria)) {
      return(NULL)
    }

    # O número de negativos é limitado ao que existe. Na escala completa há
    # bem mais candidatos que negativos pedidos e o limite não morde, mas em
    # população pequena o sorteio sem reposição falharia. O índice via
    # `sample.int` evita a armadilha de `sample()` com vetor de tamanho um,
    # a mesma que corrompeu a banda de custo de N2.
    disponiveis <- setdiff(candidatos, positivo)
    negativos <- disponiveis[sample.int(
      length(disponiveis), size = min(amostra_negativos, length(disponiveis))
    )]
    avaliados <- c(positivo, negativos)

    tibble::tibble(
      avaliado = avaliados,
      alocado = c(TRUE, rep(FALSE, length(negativos))),
      perfil_medio = as.numeric(perfis[avaliados, ] %*% perfil_categoria[setor_projeto, ]),
      perfil_por_alocacao = as.numeric(
        perfis[avaliados, ] %*% perfil_por_alocacao[setor_projeto, ]
      ),
      perfil_sem_vazamento = pontuar_sem_vazamento(
        avaliados, setor_projeto, perfis, pertence
      ),
      one_hot = as.numeric(
        setor_consultor$segmento[match(avaliados, setor_consultor$consultor_id)] == setor_projeto
      ),
      popularidade = as.numeric(grau_treino[avaliados]),
      par = i
    )
  }) |>
    purrr::list_rbind()

  avaliacoes$one_hot[is.na(avaliacoes$one_hot)] <- 0

  resultados <- purrr::map(
    c(
      "perfil_medio", "perfil_por_alocacao", "perfil_sem_vazamento",
      "one_hot", "popularidade"
    ),
    function(mecanismo) {
      escore <- avaliacoes[[mecanismo]]
      tibble::tibble(
        mecanismo = mecanismo,
        auc = calcular_auc(escore, avaliacoes$alocado),
        valores_distintos = dplyr::n_distinct(escore),
        fracao_empatada = 1 - dplyr::n_distinct(escore) / length(escore)
      )
    }
  ) |>
    purrr::list_rbind() |>
    dplyr::arrange(dplyr::desc(auc))

  list(
    resultados = resultados,
    avaliacoes = avaliacoes,
    n_teste = dplyr::n_distinct(avaliacoes$par),
    n_treino = nrow(treino),
    data_corte = data_corte
  )
}

#' Âncoras reais da ponte entre frente da pessoa e offering do projeto
#'
#' Da segunda consulta às bases comerciais. São o único número real que este
#' bloco tem sobre quanto de afinidade existe entre o atributo da pessoa e a
#' categoria do projeto, e valem mais que qualquer medida sobre o sintético,
#' onde a afinidade foi plantada.
#'
#' A leitura que eles impõem é dupla, e é o achado: a afinidade **existe**,
#' com lift de quase dez para a frente de Data Analytics, e ao mesmo tempo é
#' **fraca em volume**, porque a maior parte das horas de qualquer frente cai
#' nas offerings grandes.
#'
#' Ressalvas registradas na consulta: só cerca de metade das linhas de
#' alocação resolvem frente, 42% das horas não têm chave para o RH, e os
#' grupos das frentes de Digital têm de cinco a nove pessoas.
#'
#' @return tibble com `frente`, `offering`, `share_horas` e `lift`.
referencias_ponte_frente_offering <- function() {
  tibble::tribble(
    ~frente, ~offering, ~share_horas, ~lift,
    "Data Analytics", "Data Analytics & AI", 0.159, 9.94,
    "Tech Consulting", "Tech Consulting", 0.429, 4.50,
    "Digital Products & Platforms", "Digital", 0.182, 3.27,
    "Data Science", "Tech Consulting", 0.166, 1.75
  )
}

#' Lista quem formou o perfil de cada categoria, e a soma dos perfis
#'
#' Guardar a soma e o conjunto de membros, em vez da média pronta, é o que
#' permite tirar um consultor da média em tempo de pontuação sem recalcular
#' nada. Existe para `pontuar_sem_vazamento()`.
#'
#' @param alocacoes_treino tibble de alocações do treino.
#' @param projetos tibble com a categoria de cada projeto.
#' @param perfis Matriz de `construir_matriz_de_perfis()`.
#' @param categoria Nome da coluna de categoria do projeto.
#' @return Lista com `soma` (matriz de categorias por skills), `membros`
#'   (lista de vetores de `consultor_id`) e `n` por categoria.
membros_por_categoria <- function(alocacoes_treino, projetos, perfis,
                                   categoria = "setor") {
  categoria_do_projeto <- projetos[[categoria]][
    match(alocacoes_treino$projeto_id, projetos$projeto_id)
  ]
  consultor <- alocacoes_treino$consultor_id

  usaveis <- consultor %in% rownames(perfis) & !is.na(categoria_do_projeto)
  consultor <- consultor[usaveis]
  categoria_do_projeto <- categoria_do_projeto[usaveis]

  categorias <- sort(unique(categoria_do_projeto))
  membros <- lapply(categorias, function(cat) unique(consultor[categoria_do_projeto == cat]))
  names(membros) <- categorias

  soma <- t(vapply(
    membros,
    function(ids) colSums(perfis[ids, , drop = FALSE]),
    numeric(ncol(perfis))
  ))

  list(soma = soma, membros = membros, n = lengths(membros))
}

#' Pontua consultores contra o perfil da categoria, sem vazamento
#'
#' Se o consultor avaliado ajudou a formar o perfil da categoria, o cosseno
#' dele contra aquele perfil mede em parte a semelhança dele **consigo
#' mesmo**. Numa divisão temporal de alocações isso não é evitado pela
#' divisão: a mesma pessoa aparece nos dois lados, porque o que foi separado
#' foram as alocações e não as pessoas.
#'
#' Esta função tira o avaliado da média antes de pontuá-lo, o que é a forma
#' de deixar um de fora aplicada ao perfil da categoria. A diferença entre
#' pontuar com e sem essa exclusão é a medida do vazamento, e é ela que o
#' bloco II.6 reporta.
#'
#' @param avaliados Vetor de `consultor_id` a pontuar.
#' @param categoria Categoria do projeto avaliado.
#' @param perfis Matriz de `construir_matriz_de_perfis()`.
#' @param pertence Lista de `membros_por_categoria()`.
#' @return Vetor de escores, na ordem de `avaliados`.
pontuar_sem_vazamento <- function(avaliados, categoria, perfis, pertence) {
  soma <- pertence$soma[categoria, ]
  n <- pertence$n[[categoria]]
  membros <- pertence$membros[[categoria]]

  vapply(avaliados, function(id) {
    if (id %in% membros && n > 1) {
      perfil <- (soma - perfis[id, ]) / (n - 1)
    } else {
      perfil <- soma / n
    }
    norma <- sqrt(sum(perfil^2))
    if (norma == 0) return(0)

    sum(perfis[id, ] * perfil / norma)
  }, numeric(1))
}

#' Rótulo legível de cada mecanismo de estimação de `P`
#'
#' Os nomes internos (`perfil_por_alocacao`, `one_hot`) são identificadores de
#' coluna e não servem para figura nem tabela da monografia, que a regra 4 do
#' projeto exige em português legível. Ponto único da tradução, para figura e
#' tabela nunca divergirem no nome do mesmo mecanismo.
#'
#' @param mecanismo Vetor com os nomes internos.
#' @return Vetor com os rótulos legíveis.
rotular_mecanismo <- function(mecanismo) {
  rotulos <- c(
    one_hot = "One-hot da categoria",
    popularidade = "Popularidade (linha de base)",
    perfil_por_alocacao = "Perfil médio, ponderado por alocação",
    perfil_medio = "Perfil médio, ponderado por pessoa",
    perfil_sem_vazamento = "Perfil médio, sem vazamento"
  )
  unname(rotulos[mecanismo])
}
