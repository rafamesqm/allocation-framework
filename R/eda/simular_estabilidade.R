#' Gera uma realização do sintético, com as tabelas que as grandezas medem
#'
#' Uma realização é um sorteio, não uma propriedade do modelo. As réplicas
#' existem para que a Parte I possa dizer quanto de cada número reportado é
#' estrutura e quanto é a semente que calhou de ser usada.
#'
#' **Gerar e medir são funções separadas** para a mesma medida poder ser
#' aplicada ao conjunto versionado, que não é gerado aqui. Antes da separação
#' cada bloco recalculava o seu ponto por conta própria, e foi assim que o
#' achado II.4.1 passou a citar um ponto calculado com a experiência observada
#' ao lado de um intervalo calculado com a latente.
#'
#' A experiência entra nas duas versões, com os nomes do conjunto versionado:
#' `experiencia_setorial` é a observada, restrita a quem respondeu o
#' formulário, e `experiencia_latente` é a que alimentou a alocação. O
#' conjunto versionado só guarda a primeira.
#'
#' **A realização versionada não é a réplica de semente 1.** O `targets`
#' atribui a cada alvo uma semente própria e determinística, derivada do nome
#' do alvo, enquanto aqui os geradores correm em sequência num mesmo fluxo
#' aleatório. As duas coisas são reprodutíveis, mas não coincidem. A leitura
#' correta é que o conjunto versionado é um sorteio da mesma distribuição que
#' estas réplicas caracterizam, e não um elemento desta amostra.
#'
#' @param semente Inteiro que identifica a realização.
#' @param n_consultores,n_projetos Escala da população.
#' @param data_referencia Data de referência da geração.
#' @return Lista nomeada de tibbles, com os nomes de `carregar_dados()`.
gerar_realizacao <- function(semente,
                             n_consultores = 285,
                             n_projetos = 952,
                             data_referencia = as.Date("2026-09-01")) {
  consultores <- gerar_consultores(n_consultores = n_consultores, semente = semente)
  projetos <- gerar_projetos(
    n_projetos = n_projetos, semente = semente, data_referencia = data_referencia
  )
  # `experiencia` é o atributo latente e alimenta a alocação, que responde à
  # experiência real da pessoa. `experiencia_observada` é o que o formulário
  # capturou, e é sobre ela que a cobertura de conteúdo é medida. Misturar as
  # duas aqui reintroduziria o artefato que a Parte II encontrou.
  respondentes <- sortear_respondentes_formulario(consultores)
  experiencia <- gerar_experiencia_setorial(consultores)
  experiencia_observada <- restringir_a_respondentes(experiencia, respondentes)
  alocacoes <- gerar_alocacoes(consultores, projetos, experiencia, semente = semente)
  custo <- gerar_custo_carregado(consultores, semente = semente)
  # O espaço de skills sai da chamada para ser reaproveitado pelas grandezas
  # do bloco II.6, no fim desta função. Içá-lo não desloca o fluxo aleatório
  # porque `gerar_espaco_skills()` é determinístico.
  espaco_skills <- gerar_espaco_skills()
  skills <- gerar_skills_consultor(
    consultores, espaco_skills, respondentes = respondentes
  )
  grafo <- gerar_grafo_coalocacao(alocacoes)
  ocupacao <- agregar_ocupacao_consultor(alocacoes)

  list(
    consultores = consultores,
    projetos = projetos,
    experiencia_setorial = experiencia_observada,
    experiencia_latente = experiencia,
    alocacoes = alocacoes,
    custo_carregado = custo,
    espaco_skills = espaco_skills,
    skills_consultor = skills,
    grafo_coalocacao = grafo,
    ocupacao_consultor = ocupacao
  )
}

#' Mede as grandezas que sustentam os achados, sobre qualquer conjunto
#'
#' Recebe as tabelas com os nomes de `carregar_dados()`, e por isso serve
#' tanto às réplicas, via `gerar_realizacao()`, quanto ao conjunto versionado.
#' É essa simetria que torna válida a comparação do achado I.0.5: o ponto e o
#' intervalo saem da mesma definição.
#'
#' A única grandeza que depende da experiência latente é `taxa_mesmo_setor`,
#' e ela fica `NA` quando o conjunto não traz a latente, que é o caso do
#' versionado. A versão que um analista consegue calcular é
#' `taxa_mesmo_setor_observavel`.
#'
#' @param dados Lista nomeada de tibbles.
#' @return tibble de uma linha, uma coluna por grandeza medida.
medir_grandezas <- function(dados) {
  consultores <- dados$consultores
  projetos <- dados$projetos
  experiencia_observada <- dados$experiencia_setorial
  alocacoes <- dados$alocacoes
  custo <- dados$custo_carregado
  espaco_skills <- dados$espaco_skills
  skills <- dados$skills_consultor
  grafo <- dados$grafo_coalocacao
  ocupacao <- dados$ocupacao_consultor

  nivel_num <- senioridade_numerica(consultores$nivel_senioridade)
  nivel_alocacao <- nivel_num[match(alocacoes$consultor_id, consultores$consultor_id)]

  atributos <- tibble::tibble(consultor_id = consultores$consultor_id, nivel_num = nivel_num)
  g <- construir_grafo_coalocacao(grafo, consultores, atributos)

  # Duas versões do mesmo mecanismo, de propósito. A latente é o viés que o
  # gerador de fato aplicou; a observável é a única que um analista consegue
  # calcular, porque depende de `experiencia_setorial` restrita aos
  # respondentes do formulário. A distância entre as duas é o que o bloco
  # II.4 mede, e ela só é pequena porque o gerador sorteia a resposta ao
  # formulário de forma independente da alocação. No dado real essa
  # independência é suposição, e não há como verificá-la.
  tem_latente <- !is.null(dados$experiencia_latente)
  setor_latente <- if (tem_latente) {
    setor_predominante_observado(dados$experiencia_latente)
  } else {
    NULL
  }
  setor_observado <- setor_predominante_observado(experiencia_observada)

  grupo_consultor <- if (tem_latente) {
    setor_latente$segmento[match(alocacoes$consultor_id, setor_latente$consultor_id)]
  } else {
    NULL
  }
  grupo_consultor_observavel <- setor_observado$segmento[
    match(alocacoes$consultor_id, setor_observado$consultor_id)
  ]
  setor_projeto <- projetos$setor[match(alocacoes$projeto_id, projetos$projeto_id)]

  propensao <- estimar_propensao_por_popularidade(alocacoes, consultores, projetos)
  positividade <- diagnosticar_positividade(propensao, alocacoes, consultores, projetos)

  cobertura_skills <- dplyr::n_distinct(skills$consultor_id) / nrow(consultores)

  grau_consultor <- dplyr::count(alocacoes, consultor_id)$n
  grau_projeto <- dplyr::count(alocacoes, projeto_id)$n
  linha_base <- medir_linha_base_popularidade(alocacoes, consultores, tamanhos = 20)
  identificabilidade_k5 <- medir_identificabilidade(alocacoes, consultores, projetos, fatores = 5)
  sem_interacao <- !consultores$consultor_id %in% alocacoes$consultor_id
  sem_skills <- !consultores$consultor_id %in% skills$consultor_id
  sem_setor_observado <- !consultores$consultor_id %in% experiencia_observada$consultor_id

  # **Bloco II.6, apensado ao fim de propósito.** `comparar_mecanismos_de_p()`
  # chama `set.seed()` e amostra negativos. Posto antes de qualquer gerador,
  # deslocaria o fluxo aleatório de toda a realização, e as grandezas já
  # publicadas mudariam de valor sem nenhuma mudança de modelo. Aqui, nada
  # consome aleatoriedade depois dele.
  #
  # A experiência entra na versão **observada**, e não na latente, porque é
  # sobre ela que o bloco II.6 estima o one-hot: usar a latente mediria um
  # setor que o analista não enxerga.
  discriminacao <- comparar_mecanismos_de_p(list(
    alocacoes = alocacoes,
    projetos = projetos,
    consultores = consultores,
    skills_consultor = skills,
    espaco_skills = espaco_skills,
    experiencia_setorial = experiencia_observada
  ))$resultados

  auc_de <- function(mecanismo) {
    discriminacao$auc[discriminacao$mecanismo == mecanismo]
  }
  auc_one_hot <- auc_de("one_hot")
  auc_popularidade <- auc_de("popularidade")

  # Blocos II.1, II.2, II.7 e II.8, também apensados ao fim. Do II.7 entra só
  # a divisão temporal: as outras formas sorteiam, e a temporal é a que o
  # protocolo adota para o colaborativo. `dividir_interacoes()` chama
  # `set.seed()` mesmo no ramo temporal, o que é inofensivo aqui porque nada
  # depois dele consome aleatoriedade.
  divisao_temporal <- dividir_interacoes(alocacoes, "temporal", fracao_teste = 0.3)
  avaliabilidade_temporal <- medir_divisao(
    divisao_temporal, unique(experiencia_observada$consultor_id)
  )
  linha_base_protocolo <- medir_linha_base_sob_protocolo(
    divisao_temporal$treino, divisao_temporal$teste, consultores, tamanhos = 20
  )
  linha_base_50 <- medir_linha_base_popularidade(alocacoes, consultores, tamanhos = 50)
  identificabilidade_k10 <- medir_identificabilidade(
    alocacoes, consultores, projetos, fatores = 10
  )
  filtro <- medir_reducao_pelo_filtro(consultores, projetos, experiencia_observada)
  porte <- medir_porte_do_problema(
    consultores, projetos, alocacoes, dplyr::n_distinct(ocupacao$ano_mes),
    fracao_candidatos = filtro$fracao_candidatos
  )
  variaveis_de <- function(formulacao) porte$variaveis[porte$formulacao == formulacao]

  # Poder por componente, do bloco II.7. O número de pares avaliáveis do
  # colaborativo varia bastante entre realizações, e calcular o ganho mínimo
  # sobre uma só repetiria o erro que estas réplicas existem para evitar. A
  # divisão por consultor sorteia, mas `dividir_interacoes()` fixa a própria
  # semente e nada depois dela consome aleatoriedade.
  poder <- planejar_poder_por_componente(
    alocacoes, consultores, projetos, experiencia_observada
  )
  poder_de <- function(componente, piso) {
    poder[poder$componente == componente & poder$piso == piso, ]
  }
  poder_colaborativo <- poder_de("Colaborativo", "Popularidade no treino")
  poder_conteudo <- poder_de("Conteúdo", "Filtro de setor e acaso")

  tibble::tibble(
    densidade_R = nrow(alocacoes) / (nrow(consultores) * nrow(projetos)),
    cold_start_consultor = 1 - dplyr::n_distinct(alocacoes$consultor_id) / nrow(consultores),
    cold_start_projeto = 1 - dplyr::n_distinct(alocacoes$projeto_id) / nrow(projetos),
    cobertura_skills = cobertura_skills,
    fracao_sem_skills = 1 - cobertura_skills,
    cor_nivel_custo = stats::cor(nivel_num, custo$custo_carregado),
    cor_nivel_tempo_casa = stats::cor(nivel_num, consultores$tempo_casa),
    cor_nivel_pct_alocacao = stats::cor(nivel_alocacao, alocacoes$pct_alocacao),
    cor_nivel_horas_mes = stats::cor(nivel_alocacao, alocacoes$horas_mes),
    v_contrato_nivel = calcular_v_cramer(
      consultores$nivel_senioridade, consultores$tipo_contrato
    ),
    n_localizacoes = dplyr::n_distinct(consultores$localizacao),
    agrupamento_grafo = igraph::transitivity(g, type = "global"),
    isolados_grafo = sum(igraph::degree(g) == 0),
    acima_capacidade = mean(ocupacao$pct_ocupacao > 1),
    grau_medio_consultor = nrow(alocacoes) / dplyr::n_distinct(alocacoes$consultor_id),
    taxa_mesmo_setor = if (tem_latente) {
      mean(grupo_consultor == setor_projeto, na.rm = TRUE)
    } else {
      NA_real_
    },
    taxa_mesmo_setor_observavel = mean(
      grupo_consultor_observavel == setor_projeto, na.rm = TRUE
    ),
    cobertura_covariavel_alocacoes = mean(!is.na(grupo_consultor_observavel)),
    pares_sem_positividade = positividade$fracao_pares_sem_positividade,
    ess_relativo_ips = positividade$ess_relativo,
    gini_grau_consultor = calcular_lorenz(grau_consultor)$gini,
    gini_grau_projeto = calcular_lorenz(grau_projeto)$gini,
    dispersao_grau_consultor = stats::var(grau_consultor) / mean(grau_consultor),
    dispersao_grau_projeto = stats::var(grau_projeto) / mean(grau_projeto),
    recall_popularidade_20 = linha_base$recall,
    projetos_subdeterminados_k5 = identificabilidade_k5$fracao_projetos_subdeterminados,
    consultores_subdeterminados_k5 = identificabilidade_k5$fracao_consultores_subdeterminados,
    zona_morta_skills = mean(sem_interacao & sem_skills),
    cobertura_setor_observada = mean(!sem_setor_observado),
    zona_morta_setor = mean(sem_interacao & sem_setor_observado),
    massa_pct_alocacao_em_1 = mean(alocacoes$pct_alocacao == 1),
    massa_horas_mes_em_0 = mean(alocacoes$horas_mes == 0),
    auc_one_hot = auc_one_hot,
    auc_popularidade_discriminacao = auc_popularidade,
    auc_perfil_sem_vazamento = auc_de("perfil_sem_vazamento"),
    # **A folga é medida como grandeza própria, e não deduzida das duas
    # anteriores**, porque é ela, e não cada AUC em separado, que o achado
    # II.6.6 afirma ser positiva. Sobrepor os dois intervalos marginais
    # responderia outra pergunta.
    #
    # A correlação entre os dois AUCs sobre as réplicas é de cerca de -0,35, e
    # não positiva como se poderia supor por ambos responderem à concentração
    # da alocação. Com correlação negativa a diferença varia **mais** do que
    # variaria sob independência, o que torna a medida direta ainda mais
    # necessária: deduzir a folga das marginais a subestimaria.
    folga_one_hot_popularidade = auc_one_hot - auc_popularidade,
    recall_popularidade_50 = linha_base_50$recall,
    obs_por_parametro_k10 = identificabilidade_k10$obs_por_parametro,
    avaliavel_colaborativo_temporal = avaliabilidade_temporal$avaliavel_colaborativo,
    recall_protocolo_20 = linha_base_protocolo$recall,
    # Medida como grandeza própria pelo mesmo motivo da folga do one-hot: o
    # achado II.7.3 afirma que a correção morde, que é afirmar um sinal.
    correcao_linha_base_20 = linha_base$recall - linha_base_protocolo$recall,
    reducao_porte_filtro = variaveis_de("Trio consultor, projeto e mês") /
      variaveis_de("Trio, restrito ao conjunto candidato"),
    n_avaliavel_colaborativo = poder_colaborativo$n_avaliavel,
    piso_colaborativo = poder_colaborativo$valor_piso,
    ganho_minimo_colaborativo = poder_colaborativo$ganho_independencia,
    ganho_minimo_colaborativo_dominancia = poder_colaborativo$ganho_dominancia,
    n_avaliavel_conteudo = poder_conteudo$n_avaliavel,
    piso_conteudo_filtro = poder_conteudo$valor_piso,
    ganho_minimo_conteudo = poder_conteudo$ganho_independencia,
    ganho_minimo_conteudo_dominancia = poder_conteudo$ganho_dominancia
  )
}

#' Gera e mede uma realização
#'
#' @param semente Inteiro que identifica a realização.
#' @param ... Repassado a `gerar_realizacao()`.
#' @return tibble de uma linha: a semente e uma coluna por grandeza.
medir_realizacao <- function(semente, ...) {
  dplyr::bind_cols(
    tibble::tibble(semente = semente),
    medir_grandezas(gerar_realizacao(semente, ...))
  )
}

#' Repete a geração sobre várias sementes
#'
#' @param sementes Vetor de sementes.
#' @param ... Repassado a `medir_realizacao()`.
#' @return tibble com uma linha por realização.
simular_estabilidade <- function(sementes = 1:40, ...) {
  purrr::map(sementes, medir_realizacao, ...) |>
    purrr::list_rbind()
}

#' Resume a variabilidade de Monte Carlo de cada grandeza
#'
#' **O que este intervalo é, e o que não é.** Ele mede quanto cada número se
#' moveria se a população sintética fosse sorteada de novo. Não é incerteza
#' sobre a organização: o dado real é o que é, e a incerteza sobre ele vem das
#' ressalvas de cobertura das fontes registradas em `mapeamento_schema.md`, não
#' daqui. Confundir os dois transformaria variabilidade de código em margem
#' de erro sobre a empresa, que seria exatamente o tipo de afirmação que este
#' capítulo existe para evitar.
#'
#' `amplitude_relativa` é a largura do intervalo dividida pela mediana, e
#' serve para ordenar as grandezas por fragilidade: é ela que diz em quais
#' achados a semente decide o número.
#'
#' @param replicas tibble de `simular_estabilidade()`.
#' @param nivel Massa de probabilidade coberta pelo intervalo.
#' @return tibble com `medida`, `mediana`, `desvio_mc`, `inferior`,
#'   `superior`, `amplitude_relativa` e `n_replicas`.
resumir_estabilidade <- function(replicas, nivel = 0.90) {
  margem <- (1 - nivel) / 2
  n_replicas <- nrow(replicas)

  replicas |>
    dplyr::select(-semente) |>
    tidyr::pivot_longer(
      dplyr::everything(), names_to = "medida", values_to = "valor"
    ) |>
    dplyr::summarise(
      mediana = stats::median(valor),
      desvio_mc = stats::sd(valor),
      inferior = unname(stats::quantile(valor, margem)),
      superior = unname(stats::quantile(valor, 1 - margem)),
      .by = medida
    ) |>
    dplyr::mutate(
      amplitude_relativa = dplyr::if_else(
        mediana == 0, NA_real_, (superior - inferior) / abs(mediana)
      ),
      n_replicas = n_replicas
    ) |>
    dplyr::arrange(dplyr::desc(amplitude_relativa))
}

#' Extrai o intervalo de uma grandeza, formatado para o texto
#'
#' Evita que cada bloco reescreva a formatação do intervalo e divirja dos
#' demais.
#'
#' @param estabilidade tibble de `resumir_estabilidade()`.
#' @param nome_medida Nome da grandeza.
#' @param formatador Função de formatação aplicada aos três números.
#' @return Texto no formato `"mediana (inferior a superior)"`, ou `NA` se a
#'   grandeza não foi medida.
formatar_intervalo <- function(estabilidade, nome_medida, formatador = formatar_pct) {
  # Mesma proteção das duas funções irmãs abaixo. Os blocos aceitam rodar sem
  # réplicas, e só esta das três quebrava nesse caso.
  if (is.null(estabilidade)) return(NA_character_)

  linha <- estabilidade[estabilidade$medida == nome_medida, ]
  if (nrow(linha) == 0) return(NA_character_)

  paste0(
    formatador(linha$mediana), " (", formatador(linha$inferior),
    " a ", formatador(linha$superior), ")"
  )
}

#' Diz onde uma realização cai dentro do intervalo de Monte Carlo da grandeza
#'
#' Existe porque reportar um ponto que cai fora do próprio intervalo, sem
#' dizer que caiu, é o tipo de incoerência que passa despercebida na escrita e
#' não passa na banca. Aconteceu no bloco II.4: a realização versionada é o
#' extremo das cem réplicas na positividade, e o enunciado citava o ponto e o
#' intervalo lado a lado como se fossem compatíveis.
#'
#' Não é sinal de defeito quando acontece. A realização versionada é um
#' sorteio como os outros, e um sorteio em cada dez cai fora de um intervalo
#' de 90% por definição. O que não pode é o texto silenciar sobre isso.
#'
#' @param estabilidade tibble de `resumir_estabilidade()`.
#' @param nome_medida Nome da grandeza.
#' @param valor Valor observado na realização versionada.
#' @return `"dentro"`, `"abaixo"` ou `"acima"`; `NA` se a grandeza não foi
#'   medida nas réplicas.
posicao_no_intervalo <- function(estabilidade, nome_medida, valor) {
  if (is.null(estabilidade)) return(NA_character_)

  linha <- estabilidade[estabilidade$medida == nome_medida, ]
  if (nrow(linha) == 0) return(NA_character_)

  if (valor < linha$inferior) {
    "abaixo"
  } else if (valor > linha$superior) {
    "acima"
  } else {
    "dentro"
  }
}

#' Diz se o intervalo de uma grandeza fica inteiramente de um lado do zero
#'
#' Existe para achado que afirma um sinal, e não uma magnitude. Dizer que o
#' one-hot supera a linha de base é afirmar que uma diferença é positiva, e
#' essa afirmação só se sustenta se o intervalo da diferença não contiver
#' zero. Sem este teste, o texto citaria a mediana e silenciaria sobre o
#' intervalo, que foi exatamente o que aconteceu com o achado II.6.6 antes de
#' a folga passar a ser medida.
#'
#' O teste é sobre a grandeza da **diferença**, medida diretamente. Aplicá-lo
#' a cada termo em separado e comparar os intervalos responderia outra
#' pergunta, e responderia errado sempre que os dois termos forem
#' correlacionados.
#'
#' @param estabilidade tibble de `resumir_estabilidade()`.
#' @param nome_medida Nome da grandeza de diferença.
#' @return `TRUE` se o intervalo exclui zero, `FALSE` se o contém, `NA` se a
#'   grandeza não foi medida.
intervalo_separa_de_zero <- function(estabilidade, nome_medida) {
  if (is.null(estabilidade)) return(NA)

  linha <- estabilidade[estabilidade$medida == nome_medida, ]
  if (nrow(linha) == 0) return(NA)

  linha$inferior > 0 | linha$superior < 0
}

#' Descreve, em oração, os pontos que caem fora do próprio intervalo
#'
#' Complemento textual de `posicao_no_intervalo()`. O bloco II.4 escreveu
#' essa oração à mão quando a positividade da realização versionada caiu
#' fora do intervalo; o bloco II.1 precisou da mesma coisa para o piso de
#' popularidade, e a segunda ocorrência é o ponto de virar função, para os
#' blocos não divergirem na forma de dizer.
#'
#' Ponto fora não é defeito. Um sorteio em cada dez cai fora de um intervalo
#' de 90% por definição. O que não pode é o texto citar o ponto e o
#' intervalo lado a lado como se fossem compatíveis.
#'
#' @param posicoes Vetor de `posicao_no_intervalo()`, nomeado pelo que cada
#'   posição descreve, como `c("lista de 20" = "acima")`. Sem nomes, a oração
#'   fala do ponto sem qualificá-lo.
#' @return Oração iniciada por ponto e vírgula, ou texto vazio se todos os
#'   pontos estão dentro ou não foram medidos.
descrever_ponto_fora <- function(posicoes) {
  fora <- posicoes[!is.na(posicoes) & posicoes != "dentro"]
  if (length(fora) == 0) return("")

  rotulos <- names(fora)
  partes <- if (is.null(rotulos) || all(!nzchar(rotulos))) {
    paste0(fora, " do intervalo")
  } else {
    paste0(fora, " do intervalo em ", rotulos)
  }

  paste0(
    "; a realização versionada cai ", paste(partes, collapse = " e "),
    ", então o valor pontual é um sorteio extremo e não o típico"
  )
}

#' Situa o conjunto versionado dentro da distribuição das réplicas
#'
#' Os blocos citam o valor do conjunto versionado, que é o dado que o auditor
#' executa, ao lado do intervalo das réplicas. Cada bloco já declara quando o
#' seu ponto cai fora. O que faltava era a pergunta de conjunto: **o
#' conjunto versionado é uma realização típica?** Se ele cair fora em mais
#' grandezas do que um sorteio comum cairia, os números pontuais do capítulo
#' vêm de um sorteio atípico, e é preciso dizer em que direção.
#'
#' Ponto e intervalo saem da mesma função, `medir_grandezas()`, aplicada às
#' duas fontes. Grandeza que não varia entre réplicas é parâmetro imposto
#' (achado I.0.3) e não entra na comparação, porque nela o conjunto
#' versionado coincide com as réplicas por construção.
#'
#' @param versionado tibble de uma linha, de `medir_grandezas()` sobre o
#'   conjunto versionado.
#' @param replicas tibble de `simular_estabilidade()`.
#' @param nivel Massa coberta pelo intervalo.
#' @param invariantes Nomes das grandezas tratadas como invariantes. O
#'   bloco I.0 passa a sua própria classificação, que é mais rigorosa que
#'   exigir variação zero; sem ela, vale a variação zero.
#' @return tibble com uma linha por grandeza, o valor versionado, o
#'   intervalo, o percentil empírico e a posição.
situar_versionado_nas_replicas <- function(versionado, replicas, nivel = 0.90,
                                           invariantes = NULL) {
  margem <- (1 - nivel) / 2
  medidas <- setdiff(intersect(names(versionado), names(replicas)), "semente")

  purrr::map(medidas, function(medida) {
    valores <- replicas[[medida]]
    valor <- versionado[[medida]]
    invariante <- if (is.null(invariantes)) {
      dplyr::n_distinct(valores[!is.na(valores)]) <= 1
    } else {
      medida %in% invariantes
    }
    inferior <- unname(stats::quantile(valores, margem, na.rm = TRUE))
    superior <- unname(stats::quantile(valores, 1 - margem, na.rm = TRUE))

    tibble::tibble(
      medida = medida,
      valor_versionado = valor,
      inferior = inferior,
      superior = superior,
      percentil = if (is.na(valor)) NA_real_ else mean(valores <= valor, na.rm = TRUE),
      invariante = invariante,
      posicao = dplyr::case_when(
        is.na(valor) | invariante ~ NA_character_,
        valor < inferior ~ "abaixo",
        valor > superior ~ "acima",
        .default = "dentro"
      )
    )
  }) |>
    purrr::list_rbind()
}

#' Conta, para cada réplica, quantas grandezas dela caem fora do intervalo
#'
#' É a distribuição de referência para julgar o conjunto versionado, e
#' existe porque a referência óbvia está errada. Com intervalos de 90%,
#' esperam-se fora 10% das grandezas, mas só se elas fossem independentes, e
#' não são: várias respondem à mesma concentração da alocação, e algumas são
#' função direta de outras. Grandezas correlacionadas caem fora juntas, o que
#' engorda a cauda da contagem.
#'
#' **Em vez de modelar a dependência, a contagem a herda.** Cada réplica faz
#' o papel do conjunto versionado e é comparada com o intervalo das demais.
#' A distribuição dessas contagens é a de um sorteio comum, com toda a
#' correlação entre grandezas embutida, e é contra ela que o conjunto
#' versionado é julgado.
#'
#' @param replicas tibble de `simular_estabilidade()`.
#' @param medidas Grandezas a considerar, as não invariantes.
#' @param nivel Massa coberta pelo intervalo.
#' @return Vetor inteiro, uma contagem por réplica.
contar_fora_por_replica <- function(replicas, medidas, nivel = 0.90) {
  margem <- (1 - nivel) / 2
  valores <- as.matrix(replicas[, medidas, drop = FALSE])

  vapply(seq_len(nrow(valores)), function(i) {
    demais <- valores[-i, , drop = FALSE]
    inferior <- apply(demais, 2, stats::quantile, probs = margem, na.rm = TRUE)
    superior <- apply(demais, 2, stats::quantile, probs = 1 - margem, na.rm = TRUE)
    sum(valores[i, ] < inferior | valores[i, ] > superior, na.rm = TRUE)
  }, integer(1))
}

#' Julga se o conjunto versionado é uma realização típica
#'
#' Junta as duas funções anteriores numa resposta: quantas grandezas do
#' conjunto versionado caem fora, quantas um sorteio comum deixaria fora, e
#' que fração das réplicas tem pelo menos tantas fora quanto ele, que é o
#' valor-p empírico da pergunta.
#'
#' O conjunto versionado é comparado com o intervalo das cem réplicas, e cada
#' réplica com o das outras 99. A diferença de uma réplica no intervalo é
#' desprezível diante da da largura dele.
#'
#' @param versionado,replicas,invariantes Como em
#'   `situar_versionado_nas_replicas()`.
#' @param nivel Massa coberta pelo intervalo.
#' @return Lista com a tabela por grandeza, a contagem do versionado, a
#'   distribuição de referência e o valor-p empírico.
julgar_tipicidade_do_versionado <- function(versionado, replicas, nivel = 0.90,
                                            invariantes = NULL) {
  situacao <- situar_versionado_nas_replicas(versionado, replicas, nivel, invariantes)
  comparaveis <- situacao$medida[!is.na(situacao$posicao)]
  referencia <- contar_fora_por_replica(replicas, comparaveis, nivel)
  fora <- sum(situacao$posicao %in% c("abaixo", "acima"))

  list(
    situacao = situacao,
    n_comparaveis = length(comparaveis),
    n_invariantes = sum(situacao$invariante),
    n_fora = fora,
    fora_sob_independencia = (1 - nivel) * length(comparaveis),
    referencia = referencia,
    mediana_referencia = stats::median(referencia),
    valor_p = mean(referencia >= fora),
    nivel = nivel
  )
}
