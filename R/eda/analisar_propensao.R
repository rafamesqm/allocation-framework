#' Estima a propensão de observação de cada par, só por popularidade
#'
#' Propensão, no sentido de Schnabel, Swaminathan, Singh, Chandak e Joachims
#' (2016), é a probabilidade de o par (consultor, projeto) ter sido
#' **observado**, e não a de ele ser uma boa alocação. Estimadores não
#' enviesados de avaliação reponderam cada observação por `1 / propensão`,
#' para compensar o fato de que o que se vê não é amostra aleatória do que
#' poderia ter acontecido.
#'
#' O modelo aqui é o mais simples que a literatura usa e o único estimável
#' sobre a matriz inteira: a propensão do par é o produto das duas
#' popularidades, normalizado pelo total de observações. É exatamente a
#' probabilidade de aresta do modelo nulo que preserva sequências de grau,
#' o mesmo que o bloco I.5 usa como referência, e a coincidência não é
#' acidental: reponderar por popularidade e comparar contra um nulo que
#' preserva grau são a mesma hipótese vista de dois lados.
#'
#' Steck (2010) trata a propensão por popularidade como caso base, e
#' Schnabel et al. (2016) a usam como uma das formas de estimar `P(O)` quando
#' a política de registro não é conhecida. A escolha aqui é deliberada e o
#' bloco II.4 mede o que ela deixa de capturar, em vez de supor que capture.
#'
#' A soma das propensões sobre toda a matriz é, por construção, o número de
#' observações, o que faz de `propensao` uma distribuição de exposição
#' própria e não um escore arbitrário.
#'
#' **O que a grandeza é, com precisão.** `d_i * d_j / m` é a contagem esperada
#' de arestas entre o par sob o modelo de configuração, e só aproxima uma
#' probabilidade enquanto ela fica bem abaixo de 1, isto é, sob esparsidade.
#' Num par em que os dois lados fossem muito populares a expressão pode
#' passar de 1 e deixar de ser interpretável como probabilidade. A esparsidade
#' medida no bloco II.1 põe o problema longe desse regime, mas a checagem não
#' fica implícita: `diagnosticar_positividade()` devolve `propensao_maxima`
#' justamente para a aproximação poder ser conferida a cada execução em vez de
#' suposta.
#'
#' @param alocacoes tibble com `consultor_id` e `projeto_id`.
#' @param consultores,projetos tibbles que definem os dois lados da matriz.
#' @return Lista com `matriz` (consultores x projetos), `grau_consultor`,
#'   `grau_projeto` e `n_observacoes`.
estimar_propensao_por_popularidade <- function(alocacoes, consultores, projetos) {
  n_observacoes <- nrow(alocacoes)

  grau_consultor <- as.integer(table(
    factor(alocacoes$consultor_id, levels = consultores$consultor_id)
  ))
  grau_projeto <- as.integer(table(
    factor(alocacoes$projeto_id, levels = projetos$projeto_id)
  ))

  matriz <- outer(grau_consultor, grau_projeto) / n_observacoes
  dimnames(matriz) <- list(consultores$consultor_id, projetos$projeto_id)

  list(
    matriz = matriz,
    grau_consultor = grau_consultor,
    grau_projeto = grau_projeto,
    n_observacoes = n_observacoes
  )
}

#' Diagnostica positividade e o custo em variância de reponderar
#'
#' **Positividade** é a condição de todo par ter probabilidade não nula de ser
#' observado. Sem ela, reponderar não é apenas impreciso, é impossível: não há
#' peso finito que recupere um estrato que nunca poderia ter aparecido. É a
#' parte deste bloco que se transfere para o dado real, porque depende da
#' estrutura de quem nunca foi alocado, e não da magnitude do viés.
#'
#' Mesmo onde a positividade vale, reponderar custa variância. O peso é o
#' inverso da propensão, então observação rara vira peso grande e o estimador
#' passa a depender de poucas linhas. O **tamanho efetivo de amostra de Kish**,
#' `(soma dos pesos)^2 / soma dos pesos ao quadrado`, mede quantas observações
#' o estimador de fato usa depois da reponderação. É a grandeza que diz se o
#' estimador não enviesado é utilizável na prática ou se troca viés por
#' variância a ponto de não sobrar nada.
#'
#' @param propensao Lista de `estimar_propensao_por_popularidade()`.
#' @param alocacoes,consultores,projetos Dados que definem a matriz.
#' @return Lista com `fracao_pares_sem_positividade`, `propensao_maxima`,
#'   `aproximacao_valida`, `consultores_grau_zero`, `projetos_grau_zero`,
#'   `quantis_observados`, `peso_maximo`, `peso_mediano`, `ess`,
#'   `ess_relativo` e `propensao_observada`.
diagnosticar_positividade <- function(propensao, alocacoes, consultores, projetos) {
  matriz <- propensao$matriz

  linha <- match(alocacoes$consultor_id, consultores$consultor_id)
  coluna <- match(alocacoes$projeto_id, projetos$projeto_id)
  propensao_observada <- matriz[cbind(linha, coluna)]

  pesos <- 1 / propensao_observada
  ess <- sum(pesos)^2 / sum(pesos^2)

  list(
    fracao_pares_sem_positividade = mean(matriz == 0),
    pares_sem_positividade = sum(matriz == 0),
    propensao_maxima = max(matriz),
    aproximacao_valida = max(matriz) < 1,
    consultores_grau_zero = sum(propensao$grau_consultor == 0),
    projetos_grau_zero = sum(propensao$grau_projeto == 0),
    quantis_observados = stats::quantile(
      propensao_observada, c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1)
    ),
    peso_maximo = max(pesos),
    peso_mediano = stats::median(pesos),
    ess = ess,
    ess_relativo = ess / nrow(alocacoes),
    propensao_observada = propensao_observada
  )
}

#' Mede o viés de seleção e o quanto a propensão por popularidade o captura
#'
#' O ponto do bloco não é medir o viés, que neste sintético foi plantado e
#' cuja magnitude é propriedade do código. O ponto é confrontar o viés
#' observado com o que a propensão estimável prevê. Se a propensão prevê uma
#' taxa de pareamento próxima da aleatória enquanto a observada é várias vezes
#' maior, o modelo de propensão é cego justamente ao mecanismo que domina a
#' seleção, e reponderar por ele não corrige o que precisa ser corrigido.
#'
#' Reporta também a cobertura da covariável. O setor predominante do consultor
#' vem de `experiencia_setorial`, que o bloco II.3 mostrou cobrir só os
#' respondentes do formulário. Um modelo de propensão com covariável, que
#' capturaria a afinidade setorial, não tem entrada para a parte da matriz
#' cujo consultor não respondeu, e essa é uma limitação de dado real, não de
#' gerador.
#'
#' @param dados Lista nomeada de tibbles, de `carregar_dados()`.
#' @param propensao Lista de `estimar_propensao_por_popularidade()`.
#' @return Lista com as taxas, o `lift`, e as coberturas da covariável.
medir_vies_de_selecao <- function(dados, propensao) {
  setor_do_consultor <- setor_predominante_observado(dados$experiencia_setorial)

  grupo_alocacao <- setor_do_consultor$segmento[
    match(dados$alocacoes$consultor_id, setor_do_consultor$consultor_id)
  ]
  setor_alocacao <- dados$projetos$setor[
    match(dados$alocacoes$projeto_id, dados$projetos$projeto_id)
  ]

  taxa_observada <- mean(grupo_alocacao == setor_alocacao, na.rm = TRUE)
  taxa_ao_acaso <- 1 / dplyr::n_distinct(dados$projetos$setor)

  # A taxa que a propensão por popularidade prevê. Pares cujo consultor não
  # tem setor conhecido ficam fora dos dois lados da razão, para a comparação
  # ser feita sobre o mesmo conjunto de pares em que a taxa observada é
  # calculável.
  grupo_consultor <- setor_do_consultor$segmento[
    match(dados$consultores$consultor_id, setor_do_consultor$consultor_id)
  ]
  mesmo_setor <- outer(grupo_consultor, dados$projetos$setor, FUN = "==")
  peso <- propensao$matriz
  peso[is.na(mesmo_setor)] <- NA_real_
  taxa_sob_popularidade <- sum(peso * mesmo_setor, na.rm = TRUE) /
    sum(peso, na.rm = TRUE)

  list(
    taxa_observada = taxa_observada,
    taxa_ao_acaso = taxa_ao_acaso,
    taxa_sob_popularidade = taxa_sob_popularidade,
    lift_observado = taxa_observada / taxa_ao_acaso,
    lift_da_propensao = taxa_sob_popularidade / taxa_ao_acaso,
    cobertura_covariavel_alocacoes = mean(!is.na(grupo_alocacao)),
    cobertura_covariavel_consultores = mean(!is.na(grupo_consultor))
  )
}

#' Extrai o setor predominante de cada consultor a partir do que foi observado
#'
#' Existe como função própria porque a regra de desempate aparecia copiada em
#' mais de um lugar da camada de análise e copiar convida à divergência. O
#' empate é resolvido pela maior intensidade, e a ordem entre intensidades
#' iguais é arbitrária, o que está declarado também em `gerar_alocacoes()`.
#' Aquela cópia, no gerador, fica onde está de propósito: opera sobre a
#' experiência latente e pertence a outra camada.
#'
#' Opera sobre a tabela **observada**, que é a que existe em
#' `data/synthetic/`. Consultor que não respondeu o formulário não aparece, e
#' é essa ausência que o bloco II.4 mede.
#'
#' @param experiencia_setorial tibble com `consultor_id`, `segmento` e
#'   `intensidade`.
#' @return tibble com uma linha por consultor presente na entrada.
setor_predominante_observado <- function(experiencia_setorial) {
  experiencia_setorial |>
    dplyr::arrange(dplyr::desc(intensidade)) |>
    dplyr::distinct(consultor_id, .keep_all = TRUE)
}

#' Registra se existe amostra de exposição aleatória para validar propensões
#'
#' A pergunta decide o bloco. Schnabel et al. (2016) validam propensões
#' estimadas contra uma política de registro conhecida ou contra uma amostra
#' pequena de exposição aleatória. Marlin e Zemel (2009) mostram que sem uma
#' dessas duas coisas não há como distinguir um modelo de ausência correto de
#' um errado, porque os dois explicam igualmente bem o que foi observado.
#'
#' Na organização não existe nenhuma das duas. A alocação é decidida de forma
#' centralizada e subjetiva, sem registro do critério, e nunca houve
#' aleatorização. Os conjuntos públicos que permitem esse tipo de validação,
#' como o Yahoo! R3 e o coat, existem justamente porque alguém coletou uma
#' amostra aleatória de propósito, e não há equivalente aqui.
#'
#' Fica como dado versionado, e não como prosa, pelo mesmo motivo de
#' `avaliar_formas_de_hibrido()`: é premissa que decide o capítulo, e precisa
#' ser auditável e revisável se algum dia houver coleta aleatorizada.
#'
#' @return tibble com `fonte`, `disponivel` e `observacao`.
verificar_amostra_de_exposicao_aleatoria <- function() {
  tibble::tribble(
    ~fonte, ~disponivel, ~observacao,

    "Política de registro conhecida", FALSE,
    "a alocação é decidida de forma centralizada e subjetiva, sem registro do critério aplicado a cada caso, o que é o problema que motiva esta monografia",

    "Amostra de exposição aleatória", FALSE,
    "nunca houve aleatorização de alocação na organização, e não seria eticamente trivial introduzir uma, porque alocar pessoa a projeto ao acaso tem custo real para as duas partes",

    "Registro de candidatos considerados e preteridos", FALSE,
    "só a alocação efetivada é registrada; quem foi cogitado e descartado não deixa rastro em nenhuma das tabelas do crosswalk"
  )
}

#' Avalia quais estimadores de avaliação sobrevivem ao padrão medido
#'
#' Mesma forma de `avaliar_formas_de_hibrido()`: o veredito é calculado a
#' partir das grandezas medidas, e não declarado, para não envelhecer em
#' silêncio quando a medida mudar.
#'
#' @param positividade_completa Se todo par tem propensão não nula.
#' @param propensao_captura_mecanismo Se a propensão estimável reproduz o
#'   viés observado.
#' @param cobertura_covariavel Fração de consultores com a covariável que
#'   capturaria o mecanismo.
#' @param ess_relativo Tamanho efetivo de amostra após reponderar, relativo.
#' @param existe_amostra_aleatoria Se há amostra de exposição aleatória.
#' @param limite_ess Abaixo desta fração, reponderar é considerado inviável
#'   por variância. Convenção de leitura declarada, não teste.
#' @return tibble com `estimador`, `exige`, `viavel` e `justificativa`.
avaliar_estimadores_de_propensao <- function(positividade_completa,
                                              propensao_captura_mecanismo,
                                              cobertura_covariavel,
                                              ess_relativo,
                                              existe_amostra_aleatoria = FALSE,
                                              limite_ess = 0.5) {
  covariavel_completa <- isTRUE(all.equal(cobertura_covariavel, 1))
  variancia_toleravel <- ess_relativo >= limite_ess

  tibble::tribble(
    ~estimador, ~exige, ~viavel, ~justificativa,

    "IPS com propensão por popularidade",
    "positividade em todo par e propensão que capture o mecanismo de seleção",
    positividade_completa && propensao_captura_mecanismo && variancia_toleravel,
    paste0(
      "a propensão por popularidade é cega à afinidade setorial, que é o que domina a seleção, e reponderar por ela corrige a dimensão errada; some-se que ",
      formatar_pct(1 - ess_relativo),
      " do tamanho efetivo de amostra se perde na reponderação"
    ),

    "IPS com propensão por covariáveis",
    "a covariável que explica a seleção observada para todo consultor",
    covariavel_completa && positividade_completa,
    paste0(
      "o setor predominante do consultor vem do formulário de skills e existe para ",
      formatar_pct(cobertura_covariavel),
      " deles, então mais da metade da matriz não tem entrada para o modelo"
    ),

    "IPS auto-normalizado (SNIPS)",
    "positividade em todo par, trocando parte da variância por viés pequeno",
    positividade_completa,
    "reduz a variância do IPS, mas não cria peso finito para estrato de propensão zero nem enxerga mecanismo que a propensão não modela",

    "Validação contra exposição aleatória",
    "amostra de pares expostos ao acaso",
    existe_amostra_aleatoria,
    "é o que torna a propensão falsificável na literatura, e não existe nesta base nem há como construir retroativamente",

    "Estimador sobre o observado, com viés declarado",
    "que a conclusão seja enunciada sobre a população observada, não sobre a população inteira",
    TRUE,
    "não corrige o viés, mas não finge corrigi-lo; é o que resta quando a propensão não é nem estimável em toda a matriz nem validável"
  )
}
