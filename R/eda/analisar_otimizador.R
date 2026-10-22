#' Mede o porte do problema de otimização sob cada formulação
#'
#' O número de variáveis de decisão depende da granularidade escolhida, e a
#' escolha não é livre: alocar é uma decisão sobre **pessoa, projeto e
#' período**, e ignorar o período faz a restrição de capacidade deixar de ser
#' expressável, porque capacidade é por mês e não por projeto.
#'
#' As duas formulações medidas são os extremos honestos. A de par trata a
#' alocação como decisão única e perde o tempo. A de trio recupera o tempo e
#' paga em tamanho.
#'
#' O número de restrições de capacidade é o que costuma dominar um modelo
#' assim, e por isso é contado à parte: uma por consultor e mês ativo.
#'
#' **O filtro de conteúdo entra aqui como redutor de porte**, e isso amarra
#' o bloco II.6 a este: lá o one-hot foi escolhido por filtrar e não ordenar,
#' e o efeito colateral é que ele corta o espaço de decisão na proporção do
#' número de categorias. A terceira linha mede esse corte.
#'
#' @param consultores,projetos,alocacoes Dados que definem a escala.
#' @param meses_ativos Número de meses distintos com atividade.
#' @param fracao_candidatos Fração dos pares que o filtro de conteúdo deixa
#'   passar.
#' @return tibble com uma linha por formulação.
medir_porte_do_problema <- function(consultores, projetos, alocacoes, meses_ativos,
                                     fracao_candidatos = NULL) {
  n_consultores <- nrow(consultores)
  n_projetos <- nrow(projetos)

  # Meses ativos de cada projeto, para a formulação com tempo não contar
  # períodos em que o projeto nem existe.
  meses_por_projeto <- pmax(round(projetos$duracao_dias / (365.25 / 12)), 1)

  porte <- tibble::tribble(
    ~formulacao, ~variaveis, ~restricoes_capacidade, ~expressa_capacidade,
    "Par consultor e projeto",
    n_consultores * n_projetos, 0, FALSE,
    "Trio consultor, projeto e mês",
    n_consultores * sum(meses_por_projeto), n_consultores * meses_ativos, TRUE
  )

  if (is.null(fracao_candidatos)) {
    return(porte)
  }

  dplyr::bind_rows(
    porte,
    tibble::tibble(
      formulacao = "Trio, restrito ao conjunto candidato",
      variaveis = round(n_consultores * sum(meses_por_projeto) * fracao_candidatos),
      restricoes_capacidade = n_consultores * meses_ativos,
      expressa_capacidade = TRUE
    )
  )
}

#' Mede que fração dos pares o filtro de conteúdo deixa passar
#'
#' O filtro é o one-hot de setor decidido no bloco II.6, aplicado como o
#' híbrido chaveado do bloco II.3 manda: **só a quem declarou setor**. Quem
#' declarou passa nos projetos do seu setor; quem não declarou não é filtrado,
#' porque para essa pessoa o conteúdo não tem o que dizer e o chaveado a
#' encaminha a outro componente.
#'
#' Uma versão anterior deixava de fora do conjunto candidato todo consultor
#' sem setor, mais da metade do quadro, e media com isso uma redução de porte
#' de cerca de quinze vezes, metade da qual vinha da exclusão e não do
#' casamento de setor. Um otimizador montado sobre ela nunca alocaria quem não
#' respondeu o formulário. As três frações abaixo separam as duas origens.
#'
#' @param consultores,projetos Dados que definem a matriz.
#' @param experiencia_setorial tibble com o setor observado.
#' @return Lista com `fracao_candidatos`, a fração de pares que o filtro
#'   coerente com o chaveado deixa passar; `fracao_casamento`, a fração que
#'   casa entre quem declarou setor; `fracao_excluindo_sem_setor`, a da versão
#'   anterior; e `consultores_sem_setor`.
medir_reducao_pelo_filtro <- function(consultores, projetos, experiencia_setorial) {
  setor <- setor_predominante_observado(experiencia_setorial)
  setor_do_consultor <- setor$segmento[
    match(consultores$consultor_id, setor$consultor_id)
  ]
  com_setor <- !is.na(setor_do_consultor)

  # Pares em que o setor bate, calculados pelas marginais em vez da matriz
  # inteira: a soma, sobre setores, do produto das duas contagens.
  tabela_consultor <- table(setor_do_consultor[com_setor])
  tabela_projeto <- table(projetos$setor)
  comuns <- intersect(names(tabela_consultor), names(tabela_projeto))
  pares_que_casam <- sum(
    as.numeric(tabela_consultor[comuns]) * as.numeric(tabela_projeto[comuns])
  )

  pares_sem_filtro <- sum(!com_setor) * nrow(projetos)
  pares_totais <- nrow(consultores) * nrow(projetos)

  list(
    fracao_candidatos = (pares_que_casam + pares_sem_filtro) / pares_totais,
    fracao_casamento = if (any(com_setor)) {
      pares_que_casam / (sum(com_setor) * nrow(projetos))
    } else {
      NA_real_
    },
    fracao_excluindo_sem_setor = pares_que_casam / pares_totais,
    consultores_sem_setor = mean(!com_setor)
  )
}

#' Mede o quanto o histórico viola a restrição de capacidade
#'
#' A restrição que a wave 4 pretende impor é que ninguém passe de cem por
#' cento de ocupação. O histórico não respeita isso, e a pergunta que decide o
#' desenho do otimizador é **quanto** ele não respeita.
#'
#' A consequência é direta e desconfortável: um modelo que imponha a
#' capacidade como restrição dura declara infactível boa parte do que de fato
#' aconteceu. Ou a restrição entra como penalidade em vez de limite, ou a
#' medida de capacidade não significa o que o nome diz.
#'
#' @param ocupacao tibble com `consultor_id`, `ano_mes` e `pct_ocupacao`.
#' @param limite Ocupação máxima que a restrição imporia.
#' @return Lista com as frações e o excesso observado.
medir_violacao_de_capacidade <- function(ocupacao, limite = 1) {
  acima <- ocupacao$pct_ocupacao > limite

  list(
    limite = limite,
    fracao_meses_acima = mean(acima),
    consultores_com_violacao = dplyr::n_distinct(ocupacao$consultor_id[acima]) /
      dplyr::n_distinct(ocupacao$consultor_id),
    excesso_mediano = if (any(acima)) {
      stats::median(ocupacao$pct_ocupacao[acima]) - limite
    } else {
      0
    },
    ocupacao_maxima = max(ocupacao$pct_ocupacao)
  )
}

#' Registra quais insumos do otimizador existem e com que qualidade
#'
#' Tabela declarativa, no mesmo espírito de `avaliar_formas_de_hibrido()` e
#' `avaliar_estimadores_de_propensao()`: a decisão de o que a wave 4 pode
#' modelar precisa ser dado auditável, e não prosa que envelhece.
#'
#' A coluna `existe` diz se há fonte; `dimensionavel` diz se a escala dela é
#' confiável. A distinção é a mesma que o bloco II.5 precisou fazer entre
#' mecanismo e calibração, e pelo mesmo motivo: um objetivo pode ser
#' calculável e ainda assim não ter nível utilizável.
#'
#' @param margem_calibrada Se a escala da margem estrutural fecha com a
#'   âncora real.
#' @param capacidade_respeitada Se o histórico respeita a capacidade.
#' @return tibble com `insumo`, `papel`, `existe`, `dimensionavel` e
#'   `observacao`.
registrar_insumos_do_otimizador <- function(margem_calibrada = FALSE,
                                             capacidade_respeitada = FALSE) {
  tibble::tribble(
    ~insumo, ~papel, ~existe, ~dimensionavel, ~observacao,

    "Custo por hora", "objetivo e restrição orçamentária", TRUE, TRUE,
    "salário base por cargo, com âncora real; o fator de carga é declarado e o bloco II.5 mostrou que as âncoras não comportam um realista",

    "Margem", "objetivo econômico", TRUE, margem_calibrada,
    "calculável pela forma estrutural, mas o bloco II.5 mediu que as âncoras econômicas se sobredeterminam e discordam entre si (II.5.6)",

    "Afinidade prevista ŝ", "objetivo de afinidade", TRUE, FALSE,
    "o otimizador precisa de ŝ para cada par candidato, e é a wave 3 que o produz; o sinal de interação de `R` só existe para pares que aconteceram e serve para treinar e avaliar ŝ, não para substituí-lo. O que ordena dentro do filtro não ficou determinado pela EDA (III.4), e a escala de ŝ é suposição",

    "Capacidade mensal", "restrição dura", TRUE, capacidade_respeitada,
    "existe e é medida, mas o histórico a viola com frequência, o que a impede de entrar como limite sem tornar infactível o que aconteceu",

    "Férias", "restrição de disponibilidade", TRUE, TRUE,
    "uma janela por consultor, sem ambiguidade de escala",

    "Skills requeridas", "restrição de aderência", FALSE, FALSE,
    "não existe como processo na Peers, confirmado pelo dono do dado; o bloco II.6 decidiu usar categoria do projeto como filtro, que é aderência grossa",

    "Pirâmide de senioridade", "restrição de composição", FALSE, FALSE,
    "é regra de negócio conhecida e não está em tabela nenhuma; o achado I.5.3 registrou que o gerador também não a reproduz",

    "Demanda futura", "o que se aloca", FALSE, FALSE,
    "`pipeline_demandas` traz apenas identificador e estágio, sem data, tamanho nem perfil, então não alimenta o otimizador"
  )
}
