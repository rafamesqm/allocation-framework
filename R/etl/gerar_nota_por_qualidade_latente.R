#' Constrói a qualidade latente por trás de uma avaliação de desempenho
#'
#' Existe porque a versão anterior de `gerar_avaliacoes_desempenho()`
#' sorteava **cada item de cada etapa de forma independente**, o que o bloco
#' II.5 mediu e recusou: o primeiro componente principal explicava 8,5% da
#' variância entre os 14 itens, a correlação média entre eles era -0,002 e o
#' alfa de Cronbach dava negativo. Um rótulo assim não tem sinal nenhum sobre
#' a pessoa, e um modelo colaborativo treinado sobre ele não teria o que
#' aprender, por melhor que fosse.
#'
#' Nada disso é plausível. O efeito halo, isto é, a correlação alta entre
#' itens de uma mesma avaliação, é um dos fenômenos mais bem documentados da
#' literatura de avaliação de desempenho. E dois avaliadores da mesma pessoa
#' no mesmo projeto observam a mesma realidade, então precisam compartilhar
#' variância mesmo quando discordam.
#'
#' **O que é estrutura reproduzida e o que é suposição declarada.** A
#' estrutura é a decomposição: existe qualidade que persiste na pessoa,
#' qualidade específica daquela alocação, dificuldade própria de cada item e
#' ruído de cada avaliador. Isso é certo, e é o que faz o rótulo ter sinal.
#' As magnitudes de cada componente são suposição: a base de avaliação de
#' desempenho nunca teve acesso liberado (é o dado mais sensível da empresa),
#' então não há variância real para calibrar. Os valores default abaixo foram
#' escolhidos para produzir correlação entre itens na faixa alta que a
#' literatura de halo reporta, e concordância entre autoavaliação e gestor na
#' faixa baixa que as meta-análises de *self-other rating agreement*
#' reportam, que é da ordem de 0,2 a 0,35.
#'
#' **O que fica preservado.** As marginais de nota por etapa continuam sendo
#' as calibradas: os cortes ordinais são feitos por quantil do latente, o que
#' garante que a distribuição de ABAIXO/DENTRO/ACIMA saia exatamente nos
#' pesos pedidos. A correção acrescenta estrutura de dependência sem mexer em
#' nenhum número que tinha âncora.
#'
#' @param consultor_id,alocacao_id Vetores que identificam a pessoa e a
#'   alocação de cada linha.
#' @param item Vetor com o item avaliado em cada linha.
#' @param sd_consultor Desvio da qualidade que persiste na pessoa entre
#'   projetos.
#' @param sd_alocacao Desvio da qualidade específica daquela alocação.
#' @param sd_item Desvio da dificuldade própria de cada item.
#' @return Vetor numérico com a qualidade latente de cada linha, antes do
#'   ruído do avaliador.
construir_qualidade_latente <- function(consultor_id, alocacao_id, item,
                                         sd_consultor = 0.5,
                                         sd_alocacao = 0.6,
                                         sd_item = 0.25) {
  efeito_consultor <- sortear_efeito_por_grupo(consultor_id, sd_consultor)
  efeito_alocacao <- sortear_efeito_por_grupo(alocacao_id, sd_alocacao)
  efeito_item <- sortear_efeito_por_grupo(item, sd_item)

  efeito_consultor + efeito_alocacao + efeito_item
}

#' Sorteia um efeito aleatório por nível de um agrupamento
#'
#' Um sorteio por nível distinto, replicado nas linhas daquele nível. É o que
#' faz o efeito ser compartilhado dentro do grupo, que é a propriedade toda:
#' sortear por linha devolveria ruído independente e nenhuma estrutura.
#'
#' @param grupo Vetor de rótulos do agrupamento.
#' @param desvio Desvio padrão do efeito.
#' @return Vetor numérico do mesmo comprimento de `grupo`.
sortear_efeito_por_grupo <- function(grupo, desvio) {
  niveis <- unique(grupo)
  efeito <- stats::rnorm(length(niveis), mean = 0, sd = desvio)
  names(efeito) <- niveis

  unname(efeito[match(grupo, niveis)])
}

#' Converte qualidade latente em nota ordinal, respeitando marginais dadas
#'
#' O corte é por quantil do próprio latente, e não por limiar fixo. A
#' diferença importa: limiar fixo faria a distribuição de notas depender da
#' variância do latente, e mudar qualquer desvio da decomposição mexeria nas
#' marginais calibradas sem avisar. Por quantil, as marginais são exatamente
#' as pedidas, quaisquer que sejam os desvios, e a decomposição fica livre
#' para ser ajustada sem quebrar a calibração.
#'
#' @param latente Vetor numérico de qualidade latente já com ruído do
#'   avaliador.
#' @param pesos Vetor nomeado com a proporção de cada nível, na ordem
#'   ordinal.
#' @return Vetor de texto com a nota de cada linha.
cortar_latente_em_nota <- function(latente, pesos) {
  cortes <- stats::quantile(latente, probs = cumsum(pesos[-length(pesos)]))
  indice <- findInterval(latente, cortes) + 1

  names(pesos)[indice]
}

#' Sorteia o gestor avaliador de cada alocação
#'
#' A consulta ao dono do dado de RH confirmou que a avaliação é por projeto e
#' que cada uma tem gestor identificado, então o avaliador é um eixo do dado
#' e não uma convenção do gerador. Gestores diferentes avaliam alocações
#' diferentes do mesmo consultor, e é isso que faz a severidade de cada um
#' entrar como variação espúria na comparação entre pessoas.
#'
#' O tamanho do corpo de gestores sai de uma fração do **quadro**, e não do
#' número de alocações. A distinção não é sutil: dimensionar por alocação
#' produziria mais gestores do que pessoas na empresa, cada um avaliando uma
#' ou duas vezes, e a severidade individual viraria ruído em vez de um
#' deslocamento estimável. Quem é gestor avaliador não está em nenhuma tabela
#' do crosswalk, então a fração é suposição declarada.
#'
#' @param alocacao_id Vetor de identificadores de alocação, sem repetição.
#' @param fracao_gestores Fração do quadro que atua como avaliador.
#' @param n_consultores Tamanho do quadro, base para dimensionar o corpo.
#' @return tibble com `alocacao_id` e `gestor`.
sortear_gestores_avaliadores <- function(alocacao_id, fracao_gestores, n_consultores) {
  n_gestores <- max(2, round(n_consultores * fracao_gestores))
  identificadores <- sprintf("gestor_%04d", seq_len(n_gestores))

  tibble::tibble(
    alocacao_id = alocacao_id,
    gestor = sample(identificadores, length(alocacao_id), replace = TRUE)
  )
}
