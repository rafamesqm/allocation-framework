#' Conjuntos públicos de referência para porte de fatoração latente
#'
#' Servem de régua. Sem eles, dizer que há "menos de uma observação por
#' parâmetro" não informa se isso é pouco ou muito, porque o leitor não tem
#' com o que comparar. Os três abaixo são os conjuntos sobre os quais a
#' literatura de fatoração latente foi construída e validada, e seus números
#' são públicos e estáveis.
#'
#' O Netflix Prize é a referência direta de Koren, Bell e Volinsky (2009),
#' que é a âncora deste bloco.
#'
#' @return tibble com `conjunto`, `interacoes`, `usuarios` e `itens`.
referencias_identificabilidade <- function() {
  tibble::tribble(
    ~conjunto, ~interacoes, ~usuarios, ~itens,
    "Netflix Prize", 100480507, 480189, 17770,
    "MovieLens 1M", 1000209, 6040, 3706,
    "MovieLens 100K", 100000, 943, 1682
  )
}

#' Mede a identificabilidade de uma fatoração latente por número de fatores
#'
#' Fatoração latente de posto `k` atribui um vetor de `k` números a cada
#' usuário e a cada item, o que dá `(n_usuarios + n_itens) * k` parâmetros
#' livres para explicar as interações observadas. A razão entre observações e
#' parâmetros é o diagnóstico global, e vale a mesma intuição de qualquer
#' modelo: abaixo de 1, há mais coisa a estimar do que evidência para
#' estimar.
#'
#' Mais decisivo que a razão global, porém, é o corte por entidade. O vetor
#' de um usuário é estimado a partir das interações **dele**, e um usuário
#' com menos de `k` interações tem vetor que os dados não determinam. A
#' regularização torna a estimativa única, mas não cria informação: o que ela
#' faz é encolher o vetor na direção do valor a priori, e nesse limite a
#' predição tende aos termos de viés do modelo, que são essencialmente
#' popularidade. Essa é a ponte com a linha de base do bloco II.1.
#'
#' Por isso a função reporta, para cada `k`, a fração de cada lado cuja
#' contagem de interações fica abaixo do número de fatores.
#'
#' @param alocacoes tibble com `consultor_id` e `projeto_id`.
#' @param consultores,projetos tibbles que definem os dois lados.
#' @param fatores Vetor de valores de `k` a avaliar.
#' @return tibble com `fatores`, `parametros`, `observacoes`,
#'   `obs_por_parametro`, `fracao_consultores_subdeterminados` e
#'   `fracao_projetos_subdeterminados`.
medir_identificabilidade <- function(alocacoes, consultores, projetos,
                                      fatores = c(1, 2, 3, 5, 10, 20)) {
  n_observacoes <- nrow(alocacoes)
  n_consultores <- nrow(consultores)
  n_projetos <- nrow(projetos)

  # Entidade sem nenhuma interação conta como subdeterminada para qualquer
  # k >= 1: é o cold start, e ignorá-lo no denominador esconderia justamente
  # o caso mais grave.
  grau_consultor <- as.integer(table(
    factor(alocacoes$consultor_id, levels = consultores$consultor_id)
  ))
  grau_projeto <- as.integer(table(
    factor(alocacoes$projeto_id, levels = projetos$projeto_id)
  ))

  purrr::map(fatores, function(k) {
    n_parametros <- (n_consultores + n_projetos) * k

    tibble::tibble(
      fatores = k,
      parametros = n_parametros,
      observacoes = n_observacoes,
      obs_por_parametro = n_observacoes / n_parametros,
      fracao_consultores_subdeterminados = mean(grau_consultor < k),
      fracao_projetos_subdeterminados = mean(grau_projeto < k)
    )
  }) |>
    purrr::list_rbind()
}

#' Compara o porte do problema com o dos conjuntos públicos de referência
#'
#' Coloca lado a lado a razão de observações por parâmetro deste projeto e a
#' dos conjuntos sobre os quais a literatura de fatoração foi construída,
#' para o mesmo número de fatores. É essa comparação que transforma um número
#' isolado em argumento.
#'
#' @param alocacoes,consultores,projetos Dados deste projeto.
#' @param fatores Número de fatores usado na comparação.
#' @param referencias tibble de `referencias_identificabilidade()`.
#' @param incluir_real Se `TRUE`, acrescenta a escala real da Peers medida no
#'   Unity Catalog, de `escala_real_alocacao()`.
#' @return tibble com `conjunto`, `interacoes`, `usuarios`, `itens`,
#'   `parametros` e `obs_por_parametro`, incluindo este projeto na primeira
#'   linha.
comparar_porte_com_referencias <- function(alocacoes, consultores, projetos,
                                            fatores = 10,
                                            referencias = referencias_identificabilidade(),
                                            incluir_real = TRUE) {
  este_projeto <- tibble::tibble(
    conjunto = "Alocação de consultores (sintético)",
    interacoes = nrow(alocacoes),
    usuarios = nrow(consultores),
    itens = nrow(projetos)
  )

  linhas <- if (incluir_real) {
    dplyr::bind_rows(este_projeto, escala_real_alocacao(), referencias)
  } else {
    dplyr::bind_rows(este_projeto, referencias)
  }

  linhas |>
    dplyr::mutate(
      fatores = fatores,
      parametros = (usuarios + itens) * fatores,
      obs_por_parametro = interacoes / parametros
    )
}

#' Escala real da matriz de alocação da Peers, medida no Unity Catalog
#'
#' Muda o estatuto da conclusão do bloco II.2. Sem esta linha, a razão de
#' observações por parâmetro é calculada sobre o sintético, e a afirmação de
#' que ela vale para o real depende de o gerador ter acertado a escala. Com
#' ela, a razão é calculada direto sobre agregados reais documentados, e a
#' conclusão sobre identificabilidade passa a não depender do gerador em
#' nada.
#'
#' Os três números estão em `docs/mapeamento_schema.md` (Primeira rodada,
#' `vw_gtac_cltv_alocacao`): 4.598 pares distintos entre 511 colaboradores e
#' 1.833 códigos de alocação. Note que 511 é o histórico completo, com
#' desligados, e não os 285 ativos, e que o código de alocação não é a mesma
#' coisa que projeto; é a granularidade que o dado real oferece.
#'
#' **A ressalva da chave está resolvida, e a favor.** Este bloco registrava
#' que o join supunha `ID` em entidades e `Cód. Colaborador` em alocação
#' serem a mesma chave, sem confirmação. A segunda consulta comercial
#' confirmou que são: `ID` é `cod_id_funcionario`, e o nome completo coincide
#' em 98,1% dos códigos casados. Duas notas ficam. A cobertura sobe para
#' cerca de 98% usando `gold.apdata_entidades_globais` em vez da view que
#' perde cerca de cem códigos. Na base silver, 79 códigos, ou 310 linhas de
#' alocação (6%), não casam; só 9 não casam em nenhuma tabela testada.
#' As chaves Starsoft **não** servem para este join, com 18% de coincidência
#' de nome, que são colisões.
#'
#' **A ressalva que resta é outra, e é de extração.** A tabela de alocação
#' tem 4.999 linhas, e a mesma consulta mostrou que várias tabelas do fluxo
#' param em números redondos por teto de extração. Os 4.598 pares podem vir
#' de uma base truncada. Para o uso que se faz deles aqui, comparar ordem de
#' grandeza contra conjuntos públicos, isso não morde: seria preciso errar
#' por mais de uma ordem de grandeza para a conclusão mudar, e o truncamento
#' encurta a base, o que só reforçaria a conclusão de esparsidade.
#'
#' @return tibble de uma linha, no mesmo formato de
#'   `referencias_identificabilidade()`.
escala_real_alocacao <- function() {
  tibble::tibble(
    conjunto = "Alocação de consultores (real)",
    interacoes = 4598,
    usuarios = 511,
    itens = 1833
  )
}
