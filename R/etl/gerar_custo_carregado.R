#' Bandas de custo por hora, em salário base, de cada nível de senioridade
#'
#' Fonte: `silver.bi_tbl_pps_alocacao_custos`, medianas de 2025 por sigla de
#' cargo, registradas em docs/mapeamento_schema.md (segunda consulta às bases
#' comerciais). **O dono do dado confirmou que é por hora e que não inclui
#' encargos: é salário base.**
#'
#' A versão anterior usava `vw_gtac_fin_taxa_hora`, que o bloco II.5
#' identificou e a consulta confirmou ser **rate card**, isto é, tabela de
#' preço de venda. Usar preço como custo fazia a margem estrutural ficar em
#' torno de zero por construção, com 32,8% dos projetos em prejuízo contra os
#' 2% reais.
#'
#' O mapeamento de nível para banda de siglas é decisão de projeto, não
#' leitura direta: a fonte não documenta qual sigla corresponde a qual nível
#' N4 a N0. A banda segue a progressão plausível de carreira em consultoria,
#' e é a mesma partição que a versão anterior usava, para a troca de preço
#' por custo ser a única mudança.
#'
#' @return Lista nomeada por nível, com os custos por hora de cada banda.
bandas_de_custo_por_nivel <- function() {
  list(
    N4 = c(INT = 70, TR = 119, AN = 163, SAN = 183),
    N3 = c(CT = 218, SCT = 248),
    N2 = c(TL = 271),
    N1 = c(MG = 327, EMG = 334, SMG = 405),
    N0 = c(PRI = 474, MD1 = 745, MD4 = 1093)
  )
}

#' Gera custo carregado por consultor (parte de `c`, Camada 3)
#'
#' `custo_carregado` é o que o otimizador da wave 4 precisa: quanto a pessoa
#' custa à empresa por hora. Ele é construído como **salário base vezes fator
#' de carga**, porque é assim que as duas peças existem: a base tem âncora
#' real por cargo, e o fator não existe em base nenhuma consultada.
#'
#' **Por que o fator default é 1, e o que isso significa.** Encargos,
#' benefícios, décimo terceiro, férias e FGTS levariam o fator para a faixa de
#' 1,7 a 2,0 no Brasil. As âncoras reais não comportam isso, e a aritmética é
#' simples de conferir (ver `medir_fator_de_carga_implicito()`): a receita é
#' cerca de 0,87 vezes horas vezes preço, o custo base é cerca de 0,554 do
#' preço, e a despesa total do PP é 68% da receita, porque a margem líquida
#' mediana é 0,320. Isso põe o teto do fator em **1,068**: acima disso o custo
#' de pessoal sozinho excederia a despesa total declarada no PP.
#'
#' A leitura mais provável é que a margem que o PP declara seja **bruta sobre
#' salário direto**, e não líquida de verdade. O gerador não decide isso: usa
#' fator 1, o que mantém a coerência com a âncora de margem, e expõe o
#' parâmetro para o dia em que houver número. A consequência para a
#' monografia fica declarada: otimizar essa margem é otimizar uma medida
#' bruta, não lucro.
#'
#' @param consultores tibble de `gerar_consultores()`.
#' @param fator_carga Multiplicador sobre o salário base. Ver detalhe acima.
#' @param semente Inteiro para reprodutibilidade.
#' @return tibble com `consultor_id`, `custo_hora_base` e `custo_carregado`,
#'   ambos em R$/h.
gerar_custo_carregado <- function(consultores, fator_carga = 1, semente = 1) {
  set.seed(semente)

  bandas <- bandas_de_custo_por_nivel()

  # Indexar por `sample.int()` em vez de `sample()` direto na banda: com
  # banda de um valor só, `sample(271, 1)` sorteia de 1 a 271 em vez de
  # devolver 271. O nível N2 tem banda unitária e saía com média 136 em vez
  # de 271, o que o teste de monotonicidade por nível pegou.
  base_por_nivel <- vapply(as.character(consultores$nivel_senioridade), function(nivel) {
    banda <- unname(bandas[[nivel]])
    banda[sample.int(length(banda), size = 1)]
  }, numeric(1))

  base_por_nivel <- unname(base_por_nivel)

  tibble::tibble(
    consultor_id = consultores$consultor_id,
    custo_hora_base = base_por_nivel,
    custo_carregado = base_por_nivel * fator_carga
  )
}
