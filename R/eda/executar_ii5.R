#' Executa o bloco II.5 da EDA, construção do rótulo
#'
#' **Âncora na literatura.** Hu, Koren e Volinsky (2008) formalizam o
#' colaborativo sobre feedback implícito, que é o caminho de reserva quando
#' não há rótulo explícito de qualidade. A distinção entre rótulo explícito e
#' implícito é o que este bloco precisa decidir antes da wave 3.
#'
#' **Propriedade medida.** Cobertura do rótulo explícito, divergência entre
#' as etapas de avaliação, dimensionalidade dos itens, decomposição da
#' variância do escore, e se a margem pode ser atribuída a uma alocação.
#'
#' **Decisão que discrimina.** Qual dos quatro candidatos de rótulo de
#' `docs/dicionario_de_dados.md` se sustenta.
#'
#' **Este bloco motivou uma correção no gerador, e a correção veio antes das
#' conclusões.** A primeira execução mediu alfa de Cronbach negativo,
#' primeiro componente em 8,5% e correlação média entre itens de -0,002: os
#' 14 itens de cada avaliação eram sorteados de forma independente, e o
#' rótulo não tinha sinal nenhum sobre a pessoa. Concluir sobre construção de
#' rótulo em cima disso seria vacuoso, e pior, concluiria que a média dos
#' itens não é escala, o que é artefato e não propriedade do domínio. O
#' gerador passou a derivar as notas de uma qualidade latente compartilhada
#' (ver `construir_qualidade_latente()`), preservando as marginais
#' calibradas. As medidas abaixo são da versão corrigida.
#'
#' @param dados Lista nomeada de tibbles, de `carregar_dados()`.
#' @param estabilidade tibble de `resumir_estabilidade()`, opcional.
#' @param etapa_rotulo Etapa adotada como rótulo nas medidas que exigem uma.
#' @return Lista com `objetos`, `tabelas`, `figuras` e `achados`.
executar_ii5 <- function(dados, estabilidade = NULL, etapa_rotulo = "gestor_final") {
  cobertura <- medir_cobertura_do_rotulo(dados)
  divergencia <- medir_divergencia_entre_avaliadores(dados$avaliacoes_desempenho)
  dimensionalidade <- medir_dimensionalidade_dos_itens(
    dados$avaliacoes_desempenho, etapa = etapa_rotulo
  )
  variancia_escore <- decompor_variancia_do_escore(
    dados$avaliacoes_desempenho, etapa = etapa_rotulo
  )
  margem <- medir_atribuibilidade_da_margem(dados)
  construcoes <- comparar_construcoes_de_rotulo(dados$avaliacoes_desempenho)

  candidatos <- avaliar_candidatos_de_rotulo(
    margem_atribuivel = margem$tem_consultor_na_margem,
    fracao_projetos_com_equipe = margem$fracao_projetos_com_equipe,
    custo_zero_frente_mes = margem$custo_zero_frente_mes,
    custo_zero_explicavel_estrutural = margem$custo_zero_estrutural_sem_alocacao,
    alfa_cronbach = dimensionalidade$alfa_ordinal,
    fracao_margem_negativa = margem$fracao_margem_estrutural_negativa
  )
  candidatos_viaveis <- dplyr::filter(candidatos, viavel)

  par_mais_divergente <- divergencia$pares[1, ]
  posicao_variancia <- posicao_no_intervalo(
    estabilidade, "fracao_variancia_entre_consultores",
    variancia_escore$fracao_entre_consultores
  )
  etapa_mais_lenient <- divergencia$por_etapa[1, ]
  etapa_mais_severa <- divergencia$por_etapa[nrow(divergencia$por_etapa), ]

  por_nota <- dados$avaliacoes_desempenho |>
    dplyr::count(etapa, nota) |>
    dplyr::mutate(fracao = n / sum(n), .by = etapa)

  objetos <- list(
    cobertura = cobertura,
    divergencia = divergencia,
    dimensionalidade = dimensionalidade,
    variancia_escore = variancia_escore,
    margem = margem,
    construcoes = construcoes,
    bruta = construcoes[construcoes$forma == "Nota bruta", ],
    normalizada = construcoes[construcoes$forma == "Normalizada por avaliador", ],
    candidatos = candidatos,
    candidatos_viaveis = candidatos_viaveis,
    par_mais_divergente = par_mais_divergente,
    etapa_mais_lenient = etapa_mais_lenient,
    etapa_mais_severa = etapa_mais_severa,
    por_nota = por_nota,
    etapa_rotulo = etapa_rotulo,
    posicao_variancia = posicao_variancia,
    estabilidade = estabilidade
  )

  tabelas <- list(
    ii5_cobertura_rotulo = tibble::tibble(
      `Denominador` = c("Alocações", "Consultores", "Projetos"),
      `Com avaliação` = formatar_inteiro(c(
        cobertura$pares_avaliados, cobertura$consultores_avaliados,
        cobertura$projetos_avaliados
      )),
      Total = formatar_inteiro(c(
        cobertura$pares_totais, nrow(dados$consultores), nrow(dados$projetos)
      )),
      Cobertura = formatar_pct(c(
        cobertura$cobertura_pares, cobertura$cobertura_consultores,
        cobertura$cobertura_projetos
      ))
    ),
    ii5_divergencia_etapas = divergencia$por_etapa |>
      dplyr::transmute(
        Etapa = etapa,
        `Nível médio` = formatar_decimal(nivel_medio, 2),
        `Fração ACIMA` = formatar_pct(fracao_acima),
        `Fração ABAIXO` = formatar_pct(fracao_abaixo)
      ),
    ii5_concordancia_pares = divergencia$pares |>
      dplyr::transmute(
        `Etapa A` = etapa_a, `Etapa B` = etapa_b,
        `Concordância` = formatar_pct(concordancia),
        `Correlação` = formatar_decimal(correlacao, 3)
      ),
    ii5_dimensionalidade = tibble::tibble(
      Medida = c(
        "Itens avaliados",
        "Avaliações completas",
        "Variância do primeiro componente",
        "Variância dos dois primeiros",
        "Se os itens fossem independentes",
        "Correlação média entre itens (Pearson)",
        "Correlação média entre itens (policórica)",
        "Alfa de Cronbach (bruto)",
        "Alfa ordinal (policórico)",
        "Fração da variância entre consultores"
      ),
      Valor = c(
        formatar_inteiro(dimensionalidade$n_itens),
        formatar_inteiro(dimensionalidade$n_avaliacoes),
        formatar_pct(dimensionalidade$variancia_primeiro_componente),
        formatar_pct(dimensionalidade$variancia_dois_primeiros),
        formatar_pct(1 / dimensionalidade$n_itens),
        formatar_decimal(dimensionalidade$correlacao_media_itens, 3),
        formatar_decimal(dimensionalidade$correlacao_media_policorica, 3),
        formatar_decimal(dimensionalidade$alfa_cronbach, 3),
        formatar_decimal(dimensionalidade$alfa_ordinal, 3),
        formatar_pct(variancia_escore$fracao_entre_consultores)
      )
    ),
    ii5_construcoes_rotulo = construcoes |>
      dplyr::transmute(
        `Forma do rótulo` = forma,
        `Variância explicada pelo avaliador` = formatar_pct(contaminacao_avaliador),
        `Sinal entre consultores` = formatar_pct(sinal_entre_consultores)
      ),
    ii5_candidatos_rotulo = candidatos |>
      dplyr::transmute(
        Candidato = candidato, `Construção` = descricao, Exige = exige,
        `Mecanismo` = ifelse(viavel, "sim", "não"),
        `Escala calibrada` = ifelse(calibrado, "sim", "não"),
        Justificativa = justificativa
      )
  )

  figuras <- list(
    ii5_divergencia_etapas = figura_divergencia_etapas(
      por_nota,
      titulo = "Distribuição da nota em cada etapa de avaliação",
      subtitulo = "As três etapas avaliam a mesma pessoa no mesmo projeto sobre os mesmos itens"
    ),
    ii5_dimensionalidade_itens = figura_dimensionalidade_itens(
      dimensionalidade$variancia_componentes,
      titulo = "Estrutura comum entre os itens da avaliação",
      subtitulo = paste0(
        "Tracejado: o que cada componente explicaria se os ",
        dimensionalidade$n_itens, " itens fossem independentes entre si"
      )
    )
  )

  achados <- registrar_achados("II.5", tibble::tribble(
    ~codigo, ~etiqueta, ~enunciado, ~implicacao,

    "II.5.1", "instrumento",
    paste0(
      "Dizer que o rótulo é a avaliação de desempenho é ambíguo, porque as ",
      "etapas divergem. A autoavaliação põe ",
      formatar_pct(etapa_mais_lenient$fracao_acima),
      " das notas em ACIMA, contra ",
      formatar_pct(etapa_mais_severa$fracao_acima),
      " da etapa mais severa. O par mais divergente, ",
      par_mais_divergente$etapa_a, " e ", par_mais_divergente$etapa_b,
      ", concorda em ", formatar_pct(par_mais_divergente$concordancia),
      " dos itens, com correlação de ",
      formatar_decimal(par_mais_divergente$correlacao, 2), "."
    ),
    paste0(
      "**A etapa adotada é parte da definição do rótulo, não detalhe de ",
      "implementação.** Adotar a autoavaliação daria um rótulo em que quase ",
      "tudo é ACIMA, com pouco a discriminar. O achado II.5.7 mede o efeito ",
      "de normalizar, em vez de deduzi-lo daqui."
    ),

    "II.5.2", "instrumento",
    paste0(
      "Os ", dimensionalidade$n_itens, " itens medem uma coisa só, e a média ",
      "deles é escala confiável: primeiro componente em ",
      formatar_pct(dimensionalidade$variancia_primeiro_componente),
      " contra ", formatar_pct(1 / dimensionalidade$n_itens),
      " que itens independentes dariam. A correlação média entre itens é ",
      formatar_decimal(dimensionalidade$correlacao_media_itens, 2),
      " por Pearson e ",
      formatar_decimal(dimensionalidade$correlacao_media_policorica, 2),
      " por policórica, e o alfa vai de ",
      formatar_decimal(dimensionalidade$alfa_cronbach, 2), " bruto a ",
      formatar_decimal(dimensionalidade$alfa_ordinal, 2), " ordinal."
    ),
    paste0(
      "Autoriza o rótulo escalar: colapsar os itens numa média não destrói ",
      "estrutura, porque a estrutura é comum. A distância entre as duas ",
      "versões de cada medida é o preço da discretização: tratar nota de ",
      "três níveis como 1, 2 e 3 supõe espaçamento igual e atenua a ",
      "correlação, e a policórica troca essa suposição pela de normal ",
      "latente. As duas ficam reportadas porque nenhuma é neutra. **Este ",
      "bloco mediu alfa ",
      "negativo na primeira versão**, e a constatação motivou a correção do ",
      "gerador, que sorteava cada item de forma independente e produzia um ",
      "rótulo sem sinal nenhum sobre a pessoa. O efeito halo é dos fenômenos ",
      "mais bem documentados na literatura de avaliação, e um gerador que não ",
      "o reproduz torna a wave 3 impossível de demonstrar."
    ),

    "II.5.3", "âncora real",
    paste0(
      "A margem não é atribuível a uma alocação individual. ",
      "`margem_frente_mes` não tem `consultor_id`, o usuário confirmou que ",
      "frente e mês é a única granularidade que existe na Peers, e ",
      formatar_pct(margem$fracao_projetos_com_equipe),
      " dos projetos alocados têm mais de uma pessoa, com mediana de ",
      margem$mediana_equipe, " e máximo de ", margem$maior_equipe, "."
    ),
    paste0(
      "**Mata o candidato (b)**, que multiplicaria desempenho por margem na ",
      "célula de `R`. Ratear a margem de uma frente entre os membros do time ",
      "seria estimativa apresentada como captura, no mesmo espírito da ",
      "decisão já registrada sobre `skills_requeridas[]`."
    ),

    "II.5.4", "âncora real",
    paste0(
      "As duas fontes de margem têm custo zero por motivos opostos. Em ",
      "`margem_frente_mes`, ", formatar_pct(margem$custo_zero_frente_mes),
      " das linhas não têm custo apontado, e ",
      formatar_pct(margem$fracao_margem_relativa_integral),
      " das linhas com receita têm margem igual à receita. Em ",
      "`margem_estrutural`, o custo zero coincide ",
      if (margem$custo_zero_estrutural_sem_alocacao) "exatamente" else "apenas em parte",
      " com projeto sem nenhuma alocação."
    ),
    paste0(
      "**Separa o candidato (c) do (d).** O custo zero de frente e mês é ",
      "âncora real, calibrado contra os 44,7% de `peers_custointerns_vlr`, e ",
      "é incompletude de apontamento: margem igual à receita não pondera ",
      "nada. O custo zero da margem estrutural é explicável e inofensivo, ",
      "porque projeto sem alocação não é célula de `R`. A margem estrutural ",
      "não depende do apontamento, que é a fonte do problema."
    ),

    "II.5.8", "instrumento",
    paste0(
      "Conferida a plausibilidade dos **valores**, e não só a estrutura dos ",
      "zeros, a margem estrutural não fecha: ",
      formatar_pct(margem$fracao_margem_estrutural_negativa),
      " dos projetos com contrato positivo saem com margem negativa, e no ",
      "quinto percentil o prejuízo chega a ",
      formatar_decimal(abs(margem$margem_estrutural_relativa_p05), 1),
      " vezes o valor do contrato. A mediana, ",
      formatar_pct(margem$margem_estrutural_relativa_mediana),
      ", é plausível; a cauda não."
    ),
    paste0(
      "**É o primeiro caso no capítulo de grandezas individualmente ",
      "calibradas que não fecham em conjunto.** `valor_contrato`, ",
      "`horas_mes` e `custo_carregado` têm cada um sua âncora real, e a ",
      "combinação dos três produz margem implausível. A hipótese mais ",
      "provável é de granularidade: a fonte de `valor_contrato` é a tabela ",
      "`actarnet_alocacao_pessoas`, cujo nome e volume sugerem grão de ",
      "alocação e pessoa, enquanto o dicionário a trata como atributo de ",
      "projeto. Se for isso, o bloco está comparando valor de uma pessoa ",
      "contra custo do time inteiro. **Não é verificável**, porque o acesso ",
      "encerrou, e fica como limitação nomeada. A consequência prática: o ",
      "candidato (d) é a forma certa de calcular margem, mas o sintético não ",
      "calibra a escala dela, e a wave 4 não deve usar o nível da margem ",
      "sintética para nada além de testar a mecânica do otimizador."
    ),

    "II.5.5", "instrumento",
    paste0(
      "Dos quatro candidatos de rótulo do dicionário, ",
      nrow(candidatos_viaveis), " têm mecanismo que se sustenta: ",
      paste(candidatos_viaveis$candidato, collapse = " e "),
      ". Nenhum dos dois tem escala calibrada no sintético: a de (a) porque ",
      "a base de avaliação nunca teve acesso liberado, a de (d) pelo motivo ",
      "do achado II.5.8."
    ),
    paste0(
      "**A decisão habilitada é o candidato (a) para `R` e o candidato (d) ",
      "para `f`**: a célula de `R` é o desempenho, normalizado por avaliador ",
      "e com a etapa declarada; a margem sai de `R` e entra como objetivo ",
      "separado no otimizador, calculada estruturalmente de custo carregado ",
      "por horas contra valor de contrato. É a combinação que o dicionário já ",
      "suspeitava ao registrar que a granularidade da margem pesava a favor ",
      "de (c) ou (d), agora com o (c) também descartado, por motivo que o ",
      "dicionário não previa."
    ),

    "II.5.7", "instrumento",
    paste0(
      "Normalizar por avaliador não é refinamento. Sob o cenário em que cada ",
      "alocação é avaliada por um único avaliador, que é o regime real, ",
      formatar_pct(objetos$bruta$contaminacao_avaliador),
      " da variância da nota bruta é explicada por **quem** avaliou, e não ",
      "por quem foi avaliado. Padronizando dentro de cada avaliador, a ",
      "contaminação vai a zero e o sinal entre consultores **sobe** de ",
      formatar_pct(objetos$bruta$sinal_entre_consultores), " para ",
      formatar_pct(objetos$normalizada$sinal_entre_consultores), "."
    ),
    paste0(
      "Fecha o candidato (a) de verdade, em vez de repetir a palavra ",
      "`normalizado` que o dicionário usa. A normalização remove variação ",
      "espúria e **ganha** sinal, então não há troca a ponderar. A forma ",
      "usada é padronização dentro do avaliador, que tira o nível e preserva ",
      "a ordem; formas mais elaboradas exigiriam `avaliador_id`, registrado ",
      "no dicionário como disponibilidade parcial e ausente do sintético. O ",
      "cenário de avaliador único é declarado e serve para demonstrar a ",
      "necessidade, não para estimar a magnitude do problema na Peers."
    ),

    "II.5.6", "consequência do gerador",
    paste0(
      "A cobertura do rótulo é de ", formatar_pct(cobertura$cobertura_pares),
      " das alocações, e ", formatar_pct(variancia_escore$fracao_entre_consultores),
      " da variância do escore é entre consultores",
      if (is.null(estabilidade)) {
        "."
      } else {
        paste0(
          ", com ",
          formatar_intervalo(estabilidade, "fracao_variancia_entre_consultores"),
          " sobre as realizações",
          if (identical(posicao_variancia, "dentro")) {
            "."
          } else {
            paste0(
              ". O ponto versionado cai **", posicao_variancia,
              "** do intervalo, e é o intervalo que deve ser citado."
            )
          }
        )
      }
    ),
    paste0(
      "**Nenhum número deste bloco fala sobre a Peers.** A base de avaliação ",
      "de desempenho é o dado mais sensível da empresa e o acesso nunca foi ",
      "liberado: o esquema foi reconstruído de tela, e distribuição, ",
      "cobertura e estrutura de dependência são todas suposição declarada. A ",
      "cobertura é parâmetro do gerador. O que transfere é a bateria de ",
      "diagnósticos, pronta para rodar se o acesso vier. E fica o acoplamento ",
      "que o dicionário nomeou: com a margem fora de `R`, o rótulo depende ",
      "inteiramente de desempenho, e sem esse acesso a alternativa é feedback ",
      "implícito (Hu, Koren e Volinsky, 2008), que o bloco II.4 mostrou ",
      "carregar viés de seleção não corrigível."
    )
  ))

  list(objetos = objetos, tabelas = tabelas, figuras = figuras, achados = achados)
}
