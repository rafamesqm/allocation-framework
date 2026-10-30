#' Achados da wave 4, no formato de achados_w3.csv
#'
#' Cada bloco declara seus achados pelo `registrar_achados()` da EDA, com os
#' números lidos dos alvos, e o campo `natureza` separa o confirmatório (o
#' registro e a emenda 1, inclusive as nove sementes que a emenda 2 manteve
#' no orçamento registrado) do exploratório (as emendas 2 e 3).
#'
#' @param montagem Lista com `verificacao` e `conferencia`.
#' @param exato Lista com `payoff`, `ideal` e `realizada`.
#' @param nsga2 Lista com `principal`, `variabilidade` e `trajetoria`
#'   (exploratória).
#' @param avaliacao Lista com `avaliacao`, `exatos` e `concentracao`.
#' @param limite Lista com `descricao` e `valor`.
#' @param estabilidade tibble de `resumir_estabilidade_da_otimizacao()`.
#' @return tibble com os achados consolidados e a coluna `natureza`.
registrar_achados_w4 <- function(montagem, exato, nsga2, avaliacao, limite, estabilidade) {
  blocos <- list(
    achados_w4_montagem(montagem),
    achados_w4_payoff(exato),
    achados_w4_nsga2(nsga2, exato, avaliacao),
    achados_w4_emenda_2(nsga2, exato, avaliacao),
    achados_w4_emenda_3(avaliacao, limite, exato),
    achados_w4_estabilidade(estabilidade)
  )
  natureza <- c(
    W4.1 = "confirmatório", W4.2 = "confirmatório", W4.3 = "confirmatório",
    W4.4 = "exploratório", W4.5 = "exploratório", W4.6 = "confirmatório"
  )
  consolidar_achados(blocos) |>
    dplyr::mutate(natureza = unname(natureza[bloco]), .after = etiqueta)
}

# Valor de uma solução de referência da avaliação da emenda 2.
referencia_w4 <- function(avaliacao, solucao, coluna) {
  referencias <- avaliacao$avaliacao$referencias
  referencias[[coluna]][referencias$solucao == solucao]
}

# Custo de uma solução em relação ao da realizada, em texto.
custo_relativo_w4 <- function(custo, realizada) {
  formatar_pct(abs(custo / realizada[["custo"]] - 1))
}

achados_w4_montagem <- function(montagem) {
  v <- montagem$verificacao
  registrar_achados("W4.1", tibble::tibble(
    codigo = c("W4.1.1", "W4.1.2"),
    etiqueta = c("instrumento", "instrumento"),
    enunciado = c(
      paste0(
        "Os ", nrow(montagem$conferencia), " valores de verificação do pré-registro da otimização ",
        "reproduzem na montagem: ", formatar_inteiro(v$assentos), " assentos em ",
        formatar_inteiro(v$projetos), " projetos, ", formatar_inteiro(v$binarias),
        " variáveis binárias de consultor e assento, ", formatar_inteiro(v$variaveis_continuas),
        " variáveis contínuas de sobrecarga e ", formatar_inteiro(v$restricoes), " restrições. ",
        "A alocação realizada é uma solução viável do problema, e a carga que a matriz atribui a ela ",
        "coincide com a ocupação da EDA nos mesmos meses."
      ),
      paste0(
        "Com os mesmos assentos, a receita de cada projeto é fixa e a margem é a receita menos o ",
        "custo, de modo que maximizar a margem equivale a minimizar o custo. A otimização tem três ",
        "objetivos: a afinidade, o custo e a sobrecarga atribuível aos assentos."
      )
    ),
    implicacao = c(
      paste0(
        "O problema resolvido é o registrado antes de qualquer solução. A conferência fica na ",
        "pipeline: se um valor de verificação mudar, a execução para antes do solver. Como a ",
        "realizada é viável, ela serve de referência em todas as comparações da wave."
      ),
      paste0(
        "A margem não entra como quarto objetivo, e toda variação de custo entre duas soluções ",
        "é, com o sinal trocado, a variação de margem. Os valores monetários aparecem só como ",
        "diferenças relativas ao custo da realizada."
      )
    ),
    decisao = c(NA, "A otimização tem três objetivos, sem a margem, que repete o custo."),
    proveniencia = c(NA, "estrutural")
  ))
}

achados_w4_payoff <- function(exato) {
  payoff <- exato$payoff
  realizada <- exato$realizada
  linha <- \(nome) payoff$payoff[payoff$payoff$linha == nome, ]
  passos <- payoff$subproblemas
  tempo_da_linha <- \(nome) formatar_decimal(sum(passos$segundos[passos$linha == nome]), 0)
  primeiro_de_sobrecarga <- passos[passos$linha == "sobrecarga" & passos$passo == 1, ]
  sobrecarga <- linha("sobrecarga")

  registrar_achados("W4.2", tibble::tibble(
    codigo = c("W4.2.1", "W4.2.2", "W4.2.3", "W4.2.4"),
    etiqueta = c("instrumento", "instrumento", "consequência do gerador", "consequência do gerador"),
    enunciado = c(
      paste0(
        "Na tabela de payoff lexicográfica, as linhas de afinidade máxima e de custo mínimo chegam à ",
        "otimalidade nos três passos, em ", tempo_da_linha("afinidade"), " e ", tempo_da_linha("custo"),
        " segundos. A linha de sobrecarga mínima para no tempo-limite de 600 segundos nos três passos, ",
        "e o terceiro termina sem solução viável. A afinidade máxima é ",
        formatar_decimal(linha("afinidade")$afinidade, 1), ", contra ", formatar_decimal(realizada[["afinidade"]], 1),
        " na realizada; o custo mínimo fica ", custo_relativo_w4(linha("custo")$custo, realizada),
        " abaixo do dela; e a menor sobrecarga atribuível encontrada é ",
        formatar_decimal(sobrecarga$sobrecarga_atribuivel, 2), ", contra ",
        formatar_decimal(realizada[["sobrecarga"]], 1), " na realizada."
      ),
      paste0(
        "A regra de troca pedia lacuna zero, e o primeiro passo da linha de sobrecarga parou com ",
        "lacuna relativa de ", formatar_pct(primeiro_de_sobrecarga$mip_gap, 2), ": a melhor solução tem ",
        "sobrecarga atribuível de ", formatar_decimal(sobrecarga$sobrecarga_atribuivel, 3),
        " e o limite inferior certificado é ", formatar_decimal(exato$ideal[["sobrecarga"]], 3), ". ",
        "As linhas de afinidade e de custo, sobre o mesmo problema, resolvem em menos de um minuto."
      ),
      paste0(
        "O limite rígido de capacidade, nenhuma sobrecarga atribuível aos assentos, é inviável: o ",
        "solver certifica que a menor sobrecarga atribuível possível é ao menos ",
        formatar_decimal(exato$ideal[["sobrecarga"]], 3), "."
      ),
      paste0(
        "A alocação realizada é dominada por uma solução exata: a da linha de sobrecarga mínima tem ",
        "afinidade ", formatar_decimal(sobrecarga$afinidade, 1), ", custo ",
        custo_relativo_w4(sobrecarga$custo, realizada), " menor e sobrecarga atribuível de ",
        formatar_decimal(sobrecarga$sobrecarga_atribuivel, 1), ", contra ",
        formatar_decimal(realizada[["afinidade"]], 1), " de afinidade e ",
        formatar_decimal(realizada[["sobrecarga"]], 1), " de sobrecarga na realizada."
      )
    ),
    implicacao = c(
      paste0(
        "Pela regra de troca registrada, a tabela de payoff precisava chegar à otimalidade, e não ",
        "chegou. O método exato foi descartado antes da grade, e a fronteira seguiu pelo NSGA-II da ",
        "emenda 1. Os problemas que param no tempo-limite não se reproduzem entre máquinas, e os ",
        "seus valores entram como referência, com a lacuna declarada."
      ),
      paste0(
        "A troca se deu pelo critério de otimalidade estrita num objetivo cujo ótimo fica perto de ",
        "zero, e não pelo porte do problema. Com lacuna relativa, um ótimo perto de zero exige ",
        "fechar a diferença quase por inteiro. Um critério com tolerância absoluta, ou relativa à ",
        "amplitude do objetivo na tabela de payoff, teria aceitado esse passo; se o método exato se ",
        "manteria dependeria ainda dos outros dois passos da linha e da grade, que não foram ",
        "avaliados com esse critério. Fica registrado como lição de método, sem mudar a troca já feita."
      ),
      paste0(
        "A capacidade não pode entrar como limite duro, o que confirma pelo otimizador o que a EDA ",
        "mostrou no histórico (II.8.2). Dos três tratamentos da capacidade, o rígido fica como ",
        "diagnóstico de sobrealocação."
      ),
      paste0(
        "Existe alocação melhor que a realizada nos três objetivos ao mesmo tempo, com folga em ",
        "todos. No sintético, a realizada é muito dominada, e a magnitude é do gerador."
      )
    ),
    decisao = c(
      "O método da fronteira é o NSGA-II da emenda 1, pela regra de troca registrada.",
      NA, NA, NA
    ),
    limitacao = c(
      paste0(
        "A tratabilidade é a da instância sintética, e o mesmo critério aplicado ao dado real ",
        "pode dar outra decisão."
      ),
      paste0(
        "O critério de lacuna zero foi estrito para um objetivo com ótimo perto de zero, e a ",
        "comparação entre métodos exato e evolutivo fica condicionada a ele."
      ),
      NA,
      paste0(
        "A folga entre a realizada e as soluções exatas mede o gerador. No dado real, a realizada ",
        "carrega critérios que os três objetivos não veem."
      )
    ),
    proveniencia = c("depende do gerador", "estrutural", NA, "depende do gerador")
  ))
}

achados_w4_nsga2 <- function(nsga2, exato, avaliacao) {
  principal <- nsga2$principal
  sementes <- dplyr::bind_rows(principal, nsga2$variabilidade)
  realizada <- exato$realizada
  linha <- \(nome) exato$payoff$payoff[exato$payoff$payoff$linha == nome, ]
  posicao <- avaliacao$avaliacao$posicao
  faixa <- \(x, formatar) paste(formatar(min(x)), "a", formatar(max(x)))

  registrar_achados("W4.3", tibble::tibble(
    codigo = c("W4.3.1", "W4.3.2", "W4.3.3"),
    etiqueta = c("instrumento", "instrumento", "instrumento"),
    enunciado = c(
      paste0(
        "Na semente principal, com ", formatar_inteiro(200), " indivíduos e 2.000 gerações, em ",
        formatar_decimal(principal$segundos / 60, 1), " minutos, o NSGA-II entrega ",
        formatar_inteiro(principal$pontos), " pontos não dominados distintos, e ",
        if (posicao$pontos_que_dominam == posicao$pontos) "todos" else formatar_inteiro(posicao$pontos_que_dominam),
        " dominam a alocação realizada. Os extremos ficam longe dos exatos: afinidade máxima de ",
        formatar_decimal(principal$afinidade_maxima, 1), " contra ", formatar_decimal(linha("afinidade")$afinidade, 1),
        ", custo mínimo ", custo_relativo_w4(principal$custo_minimo, realizada), " abaixo do da realizada contra ",
        custo_relativo_w4(linha("custo")$custo, realizada), ", e sobrecarga atribuível mínima de ",
        formatar_decimal(principal$sobrecarga_minima, 1), " contra ",
        formatar_decimal(linha("sobrecarga")$sobrecarga_atribuivel, 2), ". Nenhum ponto é dominado pelas ",
        "soluções exatas da tabela de payoff."
      ),
      paste0(
        "Entre as ", nrow(sementes), " sementes registradas, todas com 2.000 gerações, o hipervolume na ",
        "caixa entre a realizada e o ideal certificado vai de ", faixa(sementes$hipervolume_da_realizada, \(x) formatar_decimal(x, 3)),
        ", com média de ", formatar_decimal(mean(sementes$hipervolume_da_realizada), 3), "; a afinidade máxima vai de ",
        faixa(sementes$afinidade_maxima, \(x) formatar_decimal(x, 1)), "; o custo mínimo, de ",
        faixa(abs(sementes$custo_minimo / realizada[["custo"]] - 1), formatar_pct), " abaixo do da realizada; ",
        "e a sobrecarga atribuível mínima, de ", faixa(sementes$sobrecarga_minima, \(x) formatar_decimal(x, 1)), ". ",
        "Em nenhuma semente algum ponto é dominado pelas soluções exatas."
      ),
      paste0(
        "O hipervolume registrado na emenda 1, com referência no pior valor de cada objetivo na ",
        "tabela de payoff, vale zero em todas as sementes. Na afinidade, esse pior valor é o da ",
        "solução de sobrecarga mínima, ", formatar_decimal(linha("sobrecarga")$afinidade, 1),
        ", e a referência excluía por construção todo ponto com afinidade menor."
      )
    ),
    implicacao = c(
      paste0(
        "A fronteira evolutiva melhora a realizada em todos os pontos, mas não converge no orçamento ",
        "registrado. Que nenhum ponto seja dominado pelas soluções exatas não indica que eles estejam ",
        "na fronteira verdadeira: as exatas são poucas e ficam nos extremos, e os problemas exatos do ",
        "item 10.c mostram pontos muito melhores na mesma região (W4.4.2)."
      ),
      paste0(
        "A semente principal é típica entre as dez, e as conclusões sobre a fronteira registrada não ",
        "dependem dela. O extremo menos estável é o da sobrecarga, que dobra de uma semente para ",
        "outra, coerente com a convergência lenta nesse objetivo."
      ),
      paste0(
        "É defeito de especificação, e não resultado: a medida não distingue nenhuma fronteira. A ",
        "emenda 2 o corrigiu depois do resultado, com referência na realizada e normalização pela ",
        "caixa até o ideal certificado. O hipervolume corrigido é descritivo e não decide nada."
      )
    ),
    limitacao = c(
      paste0(
        "A fronteira registrada é uma aproximação interna da fronteira verdadeira, e as taxas de ",
        "troca medidas sobre ela valem como ilustração do caminho, não como estimativa."
      ),
      NA,
      paste0(
        "A comparação com o exato registrada na emenda 1 fica sem a medida de hipervolume; a da ",
        "emenda 2 é posterior ao resultado."
      )
    ),
    proveniencia = c("depende do gerador", NA, "estrutural")
  ))
}

achados_w4_emenda_2 <- function(nsga2, exato, avaliacao) {
  trajetoria <- nsga2$trajetoria
  realizada <- exato$realizada
  na_geracao <- \(g) trajetoria[trajetoria$geracao == g, ]
  ultima <- trajetoria[nrow(trajetoria), ]
  pico <- trajetoria[which.max(trajetoria$hipervolume), ]
  custo_exato <- exato$payoff$payoff$custo[exato$payoff$payoff$linha == "custo"]
  alcancou <- trajetoria$geracao[which(trajetoria$custo_minimo <= custo_exato * (1 + 1e-3))[1]]

  problemas <- avaliacao$exatos$problemas
  problema <- \(objetivo) problemas[problemas$objetivo == objetivo, ]
  lacuna <- \(objetivo) formatar_pct(problema(objetivo)$mip_gap, 2)
  pontos <- avaliacao$avaliacao$pontos
  conflito <- avaliacao$avaliacao$conflito
  capacidade <- avaliacao$avaliacao$capacidade
  penalidade <- capacidade[capacidade$tratamento == "penalidade, pesos iguais", ]
  horas <- avaliacao$avaliacao$horas
  ref <- \(solucao, coluna) referencia_w4(avaliacao, solucao, coluna)

  registrar_achados("W4.4", tibble::tibble(
    codigo = paste0("W4.4.", 1:6),
    etiqueta = c("instrumento", "consequência do gerador", "consequência do gerador", "instrumento",
                 "consequência do gerador", "instrumento"),
    enunciado = c(
      paste0(
        "Na corrida exploratória de ", formatar_inteiro(ultima$geracao), " gerações com a semente principal, ",
        "os extremos melhoram o tempo todo: a afinidade máxima vai de ",
        formatar_decimal(na_geracao(2000)$afinidade_maxima, 1), " na geração 2.000 a ",
        formatar_decimal(ultima$afinidade_maxima, 1), ", o custo mínimo alcança o exato na geração ",
        formatar_inteiro(alcancou), ", e a sobrecarga atribuível mínima cai de ",
        formatar_decimal(na_geracao(2000)$sobrecarga_minima, 1), " para ", formatar_decimal(ultima$sobrecarga_minima, 1),
        ". O hipervolume na caixa da realizada não acompanha: chega a ", formatar_decimal(pico$hipervolume, 3),
        " na geração ", formatar_inteiro(pico$geracao), " e termina em ", formatar_decimal(ultima$hipervolume, 3),
        ", contra ", formatar_decimal(na_geracao(2000)$hipervolume, 3), " na geração 2.000."
      ),
      paste0(
        "Com os outros dois objetivos presos nos valores da realizada, o método exato alcança afinidade de ",
        formatar_decimal(problema("afinidade")$afinidade, 1), ", com lacuna de ", lacuna("afinidade"),
        " no tempo-limite; custo ", custo_relativo_w4(problema("custo")$custo, realizada),
        " abaixo do da realizada, ótimo em ", formatar_decimal(problema("custo")$segundos, 1), " segundos; ",
        "e sobrecarga atribuível de ", formatar_decimal(problema("sobrecarga")$sobrecarga, 1),
        ", com lacuna de ", lacuna("sobrecarga"), ". Nenhum ponto da fronteira registrada passa de ",
        formatar_decimal(max(pontos$afinidade), 1), " de afinidade."
      ),
      paste0(
        "Entre os extremos exatos de afinidade e de sobrecarga, a sobrecarga atribuível cresce ",
        formatar_decimal(conflito$taxa_entre_extremos_exatos, 2), " por unidade de afinidade; no caminho ",
        "da fronteira registrada, a mediana da mesma taxa é ", formatar_decimal(conflito$taxa_mediana_na_fronteira, 2),
        ". A afinidade máxima concentra os assentos: a solução exata põe ",
        formatar_pct(ref("payoff, afinidade", "fracao_nos_10_mais_populares")),
        " deles nos 10 consultores mais populares na estimação e ",
        formatar_inteiro(ref("payoff, afinidade", "maior_numero_de_assentos")), " num só consultor, contra ",
        formatar_pct(ref("realizada", "fracao_nos_10_mais_populares")), " e ",
        formatar_inteiro(ref("realizada", "maior_numero_de_assentos")), " na realizada."
      ),
      paste0(
        "Dos três tratamentos da capacidade, o limite rígido é inviável (W4.2.3); o limite calibrado do ",
        "item 8, um épsilon na sobrecarga atribuível da realizada, não restringe nenhum ponto da fronteira ",
        "registrada e coincide com o extremo de afinidade; e a penalidade com pesos iguais na caixa da ",
        "realizada escolhe o ponto de afinidade ", formatar_decimal(penalidade$afinidade, 1), ", custo ",
        custo_relativo_w4(penalidade$custo, realizada), " abaixo e sobrecarga atribuível de ",
        formatar_decimal(penalidade$sobrecarga, 1), "."
      ),
      paste0(
        "A zona morta ocupa de ", formatar_inteiro(min(pontos$assentos_da_zona_morta)), " a ",
        formatar_inteiro(max(pontos$assentos_da_zona_morta)), " assentos por ponto da fronteira registrada, ",
        "com mediana de ", formatar_inteiro(stats::median(pontos$assentos_da_zona_morta)), ", contra ",
        formatar_inteiro(ref("realizada", "assentos_da_zona_morta")), " na realizada. As soluções exatas de ",
        "afinidade máxima e de custo mínimo da tabela de payoff não usam nenhum; as do item 10.c de custo e ",
        "de sobrecarga usam ", formatar_inteiro(ref("item 10.c, custo", "assentos_da_zona_morta")), " e ",
        formatar_inteiro(ref("item 10.c, sobrecarga", "assentos_da_zona_morta")), "."
      ),
      paste0(
        "Reavaliados com a carga em horas, sem otimizar de novo, ",
        if (horas$fracao_que_domina_em_horas == 1) "todos os pontos" else paste(formatar_pct(horas$fracao_que_domina_em_horas), "dos pontos"),
        " da fronteira registrada continuam dominando a realizada, e a correlação de Spearman entre a ",
        "sobrecarga em percentual e em horas é ", formatar_decimal(horas$correlacao_de_spearman, 2), "."
      )
    ),
    implicacao = c(
      paste0(
        "O NSGA-II sem arquivo externo descarta pontos do miolo: a sobrevivência por aglomeração, com ",
        "população fixa, favorece os extremos, e mais gerações não melhoram a fronteira entregue. ",
        "Fica como achado de método. Um arquivo externo de não dominados ou uma população semeada por ",
        "soluções exatas ficam como trabalho futuro, e não são usados aqui."
      ),
      paste0(
        "Cada ganho, tomado sozinho, sai praticamente de graça nos outros dois objetivos, e o método ",
        "exato o encontra em segundos ou minutos. A fronteira evolutiva fica bem para dentro da região ",
        "que as soluções exatas alcançam, o que reforça W4.3.1: ela melhora a realizada, mas não ",
        "representa a fronteira verdadeira."
      ),
      paste0(
        "Maximizar a afinidade é, em boa parte, entregar os assentos aos consultores mais populares, ",
        "como o colaborativo já indicava na wave 3 (seção 5.5), e é aí que a sobrecarga cresce. A taxa ",
        "entre os extremos exatos é a que tem certificado; a da fronteira ilustra o caminho."
      ),
      paste0(
        "O limite calibrado, tal como registrado no item 8, sai degenerado: o épsilon na soma das ",
        "sobrecargas fica acima de todos os pontos da fronteira e não restringe ninguém. A leitura ",
        "correta do limite calibrado é por consultor e mês, e a emenda 3 a examina (W4.5.1)."
      ),
      paste0(
        "A zona morta serve para baratear e para aliviar a carga, e não para ganhar afinidade: as ",
        "soluções que mais a usam são as de custo e de sobrecarga mínimos com a afinidade presa."
      ),
      paste0(
        "As conclusões sobre a posição da realizada não dependem da medida da carga. A ordenação das ",
        "soluções pela sobrecarga é praticamente a mesma em percentual e em horas."
      )
    ),
    limitacao = c(
      paste0(
        "A corrida exploratória é posterior ao resultado e de uma semente só; mede a velocidade de ",
        "convergência, não substitui a fronteira registrada."
      ),
      paste0(
        "Os problemas de afinidade e de sobrecarga param no tempo-limite e não se reproduzem entre ",
        "máquinas; entram com a lacuna declarada."
      ),
      NA,
      paste0(
        "O tratamento da capacidade pelo limite calibrado do item 8 não corresponde ao descrito na ",
        "seção 4.3.7, e a comparação dos três tratamentos fica incompleta sem a emenda 3."
      ),
      NA, NA
    ),
    proveniencia = c("estrutural", "depende do gerador", NA, "estrutural", NA, NA)
  ))
}

# Fração do ganho de afinidade sobre a realizada que sobrevive ao limite.
fracao_do_ganho_w4 <- function(com_limite, sem_limite, realizada) {
  (com_limite - realizada[["afinidade"]]) / (sem_limite - realizada[["afinidade"]])
}

achados_w4_emenda_3 <- function(avaliacao, limite, exato) {
  realizada <- exato$realizada
  concentracao <- avaliacao$concentracao$referencias
  conc <- \(solucao, coluna) concentracao[[coluna]][concentracao$solucao == solucao]
  pontos <- avaliacao$concentracao$pontos
  descricao <- limite$descricao
  com_l <- descricao[descricao$limite == limite$valor, ]
  lim <- \(solucao, coluna) com_l[[coluna]][com_l$solucao == solucao]
  sensibilidade <- descricao[descricao$limite != limite$valor, ]
  afinidade_sem_limite <- \(solucao) referencia_w4(avaliacao, solucao, "afinidade")
  certificado <- \(solucao) descrever_certificado(lim(solucao, "status"), lim(solucao, "mip_gap"))
  ganho <- \(solucao) fracao_do_ganho_w4(lim(solucao, "afinidade"), afinidade_sem_limite(solucao), realizada)

  registrar_achados("W4.5", tibble::tibble(
    codigo = paste0("W4.5.", 1:3),
    etiqueta = c("instrumento", "consequência do gerador", "consequência do gerador"),
    enunciado = c(
      paste0(
        "O objetivo de sobrecarga soma os excessos sobre consultores e meses e é indiferente à ",
        "distribuição. No problema de afinidade máxima do item 10.c, com a sobrecarga total presa na da ",
        "realizada, a maior ocupação de um consultor num mês chega a ",
        formatar_decimal(conc("item 10.c, afinidade", "ocupacao_maxima"), 1), " meses cheios, contra ",
        formatar_decimal(conc("realizada", "ocupacao_maxima"), 1), " na realizada, e ",
        formatar_inteiro(conc("item 10.c, afinidade", "maior_numero_de_assentos")), " assentos vão para um só ",
        "consultor; o consultor mais sobrecarregado fica com ",
        formatar_pct(conc("item 10.c, afinidade", "fracao_no_mais_sobrecarregado"), 0), " da sobrecarga atribuível, ",
        "contra ", formatar_pct(conc("realizada", "fracao_no_mais_sobrecarregado"), 0), " na realizada. Na solução ",
        "de afinidade máxima da tabela de payoff, a ocupação máxima chega a ",
        formatar_decimal(conc("payoff, afinidade", "ocupacao_maxima"), 1), ", contra o máximo de 5,8 da EDA. ",
        "Nos pontos da fronteira registrada, ela fica entre ", formatar_decimal(min(pontos$ocupacao_maxima), 1),
        " e ", formatar_decimal(max(pontos$ocupacao_maxima), 1), "."
      ),
      paste0(
        "Com o limite individual em L = ", formatar_decimal(limite$valor, 2), " meses cheios, a maior ",
        "ocupação mensal da realizada, nenhuma das duas linhas da tabela de payoff completa os três passos ",
        "lexicográficos no tempo-limite: a de afinidade máxima para no passo ",
        formatar_inteiro(lim("payoff, afinidade", "passos_com_solucao")), " e a de custo mínimo no passo ",
        formatar_inteiro(lim("payoff, custo", "passos_com_solucao")), ". A afinidade máxima cai de ",
        formatar_decimal(afinidade_sem_limite("payoff, afinidade"), 1), " para ",
        formatar_decimal(lim("payoff, afinidade", "afinidade"), 1), " (", certificado("payoff, afinidade"), "), e a do ",
        "item 10.c, com custo e sobrecarga presos nos da realizada, de ",
        formatar_decimal(afinidade_sem_limite("item 10.c, afinidade"), 1), " para ",
        formatar_decimal(lim("item 10.c, afinidade", "afinidade"), 1), " (", certificado("item 10.c, afinidade"), "). ",
        "Sobrevivem ", formatar_pct(ganho("payoff, afinidade"), 0), " e ", formatar_pct(ganho("item 10.c, afinidade"), 0),
        " do ganho de afinidade sobre a realizada. O custo mínimo fica ",
        custo_relativo_w4(lim("payoff, custo", "custo"), realizada), " abaixo do da realizada, o mesmo valor sem ",
        "limite, e a sobrecarga mínima do item 10.c fica em ",
        formatar_decimal(lim("item 10.c, sobrecarga", "sobrecarga"), 1), " (", certificado("item 10.c, sobrecarga"), "). ",
        "O limite leva a ocupação máxima a L, mas não desfaz a concentração de assentos: um só consultor ",
        "ainda recebe ", formatar_inteiro(lim("payoff, afinidade", "maior_numero_de_assentos")), " assentos na linha de ",
        "afinidade máxima e ", formatar_inteiro(lim("item 10.c, afinidade", "maior_numero_de_assentos")),
        " no item 10.c, contra ", formatar_inteiro(lim("realizada", "maior_numero_de_assentos")), " na realizada, e os ",
        "meses de consultor acima de 2 na linha de afinidade máxima passam de ",
        formatar_inteiro(conc("payoff, afinidade", "meses_acima_de_2")), " sem limite para ",
        formatar_inteiro(lim("payoff, afinidade", "meses_acima_de_2")), " com ele."
      ),
      paste0(
        "Com L = ", formatar_decimal(sensibilidade$limite, 1), " meses cheios, o problema de afinidade máxima ",
        "do item 10.c ",
        if (is.na(sensibilidade$afinidade)) {
          paste0("termina sem solução (", descrever_certificado(sensibilidade$status, sensibilidade$mip_gap), ").")
        } else {
          paste0(
            "chega a afinidade ", formatar_decimal(sensibilidade$afinidade, 1), " (",
            descrever_certificado(sensibilidade$status, sensibilidade$mip_gap), "), ",
            formatar_pct(fracao_do_ganho_w4(sensibilidade$afinidade, afinidade_sem_limite("item 10.c, afinidade"), realizada), 0),
            " do ganho sem limite. A ocupação máxima desce a ", formatar_decimal(sensibilidade$ocupacao_maxima, 2),
            ", o piso dado pela carga fixa, a sobrecarga atribuível fica em ",
            formatar_decimal(sensibilidade$sobrecarga, 1), ", abaixo dos ", formatar_decimal(lim("realizada", "sobrecarga"), 1),
            " da realizada, e o consultor com mais assentos recebe ",
            formatar_inteiro(sensibilidade$maior_numero_de_assentos), "."
          )
        }
      )
    ),
    implicacao = c(
      paste0(
        "A concentração está no pico de ocupação, e não na soma: a sobrecarga total presa na da ",
        "realizada admite um consultor muito acima do maior pico que a própria realizada teve. Por isso ",
        "o limite calibrado do item 8, um épsilon na sobrecarga total, não restringe ninguém ",
        "individualmente. A seção 4.3.7 descreve o limite calibrado por consultor e mês, e a ",
        "correspondência do item 8 estava errada nesse ponto; o item 8 fica como foi registrado e ",
        "executado, e a emenda 3 examina o limite individual como exploratório."
      ),
      paste0(
        "Quase todo o ganho de afinidade sobrevive quando ninguém pode passar do pico que a realizada ",
        "já tinha, e o custo mínimo não se move. O ganho não depende do pico de ocupação de um consultor; ",
        "depende de empilhar muitos assentos de carga pequena nos consultores mais populares, e o limite ",
        "por consultor e mês não toca nisso. Com ele, o excesso se espalha por mais meses de consultor ",
        "em vez de se acumular num só."
      ),
      paste0(
        "Mesmo com um limite que nem a realizada respeita, a maior parte do ganho de afinidade ",
        "sobrevive, e a concentração de assentos cai pela primeira vez. Como o problema parou no ",
        "tempo-limite, a fração do ganho que sobrevive é um limite inferior."
      )
    ),
    limitacao = c(
      paste0(
        "O limite calibrado do item 8 avaliou uma restrição que não é a da seção 4.3.7, e a ",
        "correção é posterior ao resultado e exploratória."
      ),
      paste0(
        "Exploratório e posterior ao resultado: L é calibrado pelo pico da própria realizada, que fica ",
        "acima do intervalo de 90% entre realizações (W4.6.2). As linhas da tabela de payoff ficam ",
        "incompletas, e o custo e a sobrecarga delas não são os lexicográficos; os problemas que param ",
        "no tempo-limite entram com a lacuna declarada."
      ),
      paste0(
        "Um só valor de L além do calibrado; a curva da afinidade alcançável entre os dois não foi ",
        "traçada."
      )
    ),
    proveniencia = c("estrutural", "depende do gerador", "depende do gerador")
  ))
}

achados_w4_estabilidade <- function(estabilidade) {
  linha <- \(solucao, grandeza) estabilidade[estabilidade$solucao == solucao & estabilidade$grandeza == grandeza, ]
  intervalo <- function(solucao, grandeza, formatar) {
    l <- linha(solucao, grandeza)
    paste0(formatar(l$mediana), " (", formatar(l$inferior), " a ", formatar(l$superior), ")")
  }
  um <- \(x) formatar_decimal(x, 1)
  dois <- \(x) formatar_decimal(x, 2)
  continuas <- estabilidade[!estabilidade$grandeza %in% c("otima", "segundos"), ]
  fora <- continuas[!continuas$dentro, ]
  n <- linha("afinidade", "otima")$realizacoes

  registrar_achados("W4.6", tibble::tibble(
    codigo = c("W4.6.1", "W4.6.2"),
    etiqueta = c("instrumento", "consequência do gerador"),
    enunciado = c(
      paste0(
        "Entre ", formatar_inteiro(n), " realizações, a linha de afinidade máxima da tabela de payoff chega ao ",
        "ótimo em ", formatar_pct(linha("afinidade", "otima")$mediana, 0), " delas e a de custo mínimo em ",
        formatar_pct(linha("custo", "otima")$mediana, 0), ". A afinidade da realizada fica em ",
        intervalo("realizada", "afinidade", um), " e a sobrecarga atribuível dela em ",
        intervalo("realizada", "sobrecarga", um), "; o ganho de afinidade da linha de afinidade máxima sobre a ",
        "realizada fica em ", intervalo("afinidade", "ganho_de_afinidade", um), ", com acréscimo de sobrecarga ",
        "atribuível de ", intervalo("afinidade", "acrescimo_de_sobrecarga", um), "; e o custo mínimo, em ",
        intervalo("custo", "custo_relativo", formatar_pct), " em relação ao da realizada. A realização versionada ",
        "cai fora do intervalo de 90% em ", formatar_inteiro(nrow(fora)), " das ", formatar_inteiro(nrow(continuas)),
        " grandezas contínuas."
      ),
      paste0(
        "A ocupação máxima num mês fica em ", intervalo("realizada", "ocupacao_maxima", dois), " meses cheios na ",
        "realizada, em ", intervalo("afinidade", "ocupacao_maxima", dois), " na linha de afinidade máxima e em ",
        intervalo("custo", "ocupacao_maxima", dois), " na de custo mínimo, entre realizações. Na realização ",
        "versionada, a da realizada é ", dois(linha("realizada", "ocupacao_maxima")$versionada),
        if (linha("realizada", "ocupacao_maxima")$dentro) ", dentro do intervalo." else ", acima do intervalo."
      )
    ),
    implicacao = c(
      paste0(
        "A posição da realizada e o conflito entre afinidade e sobrecarga não são peculiaridade da ",
        "realização versionada. Onde a versionada cai fora do intervalo, o texto cita o intervalo como ",
        "o valor típico."
      ),
      paste0(
        "A concentração no pico da solução de afinidade máxima se repete entre realizações, e não é ",
        "sorteio da versionada. O pico da realizada versionada, que calibra L na emenda 3, é alto para ",
        "o gerador: na realização típica, o mesmo critério daria um limite mais apertado."
      )
    )
  ))
}
