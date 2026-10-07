test_that("nota_numerica respeita a ordem e so admite os tres niveis", {
  expect_equal(nota_numerica(c("ABAIXO", "DENTRO", "ACIMA")), c(1, 2, 3))
  expect_true(is.na(nota_numerica("OUTRA")))
})

test_that("os itens de uma avaliacao compartilham estrutura", {
  # E o teste que faltava quando o gerador sorteava cada item de forma
  # independente: o alfa de Cronbach dava negativo e nada acusava. Um rotulo
  # sem estrutura comum entre itens nao tem sinal sobre a pessoa, e a wave 3
  # nao teria o que aprender dele.
  dados <- carregar_dados()

  dim <- medir_dimensionalidade_dos_itens(dados$avaliacoes_desempenho)

  expect_gt(dim$alfa_cronbach, 0.7)
  expect_gt(dim$correlacao_media_itens, 0.2)
  # O primeiro componente precisa explicar bem mais que o que itens
  # independentes dariam, que e 1 dividido pelo numero de itens.
  expect_gt(dim$variancia_primeiro_componente, 3 / dim$n_itens)
})

test_that("os avaliadores da mesma alocacao compartilham variancia", {
  # Dois avaliadores observam a mesma realidade: podem discordar muito, mas
  # nao podem ser independentes. A versao anterior do gerador os sorteava
  # separadamente e a correlacao saia em zero.
  dados <- carregar_dados()

  div <- medir_divergencia_entre_avaliadores(dados$avaliacoes_desempenho)

  expect_true(all(div$pares$correlacao > 0.1))
  # E a concordancia entre autoavaliacao e gestor fica na faixa baixa que as
  # meta-analises de self-other rating agreement reportam.
  auto <- div$pares[div$pares$etapa_a == "individual" &
    div$pares$etapa_b == "gestor_cegas", ]
  expect_lt(auto$correlacao, 0.5)
})

test_that("a autoavaliacao e mais leniente que a avaliacao as cegas", {
  dados <- carregar_dados()

  div <- medir_divergencia_entre_avaliadores(dados$avaliacoes_desempenho)
  individual <- div$por_etapa[div$por_etapa$etapa == "individual", ]
  cegas <- div$por_etapa[div$por_etapa$etapa == "gestor_cegas", ]

  expect_gt(individual$nivel_medio, cegas$nivel_medio)
  expect_gt(individual$fracao_acima, cegas$fracao_acima)
})

test_that("o escore tem sinal sobre a pessoa, e nao so sobre a alocacao", {
  dados <- carregar_dados()

  variancia <- decompor_variancia_do_escore(dados$avaliacoes_desempenho)

  expect_gt(variancia$fracao_entre_consultores, 0.1)
  expect_lte(variancia$fracao_entre_consultores, 1)
  expect_gt(variancia$consultores_com_mais_de_uma, 0)
})

test_that("cortar_latente_em_nota devolve exatamente as marginais pedidas", {
  # E o que permite mexer na decomposicao de variancia sem quebrar a
  # calibracao das marginais, que sao o unico numero com ancora aqui.
  set.seed(1)
  latente <- stats::rnorm(10000)
  pesos <- c(ABAIXO = 0.15, DENTRO = 0.60, ACIMA = 0.25)

  nota <- cortar_latente_em_nota(latente, pesos)

  observado <- as.numeric(prop.table(table(nota))[names(pesos)])

  expect_setequal(unique(nota), names(pesos))
  expect_equal(observado, unname(pesos), tolerance = 0.01)
})

test_that("sortear_efeito_por_grupo devolve o mesmo valor dentro do grupo", {
  set.seed(1)
  grupo <- c("a", "b", "a", "c", "b")

  efeito <- sortear_efeito_por_grupo(grupo, desvio = 1)

  expect_equal(efeito[1], efeito[3])
  expect_equal(efeito[2], efeito[5])
  expect_false(efeito[1] == efeito[2])
  expect_length(efeito, length(grupo))
})

test_that("a margem nao e atribuivel a uma alocacao individual", {
  dados <- carregar_dados()

  margem <- medir_atribuibilidade_da_margem(dados)

  expect_false(margem$tem_consultor_na_margem)
  expect_gt(margem$fracao_projetos_com_equipe, 0.5)
  # O custo zero da margem estrutural coincide com projeto sem alocacao, que e
  # o que separa o candidato (d) do (c).
  expect_true(margem$custo_zero_estrutural_sem_alocacao)
})

test_that("o veredito dos candidatos de rotulo responde as medidas", {
  medido <- avaliar_candidatos_de_rotulo(
    margem_atribuivel = FALSE, fracao_projetos_com_equipe = 0.685,
    tem_custo_mensal = FALSE, custo_zero_explicavel_estrutural = TRUE,
    alfa_cronbach = 0.9, desvio_da_mediana_alvo = 0.558,
    desvio_do_prejuizo_alvo = -0.013
  )
  hipotetico <- avaliar_candidatos_de_rotulo(
    margem_atribuivel = TRUE, fracao_projetos_com_equipe = 0,
    tem_custo_mensal = TRUE, custo_zero_explicavel_estrutural = TRUE,
    alfa_cronbach = 0.9, desvio_da_mediana_alvo = 0.02,
    desvio_do_prejuizo_alvo = 0.005
  )

  expect_equal(medido$candidato[medido$viavel], c("(a)", "(d)"))
  expect_true(all(hipotetico$viavel))
  expect_true(all(nzchar(medido$exige)))
  expect_true(all(nzchar(medido$justificativa)))
})

test_that("mecanismo e calibracao sao eixos separados no veredito", {
  # O candidato (d) e o caso que obriga a distincao: o mecanismo esta certo e
  # a escala do sintetico nao fecha. Colapsar os dois num veredito so diria
  # que a forma de calcular margem esta errada, que nao e o caso.
  medido <- avaliar_candidatos_de_rotulo(
    margem_atribuivel = FALSE, fracao_projetos_com_equipe = 0.685,
    tem_custo_mensal = FALSE, custo_zero_explicavel_estrutural = TRUE,
    alfa_cronbach = 0.9, desvio_da_mediana_alvo = 0.558,
    desvio_do_prejuizo_alvo = -0.013
  )
  d <- medido[medido$candidato == "(d)", ]

  expect_true(d$viavel)
  expect_false(d$calibrado)

  calibrado <- avaliar_candidatos_de_rotulo(
    margem_atribuivel = FALSE, fracao_projetos_com_equipe = 0.685,
    tem_custo_mensal = FALSE, custo_zero_explicavel_estrutural = TRUE,
    alfa_cronbach = 0.9, desvio_da_mediana_alvo = 0.02,
    desvio_do_prejuizo_alvo = 0.005
  )
  expect_true(calibrado$calibrado[calibrado$candidato == "(d)"])
})

test_that("a fracao de prejuizo certa nao basta para calibrar a margem", {
  # O teste antigo olhava so a fracao de prejuizo, e marcava (d) como
  # calibrado com 0,7% de prejuizo contra 2% reais enquanto a mediana ficava
  # a 55,8 pontos da real, em contradicao com o texto do bloco.
  centro_errado <- avaliar_candidatos_de_rotulo(
    margem_atribuivel = FALSE, fracao_projetos_com_equipe = 0.685,
    tem_custo_mensal = FALSE, custo_zero_explicavel_estrutural = TRUE,
    alfa_cronbach = 0.9, desvio_da_mediana_alvo = 0.558,
    desvio_do_prejuizo_alvo = -0.013
  )

  expect_false(centro_errado$calibrado[centro_errado$candidato == "(d)"])
})

test_that("escala nao confiavel derruba o candidato (a)", {
  # Era o caso antes da correcao do gerador, e o veredito precisa acompanhar.
  sem_escala <- avaliar_candidatos_de_rotulo(
    margem_atribuivel = FALSE, fracao_projetos_com_equipe = 0.685,
    tem_custo_mensal = FALSE, custo_zero_explicavel_estrutural = TRUE,
    alfa_cronbach = -0.035, desvio_da_mediana_alvo = 0.558,
    desvio_do_prejuizo_alvo = -0.013
  )

  expect_false(sem_escala$viavel[sem_escala$candidato == "(a)"])
})

test_that("cada alocacao tem um avaliador identificado", {
  # Confirmado pelo dono do dado de RH: a avaliacao e por projeto e tem
  # gestor identificado. Antes disso o bloco II.5 precisava simular o
  # regime para medir o efeito da normalizacao.
  dados <- carregar_dados()
  av <- dados$avaliacoes_desempenho

  expect_true("avaliador_id" %in% names(av))
  expect_false(anyNA(av$avaliador_id))
  # Na autoavaliacao o avaliador e o proprio avaliado.
  individual <- av[av$etapa == "individual", ]
  expect_equal(individual$avaliador_id, individual$consultor_id)
  # As duas etapas de gestor da mesma alocacao tem o mesmo avaliador.
  gestor <- av[av$etapa %in% c("gestor_cegas", "gestor_final"), ]
  por_alocacao <- dplyr::summarise(
    gestor, n = dplyr::n_distinct(avaliador_id),
    .by = c(consultor_id, projeto_id)
  )
  expect_true(all(por_alocacao$n == 1))
  # E o corpo de gestores e menor que o quadro, nao maior.
  expect_lt(dplyr::n_distinct(gestor$avaliador_id), nrow(dados$consultores))
})

test_that("a nota se desloca com a senioridade do avaliado", {
  # O dono do dado confirmou que a distribuicao tende a variar por nivel,
  # alem da rubrica. A direcao e suposicao declarada; o que o teste fixa e
  # que o efeito existe e e monotonico.
  dados <- carregar_dados()

  # Correlacao sobre todas as avaliacoes, e nao monotonicidade entre as cinco
  # medias por nivel: N0 tem poucas dezenas de avaliacoes e a media dele
  # oscila, entao exigir monotonicidade estrita testaria o ruido e nao o
  # mecanismo.
  escores <- dados$avaliacoes_desempenho |>
    dplyr::filter(etapa == "gestor_final") |>
    dplyr::summarise(escore = mean(nota_numerica(nota)), .by = c(consultor_id, projeto_id)) |>
    dplyr::left_join(
      dplyr::select(dados$consultores, consultor_id, nivel_senioridade),
      by = "consultor_id"
    ) |>
    dplyr::mutate(nivel = senioridade_numerica(nivel_senioridade))

  expect_gt(cor(escores$nivel, escores$escore), 0.1)
})

test_that("normalizar por avaliador remove contaminacao sem perder sinal", {
  # A afirmacao central do candidato (a), medida em vez de repetida.
  dados <- carregar_dados()

  construcoes <- comparar_construcoes_de_rotulo(dados$avaliacoes_desempenho)
  bruta <- construcoes[construcoes$forma == "Nota bruta", ]
  normalizada <- construcoes[construcoes$forma == "Normalizada por avaliador", ]

  expect_gt(bruta$contaminacao_avaliador, 0.05)
  expect_lt(normalizada$contaminacao_avaliador, 1e-8)
  expect_gt(normalizada$sinal_entre_consultores, bruta$sinal_entre_consultores)
  expect_equal(bruta$avaliadores, normalizada$avaliadores)
})

test_that("a policorica corrige a atenuacao da discretizacao", {
  # Pearson sobre ordinal de tres niveis e limite inferior; a policorica
  # estima a correlacao entre as latentes. A ordem entre as duas nao pode
  # inverter, senao alguma das duas esta errada.
  dados <- carregar_dados()

  dim <- medir_dimensionalidade_dos_itens(dados$avaliacoes_desempenho)

  expect_gt(dim$correlacao_media_policorica, dim$correlacao_media_itens)
  expect_gt(dim$alfa_ordinal, dim$alfa_cronbach)
  expect_lt(dim$alfa_ordinal, 1)
})

test_that("o comentario pode ser dispensado sem mexer nas notas", {
  alocacoes <- carregar_dados(tabelas = "alocacoes")$alocacoes

  com <- gerar_avaliacoes_desempenho(alocacoes, semente = 7)
  sem <- gerar_avaliacoes_desempenho(alocacoes, com_comentario = FALSE, semente = 7)

  expect_true("comentario" %in% names(com))
  expect_false("comentario" %in% names(sem))
  expect_equal(sem$nota, com$nota)
})

test_that("executar_ii5 devolve as quatro partes do bloco", {
  dados <- carregar_dados()

  bloco <- executar_ii5(dados)

  expect_named(bloco, c("objetos", "tabelas", "figuras", "achados"))
  expect_true(all(vapply(bloco$tabelas, is.data.frame, logical(1))))
  expect_true(all(vapply(bloco$figuras, ggplot2::is_ggplot, logical(1))))
  expect_equal(nrow(bloco$achados), 11)
  expect_true(all(startsWith(bloco$achados$codigo, "II.5.")))
  # A cobertura do rotulo e parametro do gerador, e o achado que a reporta
  # precisa estar etiquetado como tal.
  expect_equal(
    bloco$achados$etiqueta[bloco$achados$codigo == "II.5.6"],
    "consequência do gerador"
  )
})

test_that("as figuras do bloco II.5 carregam o proprio tema", {
  tema_anterior <- ggplot2::theme_get()
  ggplot2::theme_set(ggplot2::theme_grey())
  on.exit(ggplot2::theme_set(tema_anterior))

  bloco <- executar_ii5(carregar_dados())

  for (figura in bloco$figuras) {
    expect_s3_class(figura$theme$panel.border, "element_rect")
    expect_no_error(ggplot2::ggplot_build(figura))
  }
})

test_that("pnorm_bivariada bate com a formula fechada em casos conhecidos", {
  # Sem correlacao, a acumulada conjunta e o produto das marginais.
  expect_equal(pnorm_bivariada(0.3, -1.2, 0), pnorm(0.3) * pnorm(-1.2))
  # Em correlacao 1 nao chega a ser avaliada, mas perto dela a conjunta
  # tende ao minimo das marginais.
  expect_equal(pnorm_bivariada(0, 0, 0.999), 0.5, tolerance = 0.01)
  # Caso classico: h = k = 0 com rho qualquer vale 1/4 + asin(rho)/(2 pi).
  for (rho in c(-0.6, -0.2, 0.4, 0.8)) {
    expect_equal(
      pnorm_bivariada(0, 0, rho), 0.25 + asin(rho) / (2 * pi),
      tolerance = 1e-8
    )
  }
})

test_that("a policorica recupera a correlacao latente que Pearson atenua", {
  set.seed(11)
  n <- 8000
  rho <- 0.6
  z1 <- rnorm(n)
  z2 <- rho * z1 + sqrt(1 - rho^2) * rnorm(n)
  corta <- function(z) cut(z, c(-Inf, qnorm(0.15), qnorm(0.75), Inf), labels = FALSE)

  estimada <- correlacao_policorica(corta(z1), corta(z2))

  expect_equal(estimada, rho, tolerance = 0.05)
  # E Pearson sobre as categorias fica bem abaixo, que e a atenuacao.
  expect_lt(cor(corta(z1), corta(z2)), rho - 0.05)
})

test_that("acoplar_por_posto preserva a marginal exatamente", {
  set.seed(3)
  valores <- rlnorm(500)
  referencia <- rnorm(500)

  acoplado <- acoplar_por_posto(valores, referencia, forca = 0.7)

  expect_equal(sort(acoplado), sort(valores))
  expect_gt(cor(acoplado, referencia, method = "spearman"), 0.4)
})

test_that("forca zero no acoplamento nao induz correlacao", {
  set.seed(4)
  valores <- rlnorm(2000)
  referencia <- rnorm(2000)

  solto <- acoplar_por_posto(valores, referencia, forca = 0)

  expect_equal(sort(solto), sort(valores))
  expect_lt(abs(cor(solto, referencia, method = "spearman")), 0.1)
})

test_that("o acoplamento liga contrato e esforco ao esforco alocado", {
  # O defeito que motivou: na Camada 3 tudo era independente de tudo, e a
  # margem estrutural saia sem sentido.
  dados <- carregar_dados(tabelas = c("projetos", "alocacoes"))

  horas <- dados$alocacoes |>
    dplyr::mutate(h = horas_mes * duracao_dias / (365.25 / 12)) |>
    dplyr::summarise(horas = sum(h), .by = projeto_id)
  com_horas <- dplyr::inner_join(dados$projetos, horas, by = "projeto_id")

  expect_gt(
    cor(com_horas$valor_contrato, com_horas$horas, method = "spearman"), 0.3
  )
  expect_gt(
    cor(com_horas$esforco_estimado, com_horas$horas, method = "spearman"), 0.3
  )
})

test_that("o diagnostico de escala da margem mede sem decidir", {
  # A funcao existe para tornar a evidencia auditavel, nao para concluir que
  # a taxa e preco. O veredito depende de semantica de coluna que nao esta
  # registrada e cujo acesso encerrou.
  dados <- carregar_dados()

  escala <- diagnosticar_escala_da_margem(dados)

  expect_gt(escala$centro_da_razao, 0)
  # Guarda de regressao: com custo de verdade a razao centra bem abaixo de
  # 1. Se voltar a centrar em 1, alguem repos o rate card como custo.
  expect_false(escala$razao_centra_em_um)
  expect_lt(escala$centro_da_razao, 0.7)
  expect_equal(nrow(escala$custo_mensal), 5)
  expect_gt(escala$custo_mensal_mais_senior, escala$custo_mensal_mais_junior)
  # O custo mensal implicito e a taxa vezes a jornada, sem nada no meio.
  expect_equal(
    escala$custo_mensal$custo_mensal_implicito,
    escala$custo_mensal$taxa_mediana * escala$jornada_mensal
  )
})

test_that("quem nunca foi alocado nao tem rotulo, e o teste exclui essas pessoas", {
  # Rotulo nasce de alocacao. Se o teste de associacao incluisse a zona
  # morta, ela entraria inteira na celula sem conteudo e sem rotulo e
  # fabricaria uma associacao que nao existe.
  cruzamento <- cruzar_conteudo_e_rotulo(carregar_dados())
  sem_historico <- cruzamento$quadrantes[!cruzamento$quadrantes$interacao, ]

  expect_true(all(sem_historico$com_rotulo == 0))
})

test_that("no sintetico conteudo e rotulo sao independentes, por construcao", {
  # O gerador sorteia as alocacoes avaliadas de forma uniforme. Se a razao de
  # chances sair do entorno de um, alguem acoplou as duas coberturas.
  cruzamento <- cruzar_conteudo_e_rotulo(carregar_dados())

  expect_lte(cruzamento$ic_razao[1], 1)
  expect_gte(cruzamento$ic_razao[2], 1)
  expect_equal(
    cruzamento$fracao_fim_a_fim, cruzamento$fracao_fim_a_fim_independente,
    tolerance = 0.03
  )
  expect_lte(
    cruzamento$fracao_fim_a_fim,
    min(cruzamento$fracao_alocacoes_com_conteudo, cruzamento$fracao_alocacoes_com_rotulo)
  )
})

test_that("as horas da margem saem do dado e a alocacao real aumenta o desacordo", {
  # O texto do II.5 dizia "cerca de metade das horas" digitado, e o numero
  # mudou para um quinto sem que nada acusasse quando a duracao da alocacao
  # foi recalibrada.
  dados <- carregar_dados(tabelas = c("projetos", "alocacoes"))
  sobredeterminacao <- medir_sobredeterminacao_economica()

  horas <- medir_horas_contra_contrato(dados, sobredeterminacao)

  expect_gt(horas$fracao_das_horas_implicadas, 0)
  expect_lt(horas$fracao_das_horas_implicadas, 1)
  expect_equal(horas$duracao_alocacao_dias, 18)
  expect_gt(horas$desacordo_com_alocacao, sobredeterminacao$desacordo)
})
