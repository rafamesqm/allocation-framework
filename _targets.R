# Pipeline de análise (targets). Roda por padrão sobre data/synthetic/.
# tar_make() reconstrói tudo do dado sintético ao resultado.
# Ver docs/dicionario_de_dados.md para o esquema dos dados e
# docs/mapeamento_schema.md para a calibração real por trás de cada peça.

library(targets)

tar_source("R")

tar_option_set(
  packages = c(
    "tidyverse", "data.table", "janitor", "skimr", "DataExplorer", "here", "Matrix"
  )
)

# Parâmetros do gerador sintético, num único lugar: cada gerador
# individual mantém seu próprio default documentado e
# calibrado (ver roxygen de cada função em R/etl/), útil para uso
# interativo ou em teste; esta lista é o que a pipeline de fato usa numa
# corrida, para os parâmetros compartilhados entre peças não divergirem.
parametros <- list(
  semente = 1,
  n_consultores = 285,
  n_projetos = 952,
  data_referencia = as.Date("2026-09-01"),
  fracao_cold_start = 0.17,
  prob_vies = 0.7,
  dias_ferias = 30,
  ano_ferias = 2026,
  n_oportunidades = 4302, # soma do censo real por situação, ver mapeamento_schema.md
  n_replicas_estabilidade = 100 # realizações para o intervalo de Monte Carlo do bloco I.0
)

# Realizações da wave 3 nesta corrida. Como objeto global, entra no hash dos
# alvos que o usam: trocar W3_REALIZACOES invalida as realizações.
n_realizacoes_w3 <- realizacoes_w3()

list(
  # Camada 1: Atributos -------------------------------------------------
  tar_target(espaco_skills, gerar_espaco_skills()),
  tar_target(espaco_skills_arquivo, salvar_synthetic(espaco_skills, "espaco_skills"), format = "file"),

  tar_target(consultores, gerar_consultores(
    n_consultores = parametros$n_consultores, semente = parametros$semente
  )),
  tar_target(consultores_arquivo, salvar_synthetic(consultores, "consultores"), format = "file"),

  # Os sinais de conteúdo derivados de formulário compartilham um único
  # conjunto de respondentes, porque na fonte real vêm da mesma campanha de
  # três planilhas (docs/mapeamento_schema.md). Quem não respondeu não tem
  # nenhum deles. Ver sortear_respondentes_formulario() para a consequência
  # estrutural de sortear cobertura por sinal, que é o que se fazia antes.
  #
  # `idiomas_consultor` fica de fora da restrição de propósito: sua fonte é
  # a base corporativa de colaboradores, sem nulo entre os 285
  # ativos, e não o formulário. É o único sinal de conteúdo genuinamente
  # universal, e tratá-lo como os demais apagaria a única cobertura completa
  # que os dados reais oferecem.
  tar_target(respondentes_formulario, sortear_respondentes_formulario(consultores)),

  tar_target(skills_consultor, gerar_skills_consultor(
    consultores, espaco_skills, respondentes = respondentes_formulario
  )),
  tar_target(skills_consultor_arquivo, salvar_synthetic(skills_consultor, "skills_consultor"), format = "file"),

  # Latente: a experiência setorial existe tenha ou não sido declarada, e é a
  # ela que `gerar_alocacoes()` responde. Só a parte observada é salva.
  tar_target(experiencia_setorial, gerar_experiencia_setorial(consultores)),
  tar_target(experiencia_setorial_observada, restringir_a_respondentes(
    experiencia_setorial, respondentes_formulario
  )),
  tar_target(experiencia_setorial_arquivo, salvar_synthetic(
    experiencia_setorial_observada, "experiencia_setorial"
  ), format = "file"),

  tar_target(expertise_funcional_arquivo, salvar_synthetic(
    restringir_a_respondentes(
      gerar_expertise_funcional(consultores), respondentes_formulario
    ), "expertise_funcional"
  ), format = "file"),
  tar_target(idiomas_consultor_arquivo, salvar_synthetic(
    gerar_idiomas_consultor(consultores), "idiomas_consultor"
  ), format = "file"),
  tar_target(certificacoes_consultor_arquivo, salvar_synthetic(
    gerar_certificacoes_consultor(
      consultores, respondentes = respondentes_formulario
    ), "certificacoes_consultor"
  ), format = "file"),
  tar_target(soft_skills_consultor_arquivo, salvar_synthetic(
    restringir_a_respondentes(
      gerar_soft_skills_consultor(consultores), respondentes_formulario
    ), "soft_skills_consultor"
  ), format = "file"),

  tar_target(projetos, gerar_projetos(
    n_projetos = parametros$n_projetos, semente = parametros$semente,
    data_referencia = parametros$data_referencia
  )),

  # Camada 2: Interações --------------------------------------------------
  tar_target(alocacoes, gerar_alocacoes(
    consultores, projetos, experiencia_setorial,
    semente = parametros$semente,
    fracao_cold_start_consultor = parametros$fracao_cold_start,
    fracao_cold_start_projeto = parametros$fracao_cold_start,
    prob_vies = parametros$prob_vies
  )),
  tar_target(alocacoes_arquivo, salvar_synthetic(alocacoes, "alocacoes"), format = "file"),

  # Acopla o contrato ao esforço de fato alocado e deriva dele o esforço
  # estimado. Roda depois da alocação porque é dela que sai o esforço, e não
  # há ciclo:
  # `gerar_alocacoes()` usa do projeto só setor e janela de datas. Ver
  # `acoplar_projetos_ao_esforco()` para o defeito de Camada 3 que motivou.
  tar_target(projetos_acoplados, acoplar_projetos_ao_esforco(projetos, alocacoes)),
  tar_target(projetos_arquivo, salvar_synthetic(projetos_acoplados, "projetos"), format = "file"),

  # Camada 3: Operação ------------------------------------------------------
  tar_target(custo_carregado, gerar_custo_carregado(consultores, semente = parametros$semente)),
  tar_target(custo_carregado_arquivo, salvar_synthetic(custo_carregado, "custo_carregado"), format = "file"),

  tar_target(ocupacao_consultor_arquivo, salvar_synthetic(
    agregar_ocupacao_consultor(alocacoes), "ocupacao_consultor"
  ), format = "file"),
  tar_target(ferias_consultor_arquivo, salvar_synthetic(
    gerar_ferias_consultor(
      consultores,
      dias_ferias = parametros$dias_ferias, ano_referencia = parametros$ano_ferias,
      semente = parametros$semente
    ),
    "ferias_consultor"
  ), format = "file"),
  tar_target(receita_projeto_mes_arquivo, salvar_synthetic(
    gerar_receita_projeto_mes(projetos_acoplados, semente = parametros$semente),
    "receita_projeto_mes"
  ), format = "file"),
  tar_target(margem_estrutural_arquivo, salvar_synthetic(
    gerar_margem_estrutural(alocacoes, custo_carregado, projetos_acoplados), "margem_estrutural"
  ), format = "file"),
  tar_target(pipeline_demandas_arquivo, salvar_synthetic(
    gerar_pipeline_demandas(parametros$n_oportunidades, semente = parametros$semente),
    "pipeline_demandas"
  ), format = "file"),

  # Camada 4: Relacional ------------------------------------------------
  tar_target(grafo_coalocacao_arquivo, salvar_synthetic(
    gerar_grafo_coalocacao(alocacoes), "grafo_coalocacao"
  ), format = "file"),

  # Validação -------------------------------------------------------------
  tar_target(validacao_utilidade_arquivo, salvar_synthetic(
    validar_utilidade_sintetico(semente = parametros$semente), "validacao_utilidade"
  ), format = "file"),

  # EDA, wave 2 -------------------------------------------------------------
  # Os blocos leem do disco, e não dos alvos em memória acima, de propósito:
  # é exatamente o que o auditor faz, e é o que garante que a análise rode
  # sobre os CSV versionados e não sobre um estado intermediário da sessão.
  # A dependência dos arquivos é declarada para o targets saber invalidar os
  # blocos quando o gerador mudar.
  tar_target(dados_eda, {
    c(
      alocacoes_arquivo, consultores_arquivo, projetos_arquivo,
      custo_carregado_arquivo, espaco_skills_arquivo, skills_consultor_arquivo,
      experiencia_setorial_arquivo, expertise_funcional_arquivo,
      idiomas_consultor_arquivo, certificacoes_consultor_arquivo,
      soft_skills_consultor_arquivo,
      ocupacao_consultor_arquivo, ferias_consultor_arquivo,
      receita_projeto_mes_arquivo, margem_estrutural_arquivo,
      pipeline_demandas_arquivo, grafo_coalocacao_arquivo,
      validacao_utilidade_arquivo
    )
    carregar_dados()
  }),

  # Estabilidade de Monte Carlo: repete a geração inteira sobre várias
  # sementes, para os blocos poderem reportar intervalo em vez de ponto. Não
  # lê de dados_eda de propósito, porque precisa variar a semente e o dado
  # versionado é de uma semente só.
  tar_target(replicas_estabilidade, simular_estabilidade(
    sementes = seq_len(parametros$n_replicas_estabilidade),
    n_consultores = parametros$n_consultores,
    n_projetos = parametros$n_projetos,
    data_referencia = parametros$data_referencia
  )),
  # As grandezas do conjunto versionado, pela mesma função que mede as
  # réplicas. É o que permite ao bloco I.0 dizer se ele é um sorteio típico.
  tar_target(grandezas_versionadas, medir_grandezas(dados_eda)),
  tar_target(bloco_i0, executar_i0(replicas_estabilidade, versionado = grandezas_versionadas)),
  tar_target(bloco_i0_tabelas, salvar_tabelas_bloco(bloco_i0$tabelas), format = "file"),
  tar_target(bloco_i0_figuras, salvar_figuras_bloco(
    bloco_i0$figuras, list(
      i0_estabilidade = c(17, altura_da_figura_estabilidade(
        sum(bloco_i0$objetos$classificado$classe != "diferença")
      )),
      i0_tipicidade = c(17, 9)
    )
  ), format = "file"),
  tar_target(bloco_i0_achados, salvar_tabela(bloco_i0$achados, "achados_i0"), format = "file"),
  tar_target(estabilidade, bloco_i0$objetos$estabilidade),

  tar_target(bloco_i1, executar_i1(dados_eda, estabilidade = estabilidade)),
  tar_target(bloco_i1_tabelas, salvar_tabelas_bloco(bloco_i1$tabelas), format = "file"),
  tar_target(bloco_i1_figuras, salvar_figuras_bloco(
    bloco_i1$figuras, list(i1_cobertura_entidades = c(18, 13))
  ), format = "file"),
  tar_target(bloco_i1_achados, salvar_tabela(bloco_i1$achados, "achados_i1"), format = "file"),

  tar_target(bloco_i2, executar_i2(dados_eda, estabilidade = estabilidade)),
  tar_target(bloco_i2_tabelas, salvar_tabelas_bloco(bloco_i2$tabelas), format = "file"),
  tar_target(bloco_i2_figuras, salvar_figuras_bloco(
    bloco_i2$figuras,
    list(i2_piramide_senioridade = c(16, 9), i2_projetos_distribuicoes = c(18, 8))
  ), format = "file"),
  tar_target(bloco_i2_achados, salvar_tabela(bloco_i2$achados, "achados_i2"), format = "file"),

  tar_target(bloco_i3, executar_i3(dados_eda, estabilidade = estabilidade)),
  tar_target(bloco_i3_tabelas, salvar_tabelas_bloco(bloco_i3$tabelas), format = "file"),
  tar_target(bloco_i3_figuras, salvar_figuras_bloco(
    bloco_i3$figuras, list(i3_associacoes_procedencia = c(17, 16))
  ), format = "file"),
  tar_target(bloco_i3_achados, salvar_tabela(bloco_i3$achados, "achados_i3"), format = "file"),

  tar_target(bloco_i4, executar_i4(dados_eda, data_referencia = parametros$data_referencia)),
  tar_target(bloco_i4_tabelas, salvar_tabelas_bloco(bloco_i4$tabelas), format = "file"),
  tar_target(bloco_i4_figuras, salvar_figuras_bloco(
    bloco_i4$figuras,
    list(
      i4_serie_mensal = c(18, 10), i4_sazonalidade = c(17, 9),
      i4_ocupacao_mensal = c(18, 9)
    )
  ), format = "file"),
  tar_target(bloco_i4_achados, salvar_tabela(bloco_i4$achados, "achados_i4"), format = "file"),

  tar_target(bloco_i5, executar_i5(dados_eda, semente = parametros$semente)),
  tar_target(bloco_i5_tabelas, salvar_tabelas_bloco(bloco_i5$tabelas), format = "file"),
  tar_target(bloco_i5_figuras, salvar_figuras_bloco(
    bloco_i5$figuras,
    list(i5_distribuicao_grau = c(17, 9), i5_tamanho_time = c(17, 9))
  ), format = "file"),
  tar_target(bloco_i5_achados, salvar_tabela(bloco_i5$achados, "achados_i5"), format = "file"),

  # Parte II, direcionada ---------------------------------------------------
  tar_target(bloco_ii1, executar_ii1(dados_eda, estabilidade = estabilidade)),
  tar_target(bloco_ii1_tabelas, salvar_tabelas_bloco(bloco_ii1$tabelas), format = "file"),
  tar_target(bloco_ii1_figuras, salvar_figuras_bloco(
    bloco_ii1$figuras, list(ii1_lorenz = c(16, 10), ii1_linha_base = c(16, 10))
  ), format = "file"),
  tar_target(bloco_ii1_achados, salvar_tabela(bloco_ii1$achados, "achados_ii1"), format = "file"),

  tar_target(bloco_ii2, executar_ii2(dados_eda, estabilidade = estabilidade)),
  tar_target(bloco_ii2_tabelas, salvar_tabelas_bloco(bloco_ii2$tabelas), format = "file"),
  tar_target(bloco_ii2_figuras, salvar_figuras_bloco(
    bloco_ii2$figuras, list(ii2_evidencia = c(17, 10), ii2_subdeterminacao = c(16, 9))
  ), format = "file"),
  tar_target(bloco_ii2_achados, salvar_tabela(bloco_ii2$achados, "achados_ii2"), format = "file"),

  tar_target(bloco_ii3, executar_ii3(dados_eda, estabilidade = estabilidade)),
  tar_target(bloco_ii3_tabelas, salvar_tabelas_bloco(bloco_ii3$tabelas), format = "file"),
  tar_target(bloco_ii3_figuras, salvar_figuras_bloco(
    bloco_ii3$figuras, list(ii3_cobertura_sinais = c(17, 11))
  ), format = "file"),
  tar_target(bloco_ii3_achados, salvar_tabela(bloco_ii3$achados, "achados_ii3"), format = "file"),

  tar_target(bloco_ii4, executar_ii4(dados_eda, estabilidade = estabilidade)),
  tar_target(bloco_ii4_tabelas, salvar_tabelas_bloco(bloco_ii4$tabelas), format = "file"),
  tar_target(bloco_ii4_figuras, salvar_figuras_bloco(
    bloco_ii4$figuras,
    list(
      ii4_distribuicao_propensao = c(17, 10),
      ii4_cegueira_propensao = c(19, 10)
    )
  ), format = "file"),
  tar_target(bloco_ii4_achados, salvar_tabela(bloco_ii4$achados, "achados_ii4"), format = "file"),

  tar_target(bloco_ii5, executar_ii5(dados_eda, estabilidade = estabilidade)),
  tar_target(bloco_ii5_tabelas, salvar_tabelas_bloco(bloco_ii5$tabelas), format = "file"),
  tar_target(bloco_ii5_achados, salvar_tabela(bloco_ii5$achados, "achados_ii5"), format = "file"),

  tar_target(bloco_ii6, executar_ii6(dados_eda, estabilidade = estabilidade)),
  tar_target(bloco_ii6_tabelas, salvar_tabelas_bloco(bloco_ii6$tabelas), format = "file"),
  tar_target(bloco_ii6_figuras, salvar_figuras_bloco(
    bloco_ii6$figuras, list(ii6_decomposicao_auc = c(19, 9))
  ), format = "file"),
  tar_target(bloco_ii6_achados, salvar_tabela(bloco_ii6$achados, "achados_ii6"), format = "file"),

  tar_target(bloco_ii7, executar_ii7(dados_eda, estabilidade = estabilidade)),
  tar_target(bloco_ii7_tabelas, salvar_tabelas_bloco(bloco_ii7$tabelas), format = "file"),
  tar_target(bloco_ii7_figuras, salvar_figuras_bloco(
    bloco_ii7$figuras, list(ii7_avaliabilidade = c(19, 9))
  ), format = "file"),
  tar_target(bloco_ii7_achados, salvar_tabela(bloco_ii7$achados, "achados_ii7"), format = "file"),

  tar_target(bloco_ii8, executar_ii8(dados_eda, estabilidade = estabilidade)),
  tar_target(bloco_ii8_tabelas, salvar_tabelas_bloco(bloco_ii8$tabelas), format = "file"),
  tar_target(bloco_ii8_figuras, salvar_figuras_bloco(
    bloco_ii8$figuras, list(ii8_insumos = c(19, 9))
  ), format = "file"),
  tar_target(bloco_ii8_achados, salvar_tabela(bloco_ii8$achados, "achados_ii8"), format = "file"),

  # Parte III, síntese. Não recalcula nada: consome os achados que cada bloco
  # declarou, o que faz a tabela de síntese ser derivada e não transcrita.
  tar_target(bloco_iii, executar_iii(list(
    bloco_i0$achados, bloco_i1$achados, bloco_i2$achados, bloco_i3$achados,
    bloco_i4$achados, bloco_i5$achados, bloco_ii1$achados, bloco_ii2$achados,
    bloco_ii3$achados, bloco_ii4$achados, bloco_ii5$achados, bloco_ii6$achados,
    bloco_ii7$achados, bloco_ii8$achados
  ))),
  tar_target(bloco_iii_tabelas, salvar_tabelas_bloco(bloco_iii$tabelas), format = "file"),
  tar_target(bloco_iii_figuras, salvar_figuras_bloco(
    bloco_iii$figuras, list(iii_proveniencia = c(19, 10))
  ), format = "file"),
  tar_target(bloco_iii_achados, salvar_tabela(bloco_iii$achados, "achados_iii"), format = "file"),

  # Wave 3, scoring híbrido ---------------------------------------------------
  # O pré-registro fixa a avaliação antes de qualquer modelo. Todo alvo da
  # wave 3 recebe dele sementes, divisões, grades e regras, e o arquivo é
  # rastreado para que qualquer mudança nele invalide o que veio depois.
  tar_target(pre_registro_arquivo, here::here("config", "pre_registro.yml"), format = "file"),
  tar_target(pre_registro, ler_pre_registro(pre_registro_arquivo)),

  # Etapa 1: os pisos sob o protocolo, conferidos contra os valores de
  # verificação do pré-registro. A conferência para a pipeline se algum não
  # reproduzir, e a tabela só é salva depois dela.
  tar_target(w3_pisos, medir_pisos_pre_registrados(dados_eda, pre_registro)),
  tar_target(w3_pisos_conferencia, conferir_pisos(w3_pisos, pre_registro)),
  tar_target(w3_pisos_tabela, {
    w3_pisos_conferencia
    salvar_tabela(tabelar_pisos(w3_pisos, estabilidade), "w3_pisos")
  }, format = "file"),

  # Etapa 2, conteúdo: escolha dos hiperparâmetros do mapeamento na
  # validação interna, só nos grupos de estimação. O teste contra o piso
  # entra depois de a escolha ser revista.
  tar_target(w3_conteudo_selecao, selecionar_hiperparametros_do_conteudo(dados_eda, pre_registro)),
  tar_target(w3_conteudo_hiperparametros_tabela, salvar_tabela(
    tabelar_hiperparametros_do_conteudo(w3_conteudo_selecao), "w3_conteudo_hiperparametros"
  ), format = "file"),

  # Emenda 2 do conteúdo: a escolha refeita com previsões cruzadas do mapa.
  # Os alvos da emenda 1, acima e abaixo, ficam preservados e reportados.
  tar_target(w3_conteudo_e2_selecao, selecionar_hiperparametros_do_conteudo_cruzado(dados_eda, pre_registro)),
  tar_target(w3_conteudo_e2_hiperparametros_tabela, salvar_tabela(
    tabelar_hiperparametros_do_conteudo(w3_conteudo_e2_selecao), "w3_conteudo_e2_hiperparametros"
  ), format = "file"),
  # O teste único da emenda 2, que decide o conteúdo, com a escolha conferida
  # contra a registrada antes dele.
  tar_target(w3_conteudo_e2_selecao_conferida, conferir_escolha_do_conteudo_cruzado(
    w3_conteudo_e2_selecao, pre_registro
  )),
  tar_target(w3_conteudo_e2_avaliacao, avaliar_componente_de_conteudo_cruzado(
    dados_eda, pre_registro, w3_conteudo_e2_selecao_conferida
  )),

  # O teste único do conteúdo, com a escolha conferida contra a registrada na
  # emenda de 2026-10-24: se a escolha mudar, a pipeline para antes do teste.
  tar_target(w3_conteudo_selecao_conferida, conferir_escolha_registrada(w3_conteudo_selecao, pre_registro)),
  tar_target(w3_conteudo_avaliacao, avaliar_componente_de_conteudo(
    dados_eda, pre_registro, w3_conteudo_selecao_conferida
  )),
  tar_target(w3_conteudo_resumo, resumir_avaliacao_do_conteudo(w3_conteudo_avaliacao, pre_registro)),

  # Realizações: cada uma refaz o procedimento inteiro, inclusive a escolha
  # de hiperparâmetros. Uma semente por ramo, para uma corrida interrompida
  # retomar de onde parou.
  # W3_REALIZACOES reduz o número de realizações (integração contínua no
  # push); nesse caso a trava de diretorio_de_saida() desvia as tabelas e
  # figuras para uma pasta temporária.
  tar_target(w3_conteudo_sementes, seq(pre_registro$sementes$realizacoes[1], length.out = n_realizacoes_w3)),
  tar_target(
    w3_conteudo_replicas,
    simular_avaliacao_do_conteudo(
      w3_conteudo_sementes, pre_registro,
      n_consultores = parametros$n_consultores, n_projetos = parametros$n_projetos,
      data_referencia = parametros$data_referencia
    ),
    pattern = map(w3_conteudo_sementes)
  ),

  # Achados das Etapas 1 e 2, no formato da EDA, com tabelas e figura.
  tar_target(w3_achados_pisos, registrar_achados_w3_pisos(w3_pisos_conferencia, w3_pisos, estabilidade)),
  tar_target(w3_pisos_achados, salvar_tabela(w3_achados_pisos, "achados_w3_1"), format = "file"),
  tar_target(bloco_w3_conteudo, executar_w3_conteudo(
    w3_conteudo_avaliacao, w3_conteudo_replicas, pre_registro
  )),
  tar_target(bloco_w3_conteudo_tabelas, salvar_tabelas_bloco(bloco_w3_conteudo$tabelas), format = "file"),
  tar_target(bloco_w3_conteudo_figuras, salvar_figuras_bloco(
    bloco_w3_conteudo$figuras, list(w3_conteudo_diferencas = c(18, 9))
  ), format = "file"),
  tar_target(bloco_w3_conteudo_achados, salvar_tabela(bloco_w3_conteudo$achados, "achados_w3_2"), format = "file"),

  # Emenda 2: as realizações refazem o procedimento inteiro com as previsões
  # cruzadas, inclusive a escolha, e o bloco traz o achado do defeito e da
  # correção. É a avaliação da emenda 2 que decide o conteúdo daqui em diante.
  tar_target(
    w3_conteudo_e2_replicas,
    simular_avaliacao_do_conteudo_cruzado(
      w3_conteudo_sementes, pre_registro,
      n_consultores = parametros$n_consultores, n_projetos = parametros$n_projetos,
      data_referencia = parametros$data_referencia
    ),
    pattern = map(w3_conteudo_sementes)
  ),
  tar_target(w3_conteudo_e2_correlacao, medir_correlacao_do_mapa(
    dados_eda, pre_registro, w3_conteudo_e2_selecao_conferida
  )),
  tar_target(w3_conteudo_e2_dispersao, medir_dispersao_da_lista(
    dados_eda, pre_registro, w3_conteudo_selecao_conferida, w3_conteudo_e2_selecao_conferida
  )),
  # Que parte do teste tem projeto com fator na estimação: depende só dos
  # dados e da divisão, e por isso as realizações não refazem o modelo.
  tar_target(w3_projetos_com_fator, medir_projetos_com_fator(dados_eda, pre_registro)),
  tar_target(
    w3_projetos_com_fator_replicas,
    simular_projetos_com_fator(
      w3_conteudo_sementes, pre_registro,
      n_consultores = parametros$n_consultores, n_projetos = parametros$n_projetos,
      data_referencia = parametros$data_referencia
    ),
    pattern = map(w3_conteudo_sementes)
  ),
  tar_target(bloco_w3_conteudo_e2, executar_w3_conteudo_e2(
    w3_conteudo_e2_avaliacao, w3_conteudo_e2_replicas, w3_conteudo_selecao, w3_conteudo_avaliacao,
    w3_conteudo_e2_correlacao, w3_conteudo_e2_dispersao,
    list(versionado = w3_projetos_com_fator, replicas = w3_projetos_com_fator_replicas),
    pre_registro
  )),
  tar_target(bloco_w3_conteudo_e2_tabelas, salvar_tabelas_bloco(bloco_w3_conteudo_e2$tabelas), format = "file"),
  tar_target(bloco_w3_conteudo_e2_figuras, salvar_figuras_bloco(
    bloco_w3_conteudo_e2$figuras, list(w3_conteudo_e2_diferencas = c(18, 9))
  ), format = "file"),
  tar_target(bloco_w3_conteudo_e2_achados, salvar_tabela(
    bloco_w3_conteudo_e2$achados, "achados_w3_2_emenda_2"
  ), format = "file"),

  # Etapa 3, colaborativo: escolha dos hiperparâmetros de cada alfa na
  # validação temporal interna, só no período de estimação. O teste entra
  # depois de a escolha ser revista.
  tar_target(w3_colaborativo_selecao, selecionar_hiperparametros_do_colaborativo(dados_eda, pre_registro)),
  tar_target(w3_colaborativo_hiperparametros_tabela, salvar_tabela(
    tabelar_hiperparametros_do_colaborativo(w3_colaborativo_selecao), "w3_colaborativo_hiperparametros"
  ), format = "file"),

  # A sequência H1 e H2, com a escolha conferida contra a emenda: se a
  # escolha mudar, a pipeline para antes do teste. As realizações usam as
  # mesmas sementes do conteúdo e refazem a escolha em cada uma.
  tar_target(w3_colaborativo_selecao_conferida, conferir_escolha_do_colaborativo(
    w3_colaborativo_selecao, pre_registro
  )),
  tar_target(w3_colaborativo_avaliacao, avaliar_componente_colaborativo(
    dados_eda, pre_registro, w3_colaborativo_selecao_conferida
  )),
  tar_target(
    w3_colaborativo_replicas,
    simular_avaliacao_do_colaborativo(
      w3_conteudo_sementes, pre_registro,
      n_consultores = parametros$n_consultores, n_projetos = parametros$n_projetos,
      data_referencia = parametros$data_referencia
    ),
    pattern = map(w3_conteudo_sementes)
  ),
  tar_target(bloco_w3_colaborativo, executar_w3_colaborativo(
    w3_colaborativo_avaliacao, w3_colaborativo_replicas, pre_registro
  )),
  tar_target(bloco_w3_colaborativo_tabelas, salvar_tabelas_bloco(bloco_w3_colaborativo$tabelas), format = "file"),
  tar_target(bloco_w3_colaborativo_figuras, salvar_figuras_bloco(
    bloco_w3_colaborativo$figuras, list(w3_colaborativo_diferencas = c(18, 9))
  ), format = "file"),
  tar_target(bloco_w3_colaborativo_achados, salvar_tabela(bloco_w3_colaborativo$achados, "achados_w3_3"), format = "file"),

  # Etapa 4: o híbrido montado com o que os testes adotaram, a medida
  # descritiva contra o piso composto e ŝ, insumo da wave 4. ŝ fica só no
  # store do targets; nenhuma avaliação desta wave o lê.
  tar_target(w3_adocao, consolidar_adocao(w3_conteudo_e2_avaliacao, w3_colaborativo_avaliacao)),
  tar_target(w3_hibrido_avaliacao, avaliar_hibrido(dados_eda, pre_registro, w3_adocao)),
  tar_target(w3_s_hat, calcular_s_hat(dados_eda, pre_registro, w3_adocao)),
  tar_target(bloco_w3_hibrido, executar_w3_hibrido(w3_hibrido_avaliacao, w3_s_hat, pre_registro)),
  tar_target(bloco_w3_hibrido_tabelas, salvar_tabelas_bloco(bloco_w3_hibrido$tabelas), format = "file"),
  tar_target(bloco_w3_hibrido_achados, salvar_tabela(bloco_w3_hibrido$achados, "achados_w3_4"), format = "file"),

  # Etapa 5: plausibilidade das discordâncias, com as funções do bloco II.7.
  # A planilha da rubrica sai em branco; os vereditos são do autor.
  tar_target(w3_plausibilidade, avaliar_plausibilidade_das_discordancias(
    dados_eda, pre_registro, w3_conteudo_e2_avaliacao, w3_colaborativo_avaliacao, w3_adocao
  )),
  # A rubrica preenchida pelo autor fica fora da pipeline, que só a lê para
  # contar os vereditos; lida a cada corrida, para pegar a leitura nova.
  tar_target(w3_vereditos, ler_vereditos(), cue = tar_cue(mode = "always")),
  tar_target(bloco_w3_plausibilidade, executar_w3_plausibilidade(
    w3_plausibilidade, dados_eda, w3_vereditos, pre_registro
  )),
  tar_target(bloco_w3_plausibilidade_tabelas, salvar_tabelas_bloco(bloco_w3_plausibilidade$tabelas), format = "file"),
  tar_target(bloco_w3_plausibilidade_achados, salvar_tabela(
    bloco_w3_plausibilidade$achados, "achados_w3_5"
  ), format = "file"),

  # Etapa 6: estabilidade das grandezas centrais. O segundo nível e o híbrido
  # são refeitos em cada realização, com férias de semente derivada da
  # semente da realização e a adoção fixada na da versionada.
  tar_target(w3_segundo_nivel_versionado, medir_segundo_nivel(dados_eda, pre_registro, w3_adocao)),
  tar_target(
    w3_segundo_nivel_replicas,
    simular_segundo_nivel(
      w3_conteudo_sementes, pre_registro, w3_adocao,
      dias_ferias = parametros$dias_ferias, ano_ferias = parametros$ano_ferias,
      n_consultores = parametros$n_consultores, n_projetos = parametros$n_projetos,
      data_referencia = parametros$data_referencia
    ),
    pattern = map(w3_conteudo_sementes)
  ),
  tar_target(bloco_w3_estabilidade, executar_w3_estabilidade(
    dplyr::bind_cols(
      dplyr::rename_with(resumir_avaliacao_do_conteudo_cruzado(w3_conteudo_e2_avaliacao, pre_registro), \(n) paste0("conteudo_", n)),
      dplyr::rename_with(resumir_avaliacao_do_colaborativo(w3_colaborativo_avaliacao, pre_registro), \(n) paste0("colaborativo_", n)),
      w3_segundo_nivel_versionado
    ),
    juntar_realizacoes_w3(w3_conteudo_e2_replicas, w3_colaborativo_replicas, w3_segundo_nivel_replicas),
    estabilidade,
    pre_registro
  )),
  tar_target(bloco_w3_estabilidade_tabelas, salvar_tabelas_bloco(bloco_w3_estabilidade$tabelas), format = "file"),
  tar_target(bloco_w3_estabilidade_achados, salvar_tabela(
    bloco_w3_estabilidade$achados, "achados_w3_6"
  ), format = "file"),

  # Revisão da wave 3, só descritiva: a dependência entre pares do mesmo
  # consultor, as listas de 5, 10 e 50, o tamanho do filtro e a ocupação das
  # listas de ŝ. Nada aqui muda decisão.
  tar_target(w3_tamanho_do_filtro, medir_tamanho_do_filtro(dados_eda, pre_registro)),
  tar_target(w3_ocupacao_das_listas, medir_ocupacao_das_listas(w3_s_hat, dados_eda, pre_registro)),
  tar_target(bloco_w3_revisao, executar_w3_revisao(
    list(avaliacao = w3_conteudo_avaliacao, replicas = w3_conteudo_replicas),
    list(avaliacao = w3_conteudo_e2_avaliacao, replicas = w3_conteudo_e2_replicas),
    list(avaliacao = w3_colaborativo_avaliacao, replicas = w3_colaborativo_replicas),
    list(versionado = w3_segundo_nivel_versionado, replicas = w3_segundo_nivel_replicas),
    w3_tamanho_do_filtro, w3_ocupacao_das_listas, pre_registro
  )),
  tar_target(bloco_w3_revisao_tabelas, salvar_tabelas_bloco(bloco_w3_revisao$tabelas), format = "file"),
  tar_target(bloco_w3_revisao_achados, salvar_tabela(bloco_w3_revisao$achados, "achados_w3_revisao"), format = "file"),

  # Síntese da wave 3: empilha os achados das seis etapas e tira deles as
  # tabelas de decisões e limitações do Capítulo 5. Não recalcula nada.
  tar_target(bloco_w3_sintese, executar_w3_sintese(list(
    w3_achados_pisos, bloco_w3_conteudo$achados, bloco_w3_conteudo_e2$achados, bloco_w3_colaborativo$achados,
    bloco_w3_hibrido$achados, bloco_w3_plausibilidade$achados, bloco_w3_estabilidade$achados,
    bloco_w3_revisao$achados
  ))),
  tar_target(bloco_w3_sintese_tabelas, salvar_tabelas_bloco(bloco_w3_sintese$tabelas), format = "file")
)
