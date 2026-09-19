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
  fracao_com_avaliacao = 0.5,
  dias_ferias = 30,
  ano_ferias = 2026,
  n_oportunidades = 4302, # soma do censo real por situação, ver mapeamento_schema.md
  n_replicas_estabilidade = 100 # realizações para o intervalo de Monte Carlo do bloco I.0
)

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
  # `vw_apdata_bi_entidades_globais`, base corporativa sem nulo entre os 285
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
  tar_target(projetos_arquivo, salvar_synthetic(projetos, "projetos"), format = "file"),

  # Camada 2: Interações --------------------------------------------------
  tar_target(alocacoes, gerar_alocacoes(
    consultores, projetos, experiencia_setorial,
    semente = parametros$semente,
    fracao_cold_start_consultor = parametros$fracao_cold_start,
    fracao_cold_start_projeto = parametros$fracao_cold_start,
    prob_vies = parametros$prob_vies
  )),
  tar_target(alocacoes_arquivo, salvar_synthetic(alocacoes, "alocacoes"), format = "file"),

  tar_target(avaliacoes_desempenho_arquivo, salvar_synthetic(
    gerar_avaliacoes_desempenho(
      alocacoes,
      fracao_com_avaliacao = parametros$fracao_com_avaliacao,
      semente = parametros$semente
    ),
    "avaliacoes_desempenho"
  ), format = "file"),

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
  tar_target(margem_frente_mes_arquivo, salvar_synthetic(
    gerar_margem_frente_mes(projetos, semente = parametros$semente),
    "margem_frente_mes"
  ), format = "file"),
  tar_target(margem_estrutural_arquivo, salvar_synthetic(
    gerar_margem_estrutural(alocacoes, custo_carregado, projetos), "margem_estrutural"
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
      soft_skills_consultor_arquivo, avaliacoes_desempenho_arquivo,
      ocupacao_consultor_arquivo, ferias_consultor_arquivo,
      margem_frente_mes_arquivo, margem_estrutural_arquivo,
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
  tar_target(bloco_i0, executar_i0(replicas_estabilidade)),
  tar_target(bloco_i0_tabelas, salvar_tabelas_bloco(bloco_i0$tabelas), format = "file"),
  tar_target(bloco_i0_figuras, salvar_figuras_bloco(
    bloco_i0$figuras, list(i0_estabilidade = c(17, 14))
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
  tar_target(bloco_ii4_achados, salvar_tabela(bloco_ii4$achados, "achados_ii4"), format = "file")
)
