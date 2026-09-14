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
  n_oportunidades = 4302 # soma do censo real por situação, ver mapeamento_schema.md
)

list(
  # Camada 1: Atributos -------------------------------------------------
  tar_target(espaco_skills, gerar_espaco_skills()),
  tar_target(espaco_skills_arquivo, salvar_synthetic(espaco_skills, "espaco_skills"), format = "file"),

  tar_target(consultores, gerar_consultores(
    n_consultores = parametros$n_consultores, semente = parametros$semente
  )),
  tar_target(consultores_arquivo, salvar_synthetic(consultores, "consultores"), format = "file"),

  tar_target(skills_consultor, gerar_skills_consultor(consultores, espaco_skills)),
  tar_target(skills_consultor_arquivo, salvar_synthetic(skills_consultor, "skills_consultor"), format = "file"),

  tar_target(experiencia_setorial, gerar_experiencia_setorial(consultores)),
  tar_target(experiencia_setorial_arquivo, salvar_synthetic(experiencia_setorial, "experiencia_setorial"), format = "file"),

  tar_target(expertise_funcional_arquivo, salvar_synthetic(
    gerar_expertise_funcional(consultores), "expertise_funcional"
  ), format = "file"),
  tar_target(idiomas_consultor_arquivo, salvar_synthetic(
    gerar_idiomas_consultor(consultores), "idiomas_consultor"
  ), format = "file"),
  tar_target(certificacoes_consultor_arquivo, salvar_synthetic(
    gerar_certificacoes_consultor(consultores), "certificacoes_consultor"
  ), format = "file"),
  tar_target(soft_skills_consultor_arquivo, salvar_synthetic(
    gerar_soft_skills_consultor(consultores), "soft_skills_consultor"
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

  tar_target(bloco_i1, executar_i1(dados_eda)),
  tar_target(bloco_i1_tabelas, salvar_tabelas_bloco(bloco_i1$tabelas), format = "file"),
  tar_target(bloco_i1_figuras, salvar_figuras_bloco(
    bloco_i1$figuras, list(i1_cobertura_entidades = c(18, 13))
  ), format = "file"),
  tar_target(bloco_i1_achados, salvar_tabela(bloco_i1$achados, "achados_i1"), format = "file"),

  tar_target(bloco_i2, executar_i2(dados_eda)),
  tar_target(bloco_i2_tabelas, salvar_tabelas_bloco(bloco_i2$tabelas), format = "file"),
  tar_target(bloco_i2_figuras, salvar_figuras_bloco(
    bloco_i2$figuras,
    list(i2_piramide_senioridade = c(16, 9), i2_projetos_distribuicoes = c(18, 8))
  ), format = "file"),
  tar_target(bloco_i2_achados, salvar_tabela(bloco_i2$achados, "achados_i2"), format = "file")
)
