#' Contrato de esquema das tabelas, em forma legível por máquina
#'
#' Transcrição de `docs/dicionario_de_dados.md` para uma tabela que o código
#' consegue conferir. O dicionário continua sendo a fonte de verdade em
#' prosa; esta função é o que permite verificar conformidade em vez de
#' afirmá-la, e é ela que faz a divergência entre contrato e dado aparecer
#' como resultado, não como suposição de quem escreve.
#'
#' Mantida deliberadamente plana (uma linha por coluna de cada tabela) em vez
#' de aninhada por tabela: é mais fácil de conferir linha a linha contra o
#' dicionário, que é o uso principal de quem audita.
#'
#' Vocabulário de `tipo`: `texto`, `numerico`, `data`, `logico`. É proposital
#' que seja mais grosso que as classes do R, porque o contrato é sobre
#' natureza do dado, não sobre representação (inteiro lido de CSV chega como
#' `numeric`, e isso não é divergência de esquema).
#'
#' `papel` distingue as colunas que formam a chave da tabela das demais.
#' `referencia` nomeia a coluna de origem, no formato `tabela.coluna`, das
#' colunas que são chave estrangeira.
#'
#' @return tibble com `tabela`, `coluna`, `tipo`, `papel` e `referencia`.
especificacao_tabelas <- function() {
  tibble::tribble(
    ~tabela, ~coluna, ~tipo, ~papel, ~referencia,

    # Camada 1: Atributos -------------------------------------------------
    "consultores", "consultor_id", "texto", "chave", NA_character_,
    "consultores", "nivel_senioridade", "texto", "atributo", NA_character_,
    "consultores", "tempo_casa", "numerico", "atributo", NA_character_,
    "consultores", "tempo_nivel", "numerico", "atributo", NA_character_,
    "consultores", "tipo_contrato", "texto", "atributo", NA_character_,
    "consultores", "localizacao", "texto", "atributo", NA_character_,
    "consultores", "fuso", "texto", "atributo", NA_character_,
    "consultores", "disposicao_viagem_nacional", "logico", "atributo", NA_character_,
    "consultores", "disposicao_viagem_internacional", "logico", "atributo", NA_character_,

    "espaco_skills", "skill_id", "texto", "chave", NA_character_,
    "espaco_skills", "categoria", "texto", "atributo", NA_character_,
    "espaco_skills", "nome", "texto", "atributo", NA_character_,

    "skills_consultor", "consultor_id", "texto", "chave", "consultores.consultor_id",
    "skills_consultor", "skill_id", "texto", "chave", "espaco_skills.skill_id",
    "skills_consultor", "proficiencia", "texto", "atributo", NA_character_,
    "skills_consultor", "recencia_dias", "numerico", "atributo", NA_character_,

    "experiencia_setorial", "consultor_id", "texto", "chave", "consultores.consultor_id",
    "experiencia_setorial", "segmento", "texto", "chave", NA_character_,
    "experiencia_setorial", "intensidade", "numerico", "atributo", NA_character_,

    "expertise_funcional", "consultor_id", "texto", "chave", "consultores.consultor_id",
    "expertise_funcional", "expertise_funcional", "texto", "atributo", NA_character_,

    "idiomas_consultor", "consultor_id", "texto", "chave", "consultores.consultor_id",
    "idiomas_consultor", "idioma", "texto", "chave", NA_character_,
    "idiomas_consultor", "nivel", "texto", "atributo", NA_character_,

    "certificacoes_consultor", "consultor_id", "texto", "chave", "consultores.consultor_id",
    "certificacoes_consultor", "certificacao", "texto", "chave", NA_character_,

    "soft_skills_consultor", "consultor_id", "texto", "chave", "consultores.consultor_id",
    "soft_skills_consultor", "competencia", "texto", "chave", NA_character_,
    "soft_skills_consultor", "nivel", "texto", "atributo", NA_character_,

    "projetos", "projeto_id", "texto", "chave", NA_character_,
    "projetos", "setor", "texto", "atributo", NA_character_,
    "projetos", "tipo_projeto", "texto", "atributo", NA_character_,
    "projetos", "valor_contrato", "numerico", "atributo", NA_character_,
    "projetos", "esforco_estimado", "numerico", "atributo", NA_character_,
    "projetos", "data_inicio", "data", "atributo", NA_character_,
    "projetos", "duracao_dias", "numerico", "atributo", NA_character_,
    "projetos", "data_fim", "data", "atributo", NA_character_,

    # Camada 2: Interações -------------------------------------------------
    "alocacoes", "consultor_id", "texto", "chave", "consultores.consultor_id",
    "alocacoes", "projeto_id", "texto", "chave", "projetos.projeto_id",
    "alocacoes", "papel", "texto", "atributo", NA_character_,
    "alocacoes", "pct_alocacao", "numerico", "atributo", NA_character_,
    "alocacoes", "horas_mes", "numerico", "atributo", NA_character_,
    "alocacoes", "data_inicio", "data", "atributo", NA_character_,
    "alocacoes", "duracao_dias", "numerico", "atributo", NA_character_,
    "alocacoes", "data_fim", "data", "atributo", NA_character_,

    "avaliacoes_desempenho", "consultor_id", "texto", "chave", "consultores.consultor_id",
    "avaliacoes_desempenho", "projeto_id", "texto", "chave", "projetos.projeto_id",
    "avaliacoes_desempenho", "item", "texto", "chave", NA_character_,
    "avaliacoes_desempenho", "etapa", "texto", "chave", NA_character_,
    "avaliacoes_desempenho", "categoria_item", "texto", "atributo", NA_character_,
    "avaliacoes_desempenho", "nota", "texto", "atributo", NA_character_,
    "avaliacoes_desempenho", "comentario", "texto", "atributo", NA_character_,

    # Camada 3: Operação ---------------------------------------------------
    "custo_carregado", "consultor_id", "texto", "chave", "consultores.consultor_id",
    "custo_carregado", "custo_carregado", "numerico", "atributo", NA_character_,

    "ocupacao_consultor", "consultor_id", "texto", "chave", "consultores.consultor_id",
    "ocupacao_consultor", "ano_mes", "texto", "chave", NA_character_,
    "ocupacao_consultor", "pct_ocupacao", "numerico", "atributo", NA_character_,
    "ocupacao_consultor", "horas_mes", "numerico", "atributo", NA_character_,
    "ocupacao_consultor", "n_alocacoes", "numerico", "atributo", NA_character_,

    "ferias_consultor", "consultor_id", "texto", "chave", "consultores.consultor_id",
    "ferias_consultor", "inicio_ferias", "data", "atributo", NA_character_,
    "ferias_consultor", "fim_ferias", "data", "atributo", NA_character_,

    "margem_frente_mes", "projeto_id", "texto", "chave", "projetos.projeto_id",
    "margem_frente_mes", "ano_mes", "texto", "chave", NA_character_,
    "margem_frente_mes", "receita", "numerico", "atributo", NA_character_,
    "margem_frente_mes", "custo", "numerico", "atributo", NA_character_,
    "margem_frente_mes", "margem", "numerico", "atributo", NA_character_,

    "margem_estrutural", "projeto_id", "texto", "chave", "projetos.projeto_id",
    "margem_estrutural", "valor_contrato", "numerico", "atributo", NA_character_,
    "margem_estrutural", "custo_total", "numerico", "atributo", NA_character_,
    "margem_estrutural", "margem_estrutural", "numerico", "atributo", NA_character_,

    "pipeline_demandas", "oportunidade_id", "texto", "chave", NA_character_,
    "pipeline_demandas", "estagio", "texto", "atributo", NA_character_,

    # Camada 4: Relacional -------------------------------------------------
    "grafo_coalocacao", "consultor_a", "texto", "chave", "consultores.consultor_id",
    "grafo_coalocacao", "consultor_b", "texto", "chave", "consultores.consultor_id",
    "grafo_coalocacao", "n_projetos_em_comum", "numerico", "atributo", NA_character_,

    # Validação ------------------------------------------------------------
    "validacao_utilidade", "categoria", "texto", "atributo", NA_character_,
    "validacao_utilidade", "medida", "texto", "chave", NA_character_,
    "validacao_utilidade", "real", "numerico", "atributo", NA_character_,
    "validacao_utilidade", "sintetico", "numerico", "atributo", NA_character_,
    "validacao_utilidade", "diferenca_relativa", "numerico", "atributo", NA_character_
  )
}

#' Traduz a classe de um vetor do R para o vocabulário de tipo do contrato
#'
#' O contrato fala de natureza do dado, não de representação: inteiro e
#' dupla são os dois `numerico`, porque a diferença entre eles não é uma
#' divergência de esquema. Ver `especificacao_tabelas()`.
#'
#' @param coluna Vetor de qualquer tipo.
#' @return Um de `"texto"`, `"numerico"`, `"data"`, `"logico"` ou, quando não
#'   reconhecido, a própria classe, para a divergência ficar visível em vez
#'   de virar `NA`.
classificar_tipo <- function(coluna) {
  if (inherits(coluna, "Date")) return("data")
  if (is.logical(coluna)) return("logico")
  if (is.numeric(coluna)) return("numerico")
  if (is.character(coluna) || is.factor(coluna)) return("texto")
  class(coluna)[1]
}
