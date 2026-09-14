#' Executa o bloco I.1 da EDA, conformidade e volumetria
#'
#' Concentra num só lugar tudo que o bloco calcula, formata, desenha e
#' conclui. O documento em `analysis/` apenas lê este resultado e narra, e
#' por isso não recalcula nada: análise que roda duas vezes, uma na pipeline
#' e outra no texto, é análise que pode divergir de si mesma.
#'
#' É essa concentração que faz a regra de coerência funcionar. Qualquer
#' mudança no gerador invalida este alvo no `targets`, que reconstrói todos
#' os artefatos do bloco. Falta só re-renderizar o documento, que é um passo
#' explícito e único (ver `analysis/_renderizar.R`), deliberadamente fora da
#' pipeline para `tar_make()` não exigir Pandoc de quem audita.
#'
#' **Os enunciados dos achados embutem os valores calculados**, em vez de
#' citarem números escritos à mão. Um número que mude depois de uma correção
#' no gerador não tem como ficar escondido dentro de uma frase estática.
#'
#' @param dados Lista nomeada de tibbles, de `carregar_dados()`.
#' @param especificacao Contrato de esquema, de `especificacao_tabelas()`.
#' @return Lista com `objetos` (tibbles de análise), `tabelas` (formatadas
#'   para leitura), `figuras` (ggplots) e `achados` (registro validado).
executar_i1 <- function(dados, especificacao = especificacao_tabelas()) {
  inventario <- inventariar_tabelas(dados, especificacao)
  divergencias <- verificar_conformidade(dados, especificacao)
  integridade <- verificar_integridade_referencial(dados, especificacao)
  cobertura <- resumir_cobertura(dados, especificacao)

  skills <- cobertura[cobertura$tabela == "skills_consultor" &
    cobertura$coluna == "consultor_id", ]
  cobertura_integral <- sum(cobertura$cobertura == 1)

  objetos <- list(
    inventario = inventario,
    divergencias = divergencias,
    integridade = integridade,
    cobertura = cobertura,
    especificacao = especificacao
  )

  tabelas <- list(
    i1_inventario = inventario |>
      dplyr::transmute(
        Tabela = tabela,
        Linhas = formatar_inteiro(linhas),
        Colunas = colunas,
        `Chave declarada` = chave,
        `Chave única` = ifelse(chave_unica, "sim", "NÃO")
      ),
    i1_divergencias_contrato = divergencias |>
      dplyr::transmute(
        Tabela = tabela, Coluna = coluna,
        Divergência = tipo_divergencia, Detalhe = detalhe
      ),
    i1_integridade_referencial = integridade |>
      dplyr::transmute(
        Tabela = tabela, Coluna = coluna, Referência = referencia,
        `Valores distintos` = formatar_inteiro(valores_distintos),
        Órfãos = orfaos
      ),
    i1_cobertura_entidades = cobertura |>
      dplyr::transmute(
        Tabela = tabela, `Entidade referenciada` = entidade,
        `Com registro` = com_registro, Total = total,
        `Sem registro` = sem_registro, Cobertura = formatar_pct(cobertura)
      )
  )

  figuras <- list(
    i1_cobertura_entidades = figura_cobertura_entidades(cobertura)
  )

  achados <- registrar_achados("I.1", tibble::tribble(
    ~codigo, ~etiqueta, ~enunciado, ~implicacao,

    "I.1.1", "instrumento",
    paste0(
      "O conjunto sintético está conforme o contrato de esquema: ",
      nrow(especificacao), " colunas declaradas em ",
      dplyr::n_distinct(especificacao$tabela), " tabelas, ",
      nrow(divergencias), " divergências de nome ou tipo, e todas as ",
      nrow(inventario), " chaves declaradas são únicas no dado."
    ),
    paste0(
      "A conformidade passa a ser verificada por código a cada execução, e o ",
      "mesmo teste falha se gerador e dicionário divergirem no futuro."
    ),

    "I.1.2", "consequência do gerador",
    paste0(
      "A integridade referencial é completa: ", nrow(integridade),
      " referências declaradas, ", sum(integridade$orfaos), " chaves órfãs."
    ),
    paste0(
      "Decorre de o gerador construir as satélites a partir das entidades e ",
      "não vale como propriedade do domínio: no dado real o join entre ",
      "entidades e alocação cobre apenas 64% dos colaboradores."
    ),

    "I.1.3", "âncora real",
    paste0(
      "A ausência relevante não é célula vazia, é entidade sem registro, e ",
      "varia de cobertura integral a ", formatar_pct(min(cobertura$cobertura)),
      " no pior caso. Skills cobre ", formatar_pct(skills$cobertura),
      " dos consultores, com ", skills$sem_registro, " dos ", skills$total,
      " sem nenhum atributo técnico."
    ),
    paste0(
      "Calibrado contra a medida real de 48,4%. Qualquer componente baseado ",
      "em conteúdo opera sobre menos da metade da população, o que limita o ",
      "content-based antes de qualquer escolha de algoritmo."
    ),

    "I.1.4", "consequência do gerador",
    paste0(
      cobertura_integral, " das ", nrow(cobertura),
      " referências têm cobertura integral por construção do gerador, entre ",
      "elas custo, expertise funcional, férias, idiomas e as duas de margem."
    ),
    paste0(
      "Nenhuma delas tem calibração real de cobertura, e nenhuma análise ",
      "posterior deve tratá-las como universais no dado real sem dizer que ",
      "essa universalidade é artefato."
    )
  ))

  list(objetos = objetos, tabelas = tabelas, figuras = figuras, achados = achados)
}
