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
executar_i1 <- function(dados, especificacao = especificacao_tabelas(),
                         estabilidade = NULL) {
  inventario <- inventariar_tabelas(dados, especificacao)
  divergencias <- verificar_conformidade(dados, especificacao)
  integridade <- verificar_integridade_referencial(dados, especificacao)
  cobertura <- resumir_cobertura(dados, especificacao)

  skills <- cobertura[cobertura$tabela == "skills_consultor" &
    cobertura$coluna == "consultor_id", ]
  integrais <- dplyr::filter(cobertura, cobertura == 1)
  cobertura_integral <- nrow(integrais)

  # Os sinais que vêm da campanha de formulários compartilham exatamente o
  # mesmo conjunto de respondentes, e é isso que torna a ausência de conteúdo
  # um bloco único de consultores em vez de ausências que se compensariam.
  #
  # A lista vem declarada de `tabelas_derivadas_de_formulario()`, e não de um
  # filtro sobre a cobertura medida: `alocacoes` e `ocupacao_consultor`
  # também têm cobertura parcial, por motivo inteiramente diferente, e um
  # filtro as arrastaria para dentro da afirmação.
  sinais_de_formulario <- tabelas_derivadas_de_formulario()
  cobertura_formulario <- cobertura |>
    dplyr::filter(coluna == "consultor_id", tabela %in% sinais_de_formulario)
  cobertura_unica_formulario <- length(unique(round(cobertura_formulario$cobertura, 6))) == 1

  objetos <- list(
    inventario = inventario,
    divergencias = divergencias,
    integridade = integridade,
    cobertura = cobertura,
    especificacao = especificacao,
    integrais = integrais,
    sinais_de_formulario = sinais_de_formulario,
    cobertura_formulario = cobertura_formulario,
    cobertura_unica_formulario = cobertura_unica_formulario
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

  achados <- registrar_achados("I.1", com_sintese(tibble::tribble(
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
      "não vale como propriedade do domínio. No dado real, a chave entre ",
      "entidades e alocação é a mesma (consulta Q4), mas parte dos ",
      "colaboradores históricos de alocação não tem registro de entidade: a ",
      "view usada na calibração casava 64% deles, e a fonte certa eleva isso a ",
      "cerca de 98%. Nem no melhor caso a integridade é completa."
    ),

    "I.1.3", "âncora real",
    paste0(
      "A ausência relevante não é célula vazia, é entidade sem registro. O ",
      "caso de maior consequência é o do conteúdo, e skills é o ",
      "representante dele com âncora real: nesta realização cobre ",
      formatar_pct(skills$cobertura), " dos consultores, a mesma cobertura ",
      "dos demais sinais da campanha de formulários (ver I.1.5)",
      if (is.null(estabilidade)) {
        "."
      } else {
        paste0(
          ", e sobre as realizações a fração **sem** nenhum atributo de ",
          "conteúdo declarado fica em ",
          formatar_intervalo(estabilidade, "fracao_sem_skills"), "."
        )
      }
    ),
    paste0(
      "Calibrado contra a medida real de 48,4%. Um componente baseado em ",
      "conteúdo opera sobre aproximadamente metade da população, e isso o ",
      "limita antes de qualquer escolha de algoritmo. A magnitude não é ",
      "estimada pelo sintético, é imposta a ele pela âncora real: ver I.0.2 ",
      "para por que a ausência de variação entre realizações não deve ser ",
      "lida como robustez."
    ),

    "I.1.5", "instrumento",
    paste0(
      "Os ", length(sinais_de_formulario), " sinais de conteúdo que a fonte ",
      "real deriva da campanha de formulários de skills (",
      paste(sinais_de_formulario, collapse = ", "), ") têm cobertura ",
      if (cobertura_unica_formulario) {
        paste0(
          "idêntica entre si, de ",
          formatar_pct(max(cobertura_formulario$cobertura)),
          ", porque são as mesmas pessoas que responderam"
        )
      } else {
        paste0(
          "divergente entre si, de ",
          formatar_pct(min(cobertura_formulario$cobertura)), " a ",
          formatar_pct(max(cobertura_formulario$cobertura)),
          ", o que o gerador não deveria produzir e indica que o sorteio de ",
          "respondentes deixou de ser compartilhado"
        )
      }, "."
    ),
    paste0(
      "A consequência é estrutural e não aritmética: o cold start do lado do ",
      "conteúdo é **um bloco único** de consultores sem nenhum atributo ",
      "declarado, e não ausências independentes por sinal que se ",
      "compensariam entre si. Trocar um sinal de conteúdo por outro não ",
      "recupera ninguém, o que decide a forma do híbrido no bloco II.3."
    ),

    "I.1.4", "consequência do gerador",
    paste0(
      cobertura_integral, " das ", nrow(cobertura),
      " referências têm cobertura integral por construção do gerador: ",
      paste0(integrais$tabela, ".", integrais$coluna, collapse = ", "), "."
    ),
    paste0(
      "Nenhuma delas tem calibração real de cobertura, e nenhuma análise ",
      "posterior deve tratá-las como universais no dado real sem dizer que ",
      "essa universalidade é artefato. A única exceção é ",
      "`idiomas_consultor`, cuja fonte é `vw_apdata_bi_entidades_globais`, ",
      "base corporativa sem nulo entre os 285 ativos: ali a cobertura ",
      "integral é âncora real, não construção."
    )
  ), tibble::tribble(
    ~codigo, ~decisao, ~limitacao, ~proveniencia,

    "I.1.2", NA,
    paste0(
      "Integridade referencial completa é artefato de construção e não ",
      "propriedade do domínio: no dado real, mesmo com a fonte certa, cerca de ",
      "2% dos colaboradores de alocação não têm registro de entidade."
    ),
    "fato real",

    "I.1.3", NA,
    paste0(
      "O componente baseado em conteúdo opera sobre aproximadamente metade da ",
      "população, e isso o limita antes de qualquer escolha de algoritmo."
    ),
    "fato real",

    "I.1.4", NA,
    paste0(
      "Seis referências têm cobertura integral por construção e nenhuma ",
      "calibração real, então nenhuma análise posterior pode tratá-las como ",
      "universais no dado real sem dizer que a universalidade é artefato."
    ),
    "depende do gerador",

    "I.1.5", NA,
    paste0(
      "O cold start do lado do conteúdo é um bloco único de consultores sem ",
      "nenhum atributo declarado, e não ausências independentes por sinal que ",
      "se compensariam: trocar um sinal de conteúdo por outro não recupera ",
      "ninguém."
    ),
    "fato real"
  )))

  list(objetos = objetos, tabelas = tabelas, figuras = figuras, achados = achados)
}
