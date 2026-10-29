#' Salva todas as tabelas de um bloco da EDA
#'
#' Grava uma lista nomeada de tabelas, usando o nome de cada elemento como
#' nome do arquivo. Devolve o vetor de caminhos, que é o que
#' `tar_target(..., format = "file")` precisa para rastrear os artefatos e
#' reconstruí-los quando algo a montante mudar.
#'
#' Esse rastreamento é o que faz a regra de coerência com o gerador
#' funcionar: mexer numa função de geração invalida o alvo do bloco, e a
#' pipeline regrava os artefatos sem depender de alguém lembrar.
#'
#' @param tabelas Lista nomeada de data frames já formatados para leitura.
#' @param diretorio Diretório de destino; por padrão, o de `diretorio_de_saida()`.
#' @return Vetor de caminhos dos arquivos gravados.
salvar_tabelas_bloco <- function(tabelas, diretorio = diretorio_de_saida("tables")) {
  vapply(
    names(tabelas),
    function(nome) salvar_tabela(tabelas[[nome]], nome, diretorio = diretorio),
    character(1),
    USE.NAMES = FALSE
  )
}

#' Salva todas as figuras de um bloco da EDA
#'
#' Par de `salvar_tabelas_bloco()` para o lado das figuras. As dimensões
#' podem ser dadas por figura, via `dimensoes`, porque figura de barras com
#' muitas categorias precisa de altura maior que um painel de histogramas, e
#' forçar um tamanho único espremeria os rótulos.
#'
#' @param figuras Lista nomeada de objetos ggplot.
#' @param dimensoes Lista nomeada opcional, cada elemento um vetor
#'   `c(largura_cm, altura_cm)` para a figura de mesmo nome.
#' @param diretorio Diretório de destino; por padrão, o de `diretorio_de_saida()`.
#' @return Vetor de caminhos dos arquivos gravados.
salvar_figuras_bloco <- function(figuras, dimensoes = list(),
                                  diretorio = diretorio_de_saida("figures")) {
  vapply(
    names(figuras),
    function(nome) {
      tamanho <- dimensoes[[nome]]
      if (is.null(tamanho)) tamanho <- c(16, 10)
      salvar_figura(
        figuras[[nome]], nome,
        largura_cm = tamanho[1], altura_cm = tamanho[2], diretorio = diretorio
      )
    },
    character(1),
    USE.NAMES = FALSE
  )
}
