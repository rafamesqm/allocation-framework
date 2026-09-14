#' Figura: cobertura das tabelas satélite sobre consultores e projetos
#'
#' Mostra, por referência declarada, a fração das entidades que tem ao menos
#' um registro na tabela satélite. É a forma de ausência que importa neste
#' projeto, e o substituto do mapa de dados faltantes (ver
#' `resumir_cobertura()`).
#'
#' Barras horizontais e ordenadas da menor cobertura para a maior, porque o
#' que o leitor procura aqui é o pior caso e os rótulos são longos demais
#' para o eixo horizontal. Série única, sem legenda: a cor não codifica nada,
#' e usá-la para repetir o que o comprimento da barra já diz gastaria o canal
#' de identidade à toa.
#'
#' **O rótulo nomeia a entidade de destino, não só a tabela de origem.** As
#' barras não compartilham denominador: umas são fração de 285 consultores,
#' outras de 952 projetos, outras do catálogo de 140 skills. Sem a entidade
#' no rótulo, a figura parece comparar coisas comparáveis e não compara.
#'
#' Não leva linha de referência com a cobertura real de skills, embora ela
#' exista (48,4%, ver docs/mapeamento_schema.md). Uma linha vertical
#' atravessando um gráfico de barras categórico é lida como meta que vale
#' para todas as categorias, e esse número só diz respeito a uma delas. A
#' comparação com a âncora real fica no texto, onde o escopo dela pode ser
#' dito.
#'
#' @param cobertura tibble de `resumir_cobertura()`.
#' @return Objeto ggplot.
figura_cobertura_entidades <- function(cobertura) {
  dados_figura <- cobertura |>
    dplyr::mutate(
      entidade_destino = sub("[.].*$", "", entidade),
      rotulo = paste0(tabela, ".", coluna, " → ", entidade_destino),
      rotulo = forcats::fct_reorder(rotulo, cobertura)
    )

  ggplot2::ggplot(dados_figura, ggplot2::aes(cobertura, rotulo)) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::scale_x_continuous(
      labels = function(x) paste0(formatC(100 * x, format = "d"), "%"),
      limits = c(0, 1), expand = ggplot2::expansion(mult = c(0, 0.02))
    ) +
    rotular_figura(
      titulo = "Cobertura das tabelas satélite sobre as entidades",
      subtitulo = paste(
        "Fração da entidade de destino com ao menos um registro.",
        "Denominadores distintos por linha."
      ),
      eixo_x = "Cobertura da entidade referenciada",
      eixo_y = NULL
    )
}
