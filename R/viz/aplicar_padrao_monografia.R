#' Aplica o padrão visual da monografia à sessão inteira
#'
#' Chamada uma vez, no início do documento de análise. Faz duas coisas que
#' garantem padronização sem depender de lembrar de repetir o tema em cada
#' figura: fixa `tema_monografia()` como tema default do ggplot2 e põe a
#' primeira cor da paleta como cor default das geometrias mais usadas.
#'
#' O efeito é de sessão, e por isso a chamada é explícita, no topo do
#' documento, nunca escondida dentro de outra função (ver docs/persona.md,
#' sem estado global implícito). A alternativa, somar o tema em cada uma das
#' dezenas de figuras do capítulo, é justamente onde a padronização escapa
#' quando alguém esquece uma.
#'
#' Só as geometrias de uma série só recebem cor default. Onde há mais de uma
#' série a cor vem de uma escala, mapeada a partir dos dados, e sobrescrever
#' default ali não faria sentido.
#'
#' @param tamanho_base Tamanho da fonte base, repassado a
#'   `tema_monografia()`.
#' @return `NULL`, de forma invisível. Chamada pelo efeito.
aplicar_padrao_monografia <- function(tamanho_base = 11) {
  paleta <- paleta_monografia()
  cor_primaria <- paleta$categorica[1]

  ggplot2::theme_set(tema_monografia(tamanho_base = tamanho_base))

  ggplot2::update_geom_defaults("bar", list(fill = cor_primaria))
  ggplot2::update_geom_defaults("col", list(fill = cor_primaria))
  ggplot2::update_geom_defaults("area", list(fill = cor_primaria))
  ggplot2::update_geom_defaults("histogram", list(fill = cor_primaria))
  ggplot2::update_geom_defaults("point", list(colour = cor_primaria))
  ggplot2::update_geom_defaults("line", list(colour = cor_primaria))
  ggplot2::update_geom_defaults("step", list(colour = cor_primaria))

  invisible(NULL)
}
