#' Gera o espaço de skills sintético (objeto `S`, taxonomia)
#'
#' Reproduz a estrutura de categorias observada na aba De-para de
#' `CLTV_BD_Digital_Skills.xlsx` (ver docs/mapeamento_schema.md): skills
#' agrupadas em categorias, sem as 4 categorias que eram metadado (não skill)
#' na planilha real. Nomes de skill são placeholders (`categoria_n`), não
#' réplica dos nomes reais de ferramenta, porque o que importa para a EDA é a
#' forma da taxonomia (quantas categorias, quantas skills por categoria), não
#' o nome exato de cada tecnologia.
#'
#' `skill_adjacencia` é aspiracional (ver dicionário) e não é simulada aqui:
#' gerar uma matriz de similaridade sem base real seria inventar sinal que não
#' existe, não emular o esquema.
#'
#' Determinística (sem amostragem), por isso não recebe semente: a taxonomia
#' reproduz contagens fixas observadas na fonte real, não é sorteada.
#'
#' @return tibble com colunas `skill_id`, `categoria`, `nome`.
gerar_espaco_skills <- function() {
  categorias <- tibble::tribble(
    ~categoria, ~n_skills,
    "backend", 19L,
    "tecnologia_de_dados", 19L,
    "frontend", 18L,
    "banco_de_dados", 17L,
    "devops", 16L,
    "agilidade", 14L,
    "mobile", 13L,
    "plataformas_corporativas", 9L,
    "testes", 9L,
    "rpa", 6L
  )

  categorias |>
    dplyr::reframe(
      nome = paste0(categoria, "_", seq_len(n_skills)),
      .by = categoria
    ) |>
    dplyr::mutate(skill_id = paste0("skill_", dplyr::row_number()), .before = categoria)
}
