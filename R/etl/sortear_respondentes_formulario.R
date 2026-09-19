#' Sorteia o conjunto de consultores que respondeu às planilhas de skills
#'
#' Ponto único de verdade para a cobertura dos sinais derivados de
#' formulário. Existe porque a alternativa, cada gerador sortear sua própria
#' cobertura, produz uma estrutura errada: na fonte real
#' (`data/raw/skills/`), experiência setorial, expertise funcional, soft
#' skills e certificações vêm todas do mesmo Formulário de Skills 2.1, e as
#' skills técnicas das outras duas planilhas da mesma campanha. Quem não
#' respondeu não tem nenhum desses sinais, e não um subconjunto sorteado à
#' parte de cada vez.
#'
#' A consequência para o modelo é estrutural e não cosmética: o cold start do
#' lado do conteúdo é um bloco único de consultores sem nenhum atributo
#' declarado, não uma colcha de ausências independentes que se compensariam
#' entre si. Sortear de forma independente por sinal faria a interseção das
#' ausências parecer rara quando na verdade ela é a regra.
#'
#' Cobertura calibrada pela Sexta rodada (docs/mapeamento_schema.md): 138 dos
#' 285 ativos (48,4%) têm ao menos uma resposta de skills, medido por
#' interseção de hashes de e-mail. O sorteio é de um número fixo de
#' respondentes, e não um Bernoulli por consultor, para a contagem bater
#' exatamente com o agregado real em vez de flutuar em torno dele. Mesma
#' escolha feita em `gerar_tipo_contrato()`, pelo mesmo motivo.
#'
#' Ressalva de fidelidade: a cobertura de 48,4% é do conjunto das três
#' planilhas. A cobertura do Formulário 2.1 isolado, que é a fonte de três
#' dos sinais, nunca foi agregada e o acesso ao arquivo está encerrado
#' (docs/mapeamento_schema.md, fechamento da wave 1). Usar 48,4% para todos
#' é, portanto, um teto: a cobertura real dos sinais do 2.1 é igual ou menor.
#' A escolha é declarada e conservadora no sentido que importa, porque
#' superestimar a cobertura do conteúdo é o erro que levaria a uma conclusão
#' otimista sobre a arquitetura do híbrido.
#'
#' @param consultores tibble de `gerar_consultores()` (usa só `consultor_id`).
#' @param cobertura_populacional Proporção de consultores ativos com ao menos
#'   uma resposta de formulário.
#' @return Vetor de `consultor_id` dos respondentes.
sortear_respondentes_formulario <- function(consultores,
                                            cobertura_populacional = 0.484) {
  n_respondentes <- round(nrow(consultores) * cobertura_populacional)
  sample(consultores$consultor_id, size = n_respondentes)
}

#' Restringe uma tabela de atributo ao que o formulário de fato observou
#'
#' Separa o atributo latente da sua observação. O consultor que não
#' respondeu o formulário tem experiência setorial na realidade, ela
#' simplesmente não foi capturada, e é por isso que os geradores de atributo
#' seguem produzindo a tabela completa: o mecanismo de alocação responde à
#' experiência real da pessoa, não ao registro dela.
#'
#' O que o modelo enxerga, e portanto o que vai para `data/synthetic/`, é só
#' a parte observada. Tratar a ausência como ausência do atributo, e não como
#' dado faltante, mudaria a natureza do problema: cold start de conteúdo aqui
#' é problema de medida, não de inexistência.
#'
#' @param tabela tibble com coluna `consultor_id`.
#' @param respondentes Vetor de `consultor_id`, de
#'   `sortear_respondentes_formulario()`.
#' @return A tabela com apenas as linhas dos respondentes.
restringir_a_respondentes <- function(tabela, respondentes) {
  stopifnot("consultor_id" %in% names(tabela))
  dplyr::filter(tabela, .data$consultor_id %in% respondentes)
}
