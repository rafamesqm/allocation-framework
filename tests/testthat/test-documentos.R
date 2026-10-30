test_that("a guarda dos documentos barra valor inline vazio, NA ou n/a", {
  # O bloco I.2 publicou "()" e "contra ," no lugar de números, mais de uma
  # vez, porque citava grandezas que tinham deixado de existir. A guarda em
  # outputs/results/eda/_comum.R faz a renderização parar nesses casos. O de comprimento
  # zero é o que importa: o knitr o imprime vazio sem passar pelo gancho de
  # saída, e por isso a guarda fica no de avaliação.
  withr::defer(knitr::knit_hooks$restore())
  source(here::here("outputs", "results", "eda", "_comum.R"), local = new.env())

  renderizar <- function(texto) knitr::knit(text = texto, quiet = TRUE)

  expect_error(renderizar("antes `r numeric(0)` depois"), "Valor inline vazio")
  expect_error(renderizar("antes `r NA` depois"), "Valor inline vazio")
  expect_error(renderizar("antes `r 'n/a'` depois"), "Valor inline vazio")
  expect_match(renderizar("antes `r 1 + 1` depois"), "antes 2 depois")
})

test_that("as tabelas dos documentos saem com decimal em virgula", {
  # Coluna numerica crua saia do kable com ponto decimal, em 20 tabelas de 12
  # documentos, contra a regra de pt-BR em todo texto final.
  withr::defer(knitr::knit_hooks$restore())
  ambiente <- new.env()
  source(here::here("outputs", "results", "eda", "_comum.R"), local = ambiente)

  tabela <- ambiente$mostrar_tabela(tibble::tibble(x = 0.57, n = 1196430), "teste")
  texto <- paste(tabela, collapse = "\n")

  expect_match(texto, "0,57")
  expect_match(texto, "1.196.430", fixed = TRUE)
  expect_no_match(texto, "0.57", fixed = TRUE)
})
