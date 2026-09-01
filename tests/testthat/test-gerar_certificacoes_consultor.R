test_that("gerar_certificacoes_consultor produz uma tabela longa consistente", {
  consultores <- gerar_consultores(n_consultores = 300)
  set.seed(1)
  certificacoes <- gerar_certificacoes_consultor(consultores)

  expect_named(certificacoes, c("consultor_id", "certificacao"))
  expect_true(all(certificacoes$consultor_id %in% consultores$consultor_id))
  expect_false(anyNA(certificacoes))
})

test_that("nem todo consultor tem certificação", {
  consultores <- gerar_consultores(n_consultores = 300)
  set.seed(2)
  certificacoes <- gerar_certificacoes_consultor(consultores)

  expect_true(dplyr::n_distinct(certificacoes$consultor_id) < nrow(consultores))
})

test_that("certificacao fica dentro do catálogo genérico", {
  consultores <- gerar_consultores(n_consultores = 300)
  set.seed(3)
  certificacoes <- gerar_certificacoes_consultor(consultores)

  catalogo_esperado <- c("PMP", "Scrum Master", "AWS Certified", "Six Sigma", "ITIL", "Prince2", "SAFe")
  expect_true(all(certificacoes$certificacao %in% catalogo_esperado))
})
