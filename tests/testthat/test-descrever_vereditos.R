test_that("os vereditos da escala vão para o plural", {
  expect_equal(pluralizar_veredito("plausível"), "plausíveis")
  expect_equal(pluralizar_veredito("plausível com ressalva"), "plausíveis com ressalva")
  expect_equal(pluralizar_veredito("implausível"), "implausíveis")
  expect_equal(pluralizar_veredito("indeterminável"), "indetermináveis")
})

test_that("ramo com um veredito só sai numa frase, e ramo misto sai contado", {
  vereditos <- tibble::tibble(
    Ramo = c(rep("colaborativo", 4), rep("conteúdo", 6)),
    Veredito = c(
      rep("implausível", 4),
      "plausível", rep("plausível com ressalva", 3), rep("implausível", 2)
    )
  )

  expect_equal(
    descrever_vereditos_do_ramo(vereditos, "colaborativo"),
    "os quatro casos do colaborativo foram julgados implausíveis"
  )
  expect_equal(
    descrever_vereditos_do_ramo(vereditos, "conteúdo"),
    "os seis do conteúdo, um plausível, três plausíveis com ressalva e dois implausíveis"
  )
})

test_that("sem rubrica preenchida, a leitura devolve NULL", {
  expect_null(ler_vereditos(file.path(withr::local_tempdir(), "ausente.csv")))
})
