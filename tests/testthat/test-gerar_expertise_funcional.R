test_that("gerar_expertise_funcional produz uma linha por consultor", {
  consultores <- gerar_consultores(n_consultores = 200)
  expertise <- gerar_expertise_funcional(consultores)

  expect_equal(nrow(expertise), nrow(consultores))
  expect_named(expertise, c("consultor_id", "expertise_funcional"))
  expect_setequal(expertise$consultor_id, consultores$consultor_id)
  expect_false(anyNA(expertise))
})

test_that("expertise_funcional fica dentro do catálogo de offerings", {
  consultores <- gerar_consultores(n_consultores = 200)
  expertise <- gerar_expertise_funcional(consultores)

  offerings_esperados <- c(
    "Experience & Efficiency", "Strategy and M&A",
    "Data Analytics & Artificial Intelligence", "Supply Chain",
    "Tech Consulting", "Finance", "Talent & Organization",
    "Digital Products & Platforms", "Cyber Security", "Digital Factory",
    "Peers.AI", "Educação Pública"
  )
  expect_true(all(expertise$expertise_funcional %in% offerings_esperados))
})
