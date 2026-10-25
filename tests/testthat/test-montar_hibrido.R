contexto_hibrido_de_teste <- function() {
  consultores <- paste0("c", 1:6)
  # c1, c2: setor A; c3: setor B; c4, c5, c6: não declararam.
  list(
    consultores = consultores,
    projetos = c("p1", "p2"),
    setor_do_consultor = c("A", "A", "B", NA, NA, NA),
    setor_do_projeto = c("A", "B"),
    chave = matrix(c(0.6, 0.5, 0.4, 0.3, 0.2, 0.1), 6, 2, dimnames = list(consultores, c("p1", "p2"))),
    lista = 2
  )
}

test_that("a lista comuta pelo quadrante e põe quem tem histórico antes", {
  contexto <- contexto_hibrido_de_teste()
  # Histórico: c2 (2 alocações), c4 (1), c5 (3). c1 e c3 só formulário; c6 zona morta.
  popularidade <- matrix(c(0, 2, 0, 1, 3, 0), 6, 2, dimnames = dimnames(contexto$chave))
  lista <- montar_listas_do_hibrido("p1", contexto, popularidade)

  # c3 declarou outro setor e c6 é zona morta: fora da lista.
  expect_equal(lista$consultor_id, c("c5", "c2", "c4", "c1"))
  expect_equal(lista$quadrante, c("só histórico", "histórico e formulário", "só histórico", "só formulário"))
  expect_equal(lista$ramo, c("colaborativo", "colaborativo", "colaborativo", "conteúdo"))
  expect_equal(lista$passa_filtro, c(NA, TRUE, NA, TRUE))
  expect_equal(lista$n_candidatos, rep(4L, 4))
})

test_that("só formulário é ordenado pela chave, depois de todo o histórico", {
  contexto <- contexto_hibrido_de_teste()
  popularidade <- matrix(0, 6, 2, dimnames = dimnames(contexto$chave))
  lista <- montar_listas_do_hibrido("p1", contexto, popularidade)
  # Sem histórico ninguém de fora do setor entra, e c2 vem antes de c1 pela chave.
  expect_equal(lista$consultor_id, c("c2", "c1"))
})

test_that("ŝ é o percentil na lista, em [0, 1]", {
  expect_equal(calcular_percentil_na_lista(1:5, 5), c(1, 0.75, 0.5, 0.25, 0))
  expect_equal(calcular_percentil_na_lista(1, 1), 1)
})

test_that("ŝ não tem valor fora do filtro e é ausente na zona morta", {
  skip_if_not(dir.exists(here::here("_targets")), "store do targets ausente")
  s_hat <- targets::tar_read(w3_s_hat, store = here::here("_targets"))

  expect_true(all(is.na(s_hat$s_hat[s_hat$quadrante == "zona morta"])))
  expect_false(anyNA(s_hat$s_hat[s_hat$quadrante != "zona morta"]))
  expect_true(all(s_hat$s_hat >= 0 & s_hat$s_hat <= 1, na.rm = TRUE))
  # Quem declarou e passou no filtro tem o setor do projeto; quem não
  # declarou não passa por filtro nenhum.
  expect_true(all(s_hat$passa_filtro[s_hat$quadrante %in% c("histórico e formulário", "só formulário")]))
  expect_true(all(is.na(s_hat$passa_filtro[s_hat$quadrante == "só histórico"])))
  expect_false(any(duplicated(s_hat[, c("projeto_id", "consultor_id")])))
})

test_that("a medida do híbrido coincide com o piso composto em cada quadrante", {
  # Com os dois testes sem rejeição, o colaborativo ordena pela popularidade e
  # o conteúdo deixa o filtro ao acaso: híbrido e piso composto são iguais por
  # construção, mas saem de implementações diferentes. Divergência aqui é
  # defeito numa delas, ou uma adoção nova que o híbrido não acompanhou.
  skip_if_not(dir.exists(here::here("_targets")), "store do targets ausente")
  avaliacao <- targets::tar_read(w3_hibrido_avaliacao, store = here::here("_targets"))

  expect_equal(avaliacao$adocao, list(colaborativo = "popularidade", conteudo = "acaso"))
  por_quadrante <- split(avaliacao$pares, avaliacao$pares$quadrante)
  for (quadrante in names(por_quadrante)) {
    pares <- por_quadrante[[quadrante]]
    expect_identical(pares$acerto_hibrido, as.integer(pares$acerto_piso_composto), info = quadrante)
  }
  expect_true(all(avaliacao$pares$acerto_hibrido[avaliacao$pares$quadrante == "zona morta"] == 0))
})
