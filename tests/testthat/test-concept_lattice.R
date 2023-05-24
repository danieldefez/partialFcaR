context("ConceptLattice")

objects <- paste0("O", 1:6)
n_objects <- length(objects)

attributes <- paste0("P", 1:6)
n_attributes <- length(attributes)

I <- matrix(data = c(0, 1, -1, 0, 0, -1,
                     0, 1, -1, 0, 0, -1,
                     -1, 1, 0, 0, 1, 0,
                     -1, 0, 0, 1, -1, 0,
                     1, 0, 0, -1, 0, 0,
                     0, 0, 1, 0, 0, 1),
            nrow = n_objects,
            byrow = FALSE)

colnames(I) <- attributes
rownames(I) <- objects

pfc <- PartialFormalContext$new(I)

test_that("partialFcaR uses empty concept lattices", {

  expect_error(pfc$concepts$print(), NA)
  expect_error(pfc$concepts, NA)

})

expect_error(pfc$find_concepts(), NA)

test_that("partialFcaR creates a ConceptLattice", {

  expect_is(pfc$concepts, "ConceptLattice")

})

test_that("partialFcaR plots a ConceptLattice", {

  skip_on_cran()
  skip_if_not_installed("hasseDiagram")
  expect_error(pfc$concepts$plot(), NA)
  expect_error(pfc$concepts$plot(object_names = FALSE), NA)

  # expect_error(pfc$concepts$plot(to_latex = TRUE), NA)
  # expect_error(pfc$concepts$plot(to_latex = TRUE,
  #                               filename = "./test.tex",
  #                               caption = "Test",
  #                               label = "fig:test",
  #                               pointsize = 12), NA)
  #
  # expect_error(pfc$concepts$plot(to_latex = TRUE,
  #                               object_names = TRUE,
  #                               filename = "./test2.tex",
  #                               caption = "Test",
  #                               label = "fig:test",
  #                               pointsize = 12), NA)


})

test_that("partialFcaR prints a ConceptLattice", {

  expect_output(pfc$concepts$print())

})

test_that("partialFcaR writes a ConceptLattice to LaTeX", {

  expect_error(pfc$concepts$to_latex(), NA)
  expect_error(pfc$concepts$to_latex(numbered = FALSE, align = FALSE), NA)
  #expect_error(pfc$concepts$to_latex(ncols = 2), NA)

  expect_error(pfc$concepts[2]$to_latex(), NA)

  expect_error(pfc$concepts[1:3]$to_latex(), NA)

})

test_that("partialFcaR extracts concepts from a ConceptLattice", {

  expect_error(L <- pfc$concepts[2:3], NA)
  expect_is(L, "ConceptSet")
  expect_is(L$to_list()[[1]], "Concept")
  expect_error(pfc$concepts[pfc$concepts$support() > 0.5], NA)

})

test_that("partialFcaR computes the sublattice of a ConceptLattice", {

  L <- pfc$concepts[2:3]

  expect_error(cl <- pfc$concepts$sublattice(2:3), NA)
  expect_is(cl, "ConceptLattice")

  expect_error(cl <- pfc$concepts$sublattice(L), NA)
  expect_is(cl, "ConceptLattice")

  L <- pfc$concepts[2]

  expect_error(cl <- pfc$concepts$sublattice(2), NA)
  expect_is(cl, "ConceptLattice")

  expect_error(cl <- pfc$concepts$sublattice(L), NA)
  expect_is(cl, "ConceptLattice")

  expect_error(cl <- pfc$concepts$sublattice(pfc$concepts$sub(2)), NA)
  expect_is(cl, "ConceptLattice")

  expect_error(cl <- pfc$concepts$sublattice(pfc$concepts$support() > 0.1), NA)
  expect_is(cl, "ConceptLattice")

})

test_that("partialFcaR computes the join- and meet- irreducibles of a ConceptLattice", {

  expect_error(ji <- pfc$concepts$join_irreducibles(), NA)
  expect_error(mi <- pfc$concepts$meet_irreducibles(), NA)

})

test_that("partialFcaR computes the suprema and infima of sets of concepts", {

  L <- pfc$concepts[2:3]

  expect_error(pfc$concepts$supremum(L), NA)
  expect_error(pfc$concepts$supremum(2:3), NA)
  expect_error(pfc$concepts$infimum(L), NA)
  expect_error(pfc$concepts$infimum(2:3), NA)

})

test_that("partialFcaR computes the subconcepts and superconcepts of a given concept", {

  L <- pfc$concepts[2:4]

  expect_error(pfc$concepts$subconcepts(L[3]), NA)
  expect_error(pfc$concepts$superconcepts(L[3]), NA)

})

test_that("partialFcaR computes the support of concepts", {

  expect_error(pfc$concepts$support(), NA)
  expect_error(pfc$concepts$support(), NA)

})

test_that("partialFcaR computes the size of a ConceptLattice", {

  expect_error(pfc$concepts$size(), NA)
  pfc <- PartialFormalContext$new(I)
  expect_error(sz <- pfc$concepts$size(), NA)
  expect_equal(sz, 0)

})

test_that("partialFcaR finds the lower and upper neighbours of a concept",
          {

            C <- pfc$concepts[2]
            expect_error(pfc$concepts$lower_neighbours(C), NA)
            expect_error(pfc$concepts$upper_neighbours(C), NA)

          })


test_that("partialFcaR decomposes concepts in its meet-irreducible elements", {

  L <- pfc$concepts[2:3]

  expect_error(cl <- pfc$concepts$decompose(L), NA)
  expect_is(cl, "list")
  expect_is(cl[[1]], "ConceptSet")

})
