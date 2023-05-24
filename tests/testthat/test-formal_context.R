context("Partial Formal Context")

if (requireNamespace("arules", quietly = TRUE)) {

  data("Mushroom", package = "arules")
  expect_warning(
    mush <- arules::apriori(Mushroom,
                            parameter = list(conf = 1,
                                             maxlen = 4)))

}

test_that("partialFcaR creates a formal context", {

  objects <- paste0("O", 1:6)
  n_objects <- length(objects)

  attributes <- paste0("P", 1:6)
  n_attributes <- length(attributes)

  I <- matrix(data = c(0, 1, -1, 0, 0, -1,
                       1, 1, -1, 0, 0, 0,
                       -1, 1, 0, 0, 1, 0,
                       -1, 0, 0, 1, -1, 0,
                       1, 0, 0, -1, 0, 0,
                       0, 0, 1, 0, 0, 0),
              nrow = n_objects,
              byrow = FALSE)

  colnames(I) <- attributes
  rownames(I) <- objects

  fc <- PartialFormalContext$new()
  expect_is(fc, "PartialFormalContext")
  expect_output(fc$print())

  fc <- PartialFormalContext$new(I = I)

  expect_is(fc, "PartialFormalContext")

  expect_equal(fc$dim(), c(n_objects, n_attributes))
  expect_output(fc$print())

  # Now, without names
  I <- matrix(data = c(0, 1, -1, 0, 0, -1,
                       1, 1, -1, 0, 0, 0,
                       -1, 1, 0, 0, 1, 0,
                       -1, 0, 0, 1, -1, 0,
                       1, 0, 0, -1, 0, 0,
                       0, 0, 0, 0, 0, 0),
              nrow = n_objects,
              byrow = FALSE)

  fc <- PartialFormalContext$new(I = I)

  expect_is(fc, "PartialFormalContext")

  # Return the incidence matrix
  expect_true(all(fc$incidence() == I))

})

# test_that("partialFcaR imports from CXT and CSV files", {
# 
#   # Read CSV
#   filename <- system.file("contexts", "airlines.csv",
#                           package = "partialFcaR")
# 
#   fc <- PartialFormalContext$new(filename)
#   expect_is(fc, "PartialFormalContext")
# 
#   # Read CXT
#   filename <- system.file("contexts", "lives_in_water.cxt",
#                           package = "partialFcaR")
# 
#   fc <- PartialFormalContext$new(filename)
#   expect_is(fc, "PartialFormalContext")
# 
# })
# 

test_that("partialFcaR imports a formal context with constant columns", {

  objects <- paste0("O", 1:6)
  n_objects <- length(objects)

  attributes <- paste0("P", 1:6)
  n_attributes <- length(attributes)

  I <- matrix(data = c(0, 1, -1, 0, 0, -1,
                       1, 1, 1, 1, 1, 1,
                       -1, 1, 0, 0, 1, 0,
                       -1, 0, 0, 1, -1, 0,
                       1, 0, 0, -1, 0, 0,
                       0, 0, 0, 0, 0, 0),
              nrow = n_objects,
              byrow = FALSE)

  colnames(I) <- attributes
  rownames(I) <- objects

  fc <- PartialFormalContext$new(I = I, remove_const = TRUE)

  expect_is(fc, "PartialFormalContext")

})

test_that("partialFcaR exports FormalContexts to LaTeX", {

  objects <- paste0("O", 1:6)
  n_objects <- length(objects)

  attributes <- paste0("P", 1:6)
  n_attributes <- length(attributes)

  I <- matrix(data = c(0, 1, -1, 0, 0, -1,
                       1, 1, 1, 1, 1, 1,
                       -1, 1, 0, 0, 1, 0,
                       -1, 0, 0, 1, -1, 0,
                       1, 0, 0, -1, 0, 0,
                       0, 0, 0, 0, 0, 0),
              nrow = n_objects,
              byrow = FALSE)

  colnames(I) <- attributes
  rownames(I) <- objects

  fc <- PartialFormalContext$new(I)

  expect_error(fc$to_latex(fraction = "frac"), NA)

})

test_that("partialFcaR extracts concepts", {

  objects <- paste0("O", 1:6)
  n_objects <- length(objects)

  attributes <- paste0("P", 1:6)
  n_attributes <- length(attributes)

  I <- matrix(data = c(0, 1, -1, 0, 0, -1,
                       1, 1, -1, 0, 0, 0,
                       -1, 1, 0, 0, 1, 0,
                       -1, 0, 0, 1, -1, 0,
                       1, 0, 0, -1, 0, 0,
                       0, 0, 1, 0, 0, 0),
              nrow = n_objects,
              byrow = FALSE)

  colnames(I) <- attributes
  rownames(I) <- objects

  fc <- PartialFormalContext$new(I = I)

  fc$find_concepts(verbose = TRUE)

  expect_is(fc$concepts, "ConceptLattice")

})

test_that("partialFcaR extracts implications", {

  objects <- paste0("O", 1:6)
  n_objects <- length(objects)

  attributes <- paste0("P", 1:6)
  n_attributes <- length(attributes)

  I <- matrix(data = c(0, 1, -1, 0, 0, -1,
                       1, 1, -1, 0, 0, 0,
                       -1, 1, 0, 0, 1, 0,
                       -1, 0, 0, 1, -1, 0,
                       1, 0, 0, -1, 0, 0,
                       0, 0, 1, 0, 0, 0),
              nrow = n_objects,
              byrow = FALSE)

  colnames(I) <- attributes
  rownames(I) <- objects

  fc <- PartialFormalContext$new(I = I)

  fc$find_implications(verbose = TRUE)

  expect_is(fc$implications, "ImplicationSet")

})

test_that("partialFcaR generate plots", {

  skip_on_cran()

  objects <- paste0("O", 1:6)
  n_objects <- length(objects)

  attributes <- paste0("P", 1:6)
  n_attributes <- length(attributes)

  I <- matrix(data = c(0, 1, -1, 0, 0, -1,
                       1, 1, -1, 0, 0, 0,
                       -1, 1, 0, 0, 1, 0,
                       -1, 0, 0, 1, -1, 0,
                       1, 0, 0, -1, 0, 0,
                       0, 0, 1, 0, 0, 0),
              nrow = n_objects,
              byrow = FALSE)

  colnames(I) <- attributes
  rownames(I) <- objects

  fc <- PartialFormalContext$new(I = I)

  fc$find_implications()

  expect_error(fc$plot(), NA)
  # expect_error(fc$plot(to_latex = TRUE), NA)
  # expect_error(fc$plot(to_latex = TRUE,
  #                      filename = "./test.tex",
  #                      caption = "Test",
  #                      label = "fig:test",
  #                      pointsize = 12), NA)

  fc <- PartialFormalContext$new()

  expect_error(fc$plot())


})


test_that("partialFcaR prints large formal contexts", {

  I <- matrix(data = sample(c(0, 1),
                            size = 400,
                            replace = TRUE),
              nrow = 20)
  colnames(I) <- paste0("ATT_", seq(ncol(I)))

  fc <- PartialFormalContext$new(I)
  expect_error(fc$print(), NA)
  expect_output(fc$print())

})


test_that("partialFcaR computes intents, extents and closures of Sets", {

  objects <- paste0("O", 1:6)
  n_objects <- length(objects)

  attributes <- paste0("P", 1:6)
  n_attributes <- length(attributes)

  I <- matrix(data = c(0, 1, -1, 0, 0, -1,
                       1, 1, -1, 0, 0, 0,
                       -1, 1, 0, 0, 1, 0,
                       -1, 0, 0, 1, -1, 0,
                       1, 0, 0, -1, 0, 0,
                       0, 0, 1, 0, 0, 0),
              nrow = n_objects,
              byrow = FALSE)

  colnames(I) <- attributes
  rownames(I) <- objects

  fc <- PartialFormalContext$new(I = I)
  fc$find_implications()

  c1 <- fc$concepts[2]$to_list()[[1]]
  expect_error(fc$extent(c1$get_intent()), NA)
  expect_error(fc$intent(c1$get_extent()), NA)
  expect_error(fc$closure(c1$get_intent()), NA)

  expect_warning(fc$intent(c1$get_intent()))
  expect_warning(fc$extent(c1$get_extent()))
  # expect_warning(fc$closure(c1$get_extent()))

  S <- Set$new(attributes = rev(attributes))
  S$assign(P6 = -1, P5 = -1)
  expect_warning(cl <- fc$closure(S))


})

test_that("partialFcaR checks for concepts", {

  objects <- paste0("O", 1:6)
  n_objects <- length(objects)

  attributes <- paste0("P", 1:6)
  n_attributes <- length(attributes)

  I <- matrix(data = c(0, 1, -1, 0, 0, -1,
                       1, 1, -1, 0, 0, 0,
                       -1, 1, 0, 0, 1, 0,
                       -1, 0, 0, 1, -1, 0,
                       1, 0, 0, -1, 0, 0,
                       0, 0, 1, 0, 0, 0),
              nrow = n_objects,
              byrow = FALSE)

  colnames(I) <- attributes
  rownames(I) <- objects

  fc <- PartialFormalContext$new(I = I)
  fc$find_concepts()

  for (i in seq(fc$concepts$size())) {

    C <- fc$concepts[i]$to_list()[[1]]

    expect_error(fc$is_closed(C$get_intent()), NA)
    expect_error(fc$is_concept(C), NA)
    expect_error(fc$extent(C$get_intent()), NA)
    expect_error(fc$intent(C$get_extent()), NA)
    expect_error(fc$closure(C$get_intent()), NA)

  }

})

test_that("partialFcaR computes object and attribute concepts", {

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

  fc <- PartialFormalContext$new(I)

  expect_error(fc$att_concept("P1", value = -1), NA)
  expect_error(fc$obj_concept("O3"), NA)

})
