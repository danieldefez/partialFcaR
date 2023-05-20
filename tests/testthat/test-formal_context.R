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

test_that("partialFcaR imports from CXT and CSV files", {

  # Read CSV
  filename <- system.file("contexts", "airlines.csv",
                          package = "partialFcaR")

  fc <- PartialFormalContext$new(filename)
  expect_is(fc, "PartialFormalContext")

  # Read CXT
  filename <- system.file("contexts", "lives_in_water.cxt",
                          package = "partialFcaR")

  fc <- PartialFormalContext$new(filename)
  expect_is(fc, "PartialFormalContext")

})

test_that("partialFcaR computes the dual formal context", {

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
  fc <- PartialFormalContext$new(I)

  fc2 <- fc$dual()
  expect_is(fc2, "PartialFormalContext")

  expect_equal(fc2$dim(), c(n_attributes, n_objects))
  expect_output(fc2$print())
  expect_equal(fc2$objects, fc$attributes)
  expect_equal(fc2$attributes, fc$objects)

})

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

  fc2 <- PartialFormalContext$new(planets)

  expect_error(fc2$to_latex(), NA)


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

test_that("partialFcaR imports formal contexts from arules", {

  skip_if_not_installed("arules")

  fc <- PartialFormalContext$new(I = Mushroom)

  expect_is(fc, "PartialFormalContext")

})


test_that("partialFcaR exports formal contexts to arules transactions", {

  skip_if_not_installed("arules")

  fc <- PartialFormalContext$new(I = Mushroom)

  expect_is(fc$to_transactions(), "transactions")

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

test_that("partialFcaR saves and loads formal contexts", {

  filename <- tempfile(fileext = ".RDS")

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

  expect_error(fc$save(filename = filename), NA)
  fc$find_implications()

  expect_error(fc$save(filename = filename), NA)

  expect_error(fc2 <- PartialFormalContext$new(), NA)
  expect_error(fc2$load(filename), NA)

  expect_error(fc2 <- PartialFormalContext$new(filename), NA)


})

# TODO: Revisar todo lo de las escalas

test_that("partialFcaR perform context scaling", {

  to_nominal <- sample(0:3, size = 10, replace = TRUE)
  to_ordinal <- sample(1:4, size = 10, replace = TRUE)
  to_interordinal <- sample(1:4, size = 10, replace = TRUE)
  to_interval <- runif(10)

  I <- cbind(nom = to_nominal,
             ord = to_ordinal,
             inter = to_interordinal,
             int = to_interval)

  fc <- PartialFormalContext$new(I)

  expect_error(fc$scale(attributes = "ord",
                        type = "ordinal"), NA)

  expect_error(fc$scale(attributes = "nom",
                        type = "nominal"), NA)

  expect_error(fc$scale(attributes = "inter",
                        type = "interordinal"), NA)

  expect_error(fc$scale(attributes = "int",
                        type = "interval",
                        values = c(0, -1, 1),
                        interval_names = c("low", "high")), NA)

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

test_that("partialFcaR clarifies and reduces contexts", {

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

  expect_error(fc2 <- fc$clarify(TRUE), NA)
  expect_error(fc$clarify(), NA)
  expect_error(fc$reduce())

  I2 <- I
  I2[I2 > 0] <- 1

  colnames(I2) <- attributes
  rownames(I2) <- objects

  fc <- PartialFormalContext$new(I2)

  # TODO: Revisar reduce
  expect_error(fc2 <- fc$reduce(TRUE), NA)
  expect_error(fc$reduce(), NA)

})

test_that("partialFcaR computes the standard context", {

  skip_on_os("solaris")

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

  expect_error(fc2 <- fc$standardize())

  expect_error(fc$find_implications(), NA)
  expect_error(fc2 <- fc$standardize(), NA)

  expect_is(fc2, "PartialFormalContext")
  expect_error(fc2$find_implications(), NA)

  expect_equal(fc$concepts$size(), fc2$concepts$size())
  # expect_error(fc$clarify(), NA)

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

  expect_error(fc$att_concept("P1"), NA)
  expect_error(fc$obj_concept("O3"), NA)

})
