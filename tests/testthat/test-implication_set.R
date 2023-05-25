context("ImplicationSet")

if (requireNamespace("arules", quietly = TRUE)) {

  data("Mushroom", package = "arules")
  expect_warning(
    mush <- arules::apriori(Mushroom,
                            parameter = list(conf = 1,
                                             maxlen = 4)))

  idx_redundant <- arules::is.redundant(mush)

  mush_clean <- mush[!idx_redundant]

}

test_that("fcaR prints implications", {

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

  expect_error(fc$implications[1:10]$print(), NA)
  expect_output(fc$implications[1:10]$print())

})

test_that("fcaR computes implication support", {

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
  expect_error(fc$implications$support(), NA)

  fc$find_implications()

  expect_error(fc$implications$support(), NA)
  expect_error(fc$implications$support(), NA)

})

test_that("fcaR exports implications to latex", {

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

  expect_error(fc$implications[1:10]$to_latex(), NA)

})

test_that("fcaR gets LHS and RHS of implications", {

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

  expect_is(fc$implications$get_LHS_matrix(), "dgCMatrix")
  expect_is(fc$implications$get_RHS_matrix(), "dgCMatrix")

})


test_that("fcaR filters and removes implications", {

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

  # TODO: FALLA el filtrado
  expect_error(fc$implications$filter(lhs = fc$attributes[1], rhs = fc$attributes[1:2]), NA)

  expect_warning(fc$implications$filter(lhs = fc$attributes[6], rhs = fc$attributes[3]))

  expect_error(fc$implications$filter(rhs = fc$attributes[1]), NA)
  expect_error(fc$implications$filter(lhs = fc$attributes[1:2]), NA)
  expect_error(fc$implications$filter(rhs = fc$attributes[1],
                                             drop = TRUE), NA)

  n <- fc$implications$cardinality()

  expect_error(imp2 <- fc$implications[-c(1:2)], NA)

  n2 <- imp2$cardinality()

  expect_equal(n2, n - 2)

})

test_that("fcaR adds implications from scratch", {

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

  fc$implications <- ImplicationSet$new(attributes = fc$attributes)
  expect_equal(fc$implications$cardinality(), 0)

  expect_output(print(fc$implications))

  lhs1 <- Set$new(attributes = fc$attributes)
  lhs1$assign(attributes = fc$attributes[1],
              values = 1)

  rhs1 <- Set$new(attributes = fc$attributes)
  rhs1$assign(fc$attributes[c(2,4)],
              values = c(1, 1))

  expect_error(fc$implications$add(lhs = lhs1, rhs = rhs1), NA)

})

test_that("fcaR filters implications", {

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

  fc$implications <- ImplicationSet$new(attributes = fc$attributes)
  expect_equal(fc$implications$cardinality(), 0)

  lhs1 <- Set$new(attributes = fc$attributes)
  lhs1$assign(attributes = fc$attributes[1],
              values = 1)

  rhs1 <- Set$new(attributes = fc$attributes)
  rhs1$assign(fc$attributes[c(2,4)],
              values = c(1, 1))

  fc$implications$add(lhs1, rhs1)

  expect_warning(fc$implications$filter(lhs = fc$attributes[5]))

})

test_that("fcaR subsets implications", {

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

  expect_error(fc$implications[fc$implications$support() > 0.1], NA)
  # TODO: FALLA
  expect_error(fc$implications[-c(1:2)], NA)
  expect_error(fc$implications[c(-1, 2)])
  expect_error(fc$implications[0], NA)

})
