context("sparisfy")

test_that(desc = "sparsify correclty sparisfies data", code = {
  
  # Example inputs
  dt1 <- data.table(
    intCol = c(1L, NA_integer_, 3L, 0L),
    realCol = c(NA, 2, NA, NA),
    logCol = c(TRUE, FALSE, TRUE, FALSE),
    ofCol = factor(c("a", "b", NA, "b"), levels=c("a", "b", "c"), ordered=TRUE),
    ufCol = factor(c("a", NA, "c", "b"), ordered=FALSE)
  )
  dt2 <- data.table(ufCol = factor(c(NA, NA, NA), levels = c("a", "b"), ordered = FALSE))
  
  # Results
  result1 <- sparsify(dt1)
  result2 <- sparsify(dt1, sparsifyNAs=TRUE)
  result3 <- sparsify(dt1[, list(realCol)], naCols = "identify")
  result4 <- sparsify(dt1[, list(realCol)], naCols = "efficient")
  result5 <- sparsify(dt2, sparsifyNAs = FALSE)
  
  # Expected outputs
  expected1 <- new(
    "dgCMatrix", 
    i = c(0L, 1L, 2L, 0L, 1L, 2L, 3L, 0L, 2L, 0L, 1L, 2L, 3L, 0L, 1L, 1L, 3L, 1L, 2L), 
    p = c(0L, 3L, 7L, 9L, 13L, 15L, 17L, 19L), 
    Dim = c(4L, 7L), 
    Dimnames = list(NULL, c("intCol", "realCol", "logCol", "ofCol", "ufCol_a", "ufCol_b", "ufCol_c")), 
    x = c(1, NA, 3, NA, 2, NA, NA, 1, 1, 1, 2, NA, 2, 1, NA, NA, 1, NA, 1), 
    factors = list()
  )
  expected2 <- new(
    "dgCMatrix", 
    i = c(0L, 2L, 1L, 0L, 2L, 0L, 1L, 3L, 0L, 3L, 2L), 
    p = c(0L, 2L, 3L, 5L, 8L, 9L, 10L, 11L), 
    Dim = c(4L, 7L), 
    Dimnames = list(NULL, c("intCol", "realCol", "logCol", "ofCol", "ufCol_a", "ufCol_b", "ufCol_c")), 
    x = c(1, 3, 2, 1, 1, 1, 2, 2, 1, 1, 1), 
    factors = list()
  )
  expected3 <- new(
    "dgCMatrix", 
    i = c(0L, 2L, 3L, 0L, 1L, 2L, 3L), 
    p = c(0L, 3L, 7L), 
    Dim = c(4L, 2L), 
    Dimnames = list(NULL, c("realCol_NA", "realCol")), x = c(1, 1, 1, NA, 2, NA, NA), factors = list()
  )
  expected4 <- new(
    "dgCMatrix", 
    i = c(1L, 0L, 1L, 2L, 3L), 
    p = c(0L, 1L, 5L), 
    Dim = c(4L, 2L), 
    Dimnames = list(NULL, c("realCol_NotNA", "realCol")), 
    x = c(1, NA, 2, NA, NA), 
    factors = list()
  )
  expected5 <- new(
    "dgCMatrix", 
    i = c(0L, 1L, 2L, 0L, 1L, 2L), 
    p = c(0L, 3L, 6L), 
    Dim = 3:2, 
    Dimnames = list(NULL, c("ufCol_a", "ufCol_b")), 
    x = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), 
    factors = list()
  )
  
  # Checks
  expect_equal(result1, expected1)
  expect_equal(result2, expected2)
  expect_equal(result3, expected3)
  expect_equal(result4, expected4)
  expect_equal(result5, expected5)
})