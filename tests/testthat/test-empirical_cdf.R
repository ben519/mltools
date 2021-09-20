context("empirical_cdf")

test_that(desc = "empirical_cdf correctly bins data", code = {
  
  # Example inputs
  dt1 <- data.table(x=c(0.3, 1.3, 1.4, 3.6, 4.0), y=c(1.2, 1.2, 3.8, 3.9, 4.0))
  ub1 <- CJ(x=1:4, y=1:4)
  
  # Results
  result1 <- empirical_cdf(x = dt1, ubounds = ub1)
  
  # Expected outputs
  expected1 <- structure(list(
    x = c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4), 
    y = c(1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4), 
    N.cum = c(0L, 1L, 1L, 1L, 0L, 2L, 2L, 3L, 0L, 2L, 2L, 3L, 0L, 2L, 2L, 5L), 
    CDF = c(0, 0.2, 0.2, 0.2, 0, 0.4, 0.4, 0.6, 0, 0.4, 0.4, 0.6, 0, 0.4, 0.4, 1)
  ), 
  class = c("data.table", "data.frame"), 
  row.names = c(NA, -16L)
  )
  
  # Checks
  expect_equal(result1, expected1)
})
