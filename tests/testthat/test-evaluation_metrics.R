context("evaluation metrics")

test_that(desc = "evaluation metrics work properly", code = {
  
  x1 <- c(1, 2, 3)
  w1 <- c(1, 2, 1)
  y1 <- c(1.5, 2, 2.5)
  
  expect_equal(mse(preds = x1, actuals = y1, na.rm = FALSE), 0.1666667, tolerance = 0.0000001)
  expect_equal(mse(preds = x1, actuals = y1, na.rm = FALSE, weights = w1), 0.125)
  expect_equal(mse(preds = c(x1, NA), actuals = c(NA, y1), na.rm = TRUE), 0.625)
  
  expect_equal(rmse(preds = x1, actuals = y1, na.rm = FALSE), sqrt(0.1666667), tolerance = 0.0000001)
  expect_equal(rmse(preds = x1, actuals = y1, na.rm = FALSE, weights = w1), sqrt(0.125))
  expect_equal(rmse(preds = c(x1, NA), actuals = c(NA, y1), na.rm = TRUE), sqrt(0.625))
  
  expect_equal(msle(preds = x1, actuals = y1, na.rm = FALSE), 0.02254123)
  expect_equal(msle(preds = x1, actuals = y1, na.rm = FALSE, weights = w1), 0.01690592)
  expect_equal(msle(preds = c(x1, NA), actuals = c(NA, y1), na.rm = TRUE), 0.05800106)
  
  expect_equal(rmsle(preds = x1, actuals = y1, na.rm = FALSE), sqrt(0.02254123))
  expect_equal(rmsle(preds = x1, actuals = y1, na.rm = FALSE, weights = w1), sqrt(0.01690592))
  expect_equal(rmsle(preds = c(x1, NA), actuals = c(NA, y1), na.rm = TRUE), sqrt(0.05800106))
})