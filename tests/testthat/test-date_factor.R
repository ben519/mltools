context("date_factor")

test_that(desc = "dates are properly converted to factors", code = {
  
  # Convert vector of dates to ordered factor
  dts <- as.Date(c("2014-1-1", "2015-1-15", "2015-6-1"))
  result <- date_factor(dts, type = "yearmonth")
  
  expected <- factor(
    c("2014 Jan", "2015 Jan", "2015 Jun"), 
    levels = c(
      "2014 Jan", "2014 Feb", "2014 Mar", "2014 Apr", "2014 May", "2014 Jun", "2014 Jul", "2014 Aug", "2014 Sep", "2014 Oct", 
      "2014 Nov", "2014 Dec", "2015 Jan", "2015 Feb", "2015 Mar", "2015 Apr", "2015 May", "2015 Jun"
    ), 
    ordered = TRUE
  )
  
  expect_equal(result, expected)
})