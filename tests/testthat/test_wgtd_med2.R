source("~/jointcenteR/tests/testthat/test_data.R")

test_that("`wgtd_med2` produces weighted table", {
  result <- wgtd_med2(acs22, ten, hincp, wgtp)  
  expect_equal(result, medIncTen)
})

test_that("`wgtd_med2` produces unweighted table", {
  result <- wgtd_med2(acs22, ten, hincp)  
  expect_equal(result, medIncTen_unwgt)
})

test_that("`wgtd_med2` returns error if invalid df", {
  expect_error(wgtd_med2(fakedf, ten, hincp, wgtp))
})

test_that("`wgtd_med2` returns error if invalid var", {
  expect_error(wgtd_med2(acs22, fakevar, hincp, wgtp))
})

test_that("`wgtd_med2` returns error if invalid var2", {
  expect_error(wgtd_med2(acs22, ten, fakevar, wgtp))
})

test_that("`wgtd_med2` returns error if invalid weight provided", {
  expect_error(wgtd_med2(acs22, ten, hincp, fakewgtp))
})
