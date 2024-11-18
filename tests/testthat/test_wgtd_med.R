source("~/jointcenteR/tests/testthat/test_data.R")

test_that("`wgtd_med` produces weighted table", {
  result <- wgtd_med(acs22, hincp, wgtp)  
  expect_equal(result, medInc)
})

test_that("`wgtd_med` produces unweighted table", {
  result <- wgtd_med(acs22, hincp)  
  expect_equal(result, medInc_unwgt)
})

test_that("`wgtd_med` returns error if invalid df", {
  expect_error(wgtd_med(fakedf, hincp, wgtp))
})

test_that("`wgtd_med` returns error if invalid var", {
  expect_error(wgtd_med(acs22, fakevar, wgtp))
})

test_that("`wgtd_med` returns error if invalid weight provided", {
  expect_error(wgtd_med(acs22, hincp, fakewgtp))
})
