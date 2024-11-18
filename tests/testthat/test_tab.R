source("~/jointcenteR/tests/testthat/test_data.R")

test_that("`tab` produces weighted table", {
  result <- tab(acs22, ten, wgtp)  
  expect_equal(result, tenTable)
})

test_that("`tab` produces unweighted table", {
  result <- tab(acs22, ten)  
  expect_equal(result, tenTable_unwgt)
})

test_that("`tab` returns error if invalid df", {
  expect_error(tab(fakedf, ten, wgtp))
})

test_that("`tab` returns error if invalid var", {
  expect_error(tab(acs22, fakevar, wgtp))
})

test_that("`tab` returns error if invalid weight provided", {
  expect_error(tab(acs22, ten, fakewgtp))
})