source("~/jointcenteR/tests/testthat/test_data.R")

test_that("`tab2` produces weighted table", {
  result <- tab2(acs22, ten, dis, wgtp)  
  expect_equal(result, tenDisTable)
})

test_that("`tab2` produces unweighted table", {
  result <- tab2(acs22, ten, dis)  
  expect_equal(result, tenDisTable_unwgt)
})

test_that("`tab2` returns error if invalid df", {
  expect_error(tab2(fakedf, ten, dis, wgtp))
})

test_that("`tab2` returns error if invalid var", {
  expect_error(tab2(acs22, fakevar, dis, wgtp))
})

test_that("`tab2` returns error if invalid var2", {
  expect_error(tab2(acs22, ten, fakevar, wgtp))
})

test_that("`tab2` returns error if invalid weight provided", {
  expect_error(tab2(acs22, ten, dis, fakewgtp))
})
