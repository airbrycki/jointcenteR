source("~/jointcenteR/tests/testthat/test_data.R")

test_that("`tab3` produces weighted table", {
  result <- tab3(acs22, region, ten, dis, wgtp)  
  expect_equal(result, regionTenDisTable)
})

test_that("`tab3` produces unweighted table", {
  result <- tab3(acs22, region, ten, dis)  
  expect_equal(result, regionTenDisTable_unwgt)
})

test_that("`tab3` returns error if invalid df", {
  expect_error(tab3(fakedf, region, ten, dis, wgtp))
})

test_that("`tab3` returns error if invalid var", {
  expect_error(tab3(acs22, fakevar, ten, dis, wgtp))
})

test_that("`tab3` returns error if invalid var2", {
  expect_error(tab3(acs22, ten, fakevar, dis, wgtp))
})

test_that("`tab3` returns error if invalid var3", {
  expect_error(tab3(acs22, region, ten, fakevar, wgtp))
})

test_that("`tab3` returns error if invalid weight provided", {
  expect_error(tab3(acs22, ten, dis, fakewgtp))
})

