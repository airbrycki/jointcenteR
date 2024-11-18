test_that("`inflate_ls` returns expected values", {
  expect_equal(inflate_ls(1000, 2000, 2010), 1258.97)
  expect_equal(inflate_ls(500, 1960, 2020), 3765.79)
})

test_that("`inflate_ls` returns expected values for vector input", {
  expect_equal(inflate_ls(c(50, 200), 1960, 2000), c(264.81, 1059.23))
})

test_that("`inflate_ls` returns error if out of date bounds", {
  expect_error(inflate_ls(500, 1890, 2000))
  expect_error(inflate_ls(500, 1960, 2040))
})

test_that("`inflate_ls` returns error if negative rent entered", {
  expect_error(inflate_ls(-10, 1890, 2000))
  expect_error(inflate_ls(0, 1960, 2040))
})

test_that("`inflate_ls` stops if val is non-numeric", {
  expect_error(inflate_ls(c("quack!")))
  expect_error(inflate_ls(c("1", "2", "3")))
})

test_that("`inflate_ls` warns about NAs in input", {
  expect_error(inflate_ls(NA, 1960, 2000))
})
