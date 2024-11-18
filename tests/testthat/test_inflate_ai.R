test_that("`inflate_ai` returns expected values", {
  expect_equal(inflate_ai(1000, 2000, 2010), 1266.29)
  expect_equal(inflate_ai(500, 1960, 2020), 4375.51)
})

test_that("`inflate_ai` returns expected values for vector input", {
  expect_equal(inflate_ai(c(50, 200), 1960, 2000), c(291.12, 1164.50))
})

test_that("`inflate_ai` returns error if out of date bounds", {
  expect_error(inflate_ai(500, 1890, 2000))
  expect_error(inflate_ai(500, 1960, 2040))
})

test_that("`inflate_ai` stops if val is non-numeric", {
  expect_error(inflate_ai(c("quack!")))
  expect_error(inflate_ai(c("1", "2", "3")))
})

test_that("`inflate_ai` warns about NAs in input", {
  expect_error(inflate_ai(NA, 1960, 2000))
})