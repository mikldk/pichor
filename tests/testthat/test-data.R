context("Data")

test_that("get_keys_coords()", {
  d <- get_keys_coords()

  expect_true(is(d, "pichor_key_koords"))
})