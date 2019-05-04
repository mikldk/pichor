context("Data")

test_that("keys_coords", {
  expect_true(is(keys_coords, "pichor_key_koords"))
  
  # dput(colnames(keys_coords))
  expect_equal(colnames(keys_coords), 
               c("key", "key_color", 
                 "xmin", "ymin", "xmax", "ymax", 
                 "layer", "tones", 
                 "label", "label_x", "label_y", "label_color"))
})