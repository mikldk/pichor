context("Data")

test_that("keys_chords", {
  expect_true(is(keys_chords, "pichor_key_koords"))
  
  # dput(colnames(keys_chords))
  expect_equal(colnames(keys_chords), 
               c("key", "key_color", 
                 "xmin", "ymin", "xmax", "ymax", 
                 "layer", "tones", 
                 "label", "label_x", "label_y", "label_color"))
})