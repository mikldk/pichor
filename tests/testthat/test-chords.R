context("Chords")

test_that("get_key()", {
  expect_error(get_key(""))
  expect_error(get_key("X"))
  expect_error(get_key("?"))
  expect_error(get_key("CD"))
  expect_error(get_key("CB"))
  
  expect_equal(get_key("C"), 1)
  expect_equal(get_key("C#"), 2)
  expect_equal(get_key("Db"), 2)
  expect_equal(get_key("D"), 3)
  expect_equal(get_key("D#"), 4)
  expect_equal(get_key("Eb"), 4)
  expect_equal(get_key("E"), 5)
  expect_equal(get_key("F"), 6)
  expect_equal(get_key("F#"), 7)
  expect_equal(get_key("Gb"), 7)
  expect_equal(get_key("G"), 8)
  expect_equal(get_key("G#"), 9)
  expect_equal(get_key("Ab"), 9)
  expect_equal(get_key("A"), 10)
  expect_equal(get_key("A#"), 11)
  expect_equal(get_key("Bb"), 11)
  expect_equal(get_key("B"), 12)
})

test_that("get_next_key()", {
  expect_equal(get_next_key(origin = 1, distance = 0), 1)
  expect_equal(get_next_key(origin = 1, distance = 3), 4)
  expect_equal(get_next_key(origin = 3, distance = 3), 6)
  expect_equal(get_next_key(origin = 11, distance = 1), 12)
  expect_equal(get_next_key(origin = 11, distance = 2), 1)
  expect_equal(get_next_key(origin = 11, distance = 3), 2)
})

test_that("get_key_color()", {
  expect_equal(get_key_color(1), "white")
  expect_equal(get_key_color(2), "black")
  expect_equal(get_key_color(3), "white")
  expect_equal(get_key_color(4), "black")
  expect_equal(get_key_color(5), "white")
  expect_equal(get_key_color(6), "white")
  expect_equal(get_key_color(7), "black")
  expect_equal(get_key_color(8), "white")
  expect_equal(get_key_color(9), "black")
  expect_equal(get_key_color(10), "white")
  expect_equal(get_key_color(11), "black")
  expect_equal(get_key_color(12), "white")
})


test_that("get_tones()", {
  expect_equal(get_tones(1), "C")
  expect_equal(get_tones(2), "C#/Db")
  expect_equal(get_tones(2, sep = ":", as_string = TRUE), "C#:Db")
  expect_equal(get_tones(2, sep = "\n", as_string = TRUE), "C#\nDb")
  expect_equal(get_tones(2, as_string = FALSE), c("C#", "Db"))
  
  keys <- keys_coords %>% 
    dplyr::pull(key) %>% 
    unique()
  
  for (k in keys) {
    x <- get_tones(k, as_string = FALSE)
    
    for (k1 in x) {
      expect_equal(get_next_key(k, 0), get_key(k1), info = paste0("k = ", k, ", k1 = ", k1, ", get_key(k1) = ", get_key(k1)))
    }
  }
})


test_that("get_keys_from_sequence()", {
  expect_equal(get_keys_from_sequence(1, c(1, 2, 3)), 1:4)
})


test_that("construct_chord_raw()", {
  chd <- construct_chord_raw(root_tone = "C", distances_rel = c(4, 3))
  
  expect_equal(chd$root_tone, "C")
  expect_equal(chd$root_key, 1)
  expect_equal(chd$other_keys, cumsum(c(4, 3))+chd$root_key)
  
  expect_equal(as.character(chd), "C chord with tones C, E, G")
  expect_equal(as.character(chd, brief = TRUE), "C")
})

test_that("construct_chord_major()", {
  chd <- construct_chord_major(root_tone = "C")
  expect_equal(chd$root_tone, "C")
  expect_equal(chd$root_key, 1)
  expect_equal(chd$other_keys, cumsum(c(4, 3))+chd$root_key)
  
  expect_equal(as.character(chd), "C chord (major) with tones C, E, G")
  expect_equal(as.character(chd, brief = TRUE), "C")
})

test_that("construct_chord_minor()", {
  chd <- construct_chord_minor(root_tone = "C")
  expect_equal(chd$root_tone, "C")
  expect_equal(chd$root_key, 1)
  expect_equal(chd$other_keys, cumsum(c(3, 4))+chd$root_key)
  
  expect_equal(as.character(chd), "Cm chord (minor) with tones C, D#/Eb, G")
  expect_equal(as.character(chd, brief = TRUE), "Cm")
})


test_that("get_keys_next_inversion()", {
  expect_equal(get_keys_next_inversion(c(1, 5, 8)), c(5, 8, 1+12))
  expect_equal(get_keys_next_inversion(c(5, 8, 1+12)), c(8, 1+12, 5+12))
  expect_equal(get_keys_next_inversion(c(8, 1+12, 5+12)), c(1+12, 5+12, 8+12)-12)
})

test_that("get_keys()", {
  chd <- construct_chord_minor(root_tone = "F#")
  expect_equal(get_keys(chord = chd), c(7, 10, 14))
})


test_that("get_keys()", {
  chd <- construct_chord_minor(root_tone = "F#")
  expect_equal(get_keys_highest_tone(chord = chd, highest_tone = "A"), c(2, 7, 10))
})


test_that("get_keys_next_inversion()", {
  chd <- construct_chord_major(root_tone = "C")
  
  expect_equal(get_keys_inversion(chord = chd, inversion = 0L), c(1, 5, 8))
  expect_equal(get_keys_inversion(chord = chd, inversion = 1L), 
               get_keys_next_inversion(c(1, 5, 8)))
  expect_equal(get_keys_inversion(chord = chd, inversion = 2L), 
               get_keys_next_inversion(
                 get_keys_next_inversion(c(1, 5, 8))))
})


test_that("highlight_chord()", {
  chd1 <- construct_chord_major("D")
  d1 <- keys_coords %>% 
    highlight_chord(chord = chd1, highest_tone = "F#", color = "blue") %>% 
    filter(key_color == "blue")
  expect_equal(get_keys_inversion(chd1, 2L), d1 %>% pull(key))
  
  chd2 <- construct_chord_major("G")
  d2 <- keys_coords %>% 
    highlight_chord(chord = chd2, highest_tone = "B", color = "blue") %>% 
    filter(key_color == "blue")
  expect_equal(get_keys_inversion(chd2, 2L), d2 %>% pull(key))
})

