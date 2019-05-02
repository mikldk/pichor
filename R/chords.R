#' Get key for a tone
#'  
#' @importFrom dplyr filter pull
#' 
#' @export
get_key <- function(root_tone) {
  root_tone_key <- tone_properties %>% 
    dplyr::filter(tone == root_tone)
  
  if (nrow(root_tone_key) != 1L) {
    stop("root_tone '", root_tone, "' not found")
  }
  
  key <- root_tone_key %>% dplyr::pull(key)
  
  return(key)
}

#' Get tones for a key
#'  
#' @param key key to get tone(s) for
#' @param sep when returning string, chactecter to concatenate with
#' @param as_string whether to return tones as string concatenated with `sep` or as a vector
#' 
#' @importFrom dplyr filter pull
#' 
#' @export
get_tones <- function(key, sep = "/", as_string = TRUE) {
  key_mod12 <- get_next_key(origin = key, distance = 0)
  
  tone_key <- tone_properties %>% 
    dplyr::filter(key == key_mod12)
  
  if (nrow(tone_key) == 0L) {
    stop("key '", key, "' not found")
  }
  
  tones <- tone_key %>% dplyr::pull(tone)
  
  if (as_string) {
    return(paste0(tones, collapse = sep))
  }
  
  return(tones)
}

#' Get key color
#' 
#' @param key key number
#' 
#' @importFrom dplyr filter pull distinct
#' 
#' @export
get_key_color <- function(key) {
  key_mod12 <- ((key - 1) %% 12) + 1
  
  d_key_color <- tone_properties %>% 
    dplyr::filter(key == key_mod12) %>% 
    dplyr::distinct(key_color)
  
  if (nrow(d_key_color) != 1L) {
    stop("key '", key, "' reduced to '", key_mod12, "' not found")
  }
  
  key_color <- d_key_color %>% dplyr::pull(key_color)
  
  return(key_color)
}

#' Get next key
#' 
#' @param origin origin key
#' @param distance number of keys to move
#' 
#' @export
get_next_key <- function(origin, distance) {
  return(((origin - 1 + distance) %% 12) + 1)
}

get_keys_from_sequence <- function(origin_key, distances_origin) {
  keys <- integer(length(distances_origin))
  
  for (i in seq_along(keys)) {
    #keys[i] <- get_next_key(origin_key, distances_origin[i])
    keys[i] <- origin_key + distances_origin[i]
  }
  
  keys <- c(origin_key, keys)
  
  return(keys)
}

#' Construct a chord by root tone and key distances
#' 
#' @param root_tone root tone of chord
#' @param distances_rel relative distances: major is e.g. `c(4, 3)`, minor `c(3, 4)` etc.
#' 
#' @importFrom magrittr "%>%"
#' @importFrom tibble tibble
#' @export
construct_chord_raw <- function(root_tone, distances_rel) {
  distances_from_root_tone <- cumsum(distances_rel)
  
  keys <- get_keys_from_sequence(get_key(root_tone), distances_from_root_tone)
  tones <- unlist(lapply(keys, get_tones))
  
  res <- list(
    root_tone = root_tone,
    root_key = keys[1L],
    other_tones = tones[-1],
    other_keys = keys[-1],
    distances_rel = distances_rel,
    distances_from_root_tone = distances_from_root_tone,
    all_keys = keys,
    all_tones = tones#,
    #inversion_no = 0L
  )
  
  class(res) <- c("pichor_chord_raw", "pichor_chord")
  
  return(res)
}

print.pichor_chord <- function(x, ...) {
  cat("Chord with root tone ", 
      x$root_tone, " and then tones ", 
      paste0(x$other_tones, collapse = ", "), "\n", sep = "")
}

#' Get major of chord
#' 
#' @param root_tone root tone of chord
#' 
#' @importFrom magrittr "%>%"
#' @importFrom tibble tibble
#' @export
construct_chord_major <- function(root_tone) {
  res <- construct_chord_raw(root_tone = root_tone, 
                             distances_rel = c(4, 3))
  
  class(res) <- c("pichor_chord_major", "pichor_chord")
  
  return(res)
}
print.pichor_chord_major <- function(x, ...) {
  cat("Major ", x$root_tone, " chord with tones ", 
      paste0(x$other_tones, collapse = ", "), "\n", sep = "")
}

#' Get minor of chord
#' 
#' @param root_tone root tone of chord
#' 
#' @importFrom magrittr "%>%"
#' @importFrom tibble tibble
#' @export
construct_chord_minor <- function(root_tone) {
  res <- construct_chord_raw(root_tone = root_tone, 
                             distances_rel = c(3, 4))
  
  class(res) <- c("pichor_chord_minor", "pichor_chord")
  
  return(res)
}
print.pichor_chord_minor <- function(x, ...) {
  cat("Minor ", x$root_tone, "m chord with tones ", 
      paste0(x$other_tones, collapse = ", "), "\n", sep = "")
}

#' Highlight keys
#' 
#' @param data data with key coordinates, e.g. from [get_keys_coords()]
#' @param key key numbers, note that can be greater than 12
#' @param color highlight color
#' 
#' @importFrom dplyr mutate case_when
#' @importFrom magrittr "%>%"
#' 
#' @export
highlight_keys <- function(data, keys, color = "lightblue") {
  if (is.null(data) || !is(data, "pichor_key_koords")) {
    stop("data must be a pichor_key_koords")
  }
  
  unknown_keys <- setdiff(keys, data %>% pull(key))
  if (length(unknown_keys) > 0) {
    stop("Some keys were unknown: ", paste0(unknown_keys, collapse = ", "))
  }
  
  newdata <- data %>%
    dplyr::mutate(key_color = dplyr::case_when(
      key %in% keys ~ color,
      TRUE ~ key_color
      )) %>% 
    dplyr::mutate(label_color = case_when(
      key_color == "black" ~ "white",
      TRUE ~ "black"))
  
  return(newdata)
}
  
get_keys_next_inversion <- function(keys) {
  new_keys <- c(keys[-1L], keys[1L] + 12)
  
  if (all(new_keys > 12L)) {
    new_keys <- new_keys - 12L
  }
  
  return(new_keys)
}

get_keys <- function(chord) {
  if (is.null(chord) || !is(chord, "pichor_chord")) {
    stop("chord must be a pichor_chord")
  }
  
  return(chord$all_keys)
}

get_keys_highest_tone <- function(chord, highest_tone) {
  if (is.null(chord) || !is(chord, "pichor_chord")) {
    stop("chord must be a pichor_chord")
  }
  
  stopifnot(length(highest_tone) == 1L)
  
  pos_high <- which(chord$all_tones == highest_tone)
  
  if (length(pos_high) != 1L) {
    stop(highest_tone, " not found in chord")
  }
  
  keys <- chord$all_keys
  
  if (pos_high == length(chord$all_tones)) {
    return(keys)
  }
  
  max_inversions <- length(chord$all_keys) - 1L
  keys_inv <- keys
  
  for (i in seq_len(max_inversions)) {
    keys_inv <- get_keys_next_inversion(keys_inv)
    tones_inv <- unlist(lapply(keys_inv, get_tones))
    
    if (tail(tones_inv, 1) == highest_tone) {
      return(keys_inv)
    }
  }
  
  stop("Unexpected that highest_tone was not found")
}

get_keys_inversion <- function(chord, inversion = 0L) {
  if (is.null(chord) || !is(chord, "pichor_chord")) {
    stop("chord must be a pichor_chord")
  }
  
  stopifnot(length(inversion) == 1L)
  
  if (any(inversion < 0L) || inversion > (length(chord$all_keys) - 1L)) {
    stop("invalid inversion")
  }
  
  keys <- chord$all_keys
  
  for (i in seq_len(inversion)) {
    keys <- get_keys_next_inversion(keys)
  }
  
  return(keys)
}

#' Highlight coord
#' 
#' @param data data with key coordinates, e.g. from [get_keys_coords()]
#' @param chord chord to highlight, e.g. from [construct_chord_major()] or [construct_chord_raw()]
#' @param inversion inversion number
#' @param highest_tone if inversion by highest tone is wanted
#' @param color highlight color
#' 
#' @details If `highest_tone` is provided, then `inversion` is ignored
#' 
#' @export
highlight_chord <- function(data, chord, inversion = 0L, highest_tone = NULL, color = "lightblue") {
  if (is.null(data) || !is(data, "pichor_key_koords")) {
    stop("data must be a pichor_key_koords")
  }

  if (is.null(chord) || !is(chord, "pichor_chord")) {
    stop("chord must be a pichor_chord")
  }
  
  if (!is.null(highest_tone)) {
    keys <- get_keys_highest_tone(chord = chord, highest_tone = highest_tone)
    return(highlight_keys(data = data, keys = keys, color = color))
  } else if (!is.null(inversion)) {
    keys <- get_keys_inversion(chord = chord, inversion = inversion)
    return(highlight_keys(data = data, keys = keys, color = color))
  } else {
    stop("Please provide form or highest_tone")
  }
}



