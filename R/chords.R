#' Get key for a tone
#'  
#' @param root_tone Root tone
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

#' Get next key
#' 
#' @param origin origin key
#' @param distance number of keys to move
#' 
#' @export
get_next_key <- function(origin, distance) {
  return(((origin - 1L + distance) %% 12L) + 1L)
}

#' Get key color
#' 
#' @param key key number
#' 
#' @importFrom dplyr filter pull distinct
#' 
#' @export
get_key_color <- function(key) {
  key_mod12 <- get_next_key(key, 0L)
  
  d_key_color <- tone_properties %>% 
    dplyr::filter(key == key_mod12) %>% 
    dplyr::distinct(key_color)
  
  if (nrow(d_key_color) != 1L) {
    stop("key '", key, "' reduced to '", key_mod12, "' not found")
  }
  
  key_color <- d_key_color %>% dplyr::pull(key_color)
  
  return(key_color)
}


# get_keys_from_sequence <- function(origin_key, distances_origin) {
#   keys <- integer(length(distances_origin))
#   
#   for (i in seq_along(keys)) {
#     #keys[i] <- get_next_key(origin_key, distances_origin[i])
#     keys[i] <- origin_key + distances_origin[i]
#   }
#   
#   keys <- c(origin_key, keys)
#   
#   return(keys)
# }
get_keys_from_sequence <- function(origin_key, distances_origin) {
  keys <- c(origin_key, origin_key + distances_origin)
  return(keys)
}

#' Construct a chord by root tone and key distances
#' 
#' @param root_tone root tone of chord
#' @param distances_rel relative distances: major is e.g. `c(4, 3)`, minor `c(3, 4)` etc.
#' @param chord_type a chord type, e.g. major, minor
#' @param label_suffix a suffix, e.g. `"m"` for minor
#' 
#' @importFrom magrittr "%>%"
#' @importFrom tibble tibble
#' @export
construct_chord_raw <- function(root_tone, distances_rel, 
                                chord_type = NULL,
                                label_suffix = NULL) {
  
  distances_from_root_tone <- cumsum(distances_rel)
  
  keys <- get_keys_from_sequence(get_key(root_tone), distances_from_root_tone)
  tones <- unlist(lapply(keys, get_tones))
  
  res <- list(
    root_tone = root_tone,
    root_key = keys[1L],
    other_tones = tones[-1L],
    other_keys = keys[-1L],
    distances_rel = distances_rel,
    distances_from_root_tone = distances_from_root_tone,
    all_keys = keys,
    all_tones = tones,
    chord_type = chord_type,
    label_suffix = label_suffix
    #inversion_no = 0L
  )
  
  class(res) <- c("pichor_chord_raw", "pichor_chord")
  
  return(res)
}

#' Get textual description of chord
#' 
#' @param x a `pichor_chord` from e.g. [construct_chord_raw()], 
#' [construct_chord_major()] or [construct_chord_minor()]
#' @param brief brief description
#' @param \dots currently not used
#' 
#' @export
as.character.pichor_chord <- function(x, brief = FALSE, ...) {
  label_suffix <- if (is.null(x$label_suffix)) "" else x$label_suffix
  
  if (brief) {
    str <- paste0(x$root_tone, label_suffix)
    return(str)
  }
  
  chord_type <- if (is.null(x$chord_type)) "" else paste0(" (", x$chord_type, ")")

  str <- paste0(x$root_tone, label_suffix, " chord", chord_type, " with tones ", 
      paste0(x$all_tones, collapse = ", "))
  
  return(str)
}

#' @export
print.pichor_chord <- function(x, brief = FALSE, ...) {
  cat(as.character(x, brief = brief), "\n", sep = "")
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
                             distances_rel = c(4L, 3L),
                             chord_type = "major",
                             label_suffix = NULL)
  
  class(res) <- c("pichor_chord_major", "pichor_chord")
  
  return(res)
}

#' Get major 7th of chord
#' 
#' @param root_tone root tone of chord
#' 
#' @importFrom magrittr "%>%"
#' @importFrom tibble tibble
#' @export
construct_chord_major_7 <- function(root_tone) {
  res <- construct_chord_raw(root_tone = root_tone, 
                             distances_rel = c(4L, 3L, 4L),
                             chord_type = "major 7th",
                             label_suffix = "maj7")
  
  class(res) <- c("pichor_chord_major_7", "pichor_chord")
  
  return(res)
}

#' Get dominant 7th of chord
#' 
#' @param root_tone root tone of chord
#' 
#' @importFrom magrittr "%>%"
#' @importFrom tibble tibble
#' @export
construct_chord_dominant_7 <- function(root_tone) {
  res <- construct_chord_raw(root_tone = root_tone, 
                             distances_rel = c(4L, 3L, 3L),
                             chord_type = "dominant 7th",
                             label_suffix = "dom7")
  
  class(res) <- c("pichor_chord_dominant_7", "pichor_chord")
  
  return(res)
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
                             distances_rel = c(3L, 4L),
                             chord_type = "minor",
                             label_suffix = "m")
  
  class(res) <- c("pichor_chord_minor", "pichor_chord")
  
  return(res)
}

get_keys_next_inversion <- function(keys) {
  new_keys <- c(keys[-1L], keys[1L] + 12L)
  
  if (all(new_keys > 12L)) {
    new_keys <- new_keys - 12L
  }
  
  return(new_keys)
}

#' Get keys for a chord
#' 
#' @param chord chord created by e.g. `construct_chord_*()`, i.e. [construct_chord_raw()], 
#' [construct_chord_major()], [construct_chord_minor()]
#' 
#' @importFrom methods is
#' 
#' @export
get_keys <- function(chord) {
  if (is.null(chord) || !methods::is(chord, "pichor_chord")) {
    stop("chord must be a pichor_chord")
  }
  
  return(chord$all_keys)
}

#' Get keys for an inversion with a specific highest tone
#' 
#' @inheritParams get_keys
#' @param highest_tone Higest tone in the inversion wanted to get keys for
#' 
#' @importFrom methods is
#' @importFrom utils tail
#' 
#' @export
get_keys_highest_tone <- function(chord, highest_tone) {
  if (is.null(chord) || !methods::is(chord, "pichor_chord")) {
    stop("chord must be a pichor_chord")
  }
  
  stopifnot(length(highest_tone) == 1L)
  
  pos_high <- if (nchar(highest_tone) == 1L) {
    which(chord$all_tones == highest_tone)
  } else {
    which(grepl(paste0("^", highest_tone, "/"), chord$all_tones) |
            grepl(paste0("/", highest_tone, "$"), chord$all_tones))  
  }
  
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
    
    x <- utils::tail(tones_inv, 1)
    
    if (nchar(highest_tone) == 1L) {
      if (x == highest_tone) {
        return(keys_inv)
      }
    } else {
      if (substr(x, 1, 2) == highest_tone) {
        return(keys_inv)
      }
      
      if (substr(x, 4, 5) == highest_tone) {
        return(keys_inv)
      } 
    }
  }
  
  stop("Unexpected that highest_tone was not found")
}

#' Get keys for an inversion
#' 
#' @inheritParams get_keys
#' @param inversion Inversion to get
#' 
#' @importFrom methods is
#' 
#' @export
get_keys_inversion <- function(chord, inversion = 0L) {
  if (is.null(chord) || !methods::is(chord, "pichor_chord")) {
    stop("chord must be a pichor_chord")
  }
  
  stopifnot(length(inversion) == 1L)
  
  if (any(inversion < 0L) || inversion > (length(chord$all_keys) - 1L)) {
    stop("invalid inversion")
  }
  
  # TODO: optimise
  keys <- chord$all_keys
  for (i in seq_len(inversion)) {
    keys <- get_keys_next_inversion(keys)
  }
  
  return(keys)
}



