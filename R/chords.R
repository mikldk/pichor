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
    keys[i] <- get_next_key(origin_key, distances_origin[i])
  }
  
  keys <- c(origin_key, keys)
  
  return(keys)
}

#' Get major of chord
#' 
#' @param root_tone root tone of chord
#' 
#' @importFrom magrittr "%>%"
#' @export
keys_major_chord <- function(root_tone) {
  keys <- get_keys_from_sequence(get_key(root_tone), c(4, 7))
  
  res <- tibble(root_tone = root_tone, 
                type = "major", 
                label = root_tone,
                keys = list(keys))
  
  class(res) <- c("chord", class(res))
  
  return(res)
}



