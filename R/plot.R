#' Plot piano
#' 
#' @param data data to plot, default `keys_coords` which is a piano
#' @param labels whether to show key labels
#' @param \dots Currently not used
#' 
#' @importFrom ggplot2 ggplot aes scale_fill_identity scale_color_identity theme_void geom_rect geom_text
#' @importFrom dplyr pull 
#' 
#' @export
ggpiano <- function(data = keys_coords, labels = TRUE, ...) {
  envir <- parent.frame()
  
  p <- ggplot2::ggplot(data = data, environment = envir)
  
  for (l in data %>% dplyr::pull(layer) %>% unique() %>% sort()) {
    p <- p + ggplot2::geom_rect(data = data %>% filter(layer == l), 
                                mapping = ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, 
                                                       group = key,
                                                       fill = key_color),
                                color = "black", 
                                show.legend = FALSE)
  }
  
  if (labels) {
    p <- p + ggplot2::geom_text(ggplot2::aes(x = label_x, 
                           y = label_y, 
                           label = label, 
                           color = label_color), show.legend = FALSE) +
      ggplot2::scale_color_identity()
  }
  
  p <- p +
    ggplot2::scale_fill_identity() +
    ggplot2::theme_void()
  
  class(p) <- c("ggpiano", class(p))
  
  return(p)
}

#' Highlight keys
#' 
#' @param data data with key coordinates, e.g. from [keys_coords]
#' @param keys key numbers, note that can be greater than 12
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

#' Highlight coord
#' 
#' @param data data with key coordinates, e.g. from [keys_coords]
#' @param chord chord to highlight, e.g. from [construct_chord_major()] or [construct_chord_raw()]
#' @param inversion inversion number
#' @param highest_tone if inversion by highest tone is wanted
#' @param color highlight color
#' 
#' @details If `highest_tone` is provided, then `inversion` is ignored
#' 
#' @importFrom methods is
#' 
#' @export
highlight_chord <- function(data, chord, inversion = 0L, highest_tone = NULL, color = "lightblue") {
  if (is.null(data) || !methods::is(data, "pichor_key_koords")) {
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

