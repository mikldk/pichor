#' Plot piano
#' 
#' @param data data to plot, default `keys_coords` which is a piano
#' 
#' @export
ggpiano <- function(data = keys_coords, ...) {
  envir <- parent.frame()
  
  p <- ggplot(mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, 
                            group = key,
                            fill = key_color), environment = envir)
  
  for (l in data %>% pull(layer) %>% unique() %>% sort()) {
    p <- p + geom_rect(data = data %>% filter(layer == l), 
                       color = "black", 
                       show.legend = FALSE)
  }
  
  # Make colors literal
  cols <- data %>% pull(key_color) %>% unique()
  names(cols) <- cols
  
  p <- p +
    scale_fill_manual(values = cols) +
    theme_void()
  
  class(p) <- c("ggpiano", class(p))
  
  return(p)
}


#' Add chord
#'
#' @inheritParams ggpiano
#' @param chord chord to plot
#' @param color color for keys in chord
#' 
#' @export
ggchord <- function(chord = NULL, color = "lightblue", data = keys_coords) {
  if (is.null(chord) || !is(chord, "chord")) {
    stop("chord must be a chord")
  }
  
  chord_long <- chord %>% unnest()
  chord_keys <- chord_long %>% pull(key)
  
  newdata <- data %>% 
    mutate(key_color = case_when(
      key %in% chord_keys ~ color,
      TRUE ~ key_color
    ))
  
  p <- ggpiano(data = newdata)
  
  return(p)
}

