#' Plot piano
#' 
#' @param data data to plot, default `keys_coords` which is a piano
#' 
#' @importFrom ggplot2 ggplot aes scale_fill_manual theme_void geom_rect
#' 
#' @export
ggpiano <- function(data = get_keys_coords(), ...) {
  envir <- parent.frame()
  
  p <- ggplot2::ggplot(data = data, environment = envir)
  
  for (l in data %>% pull(layer) %>% unique() %>% sort()) {
    p <- p + ggplot2::geom_rect(data = data %>% filter(layer == l), 
                                mapping = ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, 
                                                       group = key,
                                                       fill = key_color),
                                color = "black", 
                                show.legend = FALSE)
  }
  
  p <- p +
    ggplot2::scale_fill_identity() +
    ggplot2::theme_void()
  
  class(p) <- c("ggpiano", class(p))
  
  return(p)
}
#' 
#' 
#' #' Highlight chord
#' #'
#' #' @inheritParams ggpiano
#' #' @param chord chord to plot
#' #' @param color color for keys in chord
#' #' 
#' #' @export
#' ggchord <- function(chord = NULL, color = "lightblue", data = keys_coords) {
#'   if (is.null(chord) || !is(chord, "chord")) {
#'     stop("chord must be a chord")
#'   }
#'   
#'   chord_long <- chord %>% unnest()
#'   chord_keys <- chord_long %>% pull(key)
#'   
#'   newdata <- data %>% 
#'     mutate(key_color = case_when(
#'       key %in% chord_keys ~ color,
#'       TRUE ~ key_color
#'     ))
#'   
#'   p <- ggpiano(data = newdata)
#'   
#'   return(p)
#' }
#' 
