#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows
generate_keys <- function(number_white_keys = 14) {
  stopifnot(number_white_keys > 0)
  number_white_keys <- as.integer(number_white_keys)
  
  chords <- NULL
  
  white_keys <- 0L
  black_keys <- 0L
  
  i <- 0L
  
  while (white_keys < number_white_keys) {
    i <- i + 1L
    
    i_color <- get_key_color(i)

    if (i_color == "white") {
      # white
      white_keys <- white_keys + 1L
      
      start_x <- (white_keys-1)/number_white_keys
      end_x <- white_keys/number_white_keys
      
      chords <- dplyr::bind_rows(chords, 
                      tibble::tibble(
                        key = i,
                        key_color = "white",
                        xmin = start_x,
                        ymin = 0,
                        xmax = end_x,
                        ymax = 1,
                        layer = 1
                      ))
    } else {
      # black
      black_keys <- black_keys + 1L
      
      start_x <- (white_keys-1)/number_white_keys
      end_x <- white_keys/number_white_keys
      
      start_x2 <- ((start_x + end_x) / 2) + 1/(4*number_white_keys)
      end_x2 <- end_x + 1/(4*number_white_keys)

      chords <- bind_rows(chords, 
                      tibble(
                        key = i,
                        key_color = "black",
                        xmin = start_x2,
                        ymin = 0.45,
                        xmax = end_x2,
                        ymax = 1,
                        layer = 2
                      ))
    }
    
  }
  
  return(chords)
}

generate_tone_properties <- function() {
  tone_properties <- tribble(
    ~tone, ~key,
    
    "C", 1,
    
    "C#", 2,
    "Db", 2,
    
    "D", 3,
    
    "D#", 4,
    "Eb", 4,
    
    "E", 5,
    
    "F", 6,
    
    "F#", 7,
    "Gb", 7,
    
    "G", 8,
    
    "G#", 9,
    "Ab", 9,
    
    "A", 10,
    
    "A#", 11,
    "Bb", 11,
    
    "B", 12
  ) %>% 
    dplyr::mutate(
      key_color = ifelse(nchar(tone) == 1, "white", "black")
    )
  
  tone_properties
}

#' Generate a custom-sized piano
#' 
#' Generate a custom-sized piano, useful e.g. for synthesizers.
#' 
#' @examples 
#' d <- generate_keys_chords(10)
#' ggpiano(d, labels = TRUE)
#' d <- d %>% 
#'   dplyr::rowwise() %>% 
#'   dplyr::mutate(label = tones[[1]])
#' ggpiano(d, labels = TRUE)
#'  
#' @importFrom tibble tribble
#' @importFrom dplyr rowwise mutate
#' 
#' @export
generate_keys_chords <- function(number_white_keys = 14) {
  d_keys <- generate_keys(number_white_keys)
  
  if (FALSE) {
    library(ggplot2)
    ggplot(mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, 
                         group = key,
                         fill = key_color)) +
      geom_rect(data = d_keys %>% filter(key_color == "white"), color = "black", show.legend = FALSE) +
      geom_rect(data = d_keys %>% filter(key_color == "black"), color = "black", show.legend = FALSE) +
      scale_fill_manual(values = c("white" = "white", "black" = "black")) +
      theme_void()
  }
  
  d_tones <- generate_tone_properties() %>%
    dplyr::select(key, tone) %>% 
    dplyr::group_by(key) %>% 
    dplyr::summarise(tones = list(tone),
                     label = paste0(tone, collapse = "\n"))
  
  keys_chords <- d_keys %>% 
    dplyr::mutate(join_key = ((key - 1) %% 12) + 1) %>% 
    dplyr::left_join(d_tones, by = c("join_key" = "key")) %>% 
    dplyr::select(-join_key) %>% 
    dplyr::mutate(label_x = (xmin+xmax)/2,
                  label_y = ymin + 0.1) %>% 
    dplyr::mutate(label_color = case_when(
      key_color == "black" ~ "white",
      TRUE ~ "black"))
  
  # Put here instead of where it's generated to ease development process
  class(keys_chords) <- c("pichor_key_koords", class(keys_chords))
  
  if (FALSE) {
    d <- generate_keys_chords(10)
    d
    ggplot(mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, 
                         group = key,
                         fill = key_color)) +
      geom_rect(data = d %>% filter(key_color == "white"), color = "black", show.legend = FALSE) +
      geom_rect(data = d %>% filter(key_color == "black"), color = "black", show.legend = FALSE) +
      scale_fill_manual(values = c("white" = "white", "black" = "black")) +
      theme_void()
  }
  
  keys_chords
}
