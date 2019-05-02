generate_keys_coords <- function() {
  # Number of white keys
  N <- 14L
  
  coords <- NULL
  
  white_keys <- 0L
  black_keys <- 0L
  
  i <- 0L
  
  while (white_keys < N) {
    i <- i + 1L
    
    i_color <- get_key_color(i)

    if (i_color == "white") {
      # white
      white_keys <- white_keys + 1L
      
      start_x <- (white_keys-1)/N
      end_x <- white_keys/N
      
      coords <- bind_rows(coords, 
                      tibble(
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
      
      start_x <- (white_keys-1)/N
      end_x <- white_keys/N
      
      start_x2 <- ((start_x + end_x) / 2) + 1/(4*N)
      end_x2 <- end_x + 1/(4*N)

      coords <- bind_rows(coords, 
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
  
  return(coords)
}

if (FALSE) {
  library(ggplot2)
  d <- generate_keys_coords()
  d
  ggplot(mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, 
                group = key,
                fill = key_color)) +
    geom_rect(data = d %>% filter(key_color == "white"), color = "black", show.legend = FALSE) +
    geom_rect(data = d %>% filter(key_color == "black"), color = "black", show.legend = FALSE) +
    scale_fill_manual(values = c("white" = "white", "black" = "black")) +
    theme_void()
}

generate_sysdata <- function() {
  if (!require(usethis, quietly = FALSE)) {
    stop("usethis required")
  }
  
  if (!require(tibble, quietly = FALSE)) {
    stop("tibble required")
  }
  
  if (!require(dplyr, quietly = FALSE)) {
    stop("dplyr required")
  }
  
  if (!require(tidyr, quietly = FALSE)) {
    stop("tidyr required")
  }
  
  library(tibble)
  library(dplyr)
  library(tidyr)
  
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
  
  keys_coords <- generate_keys_coords()
  
  # Save all data
  # Internal:
  usethis::use_data(tone_properties, keys_coords,
                    internal = TRUE, overwrite = TRUE)
  
  # # External
  # usethis::use_data(keys_coords, 
  #                   internal = FALSE, overwrite = TRUE)
}
