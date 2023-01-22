generate_sysdata <- function() {
  if (!require(usethis, quietly = FALSE)) {
    stop("usethis required")
  }
  
  tone_properties <- generate_tone_properties()
  
  # Save all data
  ###############################################################
  # Internal
  ###############################################################
  usethis::use_data(tone_properties,
                    internal = TRUE, overwrite = TRUE)

  ###############################################################
  # External
  ###############################################################
  keys_chords <- generate_keys_chords(number_white_keys = 14)
  
  if (FALSE) {
    library(ggplot2)
    ggplot(mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, 
                         group = key,
                         fill = key_color)) +
      geom_rect(data = keys_chords %>% filter(key_color == "white"), color = "black", show.legend = FALSE) +
      geom_rect(data = keys_chords %>% filter(key_color == "black"), color = "black", show.legend = FALSE) +
      scale_fill_manual(values = c("white" = "white", "black" = "black")) +
      theme_void()
    
    ggpiano(keys_chords)
  }
  
  usethis::use_data(keys_chords, 
                    internal = FALSE, overwrite = TRUE)
}
