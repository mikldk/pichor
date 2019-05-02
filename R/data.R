#' Get coordinates to draw a piano
#'
#' @importFrom dplyr select left_join mutate group_by summarise
#' @importFrom magrittr "%>%"
#' 
#' @export
get_keys_coords <- function() {
  d <- tone_properties %>%
    dplyr::select(key, tone) %>% 
    dplyr::group_by(key) %>% 
    dplyr::summarise(tones = list(tone),
              label = paste0(tone, collapse = "\n"))
    
  res <- keys_coords %>% 
    dplyr::mutate(join_key = ((key - 1) %% 12) + 1) %>% 
    dplyr::left_join(d, by = c("join_key" = "key")) %>% 
    dplyr::select(-join_key) %>% 
    dplyr::mutate(label_x = (xmin+xmax)/2,
                  label_y = ymin + 0.1) %>% 
    dplyr::mutate(label_color = case_when(
             key_color == "black" ~ "white",
             TRUE ~ "black"))
  
  # Put here instead of where it's generated to ease development process
  class(res) <- c("pichor_key_koords", class(res))
  return(res)
}

print.pichor_key_koords <- function(x, ...) {
  cat("Coordinates for some of a piano's keys:\n")
  NextMethod(x)
}