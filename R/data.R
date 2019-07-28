#' Coordinates to draw a piano
#'
#' @format A data frame with 53940 rows and 10 variables:
#' \describe{
#'   \item{key}{key number}
#'   \item{key_color}{color of the key}
#'   \item{xmin}{coordinate}
#'   \item{ymin}{coordinate}
#'   \item{xmax}{coordinate}
#'   \item{ymax}{coordinate}
#'   \item{layer}{white keys in layer 1, black in layer 2, used for plotting}
#'   \item{tones}{the key's tones}
#'   \item{label}{a key's label}
#'   \item{label_x}{coordinate}
#'   \item{label_y}{coordinate}
#'   \item{label_color}{the label's color}
#' }
"keys_chords"

print.pichor_key_koords <- function(x, ...) {
  cat("Coordinates for some of a piano's keys:\n")
  NextMethod(x)
}