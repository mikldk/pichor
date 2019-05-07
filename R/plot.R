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
ggpiano <- function(data = keys_coords, labels = FALSE, ...) {
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
#' @param keys key numbers, note that they can be greater than 12
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
  
  stopifnot(!is.integer(keys))

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



#' Highlight key sequence
#' 
#' @param data data with key coordinates, e.g. from [keys_coords]
#' @param key_sequence list of vector with key numbers, note that they can be greater than 12
#' @param sequence_names names of each element in `key_sequence`
#' @param new_color highlight color for new keys (`NULL` for disable)
#' @param keep_color highlight color for keys kept at same position (`NULL` for disable)
#' @param remove_color highlight color for removed keys (`NULL` for disable)
#' 
#' @return A new dataset with two new columns: `seq_no`, that can be used in e.g. plotting, 
#' and `seq_name` that is either `seq_no` of `sequence_names` if provided.
#' 
#' @importFrom dplyr mutate case_when bind_rows left_join
#' @importFrom tibble tibble enframe
#' @importFrom forcats fct_inorder
#' @importFrom magrittr "%>%"
#' 
#' @export
highlight_key_sequence <- function(data, 
                                   key_sequence, 
                                   sequence_names = NULL,
                                   new_color = "lightblue", 
                                   keep_color = "lightgrey",
                                   remove_color = NULL) {
  if (is.null(data) || !is(data, "pichor_key_koords")) {
    stop("data must be a pichor_key_koords")
  }
  
  stopifnot(is.list(key_sequence))
  stopifnot(isTRUE(all.equal(key_sequence, lapply(key_sequence, round))))
  key_sequence <- lapply(key_sequence, as.integer)
  stopifnot(length(key_sequence) >= 1L)
  stopifnot(length(key_sequence[[1L]]) >= 1L)
  
  print(remove_color)
  stopifnot(is.null(new_color) || length(new_color) == 1L)
  stopifnot(is.null(keep_color) || length(keep_color) == 1L)
  stopifnot(is.null(remove_color) || length(remove_color) == 1L)
  
  
  if ("seq_no" %in% colnames(data)) {
    stop("data already has column seq_no")
  }
  
  if ("seq_name" %in% colnames(data)) {
    stop("data already has column seq_name")
  }
  
  if (!is.null(sequence_names)) {
    stopifnot(length(sequence_names) == length(key_sequence))
    sequence_names <- as.character(sequence_names)
  }
  
  
  unknown_keys <- setdiff(unlist(key_sequence), data %>% pull(key))
  if (length(unknown_keys) > 0) {
    stop("Some keys were unknown: ", paste0(unknown_keys, collapse = ", "))
  }
  
  newdatas <- vector("list", length(key_sequence))
  
  for (i in seq_along(key_sequence)) {
    prev_seq <- if (i == 1L) NULL else key_sequence[[i-1L]]
    seq <- key_sequence[[i]]
    
    keep_keys <- intersect(prev_seq, seq)
    new_keys <- setdiff(seq, keep_keys)
    removed_keys <- setdiff(prev_seq, seq)

    newdata <- data
    
    if (!is.null(keep_color)) {
      newdata <- newdata %>%
        dplyr::mutate(key_color = dplyr::case_when(
          key %in% keep_keys ~ keep_color,
          TRUE ~ key_color
        ))
    }
    
    if (!is.null(new_color)) {
      newdata <- newdata %>%
        dplyr::mutate(key_color = dplyr::case_when(
          key %in% new_keys ~ new_color,
          TRUE ~ key_color
        ))
    }
    
    if (!is.null(remove_color)) {
      newdata <- newdata %>%
        dplyr::mutate(key_color = dplyr::case_when(
          key %in% removed_keys ~ remove_color,
          TRUE ~ key_color
        ))
    }
    
    newdata <- newdata %>%
      dplyr::mutate(label_color = case_when(
        key_color == "black" ~ "white",
        TRUE ~ "black")) %>% 
      mutate(seq_no = i)
    
    newdatas[[i]] <- newdata
  }
  newdatas <- dplyr::bind_rows(newdatas)
  
  d_seq_nms <- tibble::tibble(seq_no = seq_along(key_sequence), 
                              seq_name = as.character(seq_no))

  if (!is.null(sequence_names)) {
    d_seq_nms <- tibble::enframe(sequence_names, 
                                 name = "seq_no", 
                                 value = "seq_name")
  }
  
  newdatas <- newdatas %>% 
    dplyr::left_join(d_seq_nms, by = "seq_no") %>% 
    dplyr::mutate(seq_name = forcats::fct_inorder(seq_name))
  
  class(newdatas) <- c("pichor_key_koords_highlight_sequence", 
                       "pichor_key_koords_highlight", 
                       class(newdatas))
  
  return(newdatas)
}
