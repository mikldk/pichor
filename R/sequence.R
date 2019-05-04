key_sequence_inversions <- function(key_sequence, inversions) {
  stopifnot(length(key_sequence) == length(inversions))
  
  new_keys_seq <- lapply(seq_along(key_sequence), function(i) {
    # TODO: optimise?
    inversion <- inversions[[i]]
    keys <- key_sequence[[i]]
    for (i in seq_len(inversion)) {
      keys <- get_keys_next_inversion(keys)
    }
    return(keys)
  })
  
  return(new_keys_seq)
}

key_sequence_inversions_distance <- function(key_sequence, inversions) {
  stopifnot(length(key_sequence) == length(inversions))
  new_keys_seq <- key_sequence_inversions(key_sequence, inversions)
  
  dist <- 0L
  
  for (i in seq_along(new_keys_seq)) {
    if (i == 1L) {
      dist <- dist + length(new_keys_seq[[i]])
      next
    }
    
    prev_seq <- new_keys_seq[[i-1L]]
    seq <- new_keys_seq[[i]]
    
    keep_keys <- intersect(prev_seq, seq)
    new_keys <- setdiff(seq, keep_keys)
    removed_keys <- setdiff(prev_seq, seq)
    
    dist <- dist + length(new_keys) + length(removed_keys)
  }
  
  return(dist)
}

#' Optimise sequence
#' 
#' Exhaustive minimisation of the distances between keys in consecutive chords
#' 
#' @inheritParams highlight_key_sequence
#' @param error_on_big stop if more than 100,000 combinations of inversions must be checked
#' 
#' @export
optim_min_dist_exhaustive <- function(key_sequence, 
                                      error_on_big = TRUE) {
  inversions <- lapply(lapply(key_sequence, seq_along), `-`, 1L)
  grd_size <- prod(unlist(lapply(inversions, length)))
  
  if (error_on_big && grd_size > 1e5) {
    stop("Grid size was ", grd_size, " > 100,000, stopping to avoid too big computations. To proceed, please call with error_on_big = FALSE argument.")
  }
  
  inv_grd <- expand.grid(inversions)
  dists <- apply(inv_grd, 1, function(invs) {
    invs_vec <- as.integer(invs)
    key_sequence_inversions_distance(key_sequence = key_sequence, 
                                     inversions = invs_vec)
  })
  
  best_inv <- as.integer(inv_grd[which.min(dists)[1L], ])
  worst_inv <- as.integer(inv_grd[which.max(dists)[1L], ])
  
  best <- key_sequence_inversions(key_sequence = key_sequence, 
                                  inversions = best_inv)
  worst <- key_sequence_inversions(key_sequence = key_sequence, 
                                  inversions = worst_inv)
  
  res <- list(best = best,
              worst = worst)
  
  return(res)
}

