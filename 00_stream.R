# Retrieve up- and downstream basins of 'id' using the NEXT_DOWN field
stream <- \(id, n = 1L, max, down, d) {
  if (n > max) {
    return()
  }
  if (down) {
    id_next <- d[["NEXT_DOWN"]][d[["HYBAS_ID"]] == id]
  } else { # Upstream
    id_next <- d[["HYBAS_ID"]][d[["NEXT_DOWN"]] %in% id]
  }
  if (all(id_next == 0) || length(id_next) == 0) {
    return()
  }
  return(c(id_next, Recall(id_next, n = n + 1L, max = max, down = down, d = d)))
}

# Retrieve up- and downstream basins of 'id' and track the order
stream_ordered <- function(id, n = 1L, max, down, d) {
  if (n > max) {
    return(NULL)
  }
  if (down) {
    id_next <- d[["NEXT_DOWN"]][d[["HYBAS_ID"]] == id]
  } else { # Upstream
    id_next <- d[["HYBAS_ID"]][d[["NEXT_DOWN"]] %in% id]
  }
  if (all(id_next == 0) || length(id_next) == 0) {
    return()
  }
  results <- cbind(id_next, n)
  id_next <- unique(id_next)
  recursive_results <- Recall(id_next, n = n + 1L, max = max, down = down, d = d)
  if (!is.null(recursive_results)) {
    results <- rbind(results, recursive_results)
  }
  return(results)
}
