#' Clear SedimentR Cache
#'
#' This function clears all stored cache in SedimentR.
#'
#' @return Invisibly returns TRUE
#' @export
clear_cache <- function() {
  if (!exists(".my_cache", envir = .GlobalEnv)) {
    message("No cache found to clear.")
    return(invisible(TRUE))
  }

  cache_env <- get(".my_cache", envir = .GlobalEnv)
  cache_items <- rlang::env_names(cache_env)

  if (length(cache_items) == 0) {
    message("Cache is already empty.")
    return(invisible(TRUE))
  }

  message("Caches to delete: ", paste(cache_items, collapse = ", "))
  confirm <- readline("Do you really want to clear them? (yes/no): ")

  if (tolower(confirm) %in% c("yes", "y")) {
    rlang::env_unbind(cache_env, cache_items)
    message("SedimentR cache successfully cleared.")
  } else {
    message("Cache clearing canceled.")
  }

  invisible(TRUE)
}
