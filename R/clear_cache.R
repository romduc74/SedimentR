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

  message("Files found in cache : ", paste(cache_items, collapse = ", "))
  cat("\n")
  confirm <- readline("Do you really want to clear them? (yes/no): ")
  cat("\n")

  if (tolower(confirm) %in% c("yes", "y")) {
    rlang::env_unbind(cache_env, cache_items)
    cat("\n")
    message("SedimentR cache successfully cleared.")
    cat("\n")
  } else {
    message("Cache clearing canceled.")
  }

  invisible(TRUE)
}
