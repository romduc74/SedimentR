# Internal cache environment for SedimentR
# This is NOT exported and persists while the package is loaded
# Ensure rlang is available and loaded quietly
if (!requireNamespace("rlang", quietly = TRUE)) {
  stop("Package 'rlang' is required but not installed.")
}

#' Store an object in the internal package cache
#'
#' @param name Character string — name of the object to store.
#' @param value Any R object — the data or model to store.
#' @return Invisibly returns TRUE.
# Create internal cache environment
#.my_cache <- rlang::new_environment(parent = emptyenv())
#' Store an object in the internal package cache
#' P
#' @noRd
set_cache <- function(name, value) {
  # Check if the cache environment exists in the global environment
  if (!exists(".my_cache", envir = .GlobalEnv)) {
    assign(".my_cache", new.env(parent = emptyenv()), envir = .GlobalEnv)
    message("Cache environment '.my_cache' created.")
  }

  # Get the cache environment
  cache_env <- get(".my_cache", envir = .GlobalEnv)

  # Set or update the object inside the cache
  assign(name, value, envir = cache_env)
  message("Object '", name, "' registered in cache.")

  invisible(TRUE)

}


#' Retrieve an object from the internal package cache
#' @noRd
get_cache <- function(name) {
  if (rlang::env_has(.my_cache, name)) {
    rlang::env_get(.my_cache, name)
  } else {
    stop("No cached object named '", name, "' found.")
  }
}

#' Clear all objects stored in the package cache
#' @noRd
clear_cache <- function() {
  rlang::env_unbind(.my_cache, rlang::env_names(.my_cache))
  invisible(TRUE)
}

#' List objects currently stored in cache
#' @noRd
list_cache <- function() {
  rlang::env_names(.my_cache)
}
