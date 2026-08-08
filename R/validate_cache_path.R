#' Normalize a cache directory path
#'
#' [xfun::cache_exec()] treats its `path` argument as a directory only when it
#' ends in a slash; without one the path is read as a *file* and nothing is
#' cached, silently. This helper appends the trailing slash so that failure
#' mode is unreachable.
#'
#' @param path a [character()] of length 1: the cache directory.
#'
#' @returns `path`, guaranteed to end in `/`.
#' @keywords internal
.validate_cache_path <- function(path) {
  if (!is.character(path) || length(path) != 1L || is.na(path)) {
    cli::cli_abort(
      "{.arg path} must be a single, non-missing string,
       not {.obj_type_friendly {path}}."
    )
  }

  if (!grepl("/$", path)) {
    path <- paste0(path, "/")
  }

  return(path)
}
