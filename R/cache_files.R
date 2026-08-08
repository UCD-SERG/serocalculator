#' List the cache files backing one cache entry
#'
#' Used by [.cache_call()] to tell a cache hit from a cache miss: a hit writes
#' nothing, so an unchanged file list means the result was loaded rather than
#' computed.
#'
#' @param cache_dir a [character()] of length 1: the `path/id/` directory.
#'
#' @returns a sorted [character()] of file names, empty if `cache_dir` does
#'   not exist.
#' @keywords internal
.cache_files <- function(cache_dir) {
  if (!dir.exists(cache_dir)) {
    return(character(0))
  }

  return(sort(list.files(cache_dir)))
}
