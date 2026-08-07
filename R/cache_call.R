#' Call a function, caching its result on disk
#'
#' The shared engine behind [sim_pop_data_multi_cached()] and
#' [est_seroincidence_by_cached()]. Wraps [xfun::cache_exec()], which keys the
#' cache on the *values* of the free variables in the cached expression: here
#' that is `args`, so the cache is invalidated whenever the arguments change
#' and reused whenever they do not.
#'
#' Every cache-control argument carries a `cache_` prefix because the wrapped
#' functions have their own `verbose` argument, which the wrappers must leave
#' free to pass through.
#'
#' @param fun the [function()] to call.
#' @param args a [list()] of arguments to pass to `fun`.
#' @param cache_path a [character()] of length 1: the directory in which to
#'   store cache files. A trailing slash is added if absent (see
#'   [.validate_cache_path()]). Results are written to `cache_path/cache_id/`.
#' @param cache_id a [character()] of length 1: a stable identifier for this
#'   cache entry. Distinct calls sharing a `cache_path` need distinct
#'   `cache_id`s.
#' @param cache_rerun a [logical()] of length 1: whether to discard any
#'   existing cache and recompute. The freshly computed result is saved, so
#'   later calls with `cache_rerun = FALSE` reuse it.
#' @param cache_verbose a [logical()] of length 1: whether to report cache hits
#'   and misses via [cli::cli_inform()].
#' @param cache_extra an object folded into the cache key alongside the
#'   arguments. `NULL` (the default) keys the cache on the arguments alone.
#'   See the note on implementation changes in [sim_pop_data_multi_cached()].
#' @param expected_class a [character()] of classes the result must inherit
#'   from, or `NULL` to skip the check. Guards against a corrupted or
#'   hand-edited cache file being returned as though it were a real result.
#'
#' @returns the value of `do.call(fun, args)`, whether computed or loaded.
#' @keywords internal
.cache_call <- function(
  fun,
  args,
  cache_path,
  cache_id,
  cache_rerun = FALSE,
  cache_verbose = TRUE,
  cache_extra = NULL,
  expected_class = NULL
) {
  cache_path <- .validate_cache_path(cache_path)
  cache_dir <- file.path(cache_path, cache_id)

  if (cache_rerun && dir.exists(cache_dir)) {
    unlink(cache_dir, recursive = TRUE)

    if (cache_verbose) {
      cli::cli_inform(
        c("i" = "{.arg cache_rerun} is {.code TRUE}:
                 discarded the cached result for {.val {cache_id}}.")
      )
    }
  }

  files_before <- .cache_files(cache_dir)

  result <- xfun::cache_exec(
    do.call(fun, args),
    path = cache_path,
    id = cache_id,
    # Hash the arguments only. Left to its own devices, `cache_exec()` would
    # also hash `fun`, whose environment is the package namespace.
    hash = "args",
    extra = cache_extra
  )

  # A hit writes nothing; a miss adds a file and (by default) purges the copy
  # it supersedes. So an unchanged file list means the result was loaded.
  loaded_from_cache <-
    length(files_before) > 0L &&
    identical(files_before, .cache_files(cache_dir))

  if (cache_verbose) {
    if (loaded_from_cache) {
      cli::cli_inform(
        c("v" = "Loaded a cached result for {.val {cache_id}}
                 from {.path {cache_dir}}.")
      )
    } else {
      cli::cli_inform(
        c("i" = "No cached result for {.val {cache_id}} matched these
                 arguments; computed it and saved it to {.path {cache_dir}}.")
      )
    }
  }

  if (!is.null(expected_class) && !inherits(result, expected_class)) {
    cli::cli_abort(
      c(
        "The cached result for {.val {cache_id}} is not the expected type.",
        "x" = "Expected an object inheriting from
               {.cls {expected_class}}, but got {.cls {class(result)}}.",
        "i" = "Recompute it with {.code cache_rerun = TRUE}, or delete
               {.path {cache_dir}}."
      )
    )
  }

  return(result)
}
