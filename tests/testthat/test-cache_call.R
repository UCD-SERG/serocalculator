test_that("`.validate_cache_path()` appends a missing trailing slash", {
  expect_equal(.validate_cache_path("cache"), "cache/")
  expect_equal(.validate_cache_path("cache/"), "cache/")
})

test_that("`.validate_cache_path()` rejects non-strings", {
  expect_error(.validate_cache_path(1), class = "rlang_error")
  expect_error(.validate_cache_path(c("a", "b")), class = "rlang_error")
  expect_error(.validate_cache_path(NA_character_), class = "rlang_error")
})

test_that("`.cache_files()` returns an empty vector for a missing directory", {
  expect_equal(
    .cache_files(file.path(withr::local_tempdir(), "absent")),
    character(0)
  )
})

# A non-deterministic body makes cache hits detectable: an identical result
# across two calls can only mean the second one loaded rather than recomputed.
nondeterministic <- function(x) list(x = x, nonce = stats::runif(1))

test_that("`.cache_call()` reuses a cached result for identical arguments", {
  dir <- withr::local_tempdir()

  first <- .cache_call(
    fun = nondeterministic, args = list(x = 1),
    cache_path = dir, cache_id = "demo", cache_verbose = FALSE
  )
  second <- .cache_call(
    fun = nondeterministic, args = list(x = 1),
    cache_path = dir, cache_id = "demo", cache_verbose = FALSE
  )

  expect_identical(first, second)
})

test_that("`.cache_call()` recomputes when the arguments change", {
  dir <- withr::local_tempdir()

  first <- .cache_call(
    fun = nondeterministic, args = list(x = 1),
    cache_path = dir, cache_id = "demo", cache_verbose = FALSE
  )
  changed <- .cache_call(
    fun = nondeterministic, args = list(x = 2),
    cache_path = dir, cache_id = "demo", cache_verbose = FALSE
  )

  expect_false(identical(first, changed))
  expect_equal(changed$x, 2)
})

test_that("`.cache_call()` recomputes and re-saves on `cache_rerun`", {
  dir <- withr::local_tempdir()

  first <- .cache_call(
    fun = nondeterministic, args = list(x = 1),
    cache_path = dir, cache_id = "demo", cache_verbose = FALSE
  )
  rerun <- .cache_call(
    fun = nondeterministic, args = list(x = 1),
    cache_path = dir, cache_id = "demo", cache_verbose = FALSE,
    cache_rerun = TRUE
  )
  after_rerun <- .cache_call(
    fun = nondeterministic, args = list(x = 1),
    cache_path = dir, cache_id = "demo", cache_verbose = FALSE
  )

  expect_false(identical(first, rerun))
  # The recomputed value is saved, so the next ordinary call reuses it.
  expect_identical(rerun, after_rerun)
})

test_that("`.cache_call()` keys separate `cache_id`s separately", {
  dir <- withr::local_tempdir()

  a <- .cache_call(
    fun = nondeterministic, args = list(x = 1),
    cache_path = dir, cache_id = "a", cache_verbose = FALSE
  )
  b <- .cache_call(
    fun = nondeterministic, args = list(x = 1),
    cache_path = dir, cache_id = "b", cache_verbose = FALSE
  )

  expect_false(identical(a, b))
})

test_that("`.cache_call()` folds `cache_extra` into the key", {
  dir <- withr::local_tempdir()

  v1 <- .cache_call(
    fun = nondeterministic, args = list(x = 1),
    cache_path = dir, cache_id = "demo", cache_verbose = FALSE,
    cache_extra = "1.0"
  )
  v1_again <- .cache_call(
    fun = nondeterministic, args = list(x = 1),
    cache_path = dir, cache_id = "demo", cache_verbose = FALSE,
    cache_extra = "1.0"
  )
  v2 <- .cache_call(
    fun = nondeterministic, args = list(x = 1),
    cache_path = dir, cache_id = "demo", cache_verbose = FALSE,
    cache_extra = "2.0"
  )

  expect_identical(v1, v1_again)
  expect_false(identical(v1, v2))
})

test_that("`.cache_call()` caches when `cache_path` lacks a trailing slash", {
  # Without normalization `cache_exec()` reads the path as a file and silently
  # caches nothing, so this guards the failure mode `.validate_cache_path()`
  # exists to prevent.
  dir <- withr::local_tempdir()
  no_slash <- file.path(dir, "nested")

  first <- .cache_call(
    fun = nondeterministic, args = list(x = 1),
    cache_path = no_slash, cache_id = "demo", cache_verbose = FALSE
  )
  second <- .cache_call(
    fun = nondeterministic, args = list(x = 1),
    cache_path = no_slash, cache_id = "demo", cache_verbose = FALSE
  )

  expect_identical(first, second)
  expect_true(dir.exists(file.path(no_slash, "demo")))
})

test_that("`.cache_call()` reports hits and misses when verbose", {
  dir <- withr::local_tempdir()

  expect_message(
    .cache_call(
      fun = nondeterministic, args = list(x = 1),
      cache_path = dir, cache_id = "demo"
    ),
    "computed it and saved it"
  )
  expect_message(
    .cache_call(
      fun = nondeterministic, args = list(x = 1),
      cache_path = dir, cache_id = "demo"
    ),
    "Loaded a cached result"
  )
})

test_that("`.cache_call()` reports discarding a cache on `cache_rerun`", {
  dir <- withr::local_tempdir()

  .cache_call(
    fun = nondeterministic, args = list(x = 1),
    cache_path = dir, cache_id = "demo", cache_verbose = FALSE
  )

  expect_message(
    .cache_call(
      fun = nondeterministic, args = list(x = 1),
      cache_path = dir, cache_id = "demo", cache_rerun = TRUE
    ),
    "discarded the cached result"
  )
})

test_that("`.cache_call()` rejects a result of the wrong class", {
  dir <- withr::local_tempdir()

  expect_error(
    .cache_call(
      fun = nondeterministic, args = list(x = 1),
      cache_path = dir, cache_id = "demo", cache_verbose = FALSE,
      expected_class = "seroincidence.by"
    ),
    class = "rlang_error"
  )
})
