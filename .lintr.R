

undesirable_functions <-
  lintr::default_undesirable_functions |>
  lintr::modify_defaults(

    # following https://github.com/r-lib/devtools/blob/2aa51ef/.lintr.R:
    # Base messaging
    "message" = "use cli::cli_inform()",
    "warning" = "use cli::cli_warn()",
    "stop" = "use cli::cli_abort()",
    # rlang messaging
    "inform" = "use cli::cli_inform()",
    "warn" = "use cli::cli_warn()",
    "abort" = "use cli::cli_abort()",
    # older cli
    "cli_alert_danger" = "use cli::cli_inform()",
    "cli_alert_info" = "use cli::cli_inform()",
    "cli_alert_success" = "use cli::cli_inform()",
    "cli_alert_warning" = "use cli::cli_inform()",

    library = paste(
      "\nuse `::`, `usethis::use_import_from()`, or `withr::local_package()`",
      "instead of modifying the global search path.",
      "\nSee:\n",
      "<https://r-pkgs.org/code.html#sec-code-r-landscape> and\n",
      "<https://r-pkgs.org/testing-design.html#sec-testing-design-self-contained>",
      "\nfor more details."
    ),

    structure = NULL,
    browser = NULL
    # see https://github.com/r-lib/lintr/pull/2227 and
    # rebuttal https://github.com/r-lib/lintr/pull/2227#issuecomment-1800302675

  )

# define snake_case with uppercase acronyms allowed;
# see https://github.com/r-lib/lintr/issues/2844 for details:
withr::local_package("rex")
snake_case_ACROs1 <- rex::rex(
  start,
  maybe("."),
  list(some_of(upper), maybe("s"), zero_or_more(digit)) %or% list(some_of(lower), zero_or_more(digit)),
  zero_or_more(
    "_",
    list(some_of(upper), maybe("s"), zero_or_more(digit)) %or% list(some_of(lower), zero_or_more(digit))
  ),
  end
)

linters <- lintr::linters_with_defaults(
  return_linter = NULL,
  trailing_whitespace_linter = NULL,
  lintr::redundant_equals_linter(),
  lintr::pipe_consistency_linter(pipe = "|>"),
  lintr::object_name_linter(
    regexes = c(snake_case_ACROs1 = snake_case_ACROs1)
  ),
  lintr::undesirable_function_linter(
    fun = undesirable_functions,
    symbol_is_undesirable = TRUE
  )
)

# prevent warnings from lintr::read_settings:
rm(undesirable_functions)
rm(snake_case_ACROs1)
exclusions <- list(
  `data-raw` = list(
    pipe_consistency_linter = Inf
  ),
  vignettes = list(
    undesirable_function_linter = Inf,
    object_name_linter = Inf
  ),
  "inst/examples" = list(
    undesirable_function_linter = Inf
  ),
  "inst/analyses" = list(
    undesirable_function_linter = Inf
  ),
  "tests/testthat.R" = list(
    undesirable_function_linter = Inf
  )

)
