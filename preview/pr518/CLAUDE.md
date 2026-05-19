# Claude Code Instructions for serocalculator

## General

See `.github/copilot-instructions.md` for full project conventions. Key
points below.

## Code Style

- Use native pipe `|>` instead of nesting function calls. Write
  `x |> f() |> g()` not `g(f(x))`.
- Avoid function calls in function arguments. Extract into named
  variables first. Write `y <- f(x); g(y)` not `g(f(x))`.
- Both rules apply to all R code including tests.
- One function per file.
- Keep functions under 100 lines; decompose into helpers.
- Internal helper functions use `.function_name()` convention.
- Use
  [`cli::cli_abort()`](https://cli.r-lib.org/reference/cli_abort.html) /
  [`cli::cli_warn()`](https://cli.r-lib.org/reference/cli_abort.html) /
  [`cli::cli_inform()`](https://cli.r-lib.org/reference/cli_abort.html)
  for messaging.
- Use `.data$column` for tidy evaluation in dplyr/tidyr/ggplot2
  pipelines.

## Function Design

- **Pass-through over interception**: Don’t name parameters in a wrapper
  just to relay them. Use `...` to forward arguments to subfunctions. A
  wrapper that adds no logic should be a one-liner:
  `f <- function(...) .g(...) |> .h()`.
- **Validate where consumed**: Check arguments in the function that
  actually uses them, not in a caller that just passes them through.
- **Use attributes for metadata**: When a data-producing function needs
  to communicate context (e.g., default title, faceting options) to a
  plotting function, store it as attributes on the data object rather
  than threading extra parameters through intermediaries.
- **`@inheritParams` / `@inheritDotParams`**: Use these instead of
  duplicating `@param` docs across functions. Use `@keywords internal`
  (not `@noRd`) for internal functions so roxygen inheritance works.
- **Leverage existing packages**: Before writing data-fetching or
  plotting utilities, check if CRAN packages already provide the
  functionality. Wrap existing packages rather than reimplementing.
- **S3 autoplot methods for plotting**: When a function produces data
  that gets plotted, give the data a custom S3 class and implement an
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  method rather than a standalone plotting function. This lets users
  call
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  directly on the data object.
- **Keep it simple**: Prefer the simplest solution. Don’t add redundant
  validation, unnecessary intermediate steps, or complexity that doesn’t
  earn its keep. If upstream already validates, don’t re-validate. If
  `x[1]` works, don’t call `match.arg(x)`.
- **Snapshot tests over manual assertions**: Use
  `expect_snapshot_value(style = "json2")` for data results and
  [`vdiffr::expect_doppelganger()`](https://vdiffr.r-lib.org/reference/expect_doppelganger.html)
  for plots instead of field-by-field `expect_equal()` calls. Don’t add
  redundant checks (e.g., `expect_s3_class()` before
  `expect_doppelganger()`).

## Workflow

- Run `devtools::document()` after modifying roxygen2 comments.
- Run `devtools::test()` before committing.
- Run
  [`lintr::lint_package()`](https://lintr.r-lib.org/reference/lint.html)
  to check style.
- Increment version with `usethis::use_version("dev")` and update
  NEWS.md.
