# Claude Code Instructions for serocalculator

## General

See `.github/copilot-instructions.md` for full project conventions. Key points below.

## Code Style

- Use native pipe `|>` instead of nesting function calls. Write `x |> f() |> g()` not `g(f(x))`.
- Avoid function calls in function arguments. Extract into named variables first. Write `y <- f(x); g(y)` not `g(f(x))`.
- Both rules apply to all R code including tests.
- One function per file.
- Keep functions under 100 lines; decompose into helpers.
- Internal helper functions use `.function_name()` convention.
- Use `cli::cli_abort()` / `cli::cli_warn()` / `cli::cli_inform()` for messaging.
- Use `.data$column` for tidy evaluation in dplyr/tidyr/ggplot2 pipelines.

## Function Design

- **Pass-through over interception**: Don't name parameters in a wrapper just to relay them. Use `...` to forward arguments to subfunctions. A wrapper that adds no logic should be a one-liner: `f <- function(...) .g(...) |> .h()`.
- **Validate where consumed**: Check arguments in the function that actually uses them, not in a caller that just passes them through.
- **Use attributes for metadata**: When a data-producing function needs to communicate context (e.g., default title, faceting options) to a plotting function, store it as attributes on the data object rather than threading extra parameters through intermediaries.
- **`@inheritParams` / `@inheritDotParams`**: Use these instead of duplicating `@param` docs across functions. Use `@keywords internal` (not `@noRd`) for internal functions so roxygen inheritance works.
- **Leverage existing packages**: Before writing data-fetching or plotting utilities, check if CRAN packages already provide the functionality. Wrap existing packages rather than reimplementing.

## Workflow

- Run `devtools::document()` after modifying roxygen2 comments.
- Run `devtools::test()` before committing.
- Run `lintr::lint_package()` to check style.
- Increment version with `usethis::use_version("dev")` and update NEWS.md.
