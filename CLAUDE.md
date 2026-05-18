# Claude Code Instructions for serocalculator

## General

See `.github/copilot-instructions.md` for full project conventions. Key points below.

## Code Style

- Use native pipe `|>` instead of nesting function calls. Write `x |> f() |> g()` not `g(f(x))`. This applies to all R code including tests.
- One function per file.
- Keep functions under 100 lines; decompose into helpers.
- Internal helper functions use `.function_name()` convention.
- Use `cli::cli_abort()` / `cli::cli_warn()` / `cli::cli_inform()` for messaging.
- Use `.data$column` for tidy evaluation in dplyr/tidyr/ggplot2 pipelines.

## Workflow

- Run `devtools::document()` after modifying roxygen2 comments.
- Run `devtools::test()` before committing.
- Run `lintr::lint_package()` to check style.
- Increment version with `usethis::use_version("dev")` and update NEWS.md.
