# Copilot Instructions for serocalculator

## Repository Overview

**serocalculator** is an R package for estimating infection rates from serological data. It translates antibody levels measured in cross-sectional population samples into estimates of the frequency with which seroconversions (infections) occur in the sampled populations. This package replaces the previous `seroincidence` package.

- **Type**: R package (statistical analysis)
- **Size**: ~43MB, ~393 files, ~84 R source files, ~5,178 lines of R code
- **Language**: R (>= 4.1.0)
- **Key Dependencies**: Rcpp, dplyr, ggplot2, tidyr, cli, foreach, doParallel
- **Lifecycle**: Stable

## Critical Setup Requirements

### Copilot Setup Workflow (Automatic Environment Configuration)

The repository includes a **`.github/workflows/copilot-setup-steps.yml`** workflow that automatically configures the GitHub Copilot coding agent's environment with all required dependencies. This workflow runs automatically when Copilot starts working on a task, ensuring a consistent and properly configured development environment.

#### What the Workflow Does

The copilot-setup-steps.yml workflow:

1. **Installs system dependencies**: All required Ubuntu packages for R package development (libcurl, libssl, libxml2, graphics libraries, etc.)
2. **Sets up R (>= 4.1.0)**: Installs the R release version that meets the package's minimum requirement
3. **Installs R package dependencies**: All Imports, Suggests, and development dependencies from DESCRIPTION
4. **Verifies installation**: Runs comprehensive checks to ensure R is properly configured

#### When It Runs

The workflow runs in the following scenarios:

- **Automatically for Copilot**: When the GitHub Copilot coding agent starts working on a task, it uses this workflow to prepare the environment
- **On workflow changes**: When `.github/workflows/copilot-setup-steps.yml` is modified (via push or pull request)
- **Manual testing**: Can be triggered manually from the repository's "Actions" tab using workflow_dispatch

#### Integration with CI Workflows

The copilot-setup-steps.yml workflow complements but does not replace the CI workflows:

- **Purpose**: Configures the Copilot agent's environment for development work, not for CI testing
- **Scope**: Runs on ubuntu-latest only, while CI workflows test on multiple platforms (Ubuntu, macOS, Windows) and R versions (release, devel, oldrel-1)
- **Alignment**: Uses the same R setup approach as the R-CMD-check.yaml workflow, ensuring consistency
- **Timeout**: Limited to 55 minutes (Copilot maximum is 59 minutes)

#### Verification Steps

The workflow includes detailed verification logging:

- **R version check**: Ensures R >= 4.1.0 requirement is met
- **Package verification**: Lists key installed packages (devtools, rcmdcheck, lintr, spelling, testthat)

#### Customization

If you need to modify the Copilot environment setup:

1. Edit `.github/workflows/copilot-setup-steps.yml`
2. Test changes by pushing to a branch or using workflow_dispatch
3. Ensure the job name remains `copilot-setup-steps` (required by Copilot)
4. Keep timeout under 59 minutes
5. Update this documentation to reflect any significant changes

### Alternative: Quick Start with Docker

**If you prefer manual Docker setup**, you can use the rocker/verse Docker image which includes R, RStudio, tidyverse, TeX, and many common R packages pre-installed.

To use Docker:

```bash
# Pull the rocker/verse image (includes R >= 4.1.0, tidyverse, devtools, and more)
docker pull rocker/verse:latest

# Run container with repository mounted
docker run -d \
  -v /home/runner/work/serocalculator/serocalculator:/workspace \
  -w /workspace \
  --name serocalculator-dev \
  rocker/verse:latest

# Execute commands in the container
docker exec serocalculator-dev R -e "devtools::install_dev_deps()"
docker exec serocalculator-dev R -e "devtools::check()"

# Or start an interactive R session
docker exec -it serocalculator-dev R

# Clean up when done
docker stop serocalculator-dev
docker rm serocalculator-dev
```

### Manual Installation (if not using devcontainer or Docker)

If the devcontainer or Docker is not available or you prefer a native installation, follow the manual installation instructions below.

### R Installation and Development Dependencies (REQUIRED)

**ALWAYS install R and all development dependencies when starting work on a pull request.** This ensures you avoid issues caused by missing dependencies or environment misconfiguration during the development process.

#### Installing R (>= 4.1.0)

The package requires R version 4.1.0 or higher. Install R for your platform:

- **Ubuntu/Linux**: 
  ```bash
  # Add CRAN repository for latest R version
  sudo apt-get update
  sudo apt-get install -y software-properties-common dirmngr
  wget -qO- https://cloud.r-project.org/bin/linux/ubuntu/pubkey.gpg | \
    sudo tee /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc
  sudo add-apt-repository \
    "deb https://cloud.r-project.org/bin/linux/ubuntu $(lsb_release -cs)-cran40/"
  sudo apt-get update
  sudo apt-get install -y r-base r-base-dev
  
  # Verify installation
  R --version
  ```

- **macOS**: 
  ```bash
  # Install using Homebrew (recommended)
  brew install r
  
  # Or download from CRAN: https://cran.r-project.org/bin/macosx/
  # Verify installation
  R --version
  ```

- **Windows**: 
  Download and install from https://cran.r-project.org/bin/windows/base/
  
  Verify installation by opening R console and checking version:
  ```r
  R.version.string
  ```

#### Installing Development Dependencies

After installing R, install all required development dependencies:

```r
# Install devtools (required for package development)
install.packages("devtools", repos = "https://cloud.r-project.org")

# Install all package dependencies (Imports, Suggests, and development needs)
# This reads DESCRIPTION file and installs everything needed
devtools::install_dev_deps(dependencies = TRUE)
```

**Alternative approach** using pak (faster parallel installation):
```r
install.packages("pak", repos = "https://cloud.r-project.org")
pak::local_install_dev_deps(dependencies = TRUE)
```

#### Verify Development Environment

After installation, verify your development environment is properly configured:

```r
# Load devtools
library(devtools)

# Check package dependencies
devtools::dev_sitrep()

# Load the package in development mode
devtools::load_all()

# Run a quick check
devtools::check_man()
```

**Note**: If you encounter issues with dependencies, particularly with system libraries, install the following system dependencies first:

- **Ubuntu/Linux**:
  ```bash
  sudo apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev
  ```

- **macOS**: Most system dependencies are handled by Homebrew, but you may need:
  ```bash
  brew install pkg-config cairo
  ```

- **Windows**: Install Rtools from https://cran.r-project.org/bin/windows/Rtools/ (choose version matching your R version)

## Build and Development Workflow

### Initial Setup

```r
# Install development dependencies
devtools::install_dev_deps()

# Or using the package manager approach
install.packages("devtools")
```

### Documentation Generation

**ALWAYS regenerate documentation after modifying roxygen2 comments in `.R` files.**

```r
# Generate documentation from roxygen2 comments
devtools::document()
# or
roxygen2::roxygenise()
```

Documentation files in `man/` and `NAMESPACE` are auto-generated. Do NOT edit them directly.

### README Updates

README.md is generated from README.Rmd. **ALWAYS edit README.Rmd, never README.md directly.**

To regenerate:
```r
rmarkdown::render("README.Rmd")
```

### Package Checking

Run R CMD check to validate the package:

```r
# Full package check (takes several minutes)
devtools::check()
# or
rcmdcheck::rcmdcheck(error_on = "note")
```

**Note**: This runs multiple validation steps including examples, tests, and documentation checks. Allow 5-10 minutes for completion.

### Testing

```r
# Run all tests
devtools::test()
# or
testthat::test_local(stop_on_warning = TRUE, stop_on_error = TRUE)
```

Tests are located in `tests/testthat/`. The package uses testthat 3.0+ with snapshot testing for validation.

### Linting

The package uses a custom lintr configuration (`.lintr`) with specific requirements:

```r
# ALWAYS load the package first before linting
devtools::load_all()

# Lint the entire package
lintr::lint_package()

# Lint specific file
lintr::lint("R/filename.R")
```

**Important**: Always run `devtools::load_all()` before linting to avoid false positives about undefined functions. This loads the package in development mode, making internal and package functions available to the linter.

**Key linting rules**:
- Use native pipe `|>` (configured in .lintr)
- Follow snake_case naming conventions
- Avoid trailing whitespace
- Ensure consistent code style

Exclusions: Some vignettes may be exempt from specific linters (see `.lintr` configuration).

### Spelling Check

```r
# Check spelling
spelling::spell_check_package()
```

Custom words are in `inst/WORDLIST` (if it exists).

## Continuous Integration (CI) Checks

The following workflows run on every PR. **All must pass** for merge:

1. **R-CMD-check.yaml**: Runs R CMD check on Ubuntu (release, devel, oldrel-1), macOS (release), and Windows (release). Fails on any NOTE. (~10-15 min)

2. **lint-changed-files.yaml**: Lints only files changed in the PR using lintr with custom `.lintr` config. Fails if lints are found. (~2-3 min)

3. **test-coverage.yaml**: Runs on macOS, generates code coverage via covr, uploads to Codecov. (~5-10 min)

4. **check-spelling.yaml**: Spell checks using spelling package. (~1-2 min)

5. **check-readme.yaml**: Renders README.Rmd and verifies it matches README.md. (~2-3 min)

6. **R-check-docs.yml**: Runs `roxygen2::roxygenise()` and checks if `man/`, `NAMESPACE`, or `DESCRIPTION` changed. Fails if documentation is out of sync. (~2-3 min)

7. **news.yaml**: Ensures NEWS.md is updated for every PR. Can be bypassed with `no-changelog` label. (~1 min)

8. **version-check.yaml**: Verifies DESCRIPTION version number increased vs. main branch. Run `usethis::use_version()` to increment. (~1 min)

9. **pkgdown.yaml**: Builds pkgdown website on PR (preview), tags, and main branch pushes. Requires Quarto setup. (~5-7 min)

10. **copilot-setup-steps.yml**: Configures the GitHub Copilot coding agent's environment automatically. Runs when Copilot starts work, when the workflow file changes, or via manual dispatch. Not a required check for PR merges. See "Copilot Setup Workflow" section for details. (~5-10 min)

### PR Commands

Team members can trigger actions by commenting on PRs:
- `/document` - Runs `roxygen2::roxygenise()` and commits changes
- `/style` - Runs `styler::style_pkg()` and commits changes

## Repository Structure

### Key Directories

- **R/**: Package source code (84 R files, ~5,178 lines)
  - Main functions for serological calculations
  - Statistical models and estimators
  - Data processing and validation
  - Plotting and visualization functions
  - `serocalculator-package.R`: Package documentation
  
- **tests/testthat/**: Unit tests
  - Uses snapshot testing with `_snaps/` subdirectory
  - Tests seed RNG for reproducibility
  
- **man/**: Auto-generated documentation - **DO NOT EDIT**

- **data/**: Package datasets
  - Example serological datasets

- **data-raw/**: Raw data processing scripts (not included in package build)

- **inst/**: Installed files
  - Additional package resources
  - `inst/WORDLIST`: Custom spelling dictionary (if exists)

- **vignettes/**: Package vignettes
  - Documentation articles
  - Usage examples

- **pkgdown/**: pkgdown website configuration
  - `_pkgdown.yml`: Site structure, reference organization

- **src/**: C++ source code (Rcpp integration)
  - Compiled code for performance-critical functions

### Configuration Files

- **DESCRIPTION**: Package metadata, dependencies, and version
- **NAMESPACE**: Auto-generated exports - **DO NOT EDIT**
- **.lintr**: Custom lintr configuration
- **.Rprofile**: Interactive session setup (if exists)
- **.Rbuildignore**: Files excluded from package build
- **serocalculator.Rproj**: RStudio project settings
- **_quarto.yml**: Quarto rendering configuration for vignettes
- **codecov.yml**: Code coverage thresholds
- **.gitignore**: Git exclusions

## Common Issues and Workarounds

### Documentation Out of Sync
**Symptom**: R-check-docs.yml workflow fails.
**Solution**: Run `devtools::document()` locally and commit the updated `man/` and `NAMESPACE` files.

### Version Not Incremented
**Symptom**: version-check.yaml workflow fails.
**Solution**: Run `usethis::use_version()` to increment the version in DESCRIPTION.

### NEWS.md Not Updated
**Symptom**: news.yaml workflow fails.
**Solution**: Add a bullet point to NEWS.md under the development version header, or add `no-changelog` label to PR if change doesn't warrant NEWS entry.

### Linting Failures
**Symptom**: lint-changed-files.yaml fails.
**Solution**: Review `.lintr` for custom rules. Common issues:
- Wrong pipe operator (use `|>` not `%>%`)
- Trailing whitespace
- Code style inconsistencies

### Compilation Issues (C++ code)
**Symptom**: Package build fails with Rcpp errors.
**Solution**: Ensure you have proper C++ compiler:
- **Linux**: Install `build-essential` and `r-base-dev`
- **macOS**: Install Xcode command line tools: `xcode-select --install`
- **Windows**: Install Rtools matching your R version

## Testing Requirements Before Code Changes

**ALWAYS establish value-based unit tests BEFORE modifying any functions.** This ensures that changes preserve existing behavior and new behavior is correctly validated.

### Testing Strategy

Choose the appropriate testing approach based on the context:

#### When to Use Snapshot Tests
Use snapshot tests (`expect_snapshot()`, `expect_snapshot_value()`, or `expect_snapshot_data()`) when:
- Testing complex data structures (data.frames, lists, model outputs)
- Validating statistical results
- Output format stability is important
- The exact values are less important than structural consistency

**Examples:**
```r
# For data frames with numeric precision control
dataset |> expect_snapshot_data(name = "test-data")

# For R objects with serialization
results |> expect_snapshot_value(style = "serialize")

# For simple output or error messages
output <- calculate_rates(data) |> expect_no_error()
testthat::expect_snapshot(output)
```

#### When to Use Explicit Value Tests
Use explicit value tests (`expect_equal()`, `expect_identical()`, etc.) when:
- Testing simple scalar outputs
- Validating specific numeric thresholds or boundaries
- Testing Boolean returns or categorical outputs
- Exact values are critical for correctness

**Examples:**
```r
# Testing exact numeric values
expect_equal(calculate_mean(c(1, 2, 3)), 2)

# Testing with tolerance for floating point
expect_equal(calculate_ratio(3, 7), 0.4285714, tolerance = 1e-6)

# Testing logical conditions
expect_true(is_valid_input(data))
expect_false(has_missing_values(complete_data))
```

#### Testing Best Practices
- **Seed randomness**: Use `withr::local_seed()` or `withr::with_seed()` for reproducible tests involving random number generation
- **Use small test cases**: Keep tests fast by using minimal data
- **Platform-specific snapshots**: Use the `variant` parameter in snapshot functions when output differs by OS
- **Test fixtures**: Store complex test data in `tests/testthat/fixtures/` for reuse

### Test-Driven Workflow
1. **Before modifying a function**: Write or verify existing tests capture the current behavior
2. **Add new tests**: Create tests for the new functionality you're adding
3. **Make changes**: Modify the function implementation
4. **Run tests**: Validate all tests pass, updating snapshots only when changes are intentional
5. **Review snapshots**: When snapshots change, review the diff to ensure changes are expected

## Code Organization Policies

**CRITICAL**: Follow these strict code organization policies for all new code and refactoring work:

### File Organization

1. **One function per file**: Each exported function and its associated S3 methods should be in its own file
   - File name should match the function name (e.g., `summary.seroincidence.R` for `summary.seroincidence()`)
   - S3 methods for the same generic can be in the same file (e.g., `compare_seroincidence.seroincidence()`, `compare_seroincidence.seroincidence.by()`, and `compare_seroincidence.default()` all in `compare_seroincidence.R`)

2. **Internal helper functions**: Move to separate files
   - Use descriptive file names (e.g., `compute_cluster_robust_var.R` for `.compute_cluster_robust_var()`)
   - Keep related internal functions together when logical
   - Internal functions should use `.function_name()` naming convention

3. **Print methods**: Each print method in its own file
   - File name: `print.{class_name}.R` (e.g., `print.seroincidence.R`)

4. **Extract anonymous functions**: Convert complex anonymous functions to named helper functions in separate files
   - If an anonymous function is longer than ~5 lines, extract it
   - Name should describe its purpose (e.g., `.helper_function_name()`)

### Example Organization

1. **Long examples**: Move to `inst/examples/exm-{function_name}.R`
   - Use `@example inst/examples/exm-{function_name}.R` in roxygen documentation
   - Keep inline `@examples` short (1-3 lines) for simple demonstrations

2. **Example file naming**: `exm-{function_name}.R`
   - Example: `exm-est_seroincidence.R` for `est_seroincidence()` examples

### Benefits

- **Easier navigation**: Find functions quickly by file name
- **Better git history**: Changes to one function don't pollute history of unrelated functions
- **Clearer code review**: Reviewers can focus on individual functions
- **Reduced merge conflicts**: Multiple people can work on different functions simultaneously
- **Better organization**: Logical structure makes codebase more maintainable

### Migration Strategy

When refactoring existing code:
1. Extract functions to separate files
2. Update any internal calls if needed
3. Run `devtools::document()` to regenerate documentation
4. Run `devtools::check()` to ensure no breakage
5. Run tests to verify functionality unchanged

## Code Style Guidelines

- **Follow tidyverse style guide**: https://style.tidyverse.org
- **Use native pipe**: `|>` not `%>%`
- **Naming**: snake_case, acronyms may be uppercase (e.g., `prep_IDs_data`)
- **Messaging**: Use `cli::cli_*()` functions for all user-facing messages
- **No `library()` in package code**: Use `::` or DESCRIPTION Imports
- **Document all exports**: Use roxygen2 (@title, @description, @param, @returns, @examples)
- **Test snapshot changes**: Use appropriate snapshot testing approaches
- **Seed tests**: Use `withr::local_seed()` for reproducible tests
- **Avoid code duplication**: Don't copy-paste substantial code chunks. Instead, decompose reusable logic into well-named helper functions
- **Quarto vignettes**: Use Quarto-style chunk options with `#|` prefix (e.g., `#| label: my-chunk`, `#| eval: false`)
- **Tidyverse replacements**: Use tidyverse/modern replacements for base R functions where available
- **Write tidy code**: Keep code clean, readable, and well-organized

## Package Development Commands Summary

```r
# Complete development workflow
devtools::load_all()           # Load package for interactive testing
devtools::document()           # Update documentation
devtools::test()               # Run tests
devtools::check()              # Full R CMD check (slow)
usethis::use_version()         # Increment version
lintr::lint_package()          # Check code style
spelling::spell_check_package() # Check spelling
rmarkdown::render("README.Rmd") # Update README
```

## Trust These Instructions

These instructions have been validated against the actual repository structure, workflows, and configuration files. When making changes:

1. **ALWAYS** install R (>= 4.1.0) and all development dependencies when starting work on a PR
2. **ALWAYS** establish value-based unit tests (snapshot or explicit value tests) BEFORE modifying functions
3. **ALWAYS** write tidy, clean, and well-organized code
4. **ALWAYS** run `devtools::document()` after modifying roxygen2 comments
5. **ALWAYS** edit README.Rmd (not README.md) for README changes
6. **ALWAYS** increment dev version number to be one ahead of main branch before requesting PR review
7. **ALWAYS** update NEWS.md for user-facing changes
8. **ALWAYS** run tests before committing (`devtools::test()`)
9. **ALWAYS** check and fix lintr issues in changed files in PRs before committing
10. **ALWAYS** run `devtools::document()` before requesting PR review
11. **ALWAYS** make sure `devtools::check()` passes before requesting PR review
12. **ALWAYS** make sure `devtools::spell_check()` passes before requesting PR review
13. **ALWAYS** run `pkgdown::build_site()` before requesting PR review to ensure the pkgdown site builds successfully
14. **ALWAYS** verify Quarto documents render successfully locally - don't rely on CI workflows
15. When `pkgdown::build_site()` has errors related to Quarto, use `quarto::quarto_render(input = "path/to/file.qmd", quiet = FALSE)` to debug

Only search for additional information if these instructions are incomplete or incorrect for your specific task.
