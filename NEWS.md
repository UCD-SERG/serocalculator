# serocalculator (development version)

## New features

* Added interactive Shiny app `curve_app()` for visualizing antigen-antibody
  kinetics models with real-time parameter sliders (#392).
* Added `antibody_decay_curve()` and `pathogen_decay_curve()` functions for
  simulating antibody and pathogen decay over time (#392).
* Added `plot_decay_curve()` for plotting decay functions using ggplot2 (#392).
* Added helper functions `t1f()` (time to end of active infection) and
  `y1f()` (peak antibody concentration) (#392).
* `autoplot.sim_results()` gains `x_var`, `group_var`, and `color_var`
  arguments, letting users choose which columns map to the x-axis, group,
  and color aesthetics instead of the previous hardcoded `sample_size` /
  `lambda.sim` mapping.
* `graph.curve.params()` now uses the 5-parameter `ab_5p()` antibody response
  model and supports `units`-aware curve parameters. (#393)
* Added `ab_5p()`, a 5-parameter antibody response model that supports
  {units}-aware inputs (e.g. `t = units::as_units(50, "days")`), building on
  the existing `bt()` active-phase helper. CI now installs the system
  `udunits2` library on macOS and Windows so the new `units` dependency can
  compile there. (#393)
* Added `sim_pop_data_2()`, a `sim_pop_data()` alternative built on `ab_5p()`
  that simulates each simulated individual's age (`sim_age()`) and time
  since their last seroconversion (`sim_time_since_last_sc()`) directly,
  rather than simulating an infection history. Both new helper functions,
  and `sim_pop_data_2()` itself, accept `units`-aware inputs.
* `sim_pop_data_multi()` gained a `sim_function` parameter (default
  `sim_pop_data`) so callers can select `sim_pop_data_2()` instead. (#393)

## Documentation

* Took the sidebar fold control from `altdoc` instead of keeping a copy here.
  `altdoc/sidebar-fold.html` and the matching block in `altdoc/styles.css` are
  replaced by `include-in-header: $ALTDOC_SIDEBAR_FOLD`, which `altdoc` stages
  at render time with script and style together.
  The same pair had been copied into `ucdavis/bcs`, and the two copies had
  already drifted -- this repo changed its copy to start folded and nothing
  carried that across.
  The sidebar still starts folded, now via `sidebar_fold: collapsed` in
  `altdoc/reference.yml`, so what used to be a source-level divergence is a
  one-line setting.

* Declared the reference manual's grouping once, in a new `altdoc/reference.yml`
  (#610).
  It used to live in two hand-maintained places that nothing kept in step: the
  `altdoc/reference.qmd` index page and the `Reference` block of
  `altdoc/quarto_website.yml`.
  Adding a topic to one and missing the other rendered cleanly and raised no
  warning, which is how it went unnoticed on #392.
  altdoc now builds both surfaces from the single file, so the two cannot
  disagree, and warns about any exported topic no section claims rather than
  leaving it off the site in silence.
  The published grouping is unchanged: the same nine sections, the same 42
  topics, in the same order.

* Moved the `altdoc` dependency off the `recursive-qmd-search` branch and onto
  the default branch, which is what supplies the grouped sidebar above.
  That branch has no commits the default branch does not already contain, so
  it was holding the docs build behind for nothing.
  The branch was named in three places -- `Remotes`, `Config/Needs/website`,
  and the `r-packages` input of the docs workflow -- and changing one of them
  alone makes the build fail to resolve rather than fall back, since `pak`
  reads the two as conflicting requirements for the same package.

* Started showing each topic's name alongside its title in the sidebar.
  Titles alone left `as_pop_data` and `load_pop_data` as adjacent entries
  reading identically, and the same for `as_noise_params` and
  `load_noise_params`, with nothing to tell a reader which was which.

* Stopped `.gitignore` from hiding new files added under `altdoc/`.
  A bare `altdoc` entry ignored the whole source directory; the files already
  in it stayed tracked only because they predate the rule, so the omission was
  invisible until a *new* one was added and silently skipped by `git add`.
  `altdoc/.gitignore` already covers the real build artifacts there
  (`_site/`, `.quarto/`, `pkgdown.yml`, `freeze.rds`), and `_quarto` is still
  ignored separately, so the entry protected nothing.
* Added a control that folds the documentation website's sidebar away, giving
  the content the width it held (#622). Quarto's `collapse-level` folds
  sections *within* the sidebar; it has no control for the sidebar itself on a
  website, so the sidebar held a fixed grid column at every viewport wide
  enough to show it. The new navbar button hides it and moves the content's
  grid start line into the vacated columns, which at a `1400px` viewport
  widens the content from `630px` to `922px`. The state is kept in
  `localStorage`, so it survives navigation between pages. The script that
  restores it is included in the page header rather than after the body, so a
  folded sidebar never flashes into view.
  Below Quarto's `992px` breakpoint the button is hidden: the sidebar is already
  a drawer laid over the content there, so it holds no width to reclaim.
* The documentation website's sidebar now starts folded. Readers arrive on a
  page to read it, and the navbar already carries the same destinations, so the
  wider content column is the better default; unfolding the sidebar is
  remembered, so the choice is still made once per reader rather than once per
  page.
* Fixed two dead external links. The `methodology` article credited the
  *Salmonella* Typhi photo to a Wikimedia Commons page that was deleted on
  2026-03-11; the deletion discussion confirms the image is CDC/PHIL
  public-domain work (PHIL ID 2115), so the credit now cites PHIL directly
  instead of the dead page. The Ubuntu R install snippet in
  `.github/copilot-instructions.md` fetched `pubkey.gpg`, which CRAN no longer
  serves (404); it now uses `marutter_pubkey.asc`.
* Figures on the documentation website now open an enlarged lightbox view when
  selected. Enabled site-wide (`lightbox: true`) across every format, including
  the revealjs slides, and covering both cross-referenced figures and plain
  images (#584).
* Restored the R code in the documentation website's vignettes and articles.
  `vignettes/_metadata.yml` set `echo: false` for the `html` and `docx`
  formats, which applied to every file under `vignettes/`, so the published
  tutorials rendered as prose and figures with no code at all, and the
  `methodology` article's "Estimating seroincidence" section --- whose only
  content is a non-evaluated code chunk --- rendered as an empty heading.
* Reviewed and revised the `methodology` article: corrected the standard-error
  description (the inverse negative Hessian is the *variance*, not the
  standard error) and the "truncated by age" characterization of the latent
  infection-time distribution, added narrative to the previously code-only
  "Estimating seroincidence" and "Multiple biomarkers" sections, moved
  "In-progress work" out of the "Validation" section into its own section,
  promoted "References" to a top-level section, and restored the captions and
  image credits on the two typhoid figures (adjacent images with no blank line
  between them are parsed as one paragraph, which drops their captions).
* Fixed dead documentation links that still used the old pkgdown site layout.
  The site root is now a redirect page, so `/articles/…`, `/reference/index.html`,
  `/news/index.html`, and `/CONTRIBUTING.html` no longer resolve; links from
  within the site are now relative so they stay on the reader's docs version,
  and the links in the README point at paths that exist. Also corrected the
  description in the README of which URL serves which docs version, and fixed
  the link to the contributing guide in the pull request template.
* Fixed a dead link in the README: the in-development documentation pointed at
  `/main/`, which has served nothing since #504 moved development docs to
  `/dev/`. Also removed a duplicated "the" in the same sentence. (#604)
* The documentation site now deploys multiple versions side by side, following
  `rpt`'s pattern: pushes to `main` deploy development docs to `/dev/`,
  published releases deploy stable docs to `/latest-tag/` (plus an archived
  `/vX.Y.Z/` copy), and the site root redirects to whichever was deployed most
  recently. A "Versions" navbar dropdown links between them. (#504)
* Updated the documentation site configuration to promote "Get started",
  "Reference", and "News" in the top navigation, and added a grouped
  `reference.qmd` index plus grouped sidebar reference sections that
  exclude internal-only topics.
* The documentation site's reference index page is now titled "Package index"
  rather than "Reference", matching the old pkgdown site and distinguishing the
  page from the navigation entry that links to it. (#594)
* Added a "Citation" entry to the documentation site's sidebar. The citation
  page was previously reachable only from the navbar "More" dropdown, unlike
  `rpt` and the default sidebar `altdoc` ships, which both list it. (#594)
* Links to the documentation site's old `/main/` paths now resolve again.
  Before the altdoc migration the development docs were published under the
  branch name, so they lived at `/main/`; they now live at `/dev/`, which left
  every `/main/...` link dead. The site now serves a root `404.html` that
  redirects any request under `/main/` to the same path under `/dev/`, deep
  links included. Redirection requires JavaScript; without it the page renders
  as a plain not-found notice linking to the documentation home page. (#599)
* Updated the documentation site's dark-mode styling to match `rpt` by adding
  the same inline-code color override used there, improving contrast on the
  home page and article text.
* Added introductory lecture slides to the `methodology` vignette
  ("Estimating Incidence Rates from Cross-Sectional Serosurveys").
* The `methodology` vignette now loads its `slidebreak` shortcode explicitly so
  the shortcode no longer leaks into HTML output, and rendered vignettes now
  suppress package startup messages.
* Fixed the date in the vignettes' title blocks, which rendered as
  "Invalid Date" (most visibly on the `methodology` slides' title slide).
  `vignettes/_metadata.yml` set the date with an inline R expression, but
  Quarto merges that file into the document metadata without a `knitr` pass,
  so the expression was never evaluated; the date now uses Quarto's own
  `today` keyword. (#597)
* Restored the `methodology` vignette's docx download link, which had been
  dropped on the (mistaken) assumption that it needed its own `docx:` format
  block; it renders fine via the `docx:` default already declared in
  `vignettes/_metadata.yml`.
* Completed the measurement-noise model in the `methodology` vignette
  (multiplicative relative error), added a "Combined biological and
  measurement noise" section, and added a "Noise and never-infected
  subjects" section explaining that additive biological noise spreads a
  never-infected subject's measured response over a positive range while
  multiplicative measurement noise leaves a true zero at zero. (#561)
* Corrected the documentation of the `eps` measurement-noise parameter
  (in `example_noise_params_pk`/`example_noise_params_sees` and the
  vignettes): `eps` is the bound on the relative measurement error
  (`Unif(-eps, eps)`), not a coefficient of variation. A measured CV
  corresponds to `eps = sqrt(3) * CV`. (#563)
* Added the never-infected density under combined biological and
  measurement noise to the `methodology` vignette: the piecewise
  closed form for `y_obs = eps_b * (1 + xi)`, matching Teunis and van
  Eijkeren (2020) Equation 19 and verified to integrate to the
  never-infected probability. (#567)
* Made the never-infected term explicit in the "per-person likelihood"
  slide of the `methodology` vignette: the observed-data likelihood
  integral is now shown split into its continuous (ever-infected) and
  discrete (`T = NA`, never-infected) parts, with `p(Y=y | T=NA)`
  defined as a point mass at zero (before noise). (#567)
* Explained, in the "Biological noise" section of the `methodology`
  vignette, why the biological-noise width `nu` is estimated as the
  95th percentile of negative controls: Teunis and van Eijkeren (2020)
  show that a uniform noise model only needs to match the true noise
  distribution's width, not its exact shape, and note that this width
  is difficult to verify against a mixed (ongoing-seroresponse)
  population -- motivating estimation from a clean negative-control
  panel instead. Also noted that the specific choice of the 95th
  percentile (rather than, e.g., the 99th or the sample maximum) is an
  adopted convention, not a result derived or optimized in the paper.
  (#567)
* Added the conditional variance `Var(y_obs | y_true)` for the combined
  biological- and measurement-noise model to the `methodology` vignette,
  derived from the independent-product-variance identity and checked
  against both single-source special cases already in the vignette.
  (#571)
* Added `Var(y_obs | T=t)`, marginalizing over between-person heterogeneity
  in `y_true`, to the `methodology` vignette: derived via the law of total
  variance from the `Var(y_obs | y_true)` formula above, with the
  between-person heterogeneity term `Var(y_true | T=t)` introduced
  symbolically (it has no closed form in this framework, since
  `serodynamics` represents curve-parameter heterogeneity as an empirical
  posterior sample rather than a stated parametric distribution). Clarified
  that the longitudinal model's residual variance is constant on the log
  scale conditional on individual random effects, while random waning rates
  can induce time-varying marginal population variance that `serocalculator`
  carries forward by averaging over kinetic-parameter draws. (#571)
* Moved `f_dev0()`'s `@examples` block to a separate example file
  (`inst/examples/exm-f_dev.R`), following the convention already used by
  other functions in this package. (#393)

## Internal

* Regenerated `NAMESPACE` and `DESCRIPTION` with `roxygen2` 8.1.0, which groups
  each package's `importFrom()` symbols into a single multi-line directive
  instead of one directive per symbol.
  The change is purely one of formatting: the exports, the S3 method
  registrations, the compiled-library declaration, and all 75 imports are
  unchanged.
  `docs-check` regenerates the documentation and fails when a tracked file
  differs afterward, so it had been red on every pull request since
  `roxygen2` 8.1.0 was released on 2026-08-04. (#636)
* Disabled the `@claude` agent bot.
  `.github/workflows/claude.yml`'s reactive triggers are commented out and its
  job carries `if: false`, so no comment, issue, or review event invokes the
  agent, and neither does a manual dispatch (the reusable workflow runs
  unattended on `workflow_dispatch` by design).
  Reviews are the only Claude capability left, and they run on request only:
  automatic review on pull-request activity is off, and a review starts when a
  collaborator comments `/review` on a pull request.
  That dispatch now pins the review run to the pull request's head branch, so
  its check-run lands on the right commit.
* Fixed two dead provenance links in workflow comments. `check-readme.yaml`
  cited a file that was deleted from `IndrajeetPatil/statsExpressions`'s
  default branch when its CI moved to reusable workflows; the citation is now
  pinned to a commit where the file exists. `test-coverage.yaml` cited a GitHub
  Actions *run* URL, which expires after roughly 90 days; it now cites the
  upstream workflow file.
* Removed `.github/CONTRIBUTING.Rmd`; `.github/CONTRIBUTING.md` is now the
  hand-maintained source. The `.Rmd` executed no code (its only chunk was
  `eval = FALSE`), so it rendered to markdown identical to a hand-written file
  while adding a source/generated pair with no CI check to keep the two in
  sync. (#613)
* Documentation website now renders HTML (primary), docx (download link on every page), and revealjs (slides for `methodology.qmd`) formats. Fixed the HTML/revealjs output-filename collision by specifying `output-file: methodology-slides.html` for revealjs in `methodology.qmd`'s frontmatter; docx goes in `_metadata.yml` globally since `.docx` has no collision risk. (#503)
* Added Codex repository guidance and R-package workflow skills. (#574)
* `news.yaml` now calls the central
  [`d-morrison/gha`](https://github.com/d-morrison/gha) `check-news.yml@v2`
  reusable workflow (bumped from the initial `@v1` pin, which predated gha's
  fix for this repo's `no-changelog` label convention) instead of invoking
  `UCD-SERG/changelog-check-action@v2` directly. (#537, #593)
* `claude.yml` and `claude-code-review.yml` now call the central
  [`d-morrison/gha`](https://github.com/d-morrison/gha) `claude.yml@v2` and
  `claude-code-review.yml@v2` reusable workflows instead of carrying their own
  copy of the agent/review machinery. (#549)
* `docs.yaml` now calls the central
  [`d-morrison/gha`](https://github.com/d-morrison/gha)
  `altdoc-multiversion-docs.yml@v2` reusable workflow instead of carrying its
  own ~360-line copy of the build/deploy logic, and the local
  `.github/scripts/generate_version_dropdown.py` and
  `generate_multiversion_landing_page.py` copies are deleted in favor of the
  composite actions in that repo. Fixes made centrally now reach this package
  instead of stopping at `rpt`. Rewrote `.github/MULTI_VERSION_DOCS.md`, which
  still described the `pkgdown` setup replaced during the altdoc migration.
  (#595)
* The `methodology` vignette's LaTeX macros now come from the shared
  [`d-morrison/macros`](https://github.com/d-morrison/macros) git submodule
  (included via `{{< include ../macros/macros.qmd >}}`) instead of a local
  `vignettes/articles/_macros.qmd`. The deck adopts the shared macro
  vocabulary (e.g. `\dens` for the density function in place of the local
  `\pdf`). (#534)
* `claude-code-review.yml` now sets `allowed_bots: github-actions[bot]` so the review still runs (and posts feedback) when `claude.yml` re-dispatches it on an `@claude review` comment; previously the bot-initiated dispatch aborted with "Workflow initiated by non-human actor".
* `claude.yml` now grants the `@claude` agent the file tools (`Read`/`Glob`/`Grep`/`Edit`/`MultiEdit`/`Write`) in `--allowedTools`; previously the agent could run checks/git/gh but not edit files, so it fell back to posting diffs for manual application.
* Added the `iterate` Claude Code skill (`.claude/skills/iterate/`) for driving a PR to a clean review verdict.
* Ported the `@claude` agent and PR-review GitHub Actions workflows (plus Claude/Copilot config: `CLAUDE.md`, `.claude/` settings and slash commands, and path-scoped `.github/instructions/`) from the UCD-SERG `qwt` template, adapted to this package. (#523)
* Claude PR review workflow now skips (rather than hard-failing) when triggered by a bot (e.g. `claude[bot]` pushing a commit). (#519)
* Added the `lint-changed-lines` CI workflow (calling the reusable
  [`d-morrison/gha`](https://github.com/d-morrison/gha)
  `lint-changed-lines.yml@v2` workflow), which flags lint issues only on the
  lines a PR actually adds or modifies (rather than whole changed files, as
  `lint-changed-files` does). This lets lint rules be adopted or tightened
  incrementally as code is touched, instead of forcing a repo-wide reformat.
  Intended to replace `lint-changed-files` as the lint gate once branch
  protection is updated to require it. (#558)
* Removed `docs.yaml`'s job-level `concurrency:` group, which resolved to the
  same string as the workflow-level group on every non-`pull_request` event and
  so deadlocked the `docs` job: the run already held that group, so the job
  could never acquire it and was failed instantly with no logs. This blocked
  every versioned documentation deploy (`/dev/`, `/latest-tag/`, `/vX.Y.Z/`).
  The workflow-level group already serializes runs by PR number or ref. (#590)

## Bug fixes

* `sim_pop_data()` and `sim_pop_data_multi()` now produce identical results
  across operating systems. Simulated inter-infection times are now rounded to
  whole days, so the number of random draws consumed no longer depends on
  platform-specific floating-point results of `log()` (which previously
  shifted the random-number stream out of sync and made simulated values, and
  their snapshots, differ between macOS, Windows, and Linux). Simulated
  values change slightly as a result of this fix. (#447)
* Corrected default axis labels in `strat_ests_barplot()` (`xlab`) and
  `strat_ests_scatterplot()` (`ylab`) to say "seroincidence" rather than
  "seroconversion"/"incidence".
* `load_noise_params()` and `load_sr_params()` now fail gracefully with informative messages when internet resources are unavailable, complying with CRAN policy (#505)
* Added Version Crosswalk article to pkgdown website to help users migrate code from v1.3.0 to v1.4.0
  - Provides clear tables comparing old and new function names
  - Includes code examples showing how to update existing code
  - Accessible as a prominent tab in the website navigation
* `autoplot.seroincidence()` now raises its "graphs cannot be extracted" error
  via `cli::cli_abort()` rather than `stop()`, so the message is formatted as a
  cross bullet naming the problem and an info bullet naming the
  `build_graph = TRUE` argument that was missing (#392).
* `antibody_decay_curve()`, `pathogen_decay_curve()`, `t1f()`, and `y1f()` now
  validate their parameters (non-negative values, `mu_y != mu_b`) and raise an
  informative `cli::cli_abort()` error instead of silently returning `NaN` or
  dividing by zero (#392).
* `plot_decay_curve()` now respects its `xmax` argument instead of always
  plotting to `x = 100` (#392).

## Compatibility

* Replaced deprecated `dplyr::is.grouped_df()` usage with `dplyr::is_grouped_df()` in `df_to_array()` for compatibility with newer dplyr releases.

## New features (cluster-robust SE)

* Added `cluster_var` and `stratum_var` parameters to `est_seroincidence()` and 
  `est_seroincidence_by()` to support cluster-robust standard error estimation. 
  When `cluster_var` is specified, `summary.seroincidence()` automatically computes 
  cluster-robust (sandwich) variance estimates to account for within-cluster 
  correlation in clustered sampling designs such as household or school-based surveys.
* `cluster_var` parameter now accepts multiple variables (e.g., `c("school", "classroom")`)
  for multi-level clustered sampling designs. Cluster-robust standard errors will account
  for all specified clustering levels.

## Bug fixes (cluster-robust SE)

* Fixed column naming issue in `summary.seroincidence()` where cluster-robust standard
  errors caused `[]` notation in column names (`SE[,1]` instead of `SE`).
* Added `se_type` column to `summary.seroincidence()` output to clearly indicate whether
  "standard" or "cluster-robust" standard errors are being used.
* Fixed `est_seroincidence_by()` to properly pass cluster and stratum variables through
  to stratified analyses. Previously, these variables were dropped during data stratification,
  causing errors when trying to use clustering with `est_seroincidence_by()`.

## Code organization

* Refactored clustering-related code following package organization policies:
  - Moved `.compute_cluster_robust_var()` to `R/compute_cluster_robust_var.R`
  - Each function now in its own file for better maintainability and git history
* Updated copilot-instructions.md with code organization policies

## Dependencies

* Replaced `ggpubr` with `patchwork` for arranging multi-panel plots,
  removing the indirect `ggrepel` transitive dependency.

# serocalculator 1.4.0

## New features

* Added support for cluster-robust standard errors in `est_seroincidence()` through
  new `cluster_var` and `stratum_var` parameters. When `cluster_var` is specified,
  `summary.seroincidence()` automatically computes cluster-robust (sandwich) variance
  estimates to account for within-cluster correlation in clustered sampling designs
  such as household or school-based surveys.
* Added `compare_seroincidence()` function for statistical comparison of seroincidence rates
  - Performs two-sample z-tests to compare seroincidence estimates
  - Returns `htest` format when comparing two single estimates
  - Returns formatted table with all pairwise comparisons for stratified estimates
  - Added examples to tutorial vignette and comprehensive unit tests
* Implemented multi-version pkgdown documentation with version dropdown menu
  - Users can now switch between main, latest-tag, and versioned releases
  - Default landing page shows latest-tag (most recent release)
  - Based on insightsengineering/r-pkgdown-multiversion setup
* Added `chain_color` option to `graph.curve.params()` to control MCMC line color (#455)
* Made `graph.curve.params()` the default sub-method for `autoplot.curve_params()` (#450)
* Added `log_x` and `log_y` options to `graph.curve.params()` sub-method for 
`autoplot.curve_params()` (#453)
* Extended `sim_pop_data_multi()` to loop over multiple sample sizes (#444)
* Added new functions `analyze_sims()` and `autoplot.sim_results()` (#444)
* Rename `estimate_scr()` to `est_seroincidence_by()` (#439)
* Rename `estimate_scr()` to `est_seroincidence()` (#432)
* Rename argument `curve_params` to `sr_params` for estimation functions (#424)
* added documentation for `count_strata()` (#431)
* Rename  `as_curve_params()` to `as_sr_params()` (#421)
* Rename `load_curve_params()` to `load_sr_params()` (#421)
* added default for `xvar` in `"scatter"` option for `autoplot.seroincidence.by()` (#417)
* Extended `autoplot.summary.seroincidence.by()` to include types for either scatter or bar plots of stratified results (#397)
* added option to add lines using `group_var` input to `autoplot.summary.seroincidence.by()` (#410)
* `autoplot.pop_data(type = "age-scatter")` now shows legend at bottom (#407)
* `autoplot.pop_data(type = "age-scatter")` now facets by antigen isotype (#406)
* Rename `est.incidence.by()` to `estimate_scr_by()` (#389)
* Rename `est.incidence()` to `estimate_scr()` (#389)
* Improved warning messages for `get_biomarker_names_var()`
* Added `get_*()` extractor functions to API (#380)
* Added optional CI error bars to `autoplot.summary.seroincidence.by()` (#372)
* Improved y-limit calculation in `graph.curve.params()` (#368)
* Added option for `graph.curve.params()` to show all curves (#368)
* Added color-coding for `graph.curve.params()` (#383)
* Added `quantiles` parameter to `graph.curve.params()` and corresponding test in `test-graph.curve.params.R` (#434)
* Removed `warn.missing.strata()` from API (#366)

* Added more details about contributing PRs in `Contributing.md` (#280)

* Added warnings for missing biomarker data (#168):
  - completely missing antigen-isotype in a stratum
  - uneven antigen-isotype counts in a stratum (likely from incomplete data)

* Split dev and release websites into:
   - release: https://ucd-serg.github.io/serocalculator/
   - dev: https://ucd-serg.github.io/serocalculator/dev/

* Fixed citations in `methodology.qmd` article (#360)

* Added outline to pkgdown website (#353)
* Added verbose option for `summary.seroincidence()` and 
`summary.seroincidence.by()` (#348)
* Extended `simulate_xsectionalData.Rmd` article to explore
`renew_params = TRUE` vs `renew_params = FALSE` (#348)

* Renamed variables for consistency (#281, #373):
  - `sim.cs()` -> `sim_pop_data()` 
  - `sim.cs.multi()` -> `sim_pop_data_multi()`

## Bug fixes

* Fixed CRAN errors (#464)
* Fixed stratification issue in enteric fever vignette (#418)
* Fixed issue in `graph.curve.params()` where MCMC samples 
with the same iteration number from different MCMC chains
would get merged by `ggplot2::aes(group = iter)` (#382)

## Internal changes

* switched `expect_snapshot_data()` to an internal function due to CRAN errors (#464)
* generalized `ab1()`
* added codecov/test-results-action to test-coverage.yaml workflow
* added test for censored data in f_dev() (#399)
* added test for `autoplot.curve_params()`
* added test for `graph.curve.params()` (#368)
* reverted Readme source file from qmd to Rmd.
* switched pkgdown GHA from `any::pkgdown` to `r-lib/pkgdown` (i.e., dev version) (#359)
* added test for `summary.seroincidence.by()` (#352)
* Started checking for use of base pipe instead of magrittr pipe
by linter (#347)
* Removed `ldpar()` from API (#345)
* Added test for `sim.cs()` (#344)
* Added test for internal function `ab()` (#342)

* Reverted name change `ldpar()`-> `row_longitudinal_parameter()` (#343)

# serocalculator 1.3.0

## New features

* Removed function 'get_additional_data()' (#332)

* Updated documentation examples to include csv files (#328)

* Added csv files for use in documentation examples (#329)

* Added `serocalculator_example()` function to help locate example data files (#329)

* Fixed a bug in computing the antibody response curve when $r=1$ (#323)

* Added example datasets with documentation for examples and testing (#314)

* Improved error messaging for `autoplot.pop_data()` (#234).

* Clarified package installation instructions in scrub typhus vignette (#234).

* Add `as_noise_params` (#228) 

* Updated `simulate_xsectionalData.Rmd` (linting, removing deprecated functions)
(#289)

* Added default value for `antigen_isos` argument in `log_likelihood()` (#286)

* Updated enteric fever example article with upgraded code and visualizations (#290)

* Added `Methodology` vignette (#284, #302, #303)

* Added template for reporting Issues 
(from `usethis::use_tidy_issue_template()`) (#270)

* Added template for pull requests 
(from <https://github.com/bcgov/ssdtools>) (#265)

## Internal changes
* Updated documentation to align with previous CRAN feedback (#328)

* Updated tests to use internal testing datasets instead of external links (#328)

* Updated `test-coverage.yml` GHA action to current `r-lib` standard (#330)

* Change default pipe setting (#312)

* Add test for missing strata in `est.incidence.by` (#227)
* Added `snapshot_value` test for `est.incidence()` (#315)

* Sped up `lint-changed-files` GitHub Action (#317)

* Added online preview builds for PRs that change the `pkgdown` website (#309)

* Added `test-autoplot.pop_data` test (#234)

* initialized [`lintr`](https://lintr.r-lib.org/) with `lintr::use_lint()` (#278)

* created unit test for `df_to_array()` (#276)

* fixed `dplyr::select()` deprecation warning in `df_to_array()` (#276)

* Added `devtag` to package (using `devtag::use_devtag()`) (#292)

* Added `@dev` tag to `?df_to_array()` (#292)

* Generalized `get_()` and `set_()` methods to be general-purpose
(no S3 class-specific methods needed yet) (#274).

* Updated GitHub Action files and reformatted `DESCRIPTION` (#268)
* Added `.gitattributes` file (<https://git-scm.com/docs/gitattributes>)
copied from <https://github.com/tidyverse/ggplot2>

* Added QR code to `README.qmd`
* Added additional automated checks through 
[GitHub actions](https://docs.github.com/en/actions), 
including:
  - check that `README.qmd` still compiles 
  (advice from [preventive-r-package-care](https://indrajeetpatil.github.io/preventive-r-package-care/#/preventive-care-for-r-packages)) (#259)
  - check `NEWS.md` for updated change log (#257)
  - lint changed files (#256)

# serocalculator 1.2.0

* Added `test-summary.pop_data` test

* Modified `test-est.incidence` test

* Added stratification to `summary.pop_data`

* Added `verbose` option for `check_pop_data()`, changing default behavior
to avoid printing an OK message.

# serocalculator 1.1.0

* Renamed `llik()` to `log_likelihood()`

* Renamed `fdev()` to `f_dev()`

* Renamed `df.to.array()` to `df_to_array()`

* Renamed `getAdditionalData()` to `get_additional_data()`

* Removed `clean_pop_data()` function

* Remove `clean_pop_data()` dependency functions documentation examples

* Added `age`, `value`, `id` and `standardize` arguments to `load_pop_data()`

* Added the following methods to `pop_data` class:

  - `set_age()`
  - `set_value()`
  - `set_id_var()`
  - `get_age()`
  - `get_values()`
  - `ids()`
  - `get_age_var()`
  - `get_values_var()`
  - `ids_varname()`
  
* Added additional warnings to `load_pop_data()`

* Added `scales::pseudo_log_trans()` to `autoplot.pop_data()` to avoid log 0

* Added `test-est.incidence-status.R` test to check output when `standardize` option is FALSE on `load_pop_data()`

* Replaced column name comparison on `check_pop_data()` to use attribute name on `pop_data` class

# serocalculator 1.0.1

* added `n_points` argument to `plot_curve_params_one_ab()`
* Added `type = "age-scatter"` option for `autoplot.pop_data()`

## serocalculator 1.0.0

* Moved underlying methods to `serocalculator` vignette

## serocalculator 0.5.0

* Spell-checking of function documentation and tutorial articles.

* Added functions and methods:

  - `load_pop_data()`
  - `check_pop_data()`
  - `summary.pop_data()`
  - `autoplot.pop_data()`
  - `load_curve_params()`

* Renamed `graph.decay.curves.by()` to `autoplot.curve_params()`

## serocalculator 0.4.0

* `plot()` methods have been renamed to `autoplot()`, matching general convention for `ggplot2`-based graphics.

* added visualization of curve parameters

* `sim.cs()` now has `format` argument to specify long or wide format for output.

### serocalculator 0.3.2

Fixed bug in passing `antigen_isos` from `est.incidence.by()` to `est.incidence()`.

### serocalculator 0.3.1

Rolled back required R version from 4.2 to 4.1

## serocalculator 0.3.0

* Fixed stability and documentation-clarity issues after initial tester feedback.

## serocalculator 0.2.0 (never formally incremented in DESCRIPTION)

* Added new vignettes.

## serocalculator 0.1.0

Forking from the seroincidence package and adding Teunis et al 2020 approach.
