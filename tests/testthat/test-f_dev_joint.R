# The joint (shared latent time) likelihood -- see issues #637 and #646.

joint_test_inputs <- function(antibodies = c("HlyE_IgA", "HlyE_IgG")) {
  list(
    antibodies = antibodies,
    curves = serocalculator::typhoid_curves_nostrat_100 |>
      dplyr::filter(.data$antigen_iso %in% antibodies),
    xs_data = serocalculator::sees_pop_data_pk_100,
    noise = serocalculator::example_noise_params_pk |>
      dplyr::filter(.data$antigen_iso %in% antibodies)
  )
}

test_that("with one biomarker, the joint likelihood equals the marginal", {
  # With biological noise only and no censoring, `serocalc.c` evaluates
  # the marginal density in closed form, so this comparison isolates the
  # shared-latent-time integral itself. Its censored-observation
  # probabilities, and its measurement-noise densities, are Riemann sums
  # with step `max(y1)/100`, which is coarse relative to the noise
  # intervals, so with censoring the two agree only to about 1e-3, and
  # with measurement noise only to a few percent. The brute-force test
  # below covers those cases instead.
  inputs <- joint_test_inputs()
  noise <- inputs$noise |> dplyr::mutate(eps = 0)
  lambdas <- c(0.05, 0.1, 0.3)

  for (cur_antibody in inputs$antibodies) {
    for (y_low in c(0, unique(noise$y.low))) {
      cur_noise <- noise |> dplyr::mutate(y.low = y_low)
      marginal <- log_likelihood(
        lambda = lambdas,
        pop_data = inputs$xs_data,
        curve_params = inputs$curves,
        noise_params = cur_noise,
        antigen_isos = cur_antibody
      )
      joint <- -f_dev_joint(
        lambda = lambdas,
        pop_data = inputs$xs_data,
        curve_params = inputs$curves,
        noise_params = cur_noise,
        antigen_isos = cur_antibody
      )
      expect_equal(
        joint,
        marginal,
        tolerance = if (y_low == 0) 1e-5 else 5e-3
      )
    }
  }
})

test_that("the joint likelihood matches a brute-force integral", {
  # Independent R implementation of the model in `src/serocalc_joint.c`:
  # for each subject, the never-infected mass plus a fine midpoint-rule
  # integral over the shared latent time of the product of the
  # per-biomarker conditional densities, averaged over paired draws.
  inputs <- joint_test_inputs()
  lambda <- 0.1
  curves <- inputs$curves |>
    dplyr::filter(.data$iter <= 10) |>
    dplyr::mutate(alpha = .data$alpha * 365.25, d = .data$r - 1)
  noise <- inputs$noise

  ystar <- function(t, y1, alpha, d) y1 / (1 + d * y1^d * alpha * t)^(1 / d)
  dens_bm <- function(y, ys, nu, eps) {
    zmin <- pmax(ys, y / (1 + eps))
    zmax <- pmin(ys + nu, y / (1 - eps))
    ifelse(zmin < zmax & zmin > 0, log(zmax / zmin) / (2 * eps * nu), 0)
  }
  prob_bm <- function(y, ys, nu, eps) {
    b1 <- pmin(pmax(y / (1 + eps) - ys, 0), nu)
    b2 <- pmin(pmax(y / (1 - eps) - ys, 0), nu)
    p <- b1
    ok <- b2 > b1 & ys + b1 > 0
    p[ok] <- p[ok] +
      (y * log((ys[ok] + b2[ok]) / (ys[ok] + b1[ok])) -
         (1 - eps) * (b2[ok] - b1[ok])) / (2 * eps)
    p / nu
  }
  cond_term <- function(y, ys, cur_noise) {
    if (y <= cur_noise$y.low) {
      prob_bm(cur_noise$y.low, ys, cur_noise$nu, cur_noise$eps)
    } else {
      dens_bm(y, ys, cur_noise$nu, cur_noise$eps)
    }
  }
  brute_force <- function(rows) {
    age <- rows$age[1]
    q_a <- exp(-lambda * age)
    p_a <- 1 - q_a
    t_grid <- seq(0, age, length.out = 20001)
    t_mid <- (t_grid[-1] + t_grid[-length(t_grid)]) / 2
    dt <- t_grid[2] - t_grid[1]
    never <- q_a
    integrand <- function(draw) {
      p_a * (lambda * exp(-lambda * t_mid) + q_a / age)
    }
    per_draw <- vapply(
      sort(unique(curves$iter)),
      FUN.VALUE = numeric(1),
      FUN = function(draw) {
        prod_t <- integrand(draw)
        for (cur_antibody in inputs$antibodies) {
          cur_noise <- noise[noise$antigen_iso == cur_antibody, ]
          cur_curve <- curves[
            curves$antigen_iso == cur_antibody & curves$iter == draw,
          ]
          y <- rows$value[rows$antigen_iso == cur_antibody]
          prod_t <- prod_t * cond_term(
            y,
            ystar(t_mid, cur_curve$y1, cur_curve$alpha, cur_curve$d),
            cur_noise
          )
        }
        sum(prod_t) * dt
      }
    )
    for (cur_antibody in inputs$antibodies) {
      cur_noise <- noise[noise$antigen_iso == cur_antibody, ]
      y <- rows$value[rows$antigen_iso == cur_antibody]
      never <- never * cond_term(y, 0, cur_noise)
    }
    log(never + mean(per_draw))
  }

  for (cur_id in unique(inputs$xs_data$id)[1:5]) {
    rows <- inputs$xs_data |> dplyr::filter(.data$id == cur_id)
    expect_equal(
      log_likelihood(
        lambda = lambda,
        pop_data = rows,
        curve_params = curves,
        noise_params = noise,
        antigen_isos = inputs$antibodies,
        method = "joint"
      ),
      brute_force(rows),
      tolerance = 1e-4
    )
  }
})

test_that("multi-biomarker joint log-likelihood is not the sum of marginals", {
  # The acceptance criterion in #646: unlike the composite method (see
  # test-log_likelihood.R), the shared-latent-time integral does not
  # factor into per-biomarker integrals.
  inputs <- joint_test_inputs()
  ll_single <- vapply(
    inputs$antibodies,
    FUN.VALUE = numeric(1),
    FUN = function(cur_antibody) {
      log_likelihood(
        lambda = 0.1,
        pop_data = inputs$xs_data,
        curve_params = inputs$curves,
        noise_params = inputs$noise,
        antigen_isos = cur_antibody,
        method = "joint"
      )
    }
  )
  ll_joint <- log_likelihood(
    lambda = 0.1,
    pop_data = inputs$xs_data,
    curve_params = inputs$curves,
    noise_params = inputs$noise,
    antigen_isos = inputs$antibodies,
    method = "joint"
  )
  ll_composite <- log_likelihood(
    lambda = 0.1,
    pop_data = inputs$xs_data,
    curve_params = inputs$curves,
    noise_params = inputs$noise,
    antigen_isos = inputs$antibodies,
    method = "composite"
  )

  expect_false(isTRUE(all.equal(ll_joint, sum(ll_single))))
  expect_false(isTRUE(all.equal(ll_joint, ll_composite)))
  expect_snapshot_value(ll_joint, style = "deparse", tolerance = 1e-6)
})

test_that("the default method is unchanged", {
  inputs <- joint_test_inputs()
  args <- list(
    lambda = 0.1,
    pop_data = inputs$xs_data,
    curve_params = inputs$curves,
    noise_params = inputs$noise,
    antigen_isos = inputs$antibodies
  )
  expect_identical(
    do.call(log_likelihood, args),
    do.call(log_likelihood, c(args, method = "composite"))
  )
})

test_that("subjects missing a biomarker contribute their other readings", {
  inputs <- joint_test_inputs()
  drop_ids <- unique(inputs$xs_data$id)[1:3]
  partial <- inputs$xs_data |>
    dplyr::filter(
      !(.data$id %in% drop_ids & .data$antigen_iso == "HlyE_IgG")
    )
  ll_partial <- log_likelihood(
    lambda = 0.1,
    pop_data = partial,
    curve_params = inputs$curves,
    noise_params = inputs$noise,
    antigen_isos = inputs$antibodies,
    method = "joint"
  )
  # The dropped subjects now enter as single-biomarker (marginal) terms,
  # evaluated through the same routine so that quadrature matches.
  ll_rest <- log_likelihood(
    lambda = 0.1,
    pop_data = partial |> dplyr::filter(!(.data$id %in% drop_ids)),
    curve_params = inputs$curves,
    noise_params = inputs$noise,
    antigen_isos = inputs$antibodies,
    method = "joint"
  )
  ll_dropped <- -f_dev_joint(
    lambda = 0.1,
    pop_data = partial |> dplyr::filter(.data$id %in% drop_ids),
    curve_params = inputs$curves,
    noise_params = inputs$noise,
    antigen_isos = "HlyE_IgA"
  )
  expect_equal(ll_partial, ll_rest + ll_dropped)
})

test_that("draws are paired by `iter` regardless of row order", {
  inputs <- joint_test_inputs()
  shuffled <- inputs$curves |>
    dplyr::slice(sample.int(dplyr::n()))
  expect_equal(
    log_likelihood(
      lambda = 0.1,
      pop_data = inputs$xs_data,
      curve_params = shuffled,
      noise_params = inputs$noise,
      antigen_isos = inputs$antibodies,
      method = "joint"
    ),
    log_likelihood(
      lambda = 0.1,
      pop_data = inputs$xs_data,
      curve_params = inputs$curves,
      noise_params = inputs$noise,
      antigen_isos = inputs$antibodies,
      method = "joint"
    )
  )
})

test_that("the joint likelihood refuses inputs it can't pair", {
  inputs <- joint_test_inputs()

  # every biomarker needs a noise model
  expect_error(
    log_likelihood(
      lambda = 0.1,
      pop_data = inputs$xs_data,
      curve_params = inputs$curves,
      noise_params = inputs$noise |> dplyr::mutate(nu = 0, eps = 0),
      antigen_isos = inputs$antibodies,
      method = "joint"
    ),
    "noise model"
  )

  # draws must match across biomarkers
  expect_error(
    log_likelihood(
      lambda = 0.1,
      pop_data = inputs$xs_data,
      curve_params = inputs$curves |>
        dplyr::filter(!(.data$antigen_iso == "HlyE_IgG" & .data$iter == 3)),
      noise_params = inputs$noise,
      antigen_isos = inputs$antibodies,
      method = "joint"
    ),
    "different posterior draws"
  )

  # without `iter`, draws are paired by position, so counts must match
  expect_error(
    log_likelihood(
      lambda = 0.1,
      pop_data = inputs$xs_data,
      curve_params = inputs$curves |>
        dplyr::filter(!(.data$antigen_iso == "HlyE_IgG" & .data$iter == 3)) |>
        dplyr::select(-"iter"),
      noise_params = inputs$noise,
      antigen_isos = inputs$antibodies,
      method = "joint"
    ),
    "different number of draws"
  )

  # a subject can't have two readings of one biomarker
  expect_error(
    log_likelihood(
      lambda = 0.1,
      pop_data = inputs$xs_data |>
        dplyr::mutate(id = ifelse(.data$id %in% c("P1", "P2"), "P1", .data$id)),
      curve_params = inputs$curves,
      noise_params = inputs$noise,
      antigen_isos = inputs$antibodies,
      method = "joint"
    ),
    "more than one"
  )

  # and it needs to know which column identifies subjects
  no_id <- inputs$xs_data |> dplyr::select(-"id")
  attr(no_id, "id_var") <- NULL
  expect_error(
    log_likelihood(
      lambda = 0.1,
      pop_data = no_id,
      curve_params = inputs$curves,
      noise_params = inputs$noise,
      antigen_isos = inputs$antibodies,
      method = "joint"
    ),
    "identifies subjects"
  )
})

test_that("the joint likelihood validates `n_t_steps`", {
  # A non-positive value doesn't fail in C: it integrates nothing, or
  # runs the loop backwards, either way returning a plausible-looking
  # number. Catch it in R instead (review of #677).
  inputs <- joint_test_inputs()
  for (bad in list(0, -5, 2.5, NA_integer_, c(10, 20), "10")) {
    expect_error(
      f_dev_joint(
        lambda = 0.1,
        pop_data = inputs$xs_data,
        curve_params = inputs$curves,
        noise_params = inputs$noise,
        antigen_isos = inputs$antibodies,
        n_t_steps = bad
      ),
      "whole number"
    )
  }
  expect_no_error(
    f_dev_joint(
      lambda = 0.1,
      pop_data = inputs$xs_data,
      curve_params = inputs$curves,
      noise_params = inputs$noise,
      antigen_isos = inputs$antibodies,
      n_t_steps = 25
    )
  )
})

test_that("a subject aged 0 contributes the never-infected mass alone", {
  # `EXPla` divides by age; at age 0 the latent-time interval is empty,
  # so it is never used and the likelihood stays finite (review of #677).
  inputs <- joint_test_inputs()
  newborn <- inputs$xs_data |>
    dplyr::filter(.data$id == unique(.data$id)[1]) |>
    dplyr::mutate(age = 0)
  ll <- log_likelihood(
    lambda = 0.1,
    pop_data = newborn,
    curve_params = inputs$curves,
    noise_params = inputs$noise,
    antigen_isos = inputs$antibodies,
    method = "joint"
  )
  expect_true(is.finite(ll))
})

test_that("ambiguous noise parameters are refused", {
  # Several rows per biomarker (one per country, say) would otherwise be
  # resolved by taking the first, silently changing the noise model
  # (review of #677).
  inputs <- joint_test_inputs()
  two_countries <- dplyr::bind_rows(
    inputs$noise |> dplyr::mutate(Country = "A"),
    inputs$noise |> dplyr::mutate(Country = "B", nu = .data$nu * 2)
  )
  expect_error(
    log_likelihood(
      lambda = 0.1,
      pop_data = inputs$xs_data,
      curve_params = inputs$curves,
      noise_params = two_countries,
      antigen_isos = inputs$antibodies,
      method = "joint"
    ),
    "more than one row"
  )
})

test_that("curve parameters may arrive as a list in raw units", {
  # The documented interface accepts a list split by `antigen_iso`; it
  # may still carry `r` rather than `d` (review of #677).
  inputs <- joint_test_inputs()
  raw_list <- split(inputs$curves, inputs$curves$antigen_iso)
  expect_equal(
    log_likelihood(
      lambda = 0.1,
      pop_data = inputs$xs_data,
      curve_params = raw_list,
      noise_params = inputs$noise,
      antigen_isos = inputs$antibodies,
      method = "joint"
    ),
    log_likelihood(
      lambda = 0.1,
      pop_data = inputs$xs_data,
      curve_params = inputs$curves,
      noise_params = inputs$noise,
      antigen_isos = inputs$antibodies,
      method = "joint"
    )
  )
})

test_that("both entry points resolve the id column the same way", {
  # `est_seroincidence()` falls back to an `id` column when the `id_var`
  # attribute is absent; `est_seroincidence_by()` must not abort where
  # the unstratified fit succeeds (review of #677).
  inputs <- joint_test_inputs()
  no_attr <- inputs$xs_data
  attr(no_attr, "id_var") <- NULL

  expect_equal(.joint_id_var_for_fit(no_attr, inputs$antibodies), "id")
  expect_no_error(
    est_seroincidence_by(
      pop_data = no_attr,
      sr_params = inputs$curves,
      noise_params = inputs$noise,
      strata = "catchment",
      curve_strata_varnames = NULL,
      noise_strata_varnames = NULL,
      antigen_isos = inputs$antibodies,
      method = "joint",
      iterlim = 1
    ) |>
      suppressWarnings()
  )
})

test_that("a single-biomarker joint fit needs no id column", {
  # With one biomarker there is nothing to pair, and `log_likelihood()`
  # routes to the marginal path, so `method = "joint"` must accept the
  # same data as `method = "composite"` (review of #677).
  inputs <- joint_test_inputs()
  no_id <- inputs$xs_data |> dplyr::select(-"id")
  attr(no_id, "id_var") <- NULL

  expect_no_error(
    log_likelihood(
      lambda = 0.1,
      pop_data = no_id,
      curve_params = inputs$curves,
      noise_params = inputs$noise,
      antigen_isos = "HlyE_IgA",
      method = "joint"
    )
  )
  expect_no_error(
    est_seroincidence(
      pop_data = no_id,
      sr_params = inputs$curves,
      noise_params = inputs$noise,
      antigen_isos = "HlyE_IgA",
      method = "joint",
      iterlim = 1
    ) |>
      suppressWarnings()
  )
  # ...while two biomarkers still require it, since pairing is real then.
  expect_error(
    est_seroincidence(
      pop_data = no_id,
      sr_params = inputs$curves,
      noise_params = inputs$noise,
      antigen_isos = inputs$antibodies,
      method = "joint",
      iterlim = 1
    ),
    "identifies subjects"
  )
})

test_that("a custom id column name survives stratification", {
  # `est_seroincidence_by()` resolves the id column once, then each
  # per-stratum `est_seroincidence()` re-derives it from the
  # `select()`-processed stratum data. That works only while the
  # `id_var` attribute survives those dplyr verbs (review of #677).
  inputs <- joint_test_inputs()
  custom <- inputs$xs_data |> dplyr::rename(subject_code = "id")
  attr(custom, "id_var") <- "subject_code"

  strata <- stratify_data(
    data = custom,
    curve_params = inputs$curves |>
      dplyr::mutate(alpha = .data$alpha * 365.25, d = .data$r - 1),
    noise_params = inputs$noise,
    strata_varnames = "catchment",
    curve_strata_varnames = NULL,
    noise_strata_varnames = NULL,
    antigen_isos = inputs$antibodies,
    id_var = "subject_code"
  ) |>
    suppressWarnings()

  expect_equal(attr(strata[[1]]$pop_data, "id_var"), "subject_code")
  expect_true("subject_code" %in% names(strata[[1]]$pop_data))
  expect_no_error(
    est_seroincidence_by(
      pop_data = custom,
      sr_params = inputs$curves,
      noise_params = inputs$noise,
      strata = "catchment",
      curve_strata_varnames = NULL,
      noise_strata_varnames = NULL,
      antigen_isos = inputs$antibodies,
      method = "joint",
      iterlim = 1
    ) |>
      suppressWarnings()
  )
})

test_that("`est_seroincidence(method = 'joint')` fits and summarizes", {
  inputs <- joint_test_inputs()
  est_joint <- est_seroincidence(
    pop_data = inputs$xs_data,
    sr_params = inputs$curves,
    noise_params = inputs$noise,
    antigen_isos = inputs$antibodies,
    method = "joint"
  )
  est_composite <- est_seroincidence(
    pop_data = inputs$xs_data,
    sr_params = inputs$curves,
    noise_params = inputs$noise,
    antigen_isos = inputs$antibodies
  )

  expect_s3_class(est_joint, "seroincidence")
  expect_equal(attr(est_joint, "method"), "joint")
  expect_equal(attr(est_joint, "id_var"), "id")
  expect_null(attr(est_composite, "method"))
  expect_false(isTRUE(all.equal(est_joint$estimate, est_composite$estimate)))

  sum_joint <- summary(est_joint, verbose = FALSE)
  expect_equal(sum_joint$se_type, "standard")
  expect_equal(sum_joint$log.lik, -est_joint$minimum)
  expect_snapshot_value(
    est_joint |> unclass() |> `attributes<-`(NULL),
    style = "deparse",
    tolerance = 1e-4
  )
  expect_snapshot(print(est_joint))

  # `cluster_var` still applies, now for the sampling design only
  est_clustered <- est_seroincidence(
    pop_data = inputs$xs_data,
    sr_params = inputs$curves,
    noise_params = inputs$noise,
    antigen_isos = inputs$antibodies,
    method = "joint",
    cluster_var = "cluster"
  )
  expect_equal(est_clustered$estimate, est_joint$estimate)
  sum_clustered <- summary(est_clustered, verbose = FALSE)
  expect_equal(sum_clustered$se_type, "cluster-robust")
  expect_true(is.finite(sum_clustered$SE))
})

test_that("a verbose joint fit doesn't warn about independence", {
  inputs <- joint_test_inputs()
  expect_no_message(
    utils::capture.output(
      est_seroincidence(
        pop_data = inputs$xs_data,
        sr_params = inputs$curves,
        noise_params = inputs$noise,
        antigen_isos = inputs$antibodies,
        method = "joint",
        verbose = TRUE
      )
    ),
    message = "independence"
  )
})

test_that("a joint fit warns when no subject has more than one biomarker", {
  inputs <- joint_test_inputs()
  unlinked <- inputs$xs_data |>
    dplyr::mutate(id = as.character(dplyr::row_number()))
  expect_warning(
    .joint_id_var_for_fit(unlinked, inputs$antibodies),
    "reduces to the composite"
  )
  expect_no_warning(
    .joint_id_var_for_fit(inputs$xs_data, inputs$antibodies)
  )
})

test_that("`est_seroincidence_by(method = 'joint')` carries the method", {
  inputs <- joint_test_inputs()
  est_by <- est_seroincidence_by(
    pop_data = inputs$xs_data,
    sr_params = inputs$curves,
    noise_params = inputs$noise,
    strata = "catchment",
    curve_strata_varnames = NULL,
    noise_strata_varnames = NULL,
    antigen_isos = inputs$antibodies,
    method = "joint"
  )
  expect_s3_class(est_by, "seroincidence.by")
  for (fit in est_by) {
    expect_equal(attr(fit, "method"), "joint")
  }
  est_by_composite <- est_seroincidence_by(
    pop_data = inputs$xs_data,
    sr_params = inputs$curves,
    noise_params = inputs$noise,
    strata = "catchment",
    curve_strata_varnames = NULL,
    noise_strata_varnames = NULL,
    antigen_isos = inputs$antibodies
  )
  expect_false(isTRUE(all.equal(
    summary(est_by)$incidence.rate,
    summary(est_by_composite)$incidence.rate
  )))
})

test_that("a joint fit warns about subjects it drops", {
  # `sim_pop_data()` readings shortly after infection are large relative
  # to the additive noise, and its response curve differs slightly from
  # the likelihood's (a rise phase, a carried-over baseline), so with a
  # narrow noise window no single latent time reproduces both biomarkers
  # for those people, and the joint likelihood drops them -- see the
  # methodology article's "Multiple biomarkers" section.
  antibodies <- c("HlyE_IgA", "HlyE_IgG")
  curves <- typhoid_curves_nostrat_100 |>
    dplyr::filter(.data$iter < 50, .data$antigen_iso %in% antibodies)
  noise <- tibble::tibble(
    antigen_iso = antibodies,
    nu = 0.5,
    eps = 0,
    y.low = 1,
    y.high = 5e6
  )
  withr::local_seed(1)
  sim_data <- sim_pop_data(
    curve_params = curves,
    lambda = 0.8,
    n_samples = 100,
    age_range = c(0, 10),
    antigen_isos = antibodies,
    n_mcmc_samples = 0,
    renew_params = FALSE,
    add_noise = TRUE,
    noise_limits = rbind(
      "HlyE_IgA" = c(min = 0, max = 0.5),
      "HlyE_IgG" = c(min = 0, max = 0.5)
    ),
    format = "long"
  )
  dropped <- .joint_dropped_subjects(
    pop_data = sim_data,
    curve_params = curves,
    noise_params = noise,
    antigen_isos = antibodies
  )
  expect_equal(dropped$n_subjects, 100)
  expect_gt(dropped$n_dropped, 0)
  expect_warning(
    est_seroincidence(
      pop_data = sim_data,
      sr_params = curves,
      noise_params = noise,
      antigen_isos = antibodies,
      method = "joint"
    ),
    class = "joint_dropped_subjects"
  )

  # the SEES example data, with its multiplicative noise, drops no one
  inputs <- joint_test_inputs()
  expect_equal(
    .joint_dropped_subjects(
      pop_data = inputs$xs_data,
      curve_params = inputs$curves,
      noise_params = inputs$noise,
      antigen_isos = inputs$antibodies
    )$n_dropped,
    0
  )
})

test_that("the joint estimator recovers a simulated incidence rate", {
  # `sim_pop_data()` draws one infection history per person and reads
  # every biomarker off it, which is exactly the shared-latent-time model.
  antibodies <- c("HlyE_IgA", "HlyE_IgG")
  curves <- typhoid_curves_nostrat_100 |>
    dplyr::filter(.data$iter < 50, .data$antigen_iso %in% antibodies)
  noise_limits <- rbind(
    "HlyE_IgA" = c(min = 0, max = 0.5),
    "HlyE_IgG" = c(min = 0, max = 0.5)
  )
  noise <- tibble::tibble(
    antigen_iso = antibodies,
    nu = 0.5,
    eps = 0,
    y.low = 1,
    y.high = 5e6
  )
  withr::local_seed(20260902)
  sim_data <- sim_pop_data(
    curve_params = curves,
    lambda = 0.2,
    n_samples = 200,
    age_range = c(0, 10),
    antigen_isos = antibodies,
    n_mcmc_samples = 0,
    renew_params = FALSE,
    add_noise = TRUE,
    noise_limits = noise_limits,
    format = "long"
  )
  est_joint <- est_seroincidence(
    pop_data = sim_data,
    sr_params = curves,
    noise_params = noise,
    antigen_isos = antibodies,
    method = "joint"
  ) |>
    suppressWarnings(classes = "joint_dropped_subjects")
  sum_joint <- summary(est_joint, verbose = FALSE)
  expect_equal(sum_joint$nlm.convergence.code |> as.integer(), 1L)
  expect_gt(sum_joint$incidence.rate, 0.1)
  expect_lt(sum_joint$incidence.rate, 0.4)
  expect_snapshot_value(
    sum_joint |> dplyr::select("incidence.rate", "SE"),
    style = "deparse",
    tolerance = 1e-4
  )
})

test_that("`n_t_steps` must be representable as an R integer", {
  # `Inf` and values above the integer range satisfy "whole number >= 1" but
  # become `NA_integer_` under `as.integer()`, which `.C()` then rejects with
  # a message naming an argument position rather than the argument the caller
  # set. Refuse them here instead (review of #677).
  inputs <- joint_test_inputs()

  for (bad in list(Inf, 2^31, 2^40)) {
    expect_error(
      log_likelihood(
        lambda = 0.1,
        pop_data = inputs$xs_data,
        curve_params = inputs$curves,
        noise_params = inputs$noise,
        antigen_isos = inputs$antibodies,
        method = "joint",
        n_t_steps = bad
      ),
      "must be a single whole number"
    )
  }

  # The largest representable value still passes validation: it is refused
  # for being impractical, never for being unrepresentable.
  expect_no_error(.validate_n_t_steps(.Machine$integer.max))
})

test_that("mixed `iter` availability across biomarkers is refused", {
  # Positional pairing is documented as the no-`iter` case. Falling back to it
  # when only some biomarkers carry `iter` silently pairs draws that the ones
  # carrying it show to be ordered differently, and equal row counts do not
  # rule that out (review of #677).
  inputs <- joint_test_inputs()
  split_curves <- split(inputs$curves, inputs$curves$antigen_iso)
  mixed <- split_curves
  mixed[[inputs$antibodies[2]]]$iter <- NULL

  expect_error(
    log_likelihood(
      lambda = 0.1,
      pop_data = inputs$xs_data,
      curve_params = mixed,
      noise_params = inputs$noise,
      antigen_isos = inputs$antibodies,
      method = "joint"
    ),
    "for some"
  )
})

test_that("a joint fit records `n_t_steps` for the cluster-robust score", {
  # `.compute_cluster_robust_var()` rebuilds the per-cluster score from the
  # fit's attributes. Without the quadrature setting it would use
  # `f_dev_joint()`'s default, pairing a sandwich middle matrix built from one
  # objective with a Hessian from another (review of #677).
  inputs <- joint_test_inputs()
  id_var <- ids_varname(inputs$xs_data)

  fit_at <- function(n_t_steps) {
    est_seroincidence(
      pop_data = inputs$xs_data,
      sr_params = inputs$curves,
      noise_params = inputs$noise,
      antigen_isos = inputs$antibodies,
      method = "joint",
      cluster_var = id_var,
      n_t_steps = n_t_steps
    )
  }

  coarse <- fit_at(7)
  expect_equal(attr(coarse, "n_t_steps"), 7)

  # A fit left at the default records nothing, so the recomputation omits the
  # argument and reproduces that default.
  default_fit <- est_seroincidence(
    pop_data = inputs$xs_data,
    sr_params = inputs$curves,
    noise_params = inputs$noise,
    antigen_isos = inputs$antibodies,
    method = "joint",
    cluster_var = id_var
  )
  expect_null(attr(default_fit, "n_t_steps"))

  # The coarse quadrature must reach the standard error, not just the fit.
  expect_false(isTRUE(all.equal(
    summary(coarse, verbose = FALSE)$SE,
    summary(default_fit, verbose = FALSE)$SE
  )))
})

test_that("a joint fit's log-likelihood graph uses the fit's quadrature", {
  # `est_seroincidence()`'s `...` is shared between `nlm()`'s control arguments
  # and `log_likelihood()`'s, so the graph helpers were called with explicit
  # arguments only and never saw a caller's `n_t_steps`: the fit ran at one
  # quadrature while its attached plot was drawn at `f_dev_joint()`'s default.
  # Cosmetic -- no estimate depended on it -- but the same mismatch this round
  # fixed for the cluster-robust score (review of #677).
  skip_on_cran()
  inputs <- joint_test_inputs()

  plotted_at <- function(n_t_steps) {
    fit <- est_seroincidence(
      pop_data = inputs$xs_data,
      sr_params = inputs$curves,
      noise_params = inputs$noise,
      antigen_isos = inputs$antibodies,
      method = "joint",
      n_t_steps = n_t_steps,
      build_graph = TRUE,
      print_graph = FALSE
    )
    ggplot2::ggplot_build(attr(fit, "ll_graph"))$data[[1]]
  }

  drawn <- plotted_at(7)
  expected <- vapply(
    drawn$x[1:3],
    \(lambda) {
      log_likelihood(
        lambda = lambda,
        pop_data = inputs$xs_data,
        curve_params = inputs$curves,
        noise_params = inputs$noise,
        antigen_isos = inputs$antibodies,
        method = "joint",
        n_t_steps = 7
      )
    },
    numeric(1)
  )

  expect_equal(drawn$y[1:3], expected)
})
