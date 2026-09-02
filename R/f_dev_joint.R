#' @title Calculate the joint (shared latent time) negative log-likelihood
#' for several biomarkers
#'
#' @description
#' Evaluates the negative log-likelihood of multi-biomarker cross-sectional
#' data under the model in which a person's biomarkers are conditionally
#' independent *given* their time since infection, so that the per-biomarker
#' densities multiply inside one integral over that shared latent time.
#' This is the model the methodology article describes; the default
#' `method = "composite"` in [log_likelihood()] instead sums the
#' per-biomarker marginal log-likelihoods, which gives each biomarker its
#' own latent time (see issues #637 and #646).
#'
#' @details
#' Subjects are paired across biomarkers by the id column of `pop_data`
#' (see [ids_varname()]); a subject may be missing some biomarkers.
#' Posterior draws of the curve parameters are paired across biomarkers
#' by `iter` (and `chain`, if present), or by position when
#' `curve_params` carries no `iter` column, so that a subject's biomarkers
#' are evaluated under one coherent draw. Every biomarker needs a noise
#' model (`nu > 0` or `eps > 0`); without noise a reading pins the latent
#' time exactly, and a joint density over several biomarkers is degenerate.
#'
#' Interface with C routine `negloglik_joint` (`src/serocalc_joint.c`).
#'
#' @param lambda a [numeric()] vector of incidence parameters,
#' in events per person-year
#' @param pop_data cross-sectional data, either a `pop_data` [data.frame()]
#' or a [list()] of them split by `antigen_iso`
#' (as [est_seroincidence()] supplies)
#' @param curve_params curve parameters in the units [f_dev()] expects
#' (`alpha` per year, `d = r - 1`), or in their raw form (`alpha` per day
#' and `r`); either a [data.frame()] or a [list()] split by `antigen_iso`
#' @param noise_params a [data.frame()] or [list()] of noise parameters
#' (`nu`, `eps`, `y.low`, `y.high`) by `antigen_iso`
#' @param antigen_isos the biomarkers to combine
#' @param id_var name of the column in `pop_data` identifying subjects;
#' by default, read from the `id_var` attribute, falling back to a column
#' named `id` or `index_id`
#' @param n_t_steps number of midpoint-rule nodes for the numerical
#' integral over the latent infection time
#' @param ... unused
#' @returns a [numeric()] vector of negative log-likelihoods,
#' corresponding to the elements of input `lambda`
#' @keywords internal
f_dev_joint <- function(
  lambda,
  pop_data,
  curve_params,
  noise_params,
  antigen_isos,
  id_var = NULL,
  n_t_steps = 100L,
  ...
) {
  inputs <- .prep_joint_inputs(
    pop_data = pop_data,
    curve_params = curve_params,
    noise_params = noise_params,
    antigen_isos = antigen_isos,
    id_var = id_var
  )

  vapply(
    X = lambda,
    FUN = function(cur_lambda) {
      .negloglik_joint(
        lambda = cur_lambda,
        inputs = inputs,
        n_t_steps = n_t_steps
      )
    },
    FUN.VALUE = numeric(1)
  )
}

.negloglik_joint <- function(lambda, inputs, n_t_steps, n_dropped = FALSE) {
  .validate_n_t_steps(n_t_steps)
  llpp <- .C(
    "negloglik_joint",
    res = as.double(0),
    lambda = as.double(lambda),
    y = as.double(inputs$y),
    obs = as.integer(inputs$obs),
    a = as.double(inputs$age),
    nsubj = as.integer(nrow(inputs$y)),
    nbio = as.integer(ncol(inputs$y)),
    nu = as.double(inputs$nu),
    eps = as.double(inputs$eps),
    y.low = as.double(inputs$y_low),
    y.high = as.double(inputs$y_high),
    y1 = as.double(inputs$y1),
    alpha = as.double(inputs$alpha),
    d = as.double(inputs$d),
    nmc = as.integer(nrow(inputs$y1)),
    nt = as.integer(n_t_steps),
    ndrop = as.integer(0)
  )
  if (n_dropped) llpp$ndrop else llpp$res
}

#' How many subjects does the joint likelihood drop?
#'
#' @description
#' A subject whose readings have zero likelihood under every posterior
#' draw --- no shared infection time is compatible with all of them ---
#' contributes nothing to the joint likelihood, as in the single-biomarker
#' routine. With several biomarkers this is far easier to trigger, since
#' one latent time has to explain every reading at once, and it is a sign
#' that the noise model is narrower than the real disagreement between a
#' person's biomarkers. The set of such subjects does not depend on
#' `lambda`.
#'
#' @inheritParams f_dev_joint
#' @returns a [list()] with `n_dropped`, the number of subjects dropped,
#' and `n_subjects`, the number of subjects
#' @keywords internal
.joint_dropped_subjects <- function(
  pop_data,
  curve_params,
  noise_params,
  antigen_isos,
  id_var = NULL,
  n_t_steps = 100L
) {
  inputs <- .prep_joint_inputs(
    pop_data = pop_data,
    curve_params = curve_params,
    noise_params = noise_params,
    antigen_isos = antigen_isos,
    id_var = id_var
  )
  list(
    n_dropped = .negloglik_joint(
      lambda = 0.1,
      inputs = inputs,
      n_t_steps = n_t_steps,
      n_dropped = TRUE
    ),
    n_subjects = nrow(inputs$y)
  )
}

#' Check the quadrature resolution before handing it to C
#'
#' @description
#' `negloglik_joint` divides the latent-time interval into `n_t_steps`
#' midpoint nodes, so a non-positive value does not error there: it
#' either integrates nothing (returning a likelihood computed from the
#' never-infected mass alone) or runs the loop backwards. Both give a
#' plausible-looking number rather than a failure, so the value is
#' checked here instead.
#'
#' @param n_t_steps the `n_t_steps` argument as passed by the caller
#' @returns `invisible(NULL)`
#' @noRd
.validate_n_t_steps <- function(n_t_steps) {
  ok <- length(n_t_steps) == 1 &&
    is.numeric(n_t_steps) &&
    !is.na(n_t_steps) &&
    n_t_steps >= 1 &&
    n_t_steps == trunc(n_t_steps)
  if (!ok) {
    cli::cli_abort(
      c(
        "{.arg n_t_steps} must be a single whole number >= 1,
        not {.val {n_t_steps}}.",
        "i" = "It sets how many midpoint nodes the integral over the
        latent infection time uses."
      )
    )
  }
  invisible(NULL)
}

#' Marshal multi-biomarker inputs for the joint likelihood
#'
#' @inheritParams f_dev_joint
#' @returns a [list()] with one row per subject in `y`, `obs` and `age`,
#' one column per biomarker in `y`, `obs`, `y1`, `alpha` and `d`,
#' and per-biomarker vectors `nu`, `eps`, `y_low` and `y_high`
#' @keywords internal
#' @noRd
.prep_joint_inputs <- function(
  pop_data,
  curve_params,
  noise_params,
  antigen_isos,
  id_var = NULL
) {
  antigen_isos <- as.character(antigen_isos)
  wide <- .prep_joint_pop_data(
    pop_data = pop_data,
    antigen_isos = antigen_isos,
    id_var = id_var
  )
  draws <- .align_joint_curve_params(
    curve_params = curve_params,
    antigen_isos = antigen_isos
  )
  noise <- .prep_joint_noise_params(
    noise_params = noise_params,
    antigen_isos = antigen_isos
  )
  c(wide, draws, noise)
}

.prep_joint_pop_data <- function(pop_data, antigen_isos, id_var = NULL) {
  if (is.data.frame(pop_data)) {
    template <- pop_data
    pop_data <- pop_data |>
      dplyr::filter(.data$antigen_iso %in% antigen_isos)
  } else {
    missing_isos <- setdiff(antigen_isos, names(pop_data))
    if (length(missing_isos) > 0) {
      cli::cli_abort(
        "{.arg pop_data} has no data for {.val {missing_isos}}."
      )
    }
    template <- pop_data[[antigen_isos[1]]]
    pop_data <- dplyr::bind_rows(pop_data[antigen_isos])
  }

  value_var <- get_values_var(template)
  age_var <- get_age_var(template)
  if (is.null(value_var) || is.null(age_var)) {
    cli::cli_abort(
      "{.arg pop_data} must be a {.cls pop_data} object
      (see {.fun as_pop_data})."
    )
  }
  if (is.null(id_var)) {
    id_var <- .joint_id_var(template)
  }
  if (!id_var %in% names(pop_data)) {
    cli::cli_abort(
      c(
        "{.arg pop_data} has no column {.val {id_var}} to pair
        each subject's biomarkers by.",
        "i" = "The joint likelihood needs to know which rows belong
        to the same person; see {.fun set_id_var}."
      )
    )
  }

  ids <- unique(pop_data[[id_var]])
  n_subj <- length(ids)
  n_bio <- length(antigen_isos)
  y <- matrix(0, nrow = n_subj, ncol = n_bio)
  obs <- matrix(0L, nrow = n_subj, ncol = n_bio)
  age <- rep(NA_real_, n_subj)

  for (k in seq_len(n_bio)) {
    cur <- pop_data[pop_data$antigen_iso == antigen_isos[k], , drop = FALSE]
    idx <- match(cur[[id_var]], ids)
    dup_ids <- ids[unique(idx[duplicated(idx)])]
    if (length(dup_ids) > 0) {
      cli::cli_abort(
        c(
          "{cli::qty(dup_ids)}Subject{?s} {.val {dup_ids}} {?has/have}
          more than one {.val {antigen_isos[k]}} reading.",
          "i" = "The joint likelihood pairs one reading per biomarker
          per subject; check that {.val {id_var}} identifies people."
        )
      )
    }
    cur_age <- cur[[age_var]]
    conflict <- !is.na(age[idx]) & abs(age[idx] - cur_age) > 1e-8
    conflict_ids <- ids[idx[conflict]]
    if (length(conflict_ids) > 0) {
      cli::cli_abort(
        c(
          "{cli::qty(conflict_ids)}Subject{?s} {.val {conflict_ids}}
          {?has/have} different ages on different biomarker rows.",
          "i" = "Check that {.val {id_var}} identifies people."
        )
      )
    }
    y[idx, k] <- cur[[value_var]]
    obs[idx, k] <- 1L
    age[idx] <- cur_age
  }

  list(y = y, obs = obs, age = age)
}

#' Which column of a `pop_data` object identifies subjects?
#' @noRd
.joint_id_var <- function(pop_data) {
  id_var <- attr(pop_data, "id_var")
  if (!is.null(id_var)) {
    return(id_var)
  }
  for (candidate in c("id", "index_id")) {
    if (candidate %in% names(pop_data)) {
      return(candidate)
    }
  }
  cli::cli_abort(
    c(
      "Can't tell which column of {.arg pop_data} identifies subjects.",
      "i" = "The joint likelihood needs to pair each person's biomarker
      readings; set the id column with {.fun set_id_var}, or pass
      {.arg id_var}."
    )
  )
}

#' Pair curve-parameter draws across biomarkers
#'
#' @returns a [list()] of `nmc` by `n_bio` matrices `y1`, `alpha`, `d`
#' @noRd
.align_joint_curve_params <- function(curve_params, antigen_isos) {
  if (is.data.frame(curve_params)) {
    curve_params <- .to_decay_params(curve_params)
    curve_params <- split(curve_params, curve_params$antigen_iso)
  } else {
    # A list split by `antigen_iso` may still hold the raw `alpha`/`r`
    # parameterization, which the documented interface allows, so
    # convert element-wise rather than only on the data.frame path.
    curve_params <- lapply(curve_params, .to_decay_params)
  }

  missing_isos <- setdiff(antigen_isos, names(curve_params))
  if (length(missing_isos) > 0) {
    cli::cli_abort(
      "{.arg curve_params} has no draws for {.val {missing_isos}}."
    )
  }
  params <- curve_params[antigen_isos]

  has_iter <- vapply(params, \(p) "iter" %in% names(p), logical(1))
  if (all(has_iter)) {
    params <- lapply(params, .order_draws)
    keys <- lapply(params, .draw_keys)
    same_keys <- vapply(
      keys,
      FUN = identical,
      FUN.VALUE = logical(1),
      y = keys[[1]]
    )
    if (!all(same_keys)) {
      cli::cli_abort(
        c(
          "{.arg curve_params} has different posterior draws
          ({.field iter}) for different biomarkers.",
          "i" = "The joint likelihood pairs draw {.field iter} of one
          biomarker with the same draw of the others; subset
          {.arg curve_params} to the draws all biomarkers share."
        )
      )
    }
  } else {
    n_draws <- vapply(params, nrow, integer(1))
    if (length(unique(n_draws)) > 1) {
      cli::cli_abort(
        c(
          "{.arg curve_params} has a different number of draws for
          different biomarkers ({.val {n_draws}}).",
          "i" = "Without an {.field iter} column, draws are paired by
          position across biomarkers, so every biomarker needs the same
          number of them."
        )
      )
    }
  }

  extract <- function(var) {
    vapply(params, \(p) as.double(p[[var]]), numeric(nrow(params[[1]])))
  }
  list(
    y1 = matrix(extract("y1"), ncol = length(params)),
    alpha = matrix(extract("alpha"), ncol = length(params)),
    d = matrix(extract("d"), ncol = length(params))
  )
}

#' Convert raw curve parameters (`alpha` per day, `r`) to the units the
#' likelihood uses (`alpha` per year, `d = r - 1`), leaving already
#' converted parameters untouched.
#' @noRd
.to_decay_params <- function(params) {
  if (is.element("d", names(params))) {
    return(params)
  }
  params |>
    dplyr::mutate(
      alpha = .data$alpha * 365.25,
      d = .data$r - 1
    )
}

.order_draws <- function(params) {
  if ("chain" %in% names(params)) {
    params[order(params$chain, params$iter), , drop = FALSE]
  } else {
    params[order(params$iter), , drop = FALSE]
  }
}

.draw_keys <- function(params) {
  if ("chain" %in% names(params)) {
    paste(params$chain, params$iter)
  } else {
    as.character(params$iter)
  }
}

.prep_joint_noise_params <- function(noise_params, antigen_isos) {
  if (!is.data.frame(noise_params)) {
    noise_params <- dplyr::bind_rows(noise_params)
  }
  idx <- match(antigen_isos, noise_params$antigen_iso)
  if (anyNA(idx)) {
    cli::cli_abort(
      "{.arg noise_params} has no row for {.val {antigen_isos[is.na(idx)]}}."
    )
  }

  # More than one row per biomarker (e.g. one per country or stratum)
  # would otherwise be resolved by `match()` taking the first, which
  # silently evaluates the likelihood under noise parameters the caller
  # never chose.
  relevant <- noise_params[noise_params$antigen_iso %in% antigen_isos, ,
                           drop = FALSE]
  n_rows <- table(as.character(relevant$antigen_iso))
  dup_isos <- names(n_rows)[n_rows > 1]
  if (length(dup_isos) > 0) {
    cli::cli_abort(
      c(
        "{.arg noise_params} has more than one row for
        {.val {dup_isos}}.",
        "i" = "The joint likelihood needs exactly one noise model per
        biomarker; subset {.arg noise_params} to the stratum being
        analyzed (e.g. one country)."
      )
    )
  }
  noise_params <- noise_params[idx, , drop = FALSE]
  nu <- as.double(noise_params$nu)
  eps <- as.double(noise_params$eps)
  no_noise <- !(nu > 0) & !(eps > 0)
  if (length(antigen_isos) > 1 && any(no_noise)) {
    cli::cli_abort(
      c(
        "The joint likelihood needs a noise model ({.field nu} > 0 or
        {.field eps} > 0) for every biomarker, but
        {.val {antigen_isos[no_noise]}} {?has/have} none.",
        "i" = "Without noise, a reading pins the latent infection time
        exactly, so a joint density over several biomarkers is degenerate.",
        "i" = "Specify noise parameters, or use {.code method = \"composite\"}."
      )
    )
  }
  list(
    nu = nu,
    eps = eps,
    y_low = as.double(noise_params$y.low),
    y_high = as.double(noise_params$y.high)
  )
}
