library(dplyr)
library(tibble)
library(serocalculator)
library(furrr)

###############################################################################
## Load longitudinal parameters

test_sim <- "https://osf.io/download/rtw5k" %>%
  load_curve_params() %>%
  dplyr::filter(iter < 500)
###############################################################################

# Define the simulation function
simulate_seroincidence <- function(
    nrep,
    n_sim,
    dmcmc = test_sim,  # Curve parameters
    antibodies = c("HlyE_IgA", "HlyE_IgG"),
    lambda = 0.2,  # Simulated incidence rate per person-year
    lifespan = c(0, 10)  # Age range
    ) {

  # biologic noise distribution
  dlims <- rbind(
    "HlyE_IgA" = c(min = 0, max = 0.5),
    "HlyE_IgG" = c(min = 0, max = 0.5)
  )

  # Noise parameters
  cond <- tibble(
    antigen_iso = c("HlyE_IgG", "HlyE_IgA"),
    nu = c(0.5, 0.5),  # Biologic noise (nu)
    eps = c(0, 0),     # Measurement noise (eps)
    y.low = c(1, 1),   # Low cutoff (llod)
    y.high = c(5e6, 5e6)  # High cutoff (y.high)
  )

  run_one_sim_iter <- function(i) {
    # Generate cross-sectional data
    csdata <- sim.cs(
      curve_params = dmcmc,
      lambda = lambda,
      n.smpl = nrep,
      age.rng = lifespan,
      antigen_isos = antibodies,
      n.mc = 0,
      renew.params = TRUE,  # Use different parameters for each simulation
      add.noise = TRUE,
      noise_limits = dlims,
      format = "long"
    )

    # Estimate seroincidence
    est <- est.incidence(
      pop_data = csdata,
      curve_params = dmcmc,
      noise_params = cond,
      lambda_start = 0.1,
      build_graph = FALSE,
      verbose = FALSE,
      print_graph = FALSE,
      antigen_isos = antibodies
    )

    # Return results for this simulation
    list(
      csdata = csdata,
      est1 = est
    )
  }


  # Perform simulations in parallel
  results <-
    furrr::future_map(
      .x = 1:n_sim,
      .f = run_one_sim_iter,
      .options = furrr_options(seed = TRUE)) |>
    structure(
      antibodies = antibodies,
      lambda = lambda, # Simulated incidence rate per person-year
      lifespan = lifespan, # Age range
      sample_size = nrep
    )

  return(results)
}

###############################################################################
## Run simulation
library(future)
future::plan(multisession)  # Use multiple sessions for parallelism (local machine)

# Run the simulations in parallel
set.seed(129251)
results_50 <- simulate_seroincidence(lambda = 0.2, nrep = 50, n_sim = 300)

set.seed(129252)
results_100 <- simulate_seroincidence(lambda = 0.2, nrep = 100, n_sim = 300)

results_100_0.1 <- simulate_seroincidence(lambda = 0.1, nrep = 100, n_sim = 300)

results_100_0.01 <- simulate_seroincidence(lambda = 0.1, nrep = 100, n_sim = 300)

# Stop parallel processing
future::plan(sequential)  # Return to sequential processing

n_obs <- function(results_list) {
  results_list |> attr("sample_size")
}

# Define a function to generate final tables
generate_final_table <- function(results_list,
                                 sample_size = n_obs(results_list)) {
  # Initialize an empty list to store the results
  summary_results <- list()

  # Loop through each of the 100 results and extract the required columns
  for (i in 1:length(results_list)) {
    # Extract the summary for each result
    result_summary <- summary(results_list[[i]]$est1)

    # Select the required columns
    extracted_columns <- result_summary %>%
      select(incidence.rate, SE, CI.lwr, CI.upr)

    # Add a column for the index (optional, for tracking)
    extracted_columns <- extracted_columns %>%
      mutate(index = i)

    # Append to the list
    summary_results[[i]] <- extracted_columns
  }

  # Combine all results into a single data frame
  final_table <- bind_rows(summary_results) %>%
    mutate(sample_size = sample_size) # Add sample size column for clarity

  attributes(final_table) <-
    c(
      attributes(final_table),
      attributes(results_list)
    )

  return(final_table)
}

# Store each sample size's summary as table
final_table_50 <- results_50 |> generate_final_table(sample_size = 50)
final_table_100 <- results_100 |> generate_final_table(sample_size = 100)
final_table_100_0.1 <- results_100_0.1 |> generate_final_table(sample_size = 100)
ft_100_0.01 <- results_100_0.01 |> generate_final_table()
lambda <- function(results_table)
{
  results_table |> attr("lambda")
}

compute_coverage <- function(
    results_table,
    true_lambda = results_table |> lambda()) {

  results_table %>%
    dplyr::mutate(
      covered = CI.lwr <= true_lambda & CI.upr >= true_lambda,
      no_error = !is.na(SE)) %>%
    dplyr::summarize(
      no_error_count = sum(no_error),
      coverage_count = sum(covered, na.rm = TRUE),
      coverage_rate = mean(covered, na.rm = TRUE))
}

# Check how many rows have CI covering the true lambda
coverage_count_50 <- final_table_50 %>% compute_coverage() |> print()

coverage_count_100 <- final_table_100 %>% compute_coverage() |> print()

final_table_100_0.1 |> compute_coverage()
ft_100_0.01 |> compute_coverage()
# Print the result
print(paste("Number of rows where CI covers true lambda:", coverage_count_50))
print(paste("Number of rows where CI covers true lambda:", coverage_count_100))

