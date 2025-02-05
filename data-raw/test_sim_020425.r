### For sample size 50's graph distribution of confidence intervals
## When renew.params=TRUE

library(dplyr)
library(tibble)
library(serocalculator)

###############################################################################
## Load longitudinal parameters

test_sim<-"https://osf.io/download/rtw5k" %>%
  load_curve_params() %>%
  dplyr::filter(iter < 500)
###############################################################################

# Define the simulation function
simulate_seroincidence <- function(nrep, n_sim, true_lambda, lambda.start) {
  # Parameters
  dmcmc <- test_sim  # Curve parameters
  antibodies <-c("HlyE_IgA", "HlyE_IgG")
  lambda <- true_lambda  # Simulated incidence rate per person-year
  lifespan <- c(0, 10)  # Age range

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

  # Perform simulations in parallel
  results <- future_map(1:n_sim, function(i) {
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
      lambda_start = lambda.start,
      build_graph = TRUE,
      verbose = FALSE,
      print_graph = FALSE,
      antigen_isos = antibodies
    )

    # Return results for this simulation
    list(
      csdata = csdata,
      est1 = est
    )
  }, .options = furrr_options(seed = TRUE))

  return(results)
}

###############################################################################
## Run simulation
plan(multisession)  # Use multiple sessions for parallelism (local machine)

# Run the simulations in parallel
set.seed(204251)
results_50_11 <- simulate_seroincidence(nrep = 50, n_sim = 300,
                                       true_lambda=0.01, lambda.start=0.005)

set.seed(204252)
results_50_22 <- simulate_seroincidence(nrep = 50, n_sim = 300,
                                       true_lambda=0.05, lambda.start=0.02)

set.seed(204253)
results_50_33 <- simulate_seroincidence(nrep = 50, n_sim = 300,
                                       true_lambda=0.1, lambda.start=0.05)

set.seed(204254)
results_50_44 <- simulate_seroincidence(nrep = 50, n_sim = 300,
                                       true_lambda=0.2, lambda.start=0.1)


# Stop parallel processing
plan(sequential)  # Return to sequential processing


###############################################################################
## First approach

# Define a function to generate final tables with all summary columns
generate_final_table <- function(results_list, sample_size, lambda_sim) {
  # Initialize an empty list to store the results
  summary_results <- list()

  # Loop through each of the 300 results and extract the full summary
  for (i in 1:300) {
    # Extract the full summary for each result
    result_summary <- summary(results_list[[i]]$est1)

    # Add an index column for tracking
    result_summary <- result_summary %>%
      mutate(index = i)

    # Append to the list
    summary_results[[i]] <- result_summary
  }

  # Combine all results into a single data frame and add sample_size and lambda.sim columns
  final_table <- bind_rows(summary_results) %>%
    mutate(sample_size = sample_size,
           lambda.sim = lambda_sim)  # Add lambda.sim column

  return(final_table)
}


# Store each sample size's summary as table
final_table_50_11 <- generate_final_table(results_50_11, 50, 0.01)
final_table_50_22 <- generate_final_table(results_50_22, 50, 0.05)
final_table_50_33 <- generate_final_table(results_50_33, 50, 0.1)
final_table_50_44 <- generate_final_table(results_50_44, 50, 0.2)

# Merge the four tables into one data frame
table_merged_50 <- bind_rows(
  final_table_50_11,
  final_table_50_22,
  final_table_50_33,
  final_table_50_44
)




table_merged_50 |>
  autoplot(xvar = "lambda.sim",
           CI = TRUE,
           dodge_width = .05) +
  ggplot2::geom_function(
    fun = function(x) x,
    col = "red",
    aes(linetype = "data-generating incidence rate")
  ) +
  labs(linetype = "") +
  scale_x_log10()

##############################################################################
## Second approach

# List of result objects
results_list <- list(
  results_50_11 = 0.01,
  results_50_22 = 0.05,
  results_50_33 = 0.1,
  results_50_44 = 0.2
)


# Extract summaries and combine into a tibble, iterating over `i = 1:300`
ests_summary <- map_dfr(names(results_list), function(res_name) {
  res_obj <- get(res_name)  # Retrieve the actual list object

  # Loop through all `i` indices from 1 to 300
  map_dfr(1:300, function(i) {
    if (!is.null(res_obj[[i]]$est1)) {  # Ensure the object exists
      res_summary <- summary(res_obj[[i]]$est1)  # Extract summary
      res_summary <- res_summary %>%
        mutate(lambda.sim = results_list[[res_name]],  # Assign corresponding lambda.sim
               index = i)  # Track index value
      return(res_summary)
    } else {
      return(NULL)  # Skip if the element is NULL
    }
  })
})

# Reorder columns to place lambda.sim and index first
ests_summary <- ests_summary %>% relocate(lambda.sim, index)


ests_summary |>
  autoplot(xvar = "lambda.sim",
           CI = TRUE,
           dodge_width = .05) +
  ggplot2::geom_function(
    fun = function(x) x,
    col = "red",
    aes(linetype = "data-generating incidence rate")
  ) +
  labs(linetype = "") +
  scale_x_log10()
