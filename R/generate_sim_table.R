#' Generate table of simulation results
#'
#' @param results_list output from [simulate_seroincidence()]
#' @param sample_size sample size of simulated data sets
#'
#' @returns a [tibble::tbl_df]
generate_sim_table <- function(
    results_list,
    sample_size = results_list |> attr("sample_size")) {
  # Initialize an empty list to store the results
  summary_results <- list()

  # Loop through each of the 100 results and extract the required columns
  for (i in 1:300) {
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

  final_table <-
    final_table |>
    structure(
      sample_size = sample_size,
      true_lambda = results_list |> attr("lambda_true")
    )

  return(final_table)
}
