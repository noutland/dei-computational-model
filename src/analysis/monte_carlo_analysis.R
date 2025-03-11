# Monte Carlo and Scenario Analysis Extensions for DEI Models
# This script extends the existing DEI models with robust Monte Carlo capabilities
# and advanced scenario testing

# Load required libraries
library(dplyr)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(parallel)  # For parallel processing
library(tidyr)     # For data tidying
library(purrr)     # For functional programming

###############################
## System Dynamics Extensions ##
###############################

# Add these functions to your existing DEI System Dynamics model environment

# Monte Carlo simulation for system dynamics model
run_monte_carlo_sim <- function(model_env, iterations = 1000, parameters_to_vary = NULL,
                               variation_range = 0.3, use_parallel = TRUE,
                               convergence_check = FALSE, convergence_threshold = 0.02,
                               stratified_sampling = FALSE) {
  # If parameters_to_vary is NULL, vary all parameters
  if (is.null(parameters_to_vary)) {
    parameters_to_vary <- names(model_env$params)
  }
  
  # Stratified sampling function to ensure critical regions are well-explored
  generate_stratified_samples <- function(param, base_value, min_val, max_val, strata = 3) {
    # Create strata boundaries
    breaks <- seq(min_val, max_val, length.out = strata + 1)
    # Sample within each stratum
    param_samples <- c()
    samples_per_stratum <- ceiling(iterations / strata)
    
    for (i in 1:strata) {
      stratum_samples <- runif(
        samples_per_stratum,
        breaks[i],
        breaks[i+1]
      )
      param_samples <- c(param_samples, stratum_samples)
    }
    
    # Take only what we need for the iterations
    return(param_samples[1:iterations])
  }
  
  # Pre-generate parameter values if using stratified sampling
  param_sample_sets <- list()
  if (stratified_sampling) {
    for (param in parameters_to_vary) {
      base_value <- model_env$params[[param]]
      min_val <- max(0, base_value * (1 - variation_range))
      max_val <- min(1, base_value * (1 + variation_range))
      param_sample_sets[[param]] <- generate_stratified_samples(param, base_value, min_val, max_val)
    }
  }
  
  # Function to run a single Monte Carlo iteration
  run_iteration <- function(i) {
    # Generate random parameter changes
    param_changes <- list()
    for (param in parameters_to_vary) {
      base_value <- model_env$params[[param]]
      if (stratified_sampling && !is.null(param_sample_sets[[param]])) {
        # Use pre-generated stratified sample
        param_changes[[param]] <- param_sample_sets[[param]][i]
      } else {
        # Use random sampling as before
        min_val <- max(0, base_value * (1 - variation_range))
        max_val <- min(1, base_value * (1 + variation_range))
        param_changes[[param]] <- runif(1, min_val, max_val)
      }
    }
    
    # Run the scenario
    solution <- model_env$run_scenario(
      paste0("Monte Carlo run ", i),
      param_changes = param_changes
    )
    
    # Extract key metrics at end time
    end_metrics <- as.list(solution[nrow(solution), ])
    end_metrics$iteration <- i
    end_metrics$param_changes <- list(param_changes)
    
    return(list(summary = end_metrics, full_results = solution))
  }
  
  # Initialize results list and progress tracking
  results <- list()
  max_iterations <- iterations
  completed_iterations <- 0
  check_interval <- min(100, ceiling(iterations / 10))  # Check convergence every 100 iterations or 10% of total
  converged <- FALSE
  
  # Function to check convergence
  check_convergence <- function(current_results, previous_results) {
    if (length(current_results) < 10 || length(previous_results) < 10) {
      return(FALSE)  # Not enough data to check convergence
    }
    
    # Extract and combine summary metrics for current set
    current_summaries <- lapply(current_results, function(x) x$summary)
    current_df <- do.call(rbind.data.frame, lapply(current_summaries, function(x) {
      data.frame(
        program_success = x$program_success,
        employee_performance = x$employee_performance,
        organizational_profit = x$organizational_profit,
        retention_recruitment = x$retention_recruitment,
        identity_safety = x$identity_safety
      )
    }))
    
    # Extract and combine summary metrics for previous set
    previous_summaries <- lapply(previous_results, function(x) x$summary)
    previous_df <- do.call(rbind.data.frame, lapply(previous_summaries, function(x) {
      data.frame(
        program_success = x$program_success,
        employee_performance = x$employee_performance,
        organizational_profit = x$organizational_profit,
        retention_recruitment = x$retention_recruitment,
        identity_safety = x$identity_safety
      )
    }))
    
    # Calculate means for key metrics
    current_means <- colMeans(current_df, na.rm = TRUE)
    previous_means <- colMeans(previous_df, na.rm = TRUE)
    
    # Calculate relative differences
    rel_diffs <- abs((current_means - previous_means) / previous_means)
    
    # Check if all differences are below threshold
    all(rel_diffs < convergence_threshold, na.rm = TRUE)
  }
  
  # Run Monte Carlo simulations with convergence checking
  if (use_parallel && iterations > 10) {
    # Determine number of cores to use (leave one free)
    num_cores <- parallel::detectCores() - 1
    
    # Create cluster
    cl <- parallel::makeCluster(num_cores)
    
    # Export necessary objects and functions to the cluster
    parallel::clusterExport(cl, c("model_env", "parameters_to_vary", "variation_range",
                                 "stratified_sampling", "param_sample_sets"),
                           envir = environment())
    
    # Load required packages on each worker
    parallel::clusterEvalQ(cl, {
      library(deSolve)   # For ODE solver
      library(igraph)    # For network operations
      library(dplyr)     # For data manipulation
      library(tidyr)     # For data reshaping
    })
    
    # Run in batches if convergence checking is enabled
    if (convergence_check) {
      while (completed_iterations < iterations && !converged) {
        batch_size <- min(check_interval, iterations - completed_iterations)
        batch_indices <- (completed_iterations + 1):(completed_iterations + batch_size)
        
        # Run batch
        batch_results <- parallel::parLapply(cl, batch_indices, function(i) {
          run_iteration(i)
        })
        
        # Add to results
        results <- c(results, batch_results)
        
        # Update completed count
        new_completed <- completed_iterations + batch_size
        
        # Check convergence if we have enough data
        if (completed_iterations >= check_interval && convergence_check) {
          previous_batch <- results[(completed_iterations - check_interval + 1):completed_iterations]
          current_batch <- batch_results
          
          if (check_convergence(current_batch, previous_batch)) {
            cat("Convergence detected after", new_completed, "iterations.\n")
            converged <- TRUE
            break
          }
        }
        
        completed_iterations <- new_completed
        cat("Completed", completed_iterations, "of", iterations, "iterations\n")
      }
    } else {
      # Run all iterations at once if not checking convergence
      results <- parallel::parLapply(cl, 1:iterations, function(i) {
        run_iteration(i)
      })
      completed_iterations <- iterations
    }
    
    # Stop the cluster
    parallel::stopCluster(cl)
  } else {
    # Run sequentially
    if (convergence_check) {
      while (completed_iterations < iterations && !converged) {
        batch_size <- min(check_interval, iterations - completed_iterations)
        batch_indices <- (completed_iterations + 1):(completed_iterations + batch_size)
        
        # Run batch
        batch_results <- lapply(batch_indices, run_iteration)
        
        # Add to results
        results <- c(results, batch_results)
        
        # Update completed count
        new_completed <- completed_iterations + batch_size
        
        # Check convergence if we have enough data
        if (completed_iterations >= check_interval) {
          previous_batch <- results[(completed_iterations - check_interval + 1):completed_iterations]
          current_batch <- batch_results
          
          if (check_convergence(current_batch, previous_batch)) {
            cat("Convergence detected after", new_completed, "iterations.\n")
            converged <- TRUE
            break
          }
        }
        
        completed_iterations <- new_completed
        cat("Completed", completed_iterations, "of", iterations, "iterations\n")
      }
    } else {
      # Run all iterations at once if not checking convergence
      results <- lapply(1:iterations, run_iteration)
      completed_iterations <- iterations
    }
  }
  
  # Extract and combine summary metrics
  summaries <- lapply(results, function(x) x$summary)
  summary_df <- do.call(rbind.data.frame, lapply(summaries, function(x) {
    data.frame(
      iteration = x$iteration,
      program_success = x$program_success,
      employee_performance = x$employee_performance,
      organizational_profit = x$organizational_profit,
      retention_recruitment = x$retention_recruitment,
      identity_safety = x$identity_safety
    )
  }))
  
  # Store all full results
  full_results <- lapply(results, function(x) x$full_results)
  
  # Return both summary and full results
  return(list(
    summary = summary_df,
    full_results = full_results,
    parameter_variations = lapply(results, function(x) x$summary$param_changes),
    iterations_completed = completed_iterations,
    converged = ifelse(convergence_check, converged, NA)
  ))
}

# Analyze and visualize Monte Carlo results
analyze_monte_carlo <- function(mc_results) {
  summary_df <- mc_results$summary
  
  # Create density plots for key outcomes
  p1 <- ggplot(summary_df, aes(x = program_success)) +
    geom_density(fill = "blue", alpha = 0.5) +
    labs(title = "Distribution of Program Success",
         x = "Program Success", y = "Density") +
    theme_minimal()
  
  p2 <- ggplot(summary_df, aes(x = organizational_profit)) +
    geom_density(fill = "green", alpha = 0.5) +
    labs(title = "Distribution of Organizational Profit",
         x = "Organizational Profit", y = "Density") +
    theme_minimal()
  
  p3 <- ggplot(summary_df, aes(x = employee_performance)) +
    geom_density(fill = "purple", alpha = 0.5) +
    labs(title = "Distribution of Employee Performance",
         x = "Employee Performance", y = "Density") +
    theme_minimal()
  
  p4 <- ggplot(summary_df, aes(x = identity_safety)) +
    geom_density(fill = "orange", alpha = 0.5) +
    labs(title = "Distribution of Identity Safety",
         x = "Identity Safety", y = "Density") +
    theme_minimal()
  
  # Calculate statistics - adding na.rm = TRUE to handle missing values
  stats <- summary_df %>%
    summarize(across(where(is.numeric),
                    list(
                      mean = ~mean(., na.rm = TRUE),
                      median = ~median(., na.rm = TRUE),
                      sd = ~sd(., na.rm = TRUE),
                      min = ~min(., na.rm = TRUE),
                      max = ~max(., na.rm = TRUE),
                      q25 = ~quantile(., 0.25, na.rm = TRUE),
                      q75 = ~quantile(., 0.75, na.rm = TRUE)
                    )))
  
  # Arrange plots
  plots <- list(p1, p2, p3, p4)
  grid_plot <- do.call(grid.arrange, c(plots, ncol = 2))
  
  return(list(
    plots = plots,
    grid_plot = grid_plot,
    stats = stats
  ))
}

# Parameter sensitivity analysis
parameter_sensitivity <- function(model_env, parameter, range = seq(0, 1, by = 0.1),
                                metrics = c("program_success", "organizational_profit"),
                                is_initial_value = FALSE) {
  results <- data.frame()
  
  for (value in range) {
    # Create empty lists for both types of changes
    param_changes <- NULL
    initial_changes <- NULL
    
    # Set up the appropriate changes based on is_initial_value flag
    if (is_initial_value) {
      initial_changes <- list()
      initial_changes[[parameter]] <- value
    } else {
      param_changes <- list()
      param_changes[[parameter]] <- value
    }
    
    # Run the scenario with the appropriate changes
    solution <- model_env$run_scenario(
      paste0("Sensitivity: ", parameter, "=", value),
      param_changes = param_changes,
      initial_changes = initial_changes
    )
    
    # Extract end-time values for the specified metrics
    end_values <- solution[nrow(solution), metrics, drop = FALSE]
    
    # Add parameter value
    end_values[[parameter]] <- value
    
    # Append to results
    results <- rbind(results, end_values)
  }
  
  # Create plots
  plots <- list()
  for (metric in metrics) {
    # Use modern aes() with .data pronoun instead of deprecated aes_string()
    p <- ggplot(results, aes(x = .data[[parameter]], y = .data[[metric]])) +
      geom_line(color = "blue", size = 1) +
      geom_point(color = "red", size = 3) +
      labs(title = paste("Sensitivity of", metric, "to", parameter),
           x = parameter, y = metric) +
      theme_minimal()
    
    plots[[metric]] <- p
  }
  
  return(list(
    results = results,
    plots = plots
  ))
}

# Advanced scenario testing
scenario_testing <- function(model_env, scenarios) {
  results <- list()
  
  for (i in seq_along(scenarios)) {
    scenario <- scenarios[[i]]
    scenario_name <- names(scenarios)[i]
    
    # Run the scenario
    solution <- model_env$run_scenario(
      scenario_name,
      param_changes = scenario$param_changes,
      initial_changes = scenario$initial_changes
    )
    
    # Store results
    results[[scenario_name]] <- solution
  }
  
  # Compare scenarios
  compare_scenarios <- function(results, metrics = c("program_success", "organizational_profit")) {
    comparison_data <- data.frame()
    
    for (scenario_name in names(results)) {
      solution <- results[[scenario_name]]
      
      # Extract time series for specified metrics
      for (metric in metrics) {
        metric_data <- data.frame(
          time = solution$time,
          value = solution[[metric]],
          metric = metric,
          scenario = scenario_name
        )
        
        comparison_data <- rbind(comparison_data, metric_data)
      }
    }
    
    # Create comparison plots
    plots <- list()
    for (metric in metrics) {
      metric_data <- comparison_data[comparison_data$metric == metric, ]
      
      p <- ggplot(metric_data, aes(x = time, y = value, color = scenario)) +
        geom_line(size = 1) +
        labs(title = paste("Comparison of", metric, "across scenarios"),
             x = "Time", y = metric) +
        theme_minimal() +
        theme(legend.position = "bottom")
      
      plots[[metric]] <- p
    }
    
    return(list(
      data = comparison_data,
      plots = plots
    ))
  }
  
  return(list(
    results = results,
    compare = compare_scenarios
  ))
}

#############################
## Network Model Extensions ##
#############################

# Add these functions to your existing DEI Network Model environment

# Monte Carlo for network model
network_monte_carlo <- function(model_env, iterations = 1000, edge_variation = 0.3,
                               use_parallel = TRUE, focus_regions = NULL) {
  # Function to run a single Monte Carlo iteration
  run_network_iteration <- function(i) {
    # Create a fresh graph for this iteration
    model_env$G <- model_env$create_graph()
    
    # Randomly vary edge weights
    for (e in 1:length(E(model_env$G))) {
      orig_weight <- E(model_env$G)[e]$weight
      
      # Vary weight within range while keeping between 0 and 1
      min_val <- max(0.1, orig_weight * (1 - edge_variation))
      max_val <- min(1.0, orig_weight * (1 + edge_variation))
      
      E(model_env$G)[e]$weight <- runif(1, min_val, max_val)
    }
    
    # Apply focus regions if provided
    if (!is.null(focus_regions)) {
      for (node_name in names(focus_regions)) {
        # Get current node value - using igraph API since model_env doesn't have get_node_value
        if (node_name %in% V(model_env$G)$name) {
          current_value <- V(model_env$G)[node_name]$value
          
          # Determine if we should apply focused sampling for this iteration
          # Use a probability approach to ensure we still explore the full space
          if (runif(1) < 0.7) {  # 70% chance of using focused region
            # Extract focus range for this node
            range <- focus_regions[[node_name]]
            
            # Sample from the focused range
            focused_value <- runif(1, range[1], range[2])
            
            # Set the node value directly using igraph API
            V(model_env$G)[node_name]$value <- focused_value
          }
        } else {
          warning(paste("Node", node_name, "not found in graph for focused sampling"))
        }
      }
    }
    
    # Run propagation
    history <- model_env$propagate_effects(iterations = 10)
    
    # Extract final values
    final_values <- sapply(history, function(x) x[length(x)])
    
    return(list(
      iteration = i,
      final_values = final_values,
      history = history
    ))
  }
  
  # Run Monte Carlo simulations
  if (use_parallel && iterations > 10) {
    # Determine number of cores to use (leave one free)
    num_cores <- parallel::detectCores() - 1
    
    # Create cluster
    cl <- parallel::makeCluster(num_cores)
    
    # Export necessary objects and functions to the cluster
    parallel::clusterExport(cl, c("model_env", "edge_variation", "focus_regions"),
                           envir = environment())
    
    # Load required packages on each worker
    parallel::clusterEvalQ(cl, {
      library(igraph)    # For network operations
      library(tidygraph) # For tidygraph operations
      library(ggraph)    # For visualization
      library(dplyr)     # For data manipulation
    })
    
    # Run in parallel
    results <- parallel::parLapply(cl, 1:iterations, function(i) {
      run_network_iteration(i)
    })
    
    # Stop the cluster
    parallel::stopCluster(cl)
  } else {
    # Run sequentially
    results <- lapply(1:iterations, run_network_iteration)
  }
  
  # Compile results
  final_values_matrix <- do.call(rbind, lapply(results, function(x) x$final_values))
  
  # Include information about focused regions in return value
  if (!is.null(focus_regions)) {
    focused_nodes <- names(focus_regions)
    focused_stats <- list()
    
    # Get available columns in final_values_matrix
    available_columns <- colnames(final_values_matrix)
    
    for (node in focused_nodes) {
      # Check if node exists in the results
      if (node %in% available_columns) {
        # Extract values for this node
        node_values <- final_values_matrix[, node]
        
        # Calculate stats for focused region
        focused_range <- focus_regions[[node]]
        in_focused_region <- node_values >= focused_range[1] & node_values <= focused_range[2]
        
        focused_stats[[node]] <- list(
          sample_count = sum(in_focused_region),
          sample_percentage = sum(in_focused_region) / length(node_values) * 100,
          mean_value = mean(node_values[in_focused_region], na.rm = TRUE),
          focus_range = focused_range
        )
      } else {
        # Node not found in results, provide placeholder stats
        warning(paste("Node", node, "not found in Monte Carlo results"))
        focused_stats[[node]] <- list(
          sample_count = 0,
          sample_percentage = 0,
          mean_value = NA,
          focus_range = focus_regions[[node]],
          error = "Node not found in results"
        )
      }
    }
  } else {
    focused_stats <- NULL
  }
  
  return(list(
    final_values = final_values_matrix,
    detailed_results = results,
    focus_regions = focus_regions,
    focus_stats = focused_stats
  ))
}

# Analyze network Monte Carlo results
analyze_network_monte_carlo <- function(mc_results) {
  # Convert to data frame
  final_values_df <- as.data.frame(mc_results$final_values)
  
  # Calculate statistics - adding na.rm = TRUE to handle missing values
  stats <- final_values_df %>%
    summarize(across(everything(),
                    list(
                      mean = ~mean(., na.rm = TRUE),
                      median = ~median(., na.rm = TRUE),
                      sd = ~sd(., na.rm = TRUE),
                      min = ~min(., na.rm = TRUE),
                      max = ~max(., na.rm = TRUE),
                      q25 = ~quantile(., 0.25, na.rm = TRUE),
                      q75 = ~quantile(., 0.75, na.rm = TRUE)
                    )))
  
  # Reshape for plotting
  final_values_long <- final_values_df %>%
    tidyr::pivot_longer(cols = everything(), 
                       names_to = "node", 
                       values_to = "value")
  
  # Create box plots for all nodes
  p1 <- ggplot(final_values_long, aes(x = node, y = value)) +
    geom_boxplot(fill = "lightblue") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Distribution of Final Node Values",
         x = "Node", y = "Value")
  
  # Create correlation matrix for key nodes
  key_nodes <- c("Leader Support", "Program Success", "Organizational Profit",
                "Employee Performance", "Identity Safety")
  
  # Safer approach to selecting columns that handles missing columns gracefully
  key_node_data <- final_values_df
  
  # Only keep columns that exist in the data
  existing_key_nodes <- intersect(key_nodes, names(key_node_data))
  
  # If we have at least 2 columns for correlation
  if(length(existing_key_nodes) >= 2) {
    key_node_data <- key_node_data[, existing_key_nodes, drop = FALSE]
    tryCatch({
      cor_matrix <- cor(key_node_data, use = "pairwise.complete.obs")
    }, error = function(e) {
      warning("Could not compute correlation matrix: ", e$message)
      # Return identity matrix as fallback
      cor_matrix <- diag(ncol(key_node_data))
      colnames(cor_matrix) <- rownames(cor_matrix) <- existing_key_nodes
    })
  } else {
    warning("Not enough key nodes found for correlation analysis")
    # Create a 1x1 matrix as fallback
    if(length(existing_key_nodes) == 1) {
      cor_matrix <- matrix(1, 1, 1)
      colnames(cor_matrix) <- rownames(cor_matrix) <- existing_key_nodes
    } else {
      cor_matrix <- matrix(numeric(0), 0, 0)
    }
  }
  
  # Safely create correlation plot
  tryCatch({
    # Only proceed if we have a valid correlation matrix with dimensions
    if(length(cor_matrix) > 0 && nrow(cor_matrix) > 0 && ncol(cor_matrix) > 0) {
      # Convert correlation matrix to long format for plotting
      cor_long <- reshape2::melt(cor_matrix)
      
      p2 <- ggplot(cor_long, aes(x = Var1, y = Var2, fill = value)) +
        geom_tile() +
        scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                            midpoint = 0, limit = c(-1, 1), name = "Correlation") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(title = "Correlation Between Key Nodes",
            x = "", y = "")
    } else {
      # No valid correlation data, create placeholder plot
      warning("No correlation data available for plotting")
      p2 <- ggplot() +
        annotate("text", x = 0, y = 0, label = "No correlation data available") +
        theme_void()
    }
  }, error = function(e) {
    warning("Could not create correlation plot: ", e$message)
    # Create a fallback empty plot
    p2 <- ggplot() +
      annotate("text", x = 0, y = 0, label = paste("Error creating correlation plot:", e$message)) +
      theme_void()
  })
  
  return(list(
    stats = stats,
    boxplot = p1,
    correlation = p2
  ))
}

# Threshold and tipping point analysis
identify_thresholds <- function(model_env, node_to_vary, 
                               range = seq(0, 10, by = 0.5),
                               target_nodes = c("Program Success", "Organizational Profit"),
                               iterations = 10) {
  results <- data.frame()
  
  for (value in range) {
    # Set the value for the node we're varying
    model_env$set_node_values(setNames(list(value), node_to_vary))
    
    # Run propagation
    history <- model_env$propagate_effects(iterations)
    
    # Extract final values for target nodes
    row_data <- data.frame(input_value = value)
    
    for (target in target_nodes) {
      if (target %in% names(history)) {
        final_value <- history[[target]][length(history[[target]])]
        row_data[[paste0(target, "_final")]] <- final_value
      }
    }
    
    # Clean column names to prevent parsing issues
    names(row_data) <- gsub(" ", "_", names(row_data))
    
    # Add to results
    results <- rbind(results, row_data)
  }
  
  # Identify potential thresholds by looking for large changes in slope
  identify_potential_thresholds <- function(data, input_col, output_col) {
    # Ensure column names don't have spaces
    clean_output_col <- gsub(" ", "_", output_col)
    if (output_col != clean_output_col && clean_output_col %in% names(data)) {
      output_col <- clean_output_col
    }
    
    # Calculate first differences
    diffs <- diff(data[[output_col]]) / diff(data[[input_col]])
    
    # Find where rate of change is significantly above average
    mean_diff <- mean(diffs, na.rm = TRUE)
    sd_diff <- sd(diffs, na.rm = TRUE)
    threshold_indices <- which(diffs > (mean_diff + 1.5 * sd_diff) |
                              diffs < (mean_diff - 1.5 * sd_diff))
    
    # Get the corresponding input values
    if (length(threshold_indices) > 0) {
      threshold_values <- data[[input_col]][threshold_indices + 1]
      return(threshold_values)
    } else {
      return(NULL)
    }
  }
  
  # Create visualization
  plots <- list()
  thresholds <- list()
  
  for (target in target_nodes) {
    output_col <- paste0(target, "_final")
    
    if (output_col %in% colnames(results)) {
      # Identify potential thresholds
      potential_thresholds <- identify_potential_thresholds(results, "input_value", output_col)
      thresholds[[target]] <- potential_thresholds
      
      # Create plot using aes() instead of deprecated aes_string()
      # And handle column names with spaces properly
      clean_output_col <- gsub(" ", "_", output_col) # Ensure no spaces in column names
      
      # Rename column if it contains spaces
      if (output_col != clean_output_col) {
        names(results)[names(results) == output_col] <- clean_output_col
        output_col <- clean_output_col
      }
      
      p <- ggplot(results, aes(x = .data$input_value, y = .data[[output_col]])) +
        geom_line(color = "blue", size = 1) +
        geom_point(size = 2) +
        labs(title = paste("Effect of", node_to_vary, "on", target),
             x = node_to_vary, y = target) +
        theme_minimal()
      
      # Add vertical lines for thresholds
      if (!is.null(potential_thresholds)) {
        for (thresh in potential_thresholds) {
          p <- p + geom_vline(xintercept = thresh, linetype = "dashed", 
                             color = "red", alpha = 0.7)
        }
      }
      
      plots[[target]] <- p
    }
  }
  
  # Reset model
  model_env$set_node_values(setNames(list(5), node_to_vary))
  
  return(list(
    results = results,
    plots = plots,
    thresholds = thresholds
  ))
}

###########################
## Statistical Analysis ##
###########################

# These functions can be used with either model

# Calculate confidence intervals for simulation results
calculate_confidence_intervals <- function(results, alpha = 0.05) {
  # If results is a matrix or data frame
  if (is.matrix(results) || is.data.frame(results)) {
    ci <- data.frame(
      mean = colMeans(results, na.rm = TRUE),
      lower = apply(results, 2, function(x) quantile(x, alpha/2, na.rm = TRUE)),
      upper = apply(results, 2, function(x) quantile(x, 1-alpha/2, na.rm = TRUE))
    )
    return(ci)
  } 
  # If results is a vector
  else if (is.vector(results)) {
    mean_val <- mean(results, na.rm = TRUE)
    lower <- quantile(results, alpha/2, na.rm = TRUE)
    upper <- quantile(results, 1-alpha/2, na.rm = TRUE)
    return(data.frame(mean = mean_val, lower = lower, upper = upper))
  }
}

# Variance decomposition using random forests
variance_decomposition <- function(mc_results, target_var) {
  if (!requireNamespace("randomForest", quietly = TRUE)) {
    install.packages("randomForest")
    library(randomForest)
  } else {
    library(randomForest)
  }
  
  # Extract parameter variations
  param_variations <- mc_results$parameter_variations
  
  # Create data frame with all parameter values
  param_df <- data.frame()
  
  for (i in seq_along(param_variations)) {
    row_data <- unlist(param_variations[[i]])
    row_data <- as.data.frame(t(row_data))
    param_df <- rbind(param_df, row_data)
  }
  
  # Add target variable
  param_df$target <- mc_results$summary[[target_var]]
  
  # Fit random forest
  rf_model <- randomForest(target ~ ., data = param_df, importance = TRUE)
  
  # Extract variable importance
  importance_df <- as.data.frame(importance(rf_model))
  importance_df$parameter <- rownames(importance_df)
  
  # Create importance plot
  importance_df <- importance_df[order(importance_df$`%IncMSE`, decreasing = TRUE), ]
  
  p <- ggplot(importance_df, aes(x = reorder(parameter, `%IncMSE`), y = `%IncMSE`)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() +
    labs(title = paste("Variable Importance for", target_var),
         x = "Parameter", y = "% Increase in MSE") +
    theme_minimal()
  
  return(list(
    rf_model = rf_model,
    importance = importance_df,
    plot = p
  ))
}

# Example usage
# Note: These functions should be called with your model instances
if (FALSE) {  # Set to TRUE to run examples
  # Load your models first
  system_model <- dei_system_model()
  network_model <- dei_network_model()
  
  # Run Monte Carlo for system dynamics model
  mc_results <- run_monte_carlo_sim(system_model, iterations = 100)
  mc_analysis <- analyze_monte_carlo(mc_results)
  
  # Parameter sensitivity for a key parameter
  sensitivity <- parameter_sensitivity(system_model, "leader_to_funding")
  
  # Define and run scenarios
  scenarios <- list(
    "High Leader Support" = list(
      initial_changes = list(leader_support = 8.0),
      param_changes = NULL
    ),
    "Low Cultural Support" = list(
      initial_changes = list(cultural_zeitgeist = 2.0),
      param_changes = NULL
    ),
    "Strong Program Implementation" = list(
      initial_changes = NULL,
      param_changes = list(implementation_to_success = 0.9)
    )
  )
  
  scenario_results <- scenario_testing(system_model, scenarios)
  comparison <- scenario_results$compare(scenario_results$results)
  
  # Network model analysis
  network_mc <- network_monte_carlo(network_model, iterations = 50)
  network_analysis <- analyze_network_monte_carlo(network_mc)
  
  # Threshold analysis
  thresholds <- identify_thresholds(network_model, "Leader Support")
  
  # Statistical analysis
  ci <- calculate_confidence_intervals(mc_results$summary$program_success)
  var_decomp <- variance_decomposition(mc_results, "program_success")
}