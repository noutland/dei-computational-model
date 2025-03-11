# DEI Simulation Statistical Analysis
# This script provides comprehensive statistical analysis for DEI simulation data

# Load required libraries
library(dplyr)        # Data manipulation
library(ggplot2)      # Visualization
library(tidyr)        # Data tidying
library(lme4)         # Mixed effects models
library(car)          # For regression diagnostics
library(randomForest) # For variable importance
library(corrplot)     # For correlation visualization
library(cluster)      # For clustering
library(factoextra)   # For cluster visualization
library(rstatix)      # For statistical tests
library(patchwork)    # For combining plots

#############################
## 1. DESCRIPTIVE STATISTICS ##
#############################

# Function to calculate descriptive statistics for simulation results
describe_simulation_data <- function(data, group_var = NULL) {
  # Identify numeric columns
  numeric_cols <- sapply(data, is.numeric)
  numeric_data <- data[, numeric_cols]
  
  if (!is.null(group_var) && group_var %in% names(data)) {
    # Group by the specified variable
    result <- data %>%
      group_by(.data[[group_var]]) %>%
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
  } else {
    # Overall statistics
    result <- numeric_data %>%
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
  }
  
  return(result)
}

# Function to create distribution plots for key metrics
plot_distributions <- function(data, metrics, title = "Distribution of Key Metrics") {
  # Ensure metrics exist in the data
  existing_metrics <- metrics[metrics %in% names(data)]
  
  if(length(existing_metrics) == 0) {
    warning("None of the specified metrics exist in the data")
    return(NULL)
  }
  
  # Wrap entire function in tryCatch to handle errors gracefully
  tryCatch({
    # Select only the metrics we need
    valid_data <- data[, existing_metrics, drop = FALSE]
    
    # Remove rows with all NA values
    valid_data <- valid_data[rowSums(!is.na(valid_data)) > 0, , drop = FALSE]
    
    # Check if we have any data left
    if(nrow(valid_data) == 0) {
      warning("No valid data points for plotting after removing NAs")
      return(NULL)
    }
    
    # Convert to long format using base R operations if needed
    if(requireNamespace("tidyr", quietly = TRUE)) {
      long_data <- tidyr::pivot_longer(valid_data,
                                    cols = colnames(valid_data),
                                    names_to = "Metric",
                                    values_to = "Value")
    } else {
      long_data <- data.frame(
        Metric = rep(names(valid_data), each = nrow(valid_data)),
        Value = unlist(valid_data),
        stringsAsFactors = FALSE
      )
    }
    
    # Remove any non-finite values
    long_data <- long_data[is.finite(long_data$Value), ]
    
    # Check if we have any data left after filtering
    if(nrow(long_data) == 0) {
      warning("No valid data points for plotting after removing non-finite values")
      return(NULL)
    }
    
    # Create plots
    p <- ggplot(long_data, aes(x = Value, fill = Metric)) +
      geom_density(alpha = 0.5) +
      facet_wrap(~ Metric, scales = "free") +
      theme_minimal() +
      labs(title = title) +
      theme(legend.position = "none")
    
    return(p)
  }, error = function(e) {
    warning(paste("Error creating density plot:", e$message))
    return(NULL)
  })
}

# Function to visualize correlations between variables
plot_correlations <- function(data, metrics) {
  # Ensure metrics exist in the data
  existing_metrics <- metrics[metrics %in% names(data)]
  
  if(length(existing_metrics) == 0) {
    warning("None of the specified metrics exist in the data")
    return(NULL)
  }
  
  # Select valid data
  valid_data <- data[, existing_metrics, drop = FALSE]
  
  # Check if we have any complete cases
  if(sum(complete.cases(valid_data)) == 0) {
    warning("No complete cases for correlation analysis")
    return(NULL)
  }
  
  # Calculate correlation matrix with error handling
  tryCatch({
    cor_matrix <- cor(valid_data, use = "pairwise.complete.obs")
    
    # Check if the correlation matrix contains valid values
    if(all(is.na(cor_matrix))) {
      warning("Correlation matrix contains only NA values")
      return(NULL)
    }
    
    # Create correlation plot
    corrplot(cor_matrix, method = "circle", type = "upper",
             tl.col = "black", tl.srt = 45, addCoef.col = "black",
             col = colorRampPalette(c("#6D9EC1", "white", "#E46726"))(200),
             title = "Correlation Between DEI Metrics")
  }, error = function(e) {
    warning(paste("Error in correlation analysis:", e$message))
    return(NULL)
  })
}

#############################
## 2. REGRESSION ANALYSIS ##
#############################

# Function to run regression models for key outcome variables
run_regression_models <- function(data, outcome_vars, predictor_vars) {
  models <- list()
  summaries <- list()
  
  # Clean column names
  clean_data <- data
  names(clean_data) <- gsub(" ", "_", names(clean_data))
  
  # Clean variable names
  clean_outcome_vars <- gsub(" ", "_", outcome_vars)
  clean_predictor_vars <- gsub(" ", "_", predictor_vars)
  
  # Check if variables exist in data
  existing_outcomes <- clean_outcome_vars[clean_outcome_vars %in% names(clean_data)]
  if(length(existing_outcomes) == 0) {
    warning("None of the outcome variables found in data")
    return(NULL)
  }
  
  existing_predictors <- clean_predictor_vars[clean_predictor_vars %in% names(clean_data)]
  if(length(existing_predictors) == 0) {
    warning("None of the predictor variables found in data")
    return(NULL)
  }
  
  for (outcome in existing_outcomes) {
    # Select only the columns we need to minimize NA impact
    model_data <- clean_data[, c(outcome, existing_predictors)]
    
    # Remove rows with NA values
    model_data <- na.omit(model_data)
    
    # Check if we have enough data left
    if(nrow(model_data) < 5) {  # Arbitrary minimum for regression
      warning(paste("Not enough complete cases for regression model of", outcome))
      models[[outcome]] <- NULL
      summaries[[outcome]] <- NULL
      next
    }
    
    # Formula for regression
    formula_str <- paste(outcome, "~", paste(existing_predictors, collapse = " + "))
    formula_obj <- as.formula(formula_str)
    
    # Run linear model with error handling
    tryCatch({
      # Run linear model
      model <- lm(formula_obj, data = model_data)
      
      # Store model and summary
      models[[outcome]] <- model
      summaries[[outcome]] <- summary(model)
      
      # Print summary
      cat("\n\n=== Regression Model for", outcome, "===\n")
      print(summaries[[outcome]])
      
      # Check assumptions
      par(mfrow = c(2, 2))
      plot(model)
      par(mfrow = c(1, 1))
    }, error = function(e) {
      warning(paste("Error in regression model for", outcome, ":", e$message))
      models[[outcome]] <- NULL
      summaries[[outcome]] <- NULL
    })
  }
  
  if(length(models) == 0) {
    warning("No successful regression models were created")
    return(NULL)
  }
  
  return(list(models = models, summaries = summaries))
}

# Function for stepwise variable selection
run_stepwise_selection <- function(data, outcome_var, predictor_vars) {
  # Clean column names to handle spaces
  clean_data <- data
  names(clean_data) <- gsub(" ", "_", names(clean_data))
  
  # Clean variable names
  clean_outcome_var <- gsub(" ", "_", outcome_var)
  clean_predictor_vars <- gsub(" ", "_", predictor_vars)
  
  # Full formula
  full_formula <- as.formula(paste(clean_outcome_var, "~", paste(clean_predictor_vars, collapse = " + ")))
  
  # Null model
  null_model <- lm(as.formula(paste(clean_outcome_var, "~ 1")), data = clean_data)
  
  # Full model
  full_model <- lm(full_formula, data = clean_data)
  
  # Stepwise selection
  step_model <- step(null_model, scope = list(lower = null_model, upper = full_model),
                    direction = "both", trace = 0)
  
  # Print results
  cat("\n\n=== Stepwise Selection for", outcome_var, "===\n")
  print(summary(step_model))
  
  return(step_model)
}

##############################
## 3. VARIANCE DECOMPOSITION ##
##############################

# Function to decompose variance using random forests
decompose_variance <- function(data, outcome_var, predictor_vars) {
  # Clean column names
  clean_data <- data
  names(clean_data) <- gsub(" ", "_", names(clean_data))
  
  # Clean variable names
  clean_outcome_var <- gsub(" ", "_", outcome_var)
  clean_predictor_vars <- gsub(" ", "_", predictor_vars)
  
  # Check if variables exist in data
  if(!(clean_outcome_var %in% names(clean_data))) {
    warning(paste("Outcome variable", outcome_var, "not found in data"))
    return(NULL)
  }
  
  existing_predictors <- clean_predictor_vars[clean_predictor_vars %in% names(clean_data)]
  if(length(existing_predictors) == 0) {
    warning("None of the predictor variables found in data")
    return(NULL)
  }
  
  # Remove NA values
  clean_data <- clean_data[, c(clean_outcome_var, existing_predictors)]
  clean_data <- na.omit(clean_data)
  
  if(nrow(clean_data) == 0) {
    warning("No complete cases available for random forest analysis")
    return(NULL)
  }
  
  # Prepare formula
  formula_str <- paste(clean_outcome_var, "~", paste(existing_predictors, collapse = " + "))
  formula_obj <- as.formula(formula_str)
  
  # Run random forest with error handling
  tryCatch({
    rf_model <- randomForest(formula_obj, data = clean_data, importance = TRUE, ntree = 500)
    
    # Get importance measures
    importance_df <- as.data.frame(importance(rf_model))
    importance_df$Variable <- rownames(importance_df)
    
    # Sort by importance
    importance_df <- importance_df[order(importance_df$`%IncMSE`, decreasing = TRUE), ]
    
    # Create importance plot
    p <- ggplot(importance_df, aes(x = reorder(Variable, `%IncMSE`), y = `%IncMSE`)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      theme_minimal() +
      labs(title = paste("Variable Importance for", outcome_var),
           x = "Variable", y = "% Increase in MSE")
    
    return(list(model = rf_model, importance = importance_df, plot = p))
  }, error = function(e) {
    warning(paste("Error in random forest analysis:", e$message))
    return(NULL)
  })
}

#########################
## 4. CLUSTER ANALYSIS ##
#########################

# Function to identify clusters of simulation runs
cluster_analysis <- function(data, variables, k_max = 10) {
  # Check if variables exist in data
  existing_vars <- variables[variables %in% names(data)]
  if(length(existing_vars) == 0) {
    warning("None of the specified variables exist in the data")
    return(NULL)
  }
  
  # Select only valid data
  valid_data <- data[, existing_vars, drop = FALSE]
  valid_data <- na.omit(valid_data)
  
  # Check if we have enough data
  if(nrow(valid_data) < 3) {  # Need at least 3 points for meaningful clustering
    warning("Not enough complete cases for cluster analysis")
    return(NULL)
  }
  
  # Get number of unique data points
  unique_rows <- nrow(unique(valid_data))
  
  # Adjust k_max if needed
  if(unique_rows < k_max) {
    k_max <- max(2, unique_rows - 1) # At least 2, but less than number of unique points
    warning(paste("Adjusting maximum number of clusters to", k_max, "due to limited data points"))
  }
  
  # Standardize data with error handling
  tryCatch({
    scaled_data <- scale(valid_data)
    
    # Determine optimal number of clusters
    fviz_nbclust_obj <- fviz_nbclust(scaled_data, kmeans, method = "silhouette", k.max = k_max)
    optimal_k <- as.numeric(gsub(".*\\s(\\d+).*", "\\1", fviz_nbclust_obj$data$clusters[which.max(fviz_nbclust_obj$data$y)]))
    
    # Ensure optimal_k is valid
    if(is.na(optimal_k) || optimal_k < 2) {
      optimal_k <- 2  # Default to 2 clusters if optimal cannot be determined
    }
    
    # Run k-means with optimal k
    set.seed(123)  # For reproducibility
    km <- kmeans(scaled_data, centers = min(optimal_k, nrow(valid_data) - 1), nstart = 25)
    
    # Add cluster assignments to original data
    result_data <- valid_data
    result_data$cluster <- as.factor(km$cluster)
    
    # Create cluster visualization
    p1 <- fviz_cluster(km, data = scaled_data, 
                      geom = "point",
                      ellipse.type = "convex",
                      palette = "jco",
                      ggtheme = theme_minimal())
    
    # Create cluster profile plot - handle NAs properly
    cluster_profiles <- result_data %>%
      group_by(cluster) %>%
      summarize(across(all_of(existing_vars), ~mean(., na.rm = TRUE)))
    
    cluster_profiles_long <- cluster_profiles %>%
      pivot_longer(cols = -cluster, names_to = "Variable", values_to = "Value")
    
    p2 <- ggplot(cluster_profiles_long, aes(x = Variable, y = Value, color = cluster, group = cluster)) +
      geom_line(linewidth = 1) +
      geom_point(size = 3) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = "Cluster Profiles", x = "Variable", y = "Mean Value")
    
    return(list(
      kmeans = km,
      data = result_data,
      optimal_k = optimal_k,
      cluster_plot = p1,
      profile_plot = p2
    ))
  }, error = function(e) {
    warning(paste("Error in cluster analysis:", e$message))
    return(NULL)
  })
}

##############################
## 5. COMPARATIVE ANALYSIS ##
##############################

# Function to compare scenarios statistically
compare_scenarios <- function(scenario_data, outcome_vars) {
  results <- list()
  plots <- list()
  
  for (outcome in outcome_vars) {
    # ANOVA
    anova_formula <- as.formula(paste(outcome, "~ scenario"))
    anova_result <- aov(anova_formula, data = scenario_data)
    
    # Tukey HSD for pairwise comparisons
    tukey_result <- TukeyHSD(anova_result)
    
    # Store results
    results[[outcome]] <- list(
      anova = anova_result,
      tukey = tukey_result
    )
    
    # Create boxplot
    p <- ggplot(scenario_data, aes(x = scenario, y = .data[[outcome]], fill = scenario)) +
      geom_boxplot() +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none") +
      labs(title = paste("Comparison of", outcome, "across Scenarios"),
           x = "Scenario", y = outcome)
    
    plots[[outcome]] <- p
  }
  
  return(list(
    results = results,
    plots = plots
  ))
}

#########################
## 6. THRESHOLD DETECTION ##
#########################

# Function to detect thresholds in response curves
detect_thresholds <- function(data, x_var, y_var, window_size = 3) {
  # Ensure data is sorted by x_var
  sorted_data <- data[order(data[[x_var]]), ]
  
  # Calculate rate of change (first derivative)
  x_values <- sorted_data[[x_var]]
  y_values <- sorted_data[[y_var]]
  
  # Calculate differences
  dx <- diff(x_values)
  dy <- diff(y_values)
  
  # Avoid division by zero
  derivatives <- dy / pmax(dx, 0.001)
  
  # Add NA for the last point (to maintain length)
  derivatives <- c(derivatives, NA)
  
  # Add derivatives to data
  result_data <- sorted_data
  result_data$derivative <- derivatives
  
  # Calculate rolling average of derivatives to smooth
  result_data$smooth_derivative <- NA
  
  for (i in 1:(nrow(result_data) - window_size + 1)) {
    window <- result_data$derivative[i:(i + window_size - 1)]
    result_data$smooth_derivative[i + window_size %/% 2] <- mean(window, na.rm = TRUE)
  }
  
  # Detect potential thresholds (points where derivative changes significantly)
  mean_derivative <- mean(result_data$smooth_derivative, na.rm = TRUE)
  sd_derivative <- sd(result_data$smooth_derivative, na.rm = TRUE)
  
  # Clean up column names to avoid parse errors with spaces
  names(result_data) <- gsub(" ", "_", names(result_data))
  
  # Ensure x_var and y_var don't have spaces
  x_var_clean <- gsub(" ", "_", x_var)
  y_var_clean <- gsub(" ", "_", y_var)
  
  # Handle missing values in derivatives
  mean_derivative <- mean(result_data$smooth_derivative, na.rm = TRUE)
  sd_derivative <- sd(result_data$smooth_derivative, na.rm = TRUE)
  
  # Points where derivative is significantly above or below average
  threshold_indices <- which(
    !is.na(result_data$smooth_derivative) &
    (result_data$smooth_derivative > (mean_derivative + 1.5 * sd_derivative) |
     result_data$smooth_derivative < (mean_derivative - 1.5 * sd_derivative))
  )
  
  threshold_points <- result_data[threshold_indices, ]
  
  # Create visualization
  p <- ggplot(result_data, aes(x = .data[[x_var_clean]], y = .data[[y_var_clean]])) +
    geom_point() +
    geom_line() +
    geom_point(data = threshold_points, color = "red", size = 3) +
    theme_minimal() +
    labs(title = paste("Threshold Detection for", y_var, "vs", x_var),
         x = x_var, y = y_var)
  
  return(list(
    data = result_data,
    threshold_points = threshold_points,
    plot = p
  ))
}

#####################
## 7. NETWORK ANALYSIS ##
#####################

# Function to analyze centrality statistics from network models
analyze_network_centrality <- function(centrality_df) {
  # Check if data frame is valid
  if(!is.data.frame(centrality_df) || nrow(centrality_df) == 0) {
    warning("Invalid centrality data frame provided")
    return(NULL)
  }
  
  # Get column names
  column_names <- names(centrality_df)
  
  # Define possible column name patterns
  std_cols <- c("Degree Centrality", "In-Degree Centrality", "Out-Degree Centrality", "Betweenness Centrality")
  alt_cols <- c("Degree.Centrality", "In.Degree.Centrality", "Out.Degree.Centrality", "Betweenness.Centrality")
  
  # Check which pattern is available
  if(all(std_cols %in% column_names)) {
    centrality_columns <- std_cols
  } else if(all(alt_cols %in% column_names)) {
    centrality_columns <- alt_cols
  } else {
    warning("Required centrality columns not found. Available columns: ",
            paste(column_names, collapse=", "))
    return(NULL)
  }
  
  # Also verify Node column exists
  if(!"Node" %in% column_names) {
    warning("Node column not found in centrality data frame")
    return(NULL)
  }
  
  # Proceed with analysis safely using try-catch
  tryCatch({
    # Calculate percentile ranks for each centrality measure
    centrality_df <- centrality_df %>%
      mutate(across(all_of(centrality_columns), list(rank = ~percent_rank(.))))
    
    # Get the rank column names (they'll have "_rank" suffix)
    rank_columns <- paste0(centrality_columns, "_rank")
    
    # Create unified influence score
    centrality_df$influence_score <- rowMeans(centrality_df[, rank_columns, drop = FALSE], na.rm = TRUE)
    
    # Sort by influence score
    centrality_df <- centrality_df[order(centrality_df$influence_score, decreasing = TRUE), ]
    
    # Create visualization
    p <- ggplot(centrality_df, aes(x = reorder(Node, influence_score), y = influence_score)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      theme_minimal() +
      labs(title = "Overall Influence Score of Network Nodes",
           x = "Node", y = "Influence Score (higher = more influential)")
    
    return(list(
      data = centrality_df,
      plot = p
    ))
  }, error = function(e) {
    warning(paste("Error in network centrality analysis:", e$message))
    return(NULL)
  })
}

# Function to analyze feedback loops
analyze_feedback_loops <- function(loops_list) {
  # Check if the loops_list is valid
  if(!is.list(loops_list) || length(loops_list) == 0) {
    warning("No valid feedback loops provided")
    return(NULL)
  }
  
  # Proceed with analysis safely using try-catch
  tryCatch({
    # Check if the loops have the expected structure
    required_fields <- c("length", "type", "strength")
    for(loop_name in names(loops_list)) {
      loop <- loops_list[[loop_name]]
      if(!is.list(loop) || !all(required_fields %in% names(loop))) {
        warning("Loop ", loop_name, " doesn't have all required fields (length, type, strength)")
        return(NULL)
      }
    }
    
    # Extract loop properties into data frame
    loops_df <- data.frame(
      loop = names(loops_list),
      length = sapply(loops_list, function(x) x$length),
      type = sapply(loops_list, function(x) x$type),
      strength = sapply(loops_list, function(x) x$strength)
    )
    
    # Sort by strength
    loops_df <- loops_df[order(loops_df$strength, decreasing = TRUE), ]
    
    # Create visualization
    p <- ggplot(loops_df, aes(x = reorder(loop, strength), y = strength, fill = type)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_fill_manual(values = c("Reinforcing" = "forestgreen", "Balancing" = "firebrick")) +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 8)) +
      labs(title = "Feedback Loops by Strength",
          x = "Loop", y = "Strength", fill = "Type")
    
    return(list(
      data = loops_df,
      plot = p
    ))
  }, error = function(e) {
    warning(paste("Error in feedback loop analysis:", e$message))
    return(NULL)
  })
}

############################
## 8. COMPLETE WORKFLOW EXAMPLE ##
############################

# This section provides an example of how to use the functions above
# with simulation data from our DEI models

if (FALSE) {  # Set to FALSE to avoid execution during source
  # Load Monte Carlo simulation results
  # Assuming mc_system$summary and other objects are available
  monte_carlo_data <- mc_system$summary
  
  # 1. Descriptive statistics
  mc_stats <- describe_simulation_data(monte_carlo_data)
  print(mc_stats)
  
  # Distribution plots
  key_metrics <- c("program_success", "organizational_profit", 
                  "employee_performance", "retention_recruitment")
  dist_plot <- plot_distributions(monte_carlo_data, key_metrics)
  print(dist_plot)
  
  # Correlation analysis
  plot_correlations(monte_carlo_data, key_metrics)
  
  # 2. Regression analysis
  # Extract parameter values from Monte Carlo runs
  param_data <- data.frame(
    do.call(rbind, lapply(mc_system$parameter_variations, unlist))
  )
  
  # Combine with outcome metrics
  regression_data <- cbind(param_data, monte_carlo_data[, key_metrics])
  
  # Run regression models
  reg_models <- run_regression_models(
    regression_data,
    outcome_vars = c("program_success", "organizational_profit"),
    predictor_vars = names(param_data)
  )
  
  # Stepwise selection
  step_model <- run_stepwise_selection(
    regression_data,
    outcome_var = "program_success",
    predictor_vars = names(param_data)
  )
  
  # 3. Variance decomposition
  var_decomp <- decompose_variance(
    regression_data,
    outcome_var = "program_success",
    predictor_vars = names(param_data)
  )
  print(var_decomp$plot)
  
  # 4. Cluster analysis
  clusters <- cluster_analysis(
    monte_carlo_data,
    variables = key_metrics
  )
  print(clusters$cluster_plot)
  print(clusters$profile_plot)
  
  # 5. Compare scenarios
  # Load scenario results
  scenario_summary <- read.csv("dei_simulation_results/scenario_summary.csv")
  
  comparisons <- compare_scenarios(
    scenario_summary,
    outcome_vars = key_metrics
  )
  
  # Print plots
  for (outcome in key_metrics) {
    print(comparisons$plots[[outcome]])
  }
  
  # 6. Threshold detection
  # Example with threshold analysis data
  threshold_data <- threshold_results[["Leader Support"]]$results
  
  thresholds <- detect_thresholds(
    threshold_data,
    x_var = "input_value",
    y_var = "Program Success_final"
  )
  print(thresholds$plot)
  
  # 7. Network analysis
  # Analyze centrality
  centrality_analysis <- analyze_network_centrality(centrality_df)
  print(centrality_analysis$plot)
  
  # Analyze feedback loops
  loops_analysis <- analyze_feedback_loops(loops)
  print(loops_analysis$plot)
}