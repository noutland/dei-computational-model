# Non-linear Relationships for DEI Computational Model
# This file implements sophisticated non-linear relationship functions 
# that better capture complex real-world interactions

#' Sigmoid function for smooth threshold transitions
#' 
#' @param x Input value
#' @param k Steepness parameter (higher = steeper transition)
#' @param x0 Midpoint of transition
#' @param ymin Minimum output value
#' @param ymax Maximum output value
#' @return Sigmoid function output between ymin and ymax
sigmoid <- function(x, k = 1, x0 = 5, ymin = 0, ymax = 1) {
  y <- 1 / (1 + exp(-k * (x - x0)))
  return(ymin + y * (ymax - ymin))
}

#' Threshold function with defined regions
#' 
#' @param x Input value
#' @param thresholds Vector of threshold values
#' @param values Vector of output values (length = length(thresholds) + 1)
#' @return Threshold function output
threshold <- function(x, thresholds = c(3, 7), values = c(0.1, 0.5, 0.9)) {
  if (length(values) != length(thresholds) + 1) {
    stop("Values vector must be one element longer than thresholds vector")
  }
  
  for (i in 1:length(thresholds)) {
    if (x < thresholds[i]) {
      return(values[i])
    }
  }
  
  return(values[length(values)])
}

#' Smooth threshold function (piecewise linear with smooth transitions)
#' 
#' @param x Input value
#' @param thresholds Vector of threshold values
#' @param values Vector of output values (length = length(thresholds) + 1)
#' @param smoothness Width of transition regions (0-1)
#' @return Smooth threshold function output
smooth_threshold <- function(x, thresholds = c(3, 7), values = c(0.1, 0.5, 0.9), smoothness = 0.2) {
  if (length(values) != length(thresholds) + 1) {
    stop("Values vector must be one element longer than thresholds vector")
  }
  
  # If x is below first threshold or above last, return corresponding value
  if (x <= thresholds[1] - smoothness) {
    return(values[1])
  }
  if (x >= thresholds[length(thresholds)] + smoothness) {
    return(values[length(values)])
  }
  
  # Check which region x falls into
  for (i in 1:length(thresholds)) {
    if (i < length(thresholds)) {
      # Between thresholds
      if (x >= thresholds[i] + smoothness && x <= thresholds[i+1] - smoothness) {
        return(values[i+1])
      }
    }
    
    # In transition zone before current threshold
    if (x >= thresholds[i] - smoothness && x <= thresholds[i] + smoothness) {
      # Calculate transition proportion
      prop <- (x - (thresholds[i] - smoothness)) / (2 * smoothness)
      # Blend values
      return(values[i] * (1 - prop) + values[i+1] * prop)
    }
  }
  
  # Fallback (should not reach here with proper thresholds)
  return(values[1])
}

#' Diminishing returns function
#' 
#' @param x Input value
#' @param scale Scale parameter for diminishing returns
#' @param max_value Maximum output value
#' @return Diminishing returns function output
diminishing_returns <- function(x, scale = 0.5, max_value = 1) {
  return(max_value * (1 - exp(-scale * x)))
}

#' Saturation function
#' 
#' @param x Input value
#' @param half_saturation Value of x at which output is half of max
#' @param hill Steepness parameter (higher = more switch-like)
#' @param max_value Maximum output value
#' @return Saturation function output
saturation <- function(x, half_saturation = 5, hill = 2, max_value = 1) {
  return(max_value * (x^hill) / (half_saturation^hill + x^hill))
}

#' Synergy function for two factors
#' 
#' @param x First factor value
#' @param y Second factor value
#' @param synergy_threshold Threshold above which synergy occurs
#' @param synergy_strength Strength of synergistic effect
#' @return Synergy multiplier
synergy <- function(x, y, synergy_threshold = 5, synergy_strength = 0.2) {
  if (x > synergy_threshold && y > synergy_threshold) {
    # Calculate how much each factor exceeds the threshold
    x_excess <- x - synergy_threshold
    y_excess <- y - synergy_threshold
    
    # Synergy is proportional to the product of excess values
    synergy_effect <- 1 + synergy_strength * (x_excess * y_excess) / 25
    return(synergy_effect)
  } else {
    # No synergy if either factor is below threshold
    return(1.0)
  }
}

#' Antagonism function for two factors
#' 
#' @param x First factor value
#' @param y Second factor value
#' @param antagonism_threshold Threshold difference for antagonism
#' @param antagonism_strength Strength of antagonistic effect
#' @return Antagonism multiplier
antagonism <- function(x, y, antagonism_threshold = 4, antagonism_strength = 0.15) {
  # Calculate difference between factors
  diff <- abs(x - y)
  
  if (diff > antagonism_threshold) {
    # Antagonism increases with difference above threshold
    antagonism_effect <- 1 - antagonism_strength * (diff - antagonism_threshold) / 5
    return(max(0.5, antagonism_effect))  # Don't let it go below 0.5
  } else {
    # No antagonism if difference is below threshold
    return(1.0)
  }
}

#' Identity safety nonlinear effect function
#' 
#' @param org_messaging Organizational messaging value
#' @param promotion_equity Promotion equity value
#' @param current_safety Current identity safety value
#' @param organizational_to_identity Parameter weight for organizational messaging
#' @param promotion_to_identity Parameter weight for promotion equity
#' @return Rate of change for identity safety
identity_safety_effect <- function(org_messaging, promotion_equity, current_safety,
                                  organizational_to_identity = 0.6, 
                                  promotion_to_identity = 0.4) {
  # Non-linear relationship for organizational messaging
  messaging_effect <- sigmoid(org_messaging, k = 1.5, x0 = 5, ymin = 0, ymax = 1) * 
                     organizational_to_identity * (org_messaging - current_safety)
  
  # More linear relationship for promotion equity
  promotion_effect <- promotion_to_identity * (promotion_equity - current_safety)
  
  # Synergistic effect when both factors are high
  synergy_multiplier <- synergy(org_messaging, promotion_equity, 
                               synergy_threshold = 6, synergy_strength = 0.3)
  
  # Combined effect with synergy
  total_effect <- (messaging_effect + promotion_effect) * synergy_multiplier
  
  return(total_effect)
}

#' Employee performance nonlinear effect function
#' 
#' @param satisfaction Employee satisfaction value
#' @param feedback Feedback/rewards value
#' @param current_performance Current employee performance value
#' @param identity_safety Identity safety value
#' @param satisfaction_to_performance Parameter weight for satisfaction
#' @param feedback_to_performance Parameter weight for feedback
#' @return Rate of change for employee performance
employee_performance_effect <- function(satisfaction, feedback, current_performance, 
                                      identity_safety,
                                      satisfaction_to_performance = 0.7,
                                      feedback_to_performance = 0.6) {
  # Basic effects
  satisfaction_effect <- satisfaction_to_performance * (satisfaction - current_performance)
  feedback_effect <- feedback_to_performance * (feedback - current_performance)
  
  # Safe environment multiplier - performance benefits more from satisfaction in safe environments
  safety_multiplier <- 0.8 + (identity_safety / 50)  # Ranges from 0.8 to 1.0
  
  # Diminishing returns on feedback at higher levels
  if (feedback > 7) {
    feedback_effect <- feedback_effect * (1 - 0.1 * (feedback - 7) / 3)
  }
  
  # Combined effect
  total_effect <- (satisfaction_effect * safety_multiplier) + feedback_effect
  
  return(total_effect)
}

#' Organizational profit nonlinear effect function
#' 
#' @param performance Employee performance value
#' @param current_profit Current organizational profit value
#' @param program_success Program success value
#' @param performance_to_profit Parameter weight for performance
#' @return Rate of change for organizational profit
organizational_profit_effect <- function(performance, current_profit, program_success,
                                        performance_to_profit = 0.8) {
  # Base effect with diminishing returns at higher performance
  base_effect <- performance_to_profit * 
               (diminishing_returns(performance, scale = 0.3, max_value = performance) - current_profit)
  
  # Program success modifier - successful programs amplify profit benefits
  program_modifier <- 0.8 + (0.2 * sigmoid(program_success, k = 1, x0 = 5))
  
  # Threshold effect - below certain performance, profits decline regardless of other factors
  if (performance < 3) {
    performance_penalty <- -0.2 * (3 - performance)
    base_effect <- base_effect + performance_penalty
  }
  
  # Combined effect
  total_effect <- base_effect * program_modifier
  
  return(total_effect)
}

#' Program success nonlinear effect function
#' 
#' @param implementation Program implementation value
#' @param retention Retention/recruitment value
#' @param current_success Current program success value
#' @param cultural_zeitgeist Cultural zeitgeist value
#' @param implementation_to_success Parameter weight for implementation
#' @param retention_to_success Parameter weight for retention
#' @return Rate of change for program success
program_success_effect <- function(implementation, retention, current_success,
                                 cultural_zeitgeist,
                                 implementation_to_success = 0.8,
                                 retention_to_success = 0.5) {
  # Implementation shows threshold effect - needs to reach critical mass
  implementation_effect <- sigmoid(implementation, k = 1.2, x0 = 4, ymin = 0, ymax = 1) *
                         implementation_to_success * (implementation - current_success)
  
  # Retention has more linear effect
  retention_effect <- retention_to_success * (retention - current_success)
  
  # Cultural impact - hostile cultures make success harder
  cultural_modifier <- 1.0
  if (cultural_zeitgeist < 3.0) {
    cultural_modifier = 0.6 + (cultural_zeitgeist / 7.5)  # Ranges from 0.6 to 1.0
  }
  
  # Combined effect
  total_effect <- (implementation_effect + retention_effect) * cultural_modifier
  
  return(total_effect)
}

#' Apply non-linear functions to system dynamics model
#' 
#' @param model_env The model environment to modify
#' @return Modified model environment with non-linear functions
apply_nonlinear_functions <- function(model_env) {
  # Store the original system_dynamics function
  original_system_dynamics <- model_env$system_dynamics
  
  # Create a new system_dynamics function with non-linear relationships
  model_env$system_dynamics <- function(t, state, parameters) {
    # Call original function to get basic rates of change
    original_result <- original_system_dynamics(t, state, parameters)
    dydt <- original_result[[1]]
    
    # Replace key relationships with non-linear versions
    
    # 1. Identity safety
    dydt["identity_safety"] <- identity_safety_effect(
      state["organizational_messaging"],
      state["promotion_equity"],
      state["identity_safety"],
      parameters$organizational_to_identity,
      parameters$promotion_to_identity
    )
    
    # 2. Employee performance
    dydt["employee_performance"] <- employee_performance_effect(
      state["employee_satisfaction"],
      state["feedback_rewards"],
      state["employee_performance"],
      state["identity_safety"],
      parameters$satisfaction_to_performance,
      parameters$feedback_to_performance
    )
    
    # 3. Organizational profit
    dydt["organizational_profit"] <- organizational_profit_effect(
      state["employee_performance"],
      state["organizational_profit"],
      state["program_success"],
      parameters$performance_to_profit
    )
    
    # 4. Program success
    dydt["program_success"] <- program_success_effect(
      state["program_implementation"],
      state["retention_recruitment"],
      state["program_success"],
      state["cultural_zeitgeist"],
      parameters$implementation_to_success,
      parameters$retention_to_success
    )
    
    return(list(dydt))
  }
  
  # Add the non-linear functions to the model environment
  model_env$sigmoid <- sigmoid
  model_env$threshold <- threshold
  model_env$smooth_threshold <- smooth_threshold
  model_env$diminishing_returns <- diminishing_returns
  model_env$saturation <- saturation
  model_env$synergy <- synergy
  model_env$antagonism <- antagonism
  
  # Also add the specific effect functions
  model_env$identity_safety_effect <- identity_safety_effect
  model_env$employee_performance_effect <- employee_performance_effect
  model_env$organizational_profit_effect <- organizational_profit_effect
  model_env$program_success_effect <- program_success_effect
  
  # Return the modified model
  return(model_env)
}

#' Test non-linear functions with visualization
#' 
#' @param output_dir Directory to save plots (NULL for no saving)
#' @return List of plots
test_nonlinear_functions <- function(output_dir = NULL) {
  # Create test data
  x_values <- seq(0, 10, by = 0.1)
  
  # Create plots
  plots <- list()
  
  # 1. Sigmoid function with different parameters
  sigmoid_data <- data.frame(
    x = rep(x_values, 3),
    y = c(
      sigmoid(x_values, k = 1, x0 = 5),
      sigmoid(x_values, k = 2, x0 = 5),
      sigmoid(x_values, k = 1, x0 = 3)
    ),
    Parameter = rep(c("k=1, x0=5", "k=2, x0=5", "k=1, x0=3"), each = length(x_values))
  )
  
  plots$sigmoid <- ggplot(sigmoid_data, aes(x = x, y = y, color = Parameter)) +
    geom_line(size = 1) +
    labs(title = "Sigmoid Function", x = "Input Value", y = "Output Value") +
    theme_minimal()
  
  # 2. Threshold and Smooth Threshold
  threshold_data <- data.frame(
    x = rep(x_values, 2),
    y = c(
      sapply(x_values, threshold, thresholds = c(3, 7), values = c(0.1, 0.5, 0.9)),
      sapply(x_values, smooth_threshold, thresholds = c(3, 7), values = c(0.1, 0.5, 0.9), smoothness = 0.5)
    ),
    Function = rep(c("Threshold", "Smooth Threshold"), each = length(x_values))
  )
  
  plots$threshold <- ggplot(threshold_data, aes(x = x, y = y, color = Function)) +
    geom_line(size = 1) +
    labs(title = "Threshold Functions", x = "Input Value", y = "Output Value") +
    theme_minimal()
  
  # 3. Diminishing Returns and Saturation
  returns_data <- data.frame(
    x = rep(x_values, 2),
    y = c(
      diminishing_returns(x_values, scale = 0.5, max_value = 1),
      saturation(x_values, half_saturation = 5, hill = 2, max_value = 1)
    ),
    Function = rep(c("Diminishing Returns", "Saturation"), each = length(x_values))
  )
  
  plots$returns <- ggplot(returns_data, aes(x = x, y = y, color = Function)) +
    geom_line(size = 1) +
    labs(title = "Response Functions", x = "Input Value", y = "Output Value") +
    theme_minimal()
  
  # 4. Identity Safety Effect
  # Create grid of values
  organizational_messaging_values <- seq(0, 10, by = 2)
  promotion_equity_values <- seq(0, 10, by = 2)
  
  identity_safety_grid <- expand.grid(
    org_messaging = organizational_messaging_values,
    promotion_equity = promotion_equity_values
  )
  
  identity_safety_grid$effect <- mapply(
    function(org, prom) {
      identity_safety_effect(org, prom, 5, 0.6, 0.4)
    },
    identity_safety_grid$org_messaging,
    identity_safety_grid$promotion_equity
  )
  
  plots$identity_safety <- ggplot(identity_safety_grid, 
                                 aes(x = org_messaging, y = promotion_equity, fill = effect)) +
    geom_tile() +
    scale_fill_viridis_c() +
    labs(title = "Identity Safety Effect", 
         x = "Organizational Messaging", 
         y = "Promotion Equity",
         fill = "Effect") +
    theme_minimal()
  
  # 5. Program Success Effect with Cultural Impact
  cultural_values <- c(1, 3, 5, 7, 9)
  implementation_values <- seq(0, 10, by = 0.5)
  
  program_success_grid <- expand.grid(
    implementation = implementation_values,
    cultural = cultural_values
  )
  
  program_success_grid$effect <- mapply(
    function(impl, cult) {
      program_success_effect(impl, 5, 5, cult, 0.8, 0.5)
    },
    program_success_grid$implementation,
    program_success_grid$cultural
  )
  
  plots$program_success <- ggplot(program_success_grid, 
                                 aes(x = implementation, y = effect, color = factor(cultural))) +
    geom_line(size = 1) +
    labs(title = "Program Success Effect", 
         x = "Implementation Level", 
         y = "Effect on Program Success",
         color = "Cultural Zeitgeist") +
    theme_minimal()
  
  # Save plots if requested
  if (!is.null(output_dir)) {
    # Create directory if it doesn't exist
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
    
    # Save each plot
    for (name in names(plots)) {
      ggsave(
        filename = file.path(output_dir, paste0(name, ".pdf")),
        plot = plots[[name]],
        width = 8,
        height = 6
      )
    }
  }
  
  return(plots)
}

# If this script is run directly, perform a simple test
if (!exists("sourced_nonlinear_relationships") || !sourced_nonlinear_relationships) {
  # Try to load required libraries for testing
  required_libs <- c("ggplot2", "viridis")
  for (lib in required_libs) {
    if (!require(lib, character.only = TRUE, quietly = TRUE)) {
      cat(paste("Warning: Library", lib, "not available. Some tests may not work.\n"))
    }
  }
  
  cat("Testing non-linear functions...\n")
  
  # Test basic functions
  x_test <- seq(0, 10, by = 2)
  cat("Sigmoid function output:", sigmoid(x_test), "\n")
  cat("Threshold function output:", sapply(x_test, threshold), "\n")
  cat("Diminishing returns output:", diminishing_returns(x_test), "\n")
  
  # Test more complex functions
  cat("\nIdentity safety effect examples:\n")
  test_cases <- data.frame(
    org_messaging = c(2, 5, 8, 8),
    promotion_equity = c(2, 5, 5, 8),
    current_safety = c(5, 5, 5, 5)
  )
  
  for (i in 1:nrow(test_cases)) {
    effect <- identity_safety_effect(
      test_cases$org_messaging[i],
      test_cases$promotion_equity[i],
      test_cases$current_safety[i]
    )
    cat(sprintf("Case %d: org_messaging=%.1f, promotion_equity=%.1f, effect=%.3f\n",
                i, test_cases$org_messaging[i], test_cases$promotion_equity[i], effect))
  }
  
  # Create plots if ggplot2 is available
  if (require(ggplot2)) {
    cat("\nCreating test plots...\n")
    plots <- test_nonlinear_functions()
    cat("Created", length(plots), "test plots.\n")
  }
  
  # Flag that we've sourced this file
  sourced_nonlinear_relationships <- TRUE
  
  cat("\nNon-linear relationships module loaded.\n")
  cat("Use apply_nonlinear_functions() to modify a DEI model with non-linear relationships.\n")
}