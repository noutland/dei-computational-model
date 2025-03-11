# DEI Model Integration
# This file implements bidirectional integration between the system dynamics model
# and the network analysis model for the DEI computational framework

#' Get mapped variable name between system model and network model
#' 
#' @param var_name Variable name in one model
#' @param direction "system_to_network" or "network_to_system"
#' @return Mapped variable name in the other model or NULL if no mapping exists
get_mapped_variable <- function(var_name, direction = "system_to_network") {
  # Define mapping between system dynamics variables and network nodes
  system_to_network_map <- list(
    "government_support" = "Government Support",
    "legal_action" = "Legal Action",
    "cultural_zeitgeist" = "Cultural Zeitgeist",
    "funding" = "Funding",
    "leader_support" = "Leader Support",
    "premortem" = "Premortem Planning",
    "program_implementation" = "Program Implementation",
    "organizational_messaging" = "Organizational Messaging",
    "program_success" = "Program Success",
    "retention_recruitment" = "Retention/Recruitment",
    "promotion_equity" = "Promotion Equity",
    "identity_safety" = "Identity Safety",
    "compensation" = "Compensation", 
    "feedback_rewards" = "Feedback/Rewards",
    "employee_motivation" = "Employee Motivation",
    "employee_engagement" = "Employee Engagement",
    "employee_satisfaction" = "Employee Satisfaction",
    "employee_performance" = "Employee Performance",
    "impact_meaning" = "Impact/Meaning",
    "organizational_profit" = "Organizational Profit"
  )
  
  # Create reverse mapping
  network_to_system_map <- setNames(names(system_to_network_map), system_to_network_map)
  
  # Return mapped name based on direction
  if (direction == "system_to_network") {
    return(system_to_network_map[[var_name]])
  } else if (direction == "network_to_system") {
    return(network_to_system_map[[var_name]])
  } else {
    stop("Invalid direction. Use 'system_to_network' or 'network_to_system'")
  }
}

#' Transfer variables from system model to network model
#' 
#' @param system_state Current state of the system dynamics model
#' @param network_model Network model to update
#' @param variables Variables to transfer (NULL for all mappable variables)
#' @return Updated network model
transfer_system_to_network <- function(system_state, network_model, variables = NULL) {
  # If no specific variables provided, transfer all mappable variables
  if (is.null(variables)) {
    variables <- names(system_state)
  }
  
  # Initialize values to update
  values_to_update <- list()
  
  # For each variable, find mapping and add to update list
  for (var_name in variables) {
    # Skip time if present
    if (var_name == "time") next
    
    # Get corresponding network node name
    network_node <- get_mapped_variable(var_name, "system_to_network")
    
    # If mapping exists, add to update list
    if (!is.null(network_node)) {
      values_to_update[[network_node]] <- system_state[[var_name]]
    }
  }
  
  # Update network model if we have values to update
  if (length(values_to_update) > 0) {
    network_model$set_node_values(values_to_update)
  }
  
  return(network_model)
}

#' Transfer variables from network model to system model
#' 
#' @param network_model Network model
#' @param system_state Current state of the system dynamics model
#' @param variables Network node names to transfer (NULL for all mappable nodes)
#' @param weight Weight given to network values (0-1, higher means more network influence)
#' @return Updated system state
transfer_network_to_system <- function(network_model, system_state, variables = NULL, weight = 0.3) {
  # Get current node values from network
  network_state <- sapply(V(network_model$G)$name, function(node) {
    V(network_model$G)[node]$value
  })
  
  # If no specific variables provided, transfer all mappable variables
  if (is.null(variables)) {
    variables <- names(network_state)
  }
  
  # For each variable, find mapping and update system state
  for (node_name in variables) {
    # Get corresponding system variable name
    system_var <- get_mapped_variable(node_name, "network_to_system")
    
    # If mapping exists and variable exists in system state, update it
    if (!is.null(system_var) && system_var %in% names(system_state)) {
      # Blend network value with system value using weight
      system_state[[system_var]] <- (1 - weight) * system_state[[system_var]] + 
                                   weight * network_state[node_name]
    }
  }
  
  return(system_state)
}

#' Run a single step of the integrated model
#'
#' @param system_model System dynamics model
#' @param network_model Network model
#' @param current_state Current system state
#' @param t Current time step
#' @param network_iterations Number of network propagation iterations per system step
#' @param network_weight Weight given to network values during integration
#' @return Updated system state after one step
run_integrated_step <- function(system_model, network_model, current_state, t, 
                              network_iterations = 1, network_weight = 0.3) {
  # 1. Run system dynamics step
  next_state <- system_model$step_simulation(current_state, t)
  
  # 2. Transfer system state to network model
  network_model <- transfer_system_to_network(next_state, network_model)
  
  # 3. Run network propagation
  network_model$propagate_effects(iterations = network_iterations)
  
  # 4. Transfer network insights back to system state
  integrated_state <- transfer_network_to_system(network_model, next_state, 
                                              weight = network_weight)
  
  return(integrated_state)
}

#' Run full integrated simulation
#'
#' @param system_model System dynamics model
#' @param network_model Network model
#' @param t_steps Number of time steps to simulate
#' @param network_iterations Network iterations per system step
#' @param network_weight Weight of network influence on system
#' @param record_network Whether to record network state history
#' @return Results of integrated simulation
run_integrated_simulation <- function(system_model, network_model, t_steps = 60,
                                    network_iterations = 1, network_weight = 0.3,
                                    record_network = FALSE) {
  # Get initial system state
  system_state <- system_model$initial_values
  
  # Set up results storage
  results <- list()
  results$system_history <- matrix(0, nrow = t_steps + 1, ncol = length(system_state) + 1)
  colnames(results$system_history) <- c(names(system_state), "time")
  
  # Store initial state
  results$system_history[1, ] <- c(unlist(system_state), 0)
  
  # If recording network, initialize network history
  if (record_network) {
    network_nodes <- V(network_model$G)$name
    results$network_history <- list()
    for (node in network_nodes) {
      results$network_history[[node]] <- numeric(t_steps + 1)
      results$network_history[[node]][1] <- V(network_model$G)[node]$value
    }
  }
  
  # Run simulation steps
  for (t in 1:t_steps) {
    # Run integrated step
    system_state <- run_integrated_step(
      system_model, 
      network_model, 
      system_state, 
      t,
      network_iterations = network_iterations,
      network_weight = network_weight
    )
    
    # Store results
    results$system_history[t+1, ] <- c(unlist(system_state), t)
    
    # If recording network, store network state
    if (record_network) {
      for (node in network_nodes) {
        results$network_history[[node]][t+1] <- V(network_model$G)[node]$value
      }
    }
  }
  
  # Convert system history to data frame
  results$system_history <- as.data.frame(results$system_history)
  
  return(results)
}

#' Add step_simulation method to system model if it doesn't exist
#'
#' @param system_model System dynamics model
#' @return System model with step_simulation method
ensure_step_simulation <- function(system_model) {
  # Check if step_simulation method exists
  if (!exists("step_simulation", envir = system_model)) {
    # Add step_simulation method that runs a single step of simulation
    system_model$step_simulation <- function(state, t) {
      # Convert state list to vector for ODE solver
      state_vector <- unlist(state)
      
      # Set up time points for just one step
      t_span <- c(t, t + 1)
      
      # Solve for one step
      solution <- ode(
        y = state_vector,
        times = t_span,
        func = system_model$system_dynamics,
        parms = system_model$params,
        method = "lsoda"
      )
      
      # Convert result to list
      result_list <- as.list(solution[2, -1])  # Skip time column, take last row
      
      return(result_list)
    }
  }
  
  return(system_model)
}

#' Prepare system model for integration
#'
#' @param system_model System dynamics model
#' @return Prepared system model
prepare_system_model <- function(system_model) {
  # Ensure step_simulation method exists
  system_model <- ensure_step_simulation(system_model)
  
  # Add method to get variable mapping
  system_model$get_mapped_variable <- get_mapped_variable
  
  return(system_model)
}

#' Prepare network model for integration
#'
#' @param network_model Network model
#' @return Prepared network model
prepare_network_model <- function(network_model) {
  # Add method to get variable mapping
  network_model$get_mapped_variable <- get_mapped_variable
  
  # Add method to get node values if it doesn't exist
  if (!exists("get_node_values", envir = network_model)) {
    network_model$get_node_values <- function() {
      values <- setNames(
        sapply(V(network_model$G)$name, function(node) {
          V(network_model$G)[node]$value
        }),
        V(network_model$G)$name
      )
      return(values)
    }
  }
  
  return(network_model)
}

#' Create integrated DEI model from system and network models
#'
#' @param system_model System dynamics model
#' @param network_model Network model
#' @param prepare_models Whether to prepare the models for integration
#' @return Integrated model environment
create_integrated_model <- function(system_model, network_model, prepare_models = TRUE) {
  # Initialize integrated model environment
  integrated_model <- new.env()
  
  # Prepare component models if requested
  if (prepare_models) {
    system_model <- prepare_system_model(system_model)
    network_model <- prepare_network_model(network_model)
  }
  
  # Store component models
  integrated_model$system_model <- system_model
  integrated_model$network_model <- network_model
  
  # Add integrated simulation methods
  integrated_model$run_integrated_step <- run_integrated_step
  integrated_model$run_integrated_simulation <- function(t_steps = 60, 
                                                      network_iterations = 1,
                                                      network_weight = 0.3,
                                                      record_network = FALSE) {
    return(run_integrated_simulation(
      system_model = integrated_model$system_model,
      network_model = integrated_model$network_model,
      t_steps = t_steps,
      network_iterations = network_iterations,
      network_weight = network_weight,
      record_network = record_network
    ))
  }
  
  # Add method to run scenarios
  integrated_model$run_scenario <- function(scenario_name, param_changes = NULL, initial_changes = NULL,
                                         t_steps = 60, network_iterations = 1, network_weight = 0.3) {
    # Store original system model parameters and initial values
    original_params <- integrated_model$system_model$params
    original_initial <- integrated_model$system_model$initial_values
    
    # Apply parameter changes if provided
    if (!is.null(param_changes)) {
      for (param_name in names(param_changes)) {
        if (param_name %in% names(integrated_model$system_model$params)) {
          integrated_model$system_model$params[[param_name]] <- param_changes[[param_name]]
        } else {
          warning(paste("Parameter", param_name, "not found in model"))
        }
      }
    }
    
    # Apply initial value changes if provided
    if (!is.null(initial_changes)) {
      for (var_name in names(initial_changes)) {
        if (var_name %in% names(integrated_model$system_model$initial_values)) {
          integrated_model$system_model$initial_values[[var_name]] <- initial_changes[[var_name]]
          
          # Also update corresponding network node if mapping exists
          network_node <- get_mapped_variable(var_name, "system_to_network")
          if (!is.null(network_node)) {
            integrated_model$network_model$set_node_values(
              setNames(list(initial_changes[[var_name]]), network_node)
            )
          }
        } else {
          warning(paste("Variable", var_name, "not found in model"))
        }
      }
    }
    
    # Run integrated simulation
    results <- integrated_model$run_integrated_simulation(
      t_steps = t_steps,
      network_iterations = network_iterations,
      network_weight = network_weight,
      record_network = TRUE
    )
    
    # Restore original parameters and initial values
    integrated_model$system_model$params <- original_params
    integrated_model$system_model$initial_values <- original_initial
    
    # Reset network model to default values
    for (node in V(integrated_model$network_model$G)$name) {
      V(integrated_model$network_model$G)[node]$value <- 5.0  # Default value
    }
    
    # Add scenario name to results
    results$scenario_name <- scenario_name
    
    return(results)
  }
  
  # Add method to visualize results
  integrated_model$plot_results <- function(results, variables = NULL, 
                                          ncol = 2, plot_network = FALSE) {
    # If no variables specified, use a default set
    if (is.null(variables)) {
      variables <- c("program_success", "organizational_profit", 
                    "identity_safety", "employee_performance")
    }
    
    # Prepare plotting data
    plot_data <- results$system_history
    
    # Create list for plots
    plot_list <- list()
    
    # Create plots for each variable
    for (var in variables) {
      if (var %in% names(plot_data)) {
        p <- ggplot(plot_data, aes_string(x = "time", y = var)) +
          geom_line(color = "blue", size = 1) +
          labs(title = paste("Evolution of", var), x = "Time (months)", y = var) +
          theme_minimal() +
          ylim(0, 10)
        
        plot_list[[var]] <- p
      }
    }
    
    # If plotting network and network history exists
    if (plot_network && !is.null(results$network_history)) {
      # Convert network history to data frame
      network_df <- data.frame(
        time = 0:length(results$network_history[[1]]-1)
      )
      for (node in names(results$network_history)) {
        network_df[[node]] <- results$network_history[[node]]
      }
      
      # Create network evolution plot
      network_data_long <- reshape2::melt(network_df, id.vars = "time",
                                         variable.name = "node", value.name = "value")
      
      # Map network nodes to groups for coloring
      node_categories <- list(
        'External' = c('Government Support', 'Legal Action', 'Cultural Zeitgeist'),
        'Program' = c('Funding', 'Leader Support', 'Premortem Planning', 
                    'Program Implementation', 'Organizational Messaging', 'Program Success'),
        'HR' = c('Retention/Recruitment', 'Promotion Equity', 'Identity Safety', 
               'Compensation', 'Feedback/Rewards'),
        'Outcomes' = c('Employee Motivation', 'Employee Engagement', 'Employee Satisfaction', 
                     'Employee Performance', 'Impact/Meaning', 'Organizational Profit')
      )
      
      # Add category column to data
      network_data_long$category <- NA
      for (cat_name in names(node_categories)) {
        network_data_long$category[network_data_long$node %in% node_categories[[cat_name]]] <- cat_name
      }
      
      # Create plots for each category
      for (cat in unique(network_data_long$category)) {
        if (!is.na(cat)) {
          cat_data <- network_data_long[network_data_long$category == cat, ]
          
          p <- ggplot(cat_data, aes(x = time, y = value, color = node)) +
            geom_line(size = 1) +
            labs(title = paste(cat, "Factors"), x = "Time (months)", y = "Value") +
            theme_minimal() +
            ylim(0, 10)
          
          plot_list[[paste("network", cat)]] <- p
        }
      }
    }
    
    # Use gridExtra to arrange plots
    if (length(plot_list) > 0) {
      grid_arrangement <- do.call(gridExtra::grid.arrange, c(plot_list, ncol = ncol))
      return(grid_arrangement)
    } else {
      return(NULL)
    }
  }
  
  return(integrated_model)
}

# If this script is run directly, perform a simple test
if (!exists("sourced_model_integration") || !sourced_model_integration) {
  # Flag that we've sourced this file
  sourced_model_integration <- TRUE
  
  cat("Model integration module loaded.\n")
  cat("Use create_integrated_model() to create an integrated DEI model.\n")
}