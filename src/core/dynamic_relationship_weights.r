# Dynamic Relationship Weights for DEI Computational Model
# This file implements conditional relationship strength functionality
# that allows connection weights to vary based on the current system state

#' Calculate dynamic relationship weights based on system state
#' 
#' @param base_weight The base (static) weight of the relationship
#' @param source_var Source variable name
#' @param target_var Target variable name
#' @param system_state Current system state as a named vector
#' @param t Current time step
#' @return Modified weight value
calculate_dynamic_weight <- function(base_weight, source_var, target_var, system_state, t = NULL) {
  # Start with the base weight
  weight_modifier <- 1.0
  
  #------------------------------------------------------------
  # LEADERSHIP EFFECTS
  #------------------------------------------------------------
  
  # Leadership influence on organizational messaging is stronger in hostile cultures
  if (source_var == "leader_support" && target_var == "organizational_messaging") {
    # More leadership influence is needed in hostile cultures
    if ("cultural_zeitgeist" %in% names(system_state)) {
      cultural_modifier <- max(0.5, 1.5 - (system_state["cultural_zeitgeist"] / 10))
      weight_modifier <- weight_modifier * cultural_modifier
    }
  }
  
  # Leadership influence on implementation has greater impact with adequate funding
  if (source_var == "leader_support" && target_var == "program_implementation") {
    if ("funding" %in% names(system_state)) {
      funding_threshold <- 0.6 + (0.4 * min(system_state["funding"] / 10, 1))
      weight_modifier <- weight_modifier * funding_threshold
    }
  }
  
  #------------------------------------------------------------
  # IMPLEMENTATION EFFECTS
  #------------------------------------------------------------
  
  # Implementation effectiveness conditioned on premortem planning quality
  if (source_var == "program_implementation" && target_var == "program_success") {
    if ("premortem" %in% names(system_state)) {
      # Premortem planning improves implementation effectiveness
      planning_modifier <- 0.7 + (0.3 * min(system_state["premortem"] / 10, 1))
      weight_modifier <- weight_modifier * planning_modifier
    }
  }
  
  #------------------------------------------------------------
  # EMPLOYEE EXPERIENCE EFFECTS
  #------------------------------------------------------------
  
  # Employee motivation to engagement connection affected by identity safety
  if (source_var == "employee_motivation" && target_var == "employee_engagement") {
    if ("identity_safety" %in% names(system_state)) {
      # Motivation translates to engagement better in safe environments
      safety_modifier <- 0.6 + (0.4 * min(system_state["identity_safety"] / 10, 1))
      weight_modifier <- weight_modifier * safety_modifier
    }
  }
  
  # Employee satisfaction to performance affected by compensation
  if (source_var == "employee_satisfaction" && target_var == "employee_performance") {
    if ("compensation" %in% names(system_state)) {
      # Diminishing returns from compensation
      compensation_level <- min(system_state["compensation"] / 10, 1)
      compensation_modifier <- 0.8 + (0.2 * log1p(compensation_level) / log1p(1))
      weight_modifier <- weight_modifier * compensation_modifier
    }
  }
  
  #------------------------------------------------------------
  # TIME-DEPENDENT EFFECTS
  #------------------------------------------------------------
  
  # Add time-dependent modifiers if time is provided
  if (!is.null(t)) {
    # Implementation-success relationship grows stronger over time
    if (source_var == "program_implementation" && target_var == "program_success") {
      # Maturity effect: programs become more effective as they mature
      maturity_factor <- min(1 + (0.3 * (t / 60)), 1.3)
      weight_modifier <- weight_modifier * maturity_factor
    }
    
    # Cultural influences fade slowly if opposed by leadership
    if (source_var == "cultural_zeitgeist" && 
        (target_var == "organizational_messaging" || target_var == "identity_safety")) {
      if ("leader_support" %in% names(system_state) && 
          system_state["leader_support"] > 7.0 && 
          system_state["cultural_zeitgeist"] < 3.0) {
        
        # Leadership counteracts negative cultural influence over time
        resistance_factor <- max(1 - (0.3 * (t / 60)), 0.7)
        weight_modifier <- weight_modifier * resistance_factor
      }
    }
  }
  
  # Return the modified weight
  return(base_weight * weight_modifier)
}

#' Apply dynamic weights to the system dynamics model
#' 
#' @param model_env The model environment to modify
#' @return Modified model environment with dynamic weights
apply_dynamic_weights_to_system_model <- function(model_env) {
  # Store the original system_dynamics function
  original_system_dynamics <- model_env$system_dynamics
  
  # Create a new system_dynamics function with dynamic weights
  model_env$system_dynamics <- function(t, state, parameters) {
    # Create a copied parameters list that we'll modify with dynamic weights
    dynamic_params <- parameters
    
    # Define relationships to make dynamic (source-target pairs)
    dynamic_relationships <- list(
      list(source = "leader_support", target = "organizational_messaging"),
      list(source = "leader_support", target = "program_implementation"),
      list(source = "program_implementation", target = "program_success"),
      list(source = "employee_motivation", target = "employee_engagement"),
      list(source = "employee_satisfaction", target = "employee_performance"),
      list(source = "cultural_zeitgeist", target = "organizational_messaging"),
      list(source = "cultural_zeitgeist", target = "identity_safety")
    )
    
    # Map relationship to parameter names
    relationship_param_map <- list(
      "leader_support_organizational_messaging" = "leader_to_organizational",
      "leader_support_program_implementation" = "leader_to_implementation",
      "program_implementation_program_success" = "implementation_to_success",
      "employee_motivation_employee_engagement" = "motivation_to_engagement",
      "employee_satisfaction_employee_performance" = "satisfaction_to_performance",
      "cultural_zeitgeist_organizational_messaging" = "cultural_to_organizational",
      "cultural_zeitgeist_identity_safety" = "cultural_to_identity"
    )
    
    # Update parameter values with dynamic weights
    for (rel in dynamic_relationships) {
      # Create a key for this relationship
      rel_key <- paste(rel$source, rel$target, sep = "_")
      
      # Get the parameter name for this relationship
      param_name <- relationship_param_map[[rel_key]]
      
      # If we have a mapping for this relationship
      if (!is.null(param_name) && param_name %in% names(parameters)) {
        # Calculate the dynamic weight
        base_weight <- parameters[[param_name]]
        dynamic_weight <- calculate_dynamic_weight(
          base_weight, 
          rel$source, 
          rel$target, 
          state, 
          t
        )
        
        # Update the parameter
        dynamic_params[[param_name]] <- dynamic_weight
      }
    }
    
    # Call the original function with modified parameters
    return(original_system_dynamics(t, state, dynamic_params))
  }
  
  # Add the dynamic weight calculation function to the model environment
  model_env$calculate_dynamic_weight <- calculate_dynamic_weight
  
  # Return the modified model
  return(model_env)
}

#' Apply dynamic weights to the network model
#' 
#' @param network_model The network model to modify
#' @return Modified network model with dynamic weights
apply_dynamic_weights_to_network_model <- function(network_model) {
  # Store the original propagate_effects function
  original_propagate_effects <- network_model$propagate_effects
  
  # Create a new propagate_effects function with dynamic weights
  network_model$propagate_effects <- function(iterations = 5) {
    # Initialize history to track changes
    history <- list()
    for (node in V(network_model$G)$name) {
      history[[node]] <- c(V(network_model$G)[node]$value)
    }
    
    # Propagate effects for specified iterations
    for (iter in 1:iterations) {
      # Create a copy of current values
      current_values <- setNames(
        sapply(V(network_model$G)$name, function(node) {
          V(network_model$G)[node]$value
        }),
        V(network_model$G)$name
      )
      
      # Update each node based on incoming edges with dynamic weights
      for (node in V(network_model$G)$name) {
        # Get all predecessors
        predecessors <- neighbors(network_model$G, node, mode = "in")
        
        if (length(predecessors) > 0) {
          # Calculate influence from each predecessor
          delta <- 0
          
          for (pred in predecessors$name) {
            edge_id <- get_edge_ids(network_model$G, c(pred, node))
            base_edge_weight <- E(network_model$G)[edge_id]$weight
            edge_sign <- E(network_model$G)[edge_id]$sign
            
            # Calculate dynamic weight
            dynamic_weight <- calculate_dynamic_weight(
              base_edge_weight,
              pred,
              node,
              current_values,
              iter
            )
            
            influence <- dynamic_weight * edge_sign * (current_values[pred] - current_values[node])
            delta <- delta + influence
          }
          
          # Apply bounded update
          new_value <- current_values[node] + (0.1 * delta)  # Dampen effect for stability
          V(network_model$G)[node]$value <- max(0, min(10, new_value))
        }
        
        # Record in history
        history[[node]] <- c(history[[node]], V(network_model$G)[node]$value)
      }
    }
    
    return(history)
  }
  
  # Add the dynamic weight calculation function to the network model
  network_model$calculate_dynamic_weight <- calculate_dynamic_weight
  
  # Return the modified model
  return(network_model)
}

#' Run a test of dynamic weights to verify their impact
#' 
#' @param system_model The system dynamics model
#' @param verbose Whether to print detailed output
#' @return List containing test results
test_dynamic_weights <- function(system_model, verbose = TRUE) {
  if (verbose) cat("Testing dynamic weight calculations...\n")
  
  # Create a test state
  test_state <- list(
    cultural_zeitgeist = 2.0,  # Hostile culture
    leader_support = 8.0,      # Strong leadership
    funding = 7.0,             # Good funding
    premortem = 6.0,           # Good planning
    identity_safety = 3.0,     # Moderate safety
    compensation = 6.0         # Good compensation
  )
  
  # Test relationships
  test_relationships <- list(
    list(source = "leader_support", target = "organizational_messaging", base_weight = 0.7),
    list(source = "program_implementation", target = "program_success", base_weight = 0.8),
    list(source = "employee_motivation", target = "employee_engagement", base_weight = 0.7)
  )
  
  # Calculate weights at different time points
  time_points <- c(0, 12, 24, 36, 48, 60)
  results <- data.frame()
  
  for (rel in test_relationships) {
    for (t in time_points) {
      dynamic_weight <- calculate_dynamic_weight(
        rel$base_weight,
        rel$source,
        rel$target,
        test_state,
        t
      )
      
      if (verbose) {
        cat(sprintf("Relationship: %s -> %s, Time: %d, Base weight: %.2f, Dynamic weight: %.2f\n",
                    rel$source, rel$target, t, rel$base_weight, dynamic_weight))
      }
      
      results <- rbind(results, data.frame(
        source = rel$source,
        target = rel$target,
        time = t,
        base_weight = rel$base_weight,
        dynamic_weight = dynamic_weight,
        change_pct = (dynamic_weight / rel$base_weight - 1) * 100
      ))
    }
  }
  
  return(list(
    results = results,
    test_state = test_state
  ))
}

# If this script is run directly, perform a simple test
if (!exists("sourced_dynamic_weights") || !sourced_dynamic_weights) {
  cat("Testing dynamic relationship weights...\n")
  
  # Create a simple stub for the system model
  stub_model <- list(
    system_dynamics = function(t, state, parameters) {
      return(list(rep(0, length(state))))
    }
  )
  
  # Run test
  test_results <- test_dynamic_weights(stub_model)
  
  # Print test summary
  cat("\nDynamic weight test summary:\n")
  cat(sprintf("Average weight change: %.1f%%\n", 
              mean(abs(test_results$results$change_pct))))
  cat(sprintf("Maximum weight change: %.1f%%\n", 
              max(abs(test_results$results$change_pct))))
  
  # Flag that we've sourced this file
  sourced_dynamic_weights <- TRUE
}