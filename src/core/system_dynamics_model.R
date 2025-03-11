# DEI System Dynamics Model in R
# This implements a computational model of DEI (Diversity, Equity, Inclusion) 
# system dynamics based on the causal loop diagram.

# Load required libraries
library(deSolve)  # For solving differential equations
library(ggplot2)  # For visualization
library(reshape2) # For data reshaping
library(dplyr)    # For data manipulation

# Create DEI System Model
dei_system_model <- function() {
  # Initialize model environment
  model_env <- new.env()
  
  # Define initial values for all state variables (0-10 scale)
  model_env$initial_values <- list(
    government_support = 5.0,
    legal_action = 3.0,
    cultural_zeitgeist = 5.0,
    funding = 5.0,
    leader_support = 5.0,
    premortem = 3.0,
    program_implementation = 4.0,
    organizational_messaging = 5.0,
    program_success = 3.0,
    retention_recruitment = 4.0,
    promotion_equity = 3.0,
    identity_safety = 4.0,
    compensation = 5.0,
    feedback_rewards = 5.0,
    employee_motivation = 4.0,
    employee_engagement = 4.0,
    employee_satisfaction = 4.0,
    employee_performance = 5.0,
    impact_meaning = 4.0,
    organizational_profit = 6.0
  )
  
  # Define parameter dictionary for influence weights (connection strengths)
  model_env$params <- list(
    # External influences
    govt_to_cultural = 0.3,
    legal_to_cultural = 0.3,
    cultural_to_leader = 0.5,
    cultural_to_organizational = -0.2,  # Note: negative influence based on diagram
    
    # Program establishment
    funding_to_premortem = 0.4,
    leader_to_funding = 0.6,
    leader_to_organizational = 0.7,
    leader_to_promotion = 0.4,
    premortem_to_implementation = 0.7,
    implementation_to_success = 0.8,
    
    # Employee experience influences
    organizational_to_retention = 0.5,
    organizational_to_identity = 0.6,
    promotion_to_identity = 0.4,
    identity_to_retention = 0.6,
    
    # Performance drivers
    compensation_to_satisfaction = 0.5,
    feedback_to_performance = 0.6,
    promotion_to_motivation = 0.5,
    impact_to_motivation = 0.7,
    
    # Employee state interactions
    motivation_to_engagement = 0.7,
    engagement_to_satisfaction = 0.6,
    satisfaction_to_performance = 0.7,
    performance_to_profit = 0.8,
    
    # Reinforcing loops
    retention_to_success = 0.5,
    success_to_leader = 0.4,
    profit_to_leader = 0.3,
    profit_to_retention = 0.4,
    success_to_funding = 0.5
  )
  
  # Time parameters
  model_env$t_span <- c(0, 60)  # Simulation time span (e.g., 60 months)
  model_env$t_eval <- seq(0, 60, by = 1)  # Points to evaluate
  
  # Define the system of differential equations
  model_env$system_dynamics <- function(t, state, parameters) {
    with(as.list(c(state, parameters)), {
      # Initialize rates of change vector
      dydt <- numeric(length(state))
      names(dydt) <- names(state)
      
      # Cultural zeitgeist influenced by government support and legal action
      dydt["cultural_zeitgeist"] <- govt_to_cultural * (government_support - cultural_zeitgeist) +
                                    legal_to_cultural * (legal_action - cultural_zeitgeist)
      
      # Leader support influenced by cultural zeitgeist, program success, and organizational profit
      dydt["leader_support"] <- cultural_to_leader * (cultural_zeitgeist - leader_support) +
                               success_to_leader * (program_success - leader_support) +
                               profit_to_leader * (organizational_profit - leader_support)
      
      # Funding influenced by leader support and program success
      dydt["funding"] <- leader_to_funding * (leader_support - funding) +
                        success_to_funding * (program_success - funding)
      
      # Premortem planning influenced by funding
      dydt["premortem"] <- funding_to_premortem * (funding - premortem)
      
      # Program implementation influenced by premortem planning
      dydt["program_implementation"] <- premortem_to_implementation * (premortem - program_implementation)
      
      # Organizational messaging influenced by leader support and cultural zeitgeist
      dydt["organizational_messaging"] <- leader_to_organizational * (leader_support - organizational_messaging) +
                                         cultural_to_organizational * (cultural_zeitgeist - organizational_messaging)
      
      # Program success influenced by implementation and retention/recruitment
      dydt["program_success"] <- implementation_to_success * (program_implementation - program_success) +
                                retention_to_success * (retention_recruitment - program_success)
      
      # Retention/Recruitment influenced by organizational messaging, identity safety, and profit
      dydt["retention_recruitment"] <- organizational_to_retention * (organizational_messaging - retention_recruitment) +
                                      identity_to_retention * (identity_safety - retention_recruitment) +
                                      profit_to_retention * (organizational_profit - retention_recruitment)
      
      # Promotion equity influenced by leader support
      dydt["promotion_equity"] <- leader_to_promotion * (leader_support - promotion_equity)
      
      # Identity safety influenced by organizational messaging and promotion equity
      dydt["identity_safety"] <- organizational_to_identity * (organizational_messaging - identity_safety) +
                               promotion_to_identity * (promotion_equity - identity_safety)
      
      # Employee motivation influenced by promotion equity and impact/meaning
      dydt["employee_motivation"] <- promotion_to_motivation * (promotion_equity - employee_motivation) +
                                   impact_to_motivation * (impact_meaning - employee_motivation)
      
      # Employee engagement influenced by motivation
      dydt["employee_engagement"] <- motivation_to_engagement * (employee_motivation - employee_engagement)
      
      # Employee satisfaction influenced by compensation and engagement
      dydt["employee_satisfaction"] <- compensation_to_satisfaction * (compensation - employee_satisfaction) +
                                     engagement_to_satisfaction * (employee_engagement - employee_satisfaction)
      
      # Employee performance influenced by satisfaction and feedback
      dydt["employee_performance"] <- satisfaction_to_performance * (employee_satisfaction - employee_performance) +
                                    feedback_to_performance * (feedback_rewards - employee_performance)
      
      # Organizational profit influenced by employee performance
      dydt["organizational_profit"] <- performance_to_profit * (employee_performance - organizational_profit)
      
      # Compensation, Feedback, and Impact/Meaning are treated as constants in this model
      dydt["compensation"] <- 0
      dydt["feedback_rewards"] <- 0
      dydt["impact_meaning"] <- 0
      
      # Government support and legal action are treated as external inputs
      dydt["government_support"] <- 0
      dydt["legal_action"] <- 0
      
      # FIX: Add negative feedback loops for hostile cultural environments
      if (cultural_zeitgeist < 3.0) {
        # Apply dampening mechanics to unrealistic positive outcomes
        dampening_factor_program <- 0.7 + (cultural_zeitgeist / 10)
        dampening_factor_retention <- 0.8 + (cultural_zeitgeist / 15)
        dampening_factor_identity <- 0.6 + (cultural_zeitgeist / 12)
        
        # Dampen the rates of change instead of the absolute values
        dydt["program_success"] <- dydt["program_success"] * dampening_factor_program
        dydt["retention_recruitment"] <- dydt["retention_recruitment"] * dampening_factor_retention
        dydt["identity_safety"] <- dydt["identity_safety"] * dampening_factor_identity
        
        # Add additional negative pressure based on the severity of the hostile environment
        hostility_factor <- (3.0 - cultural_zeitgeist) / 3.0  # 0 to 1 scale, with 1 being most hostile
        
        # Apply additional negative pressure to key metrics
        dydt["program_success"] <- dydt["program_success"] - (0.1 * hostility_factor * program_success)
        dydt["retention_recruitment"] <- dydt["retention_recruitment"] - (0.15 * hostility_factor * retention_recruitment)
        dydt["identity_safety"] <- dydt["identity_safety"] - (0.2 * hostility_factor * identity_safety)
      }
      
      # FIX: Add time-decay dynamics for hostile environment effects
      if (t > 24 && cultural_zeitgeist < 3.0) {
        # Benefits diminish over time (after 24 months)
        decay_factor <- 1 - ((t - 24) / 60)  # Linear decay over 5 years
        decay_factor <- max(0.4, decay_factor)  # Floor at 40% effectiveness
        
        # Apply decay to relevant outcomes by further reducing their rate of change
        dydt["program_success"] <- dydt["program_success"] * decay_factor
        dydt["retention_recruitment"] <- dydt["retention_recruitment"] * decay_factor
        dydt["identity_safety"] <- dydt["identity_safety"] * decay_factor
        
        # Also decay the engagement and satisfaction in hostile environments over time
        dydt["employee_engagement"] <- dydt["employee_engagement"] * decay_factor
        dydt["employee_satisfaction"] <- dydt["employee_satisfaction"] * decay_factor
      }
      
      return(list(dydt))
    })
  }
  
  # Run the simulation
  model_env$run_simulation <- function() {
    # Create initial state vector from list
    initial_state <- unlist(model_env$initial_values)
    
    # Solve the system of differential equations
    solution <- ode(
      y = initial_state,
      times = model_env$t_eval,
      func = model_env$system_dynamics,
      parms = model_env$params,
      method = "lsoda"
    )
    
    # Convert to data frame for easier plotting
    solution_df <- as.data.frame(solution)
    
    return(solution_df)
  }
  
  # Plot the simulation results
  model_env$plot_results <- function(solution_df) {
    # Convert to long format for ggplot
    solution_long <- reshape2::melt(solution_df, id.vars = "time", 
                                    variable.name = "Variable", value.name = "Value")
    
    # Create variable groups for plotting
    program_vars <- c("funding", "leader_support", "premortem", 
                      "program_implementation", "program_success")
    
    env_vars <- c("government_support", "legal_action", "cultural_zeitgeist", 
                  "organizational_messaging")
    
    employee_exp_vars <- c("retention_recruitment", "promotion_equity", "identity_safety", 
                          "compensation", "feedback_rewards")
    
    outcome_vars <- c("employee_motivation", "employee_engagement", "employee_satisfaction", 
                     "employee_performance", "impact_meaning", "organizational_profit")
    
    # Function to create a plot for a group of variables
    create_group_plot <- function(data, vars, title) {
      data_subset <- data %>% filter(Variable %in% vars)
      
      ggplot(data_subset, aes(x = time, y = Value, color = Variable)) +
        geom_line(linewidth = 1) +
        labs(title = title, x = "Time (months)", y = "Value") +
        theme_minimal() +
        theme(legend.position = "right") +
        ylim(0, 10)
    }
    
    # Create four plots
    p1 <- create_group_plot(solution_long, program_vars, "DEI Program Establishment")
    p2 <- create_group_plot(solution_long, env_vars, "Environmental Factors")
    p3 <- create_group_plot(solution_long, employee_exp_vars, "Employee Experience")
    p4 <- create_group_plot(solution_long, outcome_vars, "Organizational Outcomes")
    
    # Create a list to hold plots
    plots <- list(p1, p2, p3, p4)
    
    # Return plots
    return(plots)
  }
  
  # Run specific scenarios by modifying parameters or initial values
  model_env$run_scenario <- function(scenario_name, param_changes = NULL, initial_changes = NULL) {
    # Store original values
    original_params <- model_env$params
    original_initial <- model_env$initial_values
    
    # Apply parameter changes if provided
    if (!is.null(param_changes)) {
      for (param_name in names(param_changes)) {
        if (param_name %in% names(model_env$params)) {
          model_env$params[[param_name]] <- param_changes[[param_name]]
        } else {
          warning(paste("Parameter", param_name, "not found in model"))
        }
      }
    }
    
    # Apply initial value changes if provided
    if (!is.null(initial_changes)) {
      for (var_name in names(initial_changes)) {
        if (var_name %in% names(model_env$initial_values)) {
          model_env$initial_values[[var_name]] <- initial_changes[[var_name]]
        } else {
          warning(paste("Variable", var_name, "not found in model"))
        }
      }
    }
    
    # Run the simulation
    solution <- model_env$run_simulation()
    
    # Restore original parameters and initial values
    model_env$params <- original_params
    model_env$initial_values <- original_initial
    
    return(solution)
  }
  
  # Return the model environment
  return(model_env)
}

# Example usage
if (FALSE) {  # Set to TRUE to run the examples
  # Create the model
  model <- dei_system_model()
  
  # Run the basic simulation
  solution <- model$run_simulation()
  
  # Generate plots
  plots <- model$plot_results(solution)
  
  # Display plots
  library(gridExtra)
  do.call(grid.arrange, c(plots, ncol = 2))
  
  # Run scenario with increased leader support
  high_leader_solution <- model$run_scenario(
    "High Leader Support",
    initial_changes = list(leader_support = 8.0)
  )
  
  # Plot the high leader support scenario
  high_leader_plots <- model$plot_results(high_leader_solution)
  do.call(grid.arrange, c(high_leader_plots, ncol = 2))
  
  # Run scenario with decreased cultural support
  low_culture_solution <- model$run_scenario(
    "Low Cultural Support",
    initial_changes = list(cultural_zeitgeist = 2.0)
  )
  
  # Plot the low cultural support scenario
  low_culture_plots <- model$plot_results(low_culture_solution)
  do.call(grid.arrange, c(low_culture_plots, ncol = 2))
}