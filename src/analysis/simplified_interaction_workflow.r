# Simplified interaction workflow to ensure compatibility
# This function can be sourced within DEI_Enhanced_Analysis.r to replace the problematic sections

# Simplified version of interaction_analysis_workflow
simplified_interaction_workflow <- function(model_env, factors, levels,
                                          outcome_vars = c("program_success", "organizational_profit"),
                                          param_mode = "initial") {
  # Step 1: Design factorial experiment with our fixed function
  design <- create_factorial_design(factors, levels, include_extremes = TRUE, include_composite = TRUE)
  
  # Step 2: Run simulations
  results <- run_factorial_simulations(model_env, design, outcome_vars, param_mode)
  
  # Step 3: Calculate basic interaction effects without complex visualizations
  interactions <- list()
  for (outcome in outcome_vars) {
    # Create a simple linear model with interactions
    formula_str <- paste0(outcome, " ~ ", paste(factors, collapse = " * "))
    model <- lm(formula_str, data = results$results)
    
    # Extract model summary
    model_summary <- summary(model)
    
    # Store results
    interactions[[outcome]] <- list(
      model = model,
      summary = model_summary,
      coefficients = model_summary$coefficients
    )
  }
  
  # Step 4: Generate basic insights from model coefficients
  insights <- list()
  
  for (outcome in outcome_vars) {
    outcome_insights <- list()
    
    # Extract coefficients
    coefs <- interactions[[outcome]]$coefficients
    
    # Look for interaction terms (those containing ":")
    interaction_terms <- rownames(coefs)[grep(":", rownames(coefs))]
    
    if (length(interaction_terms) > 0) {
      # Find strongest interaction
      strongest_idx <- which.max(abs(coefs[interaction_terms, "Estimate"]))
      strongest_term <- interaction_terms[strongest_idx]
      strongest_coef <- coefs[strongest_term, "Estimate"]
      strongest_p <- coefs[strongest_term, "Pr(>|t|)"]
      
      # Create insight
      direction <- ifelse(strongest_coef > 0, "positive", "negative")
      significance <- ifelse(strongest_p < 0.05, "significant", "non-significant")
      
      outcome_insights$strongest_interaction <- paste0(
        "The strongest interaction for ", outcome, " is between ",
        gsub(":", " and ", strongest_term),
        " with a ", direction, " effect (coefficient = ", round(strongest_coef, 3), 
        "), which is statistically ", significance, " (p = ", round(strongest_p, 4), ")."
      )
      
      # Add insights for all significant interactions
      sig_terms <- interaction_terms[coefs[interaction_terms, "Pr(>|t|)"] < 0.05]
      if (length(sig_terms) > 0) {
        sig_insights <- character(length(sig_terms))
        for (i in 1:length(sig_terms)) {
          term <- sig_terms[i]
          coef <- coefs[term, "Estimate"]
          direction <- ifelse(coef > 0, "positive", "negative")
          
          sig_insights[i] <- paste0(
            "There is a significant ", direction, " interaction between ",
            gsub(":", " and ", term), " (coefficient = ", round(coef, 3), ")."
          )
        }
        outcome_insights$significant_interactions <- sig_insights
      } else {
        outcome_insights$significant_interactions <- 
          "No statistically significant interactions were found for this outcome."
      }
    } else {
      outcome_insights$interaction_note <- 
        "No interaction terms were found in the model. This might indicate a simpler relationship between factors."
    }
    
    # Add main effects insights
    main_effects <- factors[factors %in% rownames(coefs)]
    if (length(main_effects) > 0) {
      main_insights <- character(length(main_effects))
      for (i in 1:length(main_effects)) {
        factor_name <- main_effects[i]
        coef <- coefs[factor_name, "Estimate"]
        p_val <- coefs[factor_name, "Pr(>|t|)"]
        direction <- ifelse(coef > 0, "positive", "negative")
        significance <- ifelse(p_val < 0.05, "significant", "non-significant")
        
        main_insights[i] <- paste0(
          factor_name, " has a ", direction, " effect on ", outcome,
          " (coefficient = ", round(coef, 3), "), which is statistically ",
          significance, " (p = ", round(p_val, 4), ")."
        )
      }
      outcome_insights$main_effects <- main_insights
    }
    
    insights[[outcome]] <- outcome_insights
  }
  
  # Cross-outcome insights
  if (length(outcome_vars) > 1) {
    cross_insights <- list()
    
    for (i in 1:(length(outcome_vars) - 1)) {
      for (j in (i+1):length(outcome_vars)) {
        outcome1 <- outcome_vars[i]
        outcome2 <- outcome_vars[j]
        
        # Compare R-squared values
        rsq1 <- interactions[[outcome1]]$summary$r.squared
        rsq2 <- interactions[[outcome2]]$summary$r.squared
        
        better_fit <- ifelse(rsq1 > rsq2, outcome1, outcome2)
        worse_fit <- ifelse(rsq1 > rsq2, outcome2, outcome1)
        
        cross_insights[[paste(outcome1, outcome2, sep="_vs_")]] <- paste0(
          "The factors explain ", round(max(rsq1, rsq2) * 100, 1), "% of variation in ", 
          better_fit, " but only ", round(min(rsq1, rsq2) * 100, 1), "% of variation in ",
          worse_fit, ". This suggests that these factors are more influential for ",
          better_fit, "."
        )
      }
    }
    
    insights$cross_outcome <- cross_insights
  }
  
  # Return simplified results without complex visualizations
  return(list(
    design = design,
    results = results,
    interactions = interactions,
    insights = insights,
    factor_levels = levels
  ))
}