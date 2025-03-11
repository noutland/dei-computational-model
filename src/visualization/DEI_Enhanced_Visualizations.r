# DEI Enhanced Visualization Functions
# Provides advanced visualization tools for DEI computational model analysis

library(ggplot2)
library(reshape2)
library(dplyr)
library(patchwork)
library(viridis)
library(plotly)  # For interactive 3D plots

#' Create a heatmap showing interaction between two factors and their effect on an outcome
#'
#' @param data Data frame containing factorial results
#' @param factor1 First factor name
#' @param factor2 Second factor name 
#' @param outcome Outcome variable name
#' @param title Optional custom title
#' @return ggplot object with the heatmap
create_interaction_heatmap <- function(data, factor1, factor2, outcome, 
                                      title = NULL) {
  # Create a grid for the heatmap
  grid_data <- data[, c(factor1, factor2, outcome)]
  grid_data <- aggregate(
    grid_data[, outcome, drop=FALSE],
    by=list(
      Factor1=grid_data[, factor1],
      Factor2=grid_data[, factor2]
    ),
    FUN=mean,
    na.rm=TRUE
  )
  names(grid_data)[names(grid_data) == outcome] <- "outcome_mean"
  
  # Create the plot
  if (is.null(title)) {
    title <- paste("Interaction Effect of", factor1, "and", factor2, "on", outcome)
  }
  
  p <- ggplot(grid_data, aes(x = .data[[factor1]], y = .data[[factor2]], fill = outcome_mean)) +
    geom_tile() +
    scale_fill_viridis(option = "plasma", name = outcome) +
    labs(title = title,
         x = factor1, 
         y = factor2) +
    theme_minimal() +
    theme(plot.title = element_text(size = 12, face = "bold"),
          legend.position = "right")
  
  return(p)
}

#' Create an interaction profile plot showing how one factor's effect changes based on another's level
#'
#' @param data Data frame containing factorial results
#' @param x_factor Factor for x-axis
#' @param moderator Moderating factor that changes the effect
#' @param outcome Outcome variable name
#' @param moderator_levels Optional specific levels of moderator to highlight
#' @param title Optional custom title
#' @return ggplot object with interaction profile
create_interaction_profile <- function(data, x_factor, moderator, outcome, 
                                     moderator_levels = NULL, title = NULL) {
  
  # If no specific moderator levels are provided, select a reasonable number
  if (is.null(moderator_levels)) {
    all_levels <- sort(unique(data[[moderator]]))
    
    # Choose 5 levels if possible, otherwise use what's available
    if (length(all_levels) > 5) {
      # Get low, medium-low, medium, medium-high, and high
      indices <- round(seq(1, length(all_levels), length.out = 5))
      moderator_levels <- all_levels[indices]
    } else {
      moderator_levels <- all_levels
    }
  }
  
  # Filter data to include only the specified moderator levels
  plot_data <- data %>%
    filter(.data[[moderator]] %in% moderator_levels)
  
  # Convert moderator to factor for consistent colors
  plot_data[[moderator]] <- factor(plot_data[[moderator]])
  
  # Create title if not provided
  if (is.null(title)) {
    title <- paste("Effect of", x_factor, "on", outcome, "moderated by", moderator)
  }
  
  # Create the plot
  p <- ggplot(plot_data, aes(x = .data[[x_factor]], y = .data[[outcome]], 
                           color = .data[[moderator]], group = .data[[moderator]])) +
    geom_point(alpha = 0.7) +
    geom_smooth(method = "loess", se = TRUE, alpha = 0.2) +
    labs(title = title,
         x = x_factor,
         y = outcome,
         color = moderator) +
    theme_minimal() +
    theme(plot.title = element_text(size = 12, face = "bold"))
  
  return(p)
}

#' Create a contour plot highlighting thresholds where major changes occur
#'
#' @param data Data frame containing factorial results
#' @param factor1 First factor name
#' @param factor2 Second factor name
#' @param outcome Outcome variable name
#' @param bins Number of contour levels
#' @param title Optional custom title
#' @return ggplot object with contour plot
create_contour_plot <- function(data, factor1, factor2, outcome, 
                               bins = 10, title = NULL) {
  
  # Create a grid for the contour plot
  grid_data <- data[, c(factor1, factor2, outcome)]
  grid_data <- aggregate(
    grid_data[, outcome, drop=FALSE],
    by=list(
      Factor1=grid_data[, factor1],
      Factor2=grid_data[, factor2]
    ),
    FUN=mean,
    na.rm=TRUE
  )
  names(grid_data)[names(grid_data) == outcome] <- "outcome_mean"
  
  # Create title if not provided
  if (is.null(title)) {
    title <- paste("Threshold Map of", outcome, "by", factor1, "and", factor2)
  }
  
  # Create the contour plot
  p <- ggplot(grid_data, aes(x = .data[[factor1]], y = .data[[factor2]], z = outcome_mean)) +
    geom_contour_filled(bins = bins) +
    scale_fill_viridis_d(option = "viridis", name = outcome) +
    labs(title = title,
         x = factor1,
         y = factor2) +
    theme_minimal() +
    theme(plot.title = element_text(size = 12, face = "bold"))
  
  return(p)
}

#' Create a 3D response surface plot (returns plotly object)
#'
#' @param data Data frame containing factorial results
#' @param factor1 First factor name
#' @param factor2 Second factor name
#' @param outcome Outcome variable name
#' @param title Optional custom title
#' @return plotly object with 3D surface
create_response_surface <- function(data, factor1, factor2, outcome, title = NULL) {
  # Create a grid for the surface
  grid_data <- data[, c(factor1, factor2, outcome)]
  grid_data <- aggregate(
    grid_data[, outcome, drop=FALSE],
    by=list(
      Factor1=grid_data[, factor1],
      Factor2=grid_data[, factor2]
    ),
    FUN=mean,
    na.rm=TRUE
  )
  names(grid_data)[names(grid_data) == outcome] <- "outcome_mean"
  
  # Create title if not provided
  if (is.null(title)) {
    title <- paste("3D Response Surface of", outcome, "by", factor1, "and", factor2)
  }
  
  # Create the 3D surface plot using plotly
  # Update column references for plotly
  p <- plot_ly(grid_data,
              x = ~Factor1,
              y = ~Factor2,
              z = ~outcome_mean,
              type = "mesh3d",
              intensity = ~outcome_mean,
              showscale = TRUE) %>%
    layout(
      title = title,
      scene = list(
        xaxis = list(title = factor1),
        yaxis = list(title = factor2),
        zaxis = list(title = outcome)
      )
    )
  
  return(p)
}

#' Create a comprehensive interaction analysis dashboard for two factors
#'
#' @param data Data frame containing factorial results
#' @param factor1 First factor name
#' @param factor2 Second factor name
#' @param outcome Outcome variable name
#' @param save_path Optional path to save the plots
#' @return List of plot objects
create_interaction_dashboard <- function(data, factor1, factor2, outcome, save_path = NULL) {
  # Create the four different visualization types
  heatmap <- create_interaction_heatmap(data, factor1, factor2, outcome)
  
  # Create profile plots in both directions
  profile1 <- create_interaction_profile(data, factor1, factor2, outcome)
  profile2 <- create_interaction_profile(data, factor2, factor1, outcome)
  
  # Create contour plot
  contour <- create_contour_plot(data, factor1, factor2, outcome)
  
  # Combine plots using patchwork
  combined_plot <- (heatmap + contour) / (profile1 + profile2) +
    plot_annotation(title = paste("Comprehensive Interaction Analysis:", 
                                 factor1, "×", factor2, "→", outcome),
                   theme = theme(plot.title = element_text(size = 14, face = "bold")))
  
  # Save if path is provided
  if (!is.null(save_path)) {
    ggsave(save_path, combined_plot, width = 12, height = 10, units = "in", dpi = 300)
  }
  
  # Also create a 3D response surface (not included in combined plot as it's plotly)
  surface <- create_response_surface(data, factor1, factor2, outcome)
  
  # Return all plot objects
  return(list(
    combined = combined_plot,
    heatmap = heatmap,
    profile1 = profile1,
    profile2 = profile2,
    contour = contour,
    surface = surface
  ))
}

#' Generate a comprehensive set of multi-factor visualizations
#' 
#' @param factorial_results Results from factorial experiment
#' @param factors List of factors to analyze
#' @param outcomes List of outcomes to analyze
#' @param output_dir Directory to save plots
#' @return List of visualization objects
generate_comprehensive_visualizations <- function(factorial_results, factors, 
                                               outcomes, output_dir = NULL) {
  results <- list()
  
  # Create output directory if specified
  if (!is.null(output_dir)) {
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  }
  
  # For each pair of factors and each outcome
  for (i in 1:(length(factors) - 1)) {
    for (j in (i+1):length(factors)) {
      factor1 <- factors[i]
      factor2 <- factors[j]
      
      for (outcome in outcomes) {
        # Generate dashboard of visualizations
        filename <- NULL
        if (!is.null(output_dir)) {
          filename <- file.path(output_dir, 
                              paste0("interaction_", factor1, "_", factor2, "_", outcome, ".pdf"))
        }
        
        # Create the visualization dashboard
        viz <- create_interaction_dashboard(
          factorial_results$results, 
          factor1, factor2, outcome, 
          save_path = filename
        )
        
        # Store in results
        key <- paste(factor1, factor2, outcome, sep = "_")
        results[[key]] <- viz
      }
    }
  }
  
  return(results)
}