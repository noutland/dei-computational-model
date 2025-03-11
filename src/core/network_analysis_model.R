# DEI Network Analysis Model in R
# A network-based computational model of DEI systems based on the
# causal loop diagram. This model represents the relationships as a 
# directed graph with weighted edges.

# Load required libraries
library(igraph)        # For network graph creation and analysis
library(ggplot2)       # For visualization
library(dplyr)         # For data manipulation
library(tidygraph)     # For tidy graph manipulation
library(ggraph)        # For network visualization

# Create DEI Network Model
dei_network_model <- function() {
  # Initialize model environment
  model_env <- new.env()
  
  # Define nodes (variables from the DEI system)
  model_env$nodes <- c(
    'Government Support',
    'Legal Action',
    'Cultural Zeitgeist',
    'Funding',
    'Leader Support',
    'Premortem Planning',
    'Program Implementation',
    'Organizational Messaging',
    'Program Success',
    'Retention/Recruitment',
    'Promotion Equity',
    'Identity Safety',
    'Compensation',
    'Feedback/Rewards',
    'Employee Motivation',
    'Employee Engagement',
    'Employee Satisfaction',
    'Employee Performance',
    'Impact/Meaning',
    'Organizational Profit'
  )
  
  # Define node categories for visualization
  model_env$node_categories <- list(
    'External' = c('Government Support', 'Legal Action', 'Cultural Zeitgeist'),
    'Program' = c('Funding', 'Leader Support', 'Premortem Planning', 
                 'Program Implementation', 'Organizational Messaging', 'Program Success'),
    'HR' = c('Retention/Recruitment', 'Promotion Equity', 'Identity Safety', 
            'Compensation', 'Feedback/Rewards'),
    'Outcomes' = c('Employee Motivation', 'Employee Engagement', 'Employee Satisfaction', 
                  'Employee Performance', 'Impact/Meaning', 'Organizational Profit')
  )
  
  # Define edges (relationships between variables)
  model_env$edges <- data.frame(
    from = c(
      'Government Support', 'Legal Action', 'Cultural Zeitgeist', 'Cultural Zeitgeist',
      'Funding', 'Leader Support', 'Leader Support', 'Leader Support',
      'Premortem Planning', 'Program Implementation', 'Organizational Messaging', 'Organizational Messaging',
      'Promotion Equity', 'Identity Safety', 'Compensation', 'Feedback/Rewards',
      'Promotion Equity', 'Impact/Meaning', 'Employee Motivation', 'Employee Engagement',
      'Employee Satisfaction', 'Employee Performance', 'Retention/Recruitment', 'Program Success',
      'Organizational Profit', 'Organizational Profit', 'Program Success'
    ),
    to = c(
      'Cultural Zeitgeist', 'Cultural Zeitgeist', 'Leader Support', 'Organizational Messaging',
      'Premortem Planning', 'Funding', 'Organizational Messaging', 'Promotion Equity',
      'Program Implementation', 'Program Success', 'Retention/Recruitment', 'Identity Safety',
      'Identity Safety', 'Retention/Recruitment', 'Employee Satisfaction', 'Employee Performance',
      'Employee Motivation', 'Employee Motivation', 'Employee Engagement', 'Employee Satisfaction',
      'Employee Performance', 'Organizational Profit', 'Program Success', 'Leader Support',
      'Leader Support', 'Retention/Recruitment', 'Funding'
    ),
    weight = c(0.3, 0.3, 0.5, 0.2, 0.4, 0.6, 0.7, 0.4, 0.7, 0.8, 0.5, 0.6,
               0.4, 0.6, 0.5, 0.6, 0.5, 0.7, 0.7, 0.6, 0.7, 0.8, 0.5, 0.4,
               0.3, 0.4, 0.5),
    sign = c(1, 1, 1, -1, 1, 1, 1, 1, 1, 1, 1, 1,
             1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
             1, 1, 1),
    stringsAsFactors = FALSE
  )
  
  # Create the network graph
  model_env$create_graph <- function() {
    # Create node data frame with initial values
    nodes_df <- data.frame(
      name = model_env$nodes,
      value = 5.0,  # Default initial value
      stringsAsFactors = FALSE
    )
    
    # Add category to each node
    nodes_df$category <- NA
    for (cat_name in names(model_env$node_categories)) {
      nodes_df$category[nodes_df$name %in% model_env$node_categories[[cat_name]]] <- cat_name
    }
    
    # Create the graph
    g <- graph_from_data_frame(model_env$edges, vertices = nodes_df, directed = TRUE)
    
    return(g)
  }
  
  # Initialize the graph
  model_env$G <- model_env$create_graph()
  
  # Set values for specific nodes in the network
  model_env$set_node_values <- function(value_dict) {
    for (node in names(value_dict)) {
      if (node %in% V(model_env$G)$name) {
        V(model_env$G)[node]$value <- value_dict[[node]]
      } else {
        warning(paste("Node", node, "not found in graph"))
      }
    }
  }
  
  # Propagate effects through the network
  model_env$propagate_effects <- function(iterations = 5) {
    # Initialize history to track changes
    history <- list()
    for (node in V(model_env$G)$name) {
      history[[node]] <- c(V(model_env$G)[node]$value)
    }
    
    # Propagate effects for specified iterations
    for (iter in 1:iterations) {
      # Create a copy of current values
      current_values <- setNames(
        sapply(V(model_env$G)$name, function(node) V(model_env$G)[node]$value),
        V(model_env$G)$name
      )
      
      # Update each node based on incoming edges
      for (node in V(model_env$G)$name) {
        # Get all predecessors
        predecessors <- neighbors(model_env$G, node, mode = "in")
        
        if (length(predecessors) > 0) {
          # Calculate influence from each predecessor
          delta <- 0
          
          # Special case for Organizational Messaging to add interaction effects
          if (node == "Organizational Messaging") {
            # Check if both Cultural Zeitgeist and Leader Support are predecessors
            cultural_pred <- "Cultural Zeitgeist" %in% predecessors$name
            leader_pred <- "Leader Support" %in% predecessors$name
            
            if (cultural_pred && leader_pred) {
              # Get values of these key influencers
              cultural_zeitgeist_value <- current_values["Cultural Zeitgeist"]
              leader_support_value <- current_values["Leader Support"]
              
              # Add non-linear interaction term between cultural zeitgeist and messaging
              cultural_messaging_interaction <- cultural_zeitgeist_value * current_values[node] * 0.15
              
              # Get the edge weights and signs
              cultural_edge_id <- get_edge_ids(model_env$G, c("Cultural Zeitgeist", node))
              leader_edge_id <- get_edge_ids(model_env$G, c("Leader Support", node))
              
              cultural_weight <- E(model_env$G)[cultural_edge_id]$weight
              cultural_sign <- E(model_env$G)[cultural_edge_id]$sign
              leader_weight <- E(model_env$G)[leader_edge_id]$weight
              
              # Calculate the organizational messaging effect with interaction
              org_messaging_effect <- cultural_zeitgeist_value * cultural_weight * cultural_sign +
                                     leader_support_value * leader_weight +
                                     cultural_messaging_interaction
              
              # Create conditional interactions based on cultural zeitgeist levels
              if (cultural_zeitgeist_value < 3.0 && leader_support_value > 6.0) {
                # Strong leadership amplifies messaging in hostile environments
                org_messaging_effect <- org_messaging_effect * 1.2
              } else if (cultural_zeitgeist_value > 7.0) {
                # Supportive culture enhances messaging effects
                org_messaging_effect <- org_messaging_effect * 1.1
              }
              
              # Apply the special effect for this node
              delta <- org_messaging_effect - current_values[node]
              
              # Skip regular processing of these edges since we've handled them
              predecessors <- predecessors[!predecessors$name %in% c("Cultural Zeitgeist", "Leader Support")]
            }
          }
          
          # Normal processing for other predecessors or nodes
          for (pred in predecessors$name) {
            edge_id <- get_edge_ids(model_env$G, c(pred, node))  # Updated to non-deprecated function
            edge_weight <- E(model_env$G)[edge_id]$weight
            edge_sign <- E(model_env$G)[edge_id]$sign
            
            influence <- edge_weight * edge_sign * (current_values[pred] - current_values[node])
            delta <- delta + influence
          }
          
          # Apply bounded update
          new_value <- current_values[node] + (0.1 * delta)  # Dampen effect for stability
          V(model_env$G)[node]$value <- max(0, min(10, new_value))
        }
        
        # Record in history (always, even if node wasn't updated)
        history[[node]] <- c(history[[node]], V(model_env$G)[node]$value)
      }
    }
    
    return(history)
  }
  
  # Visualize the network
  model_env$visualize_network <- function() {
    # Convert igraph to tidygraph
    tidy_graph <- as_tbl_graph(model_env$G)
    
    # Define colors for different node categories
    category_colors <- c(
      'External' = 'skyblue',
      'Program' = 'lightgreen',
      'HR' = 'salmon',
      'Outcomes' = 'gold'
    )
    
    # Create the plot - using standard fonts and simplified styling
    p <- ggraph(tidy_graph, layout = "stress") +
      geom_edge_link(aes(width = weight,
                         color = ifelse(sign > 0, "Positive", "Negative"),
                         alpha = weight),
                     arrow = arrow(length = unit(4, 'mm')),
                     end_cap = circle(3, 'mm')) +
      geom_node_point(aes(size = value, color = category), alpha = 0.8) +
      geom_node_text(aes(label = name), repel = TRUE, size = 3,
                    family = "sans", fontface = "plain") +  # Use standard font
      scale_edge_color_manual(values = c("Positive" = "green4", "Negative" = "red3"),
                             name = "Influence Type") +
      scale_edge_width(range = c(0.5, 2), name = "Strength") +  # Keep using scale_edge_width
      scale_edge_alpha(range = c(0.3, 0.8)) +
      scale_color_manual(values = category_colors, name = "Factor Category") +
      scale_size_continuous(range = c(3, 12), name = "Value") +
      theme_minimal() +  # Using minimal theme instead of graph theme
      labs(title = "DEI System Network Model") +
      theme(legend.position = "right",
            text = element_text(family = "sans"))  # Ensure all text uses standard font
    
    return(p)
  }
  
  # Plot the propagation history
  model_env$plot_propagation_history <- function(history) {
    # Convert history list to data frame
    history_df <- data.frame(iteration = 0:(length(history[[1]]) - 1))
    
    for (node in names(history)) {
      history_df[[node]] <- history[[node]]
    }
    
    # Convert to long format for ggplot
    history_long <- reshape2::melt(history_df, id.vars = "iteration",
                                  variable.name = "node", value.name = "value")
    
    # Add category for each node
    history_long$category <- NA
    for (cat_name in names(model_env$node_categories)) {
      history_long$category[history_long$node %in% model_env$node_categories[[cat_name]]] <- cat_name
    }
    
    # Create plots for each category
    plot_list <- list()
    
    for (cat in unique(history_long$category)) {
      cat_data <- history_long[history_long$category == cat, ]
      
      p <- ggplot(cat_data, aes(x = iteration, y = value, color = node)) +
        geom_line(size = 1) +  # Keep using size for line thickness
        geom_point(size = 2) +
        ylim(0, 10) +
        labs(title = paste(cat, "Factors"), x = "Iteration", y = "Value") +
        theme_minimal() +
        theme(legend.position = "right",
              text = element_text(family = "sans"),  # Use standard font
              title = element_text(family = "sans"))
      
      plot_list[[cat]] <- p
    }
    
    return(plot_list)
  }
  
  # Centrality analysis
  model_env$centrality_analysis <- function() {
    # Calculate various centrality measures
    degree_centrality <- degree(model_env$G, mode = "all", normalized = TRUE)
    in_degree_centrality <- degree(model_env$G, mode = "in", normalized = TRUE)
    out_degree_centrality <- degree(model_env$G, mode = "out", normalized = TRUE)
    betweenness_centrality <- betweenness(model_env$G, directed = TRUE, normalized = TRUE)
    
    # Create data frame for results
    centrality_df <- data.frame(
      Node = V(model_env$G)$name,
      `Degree Centrality` = degree_centrality,
      `In-Degree Centrality` = in_degree_centrality,
      `Out-Degree Centrality` = out_degree_centrality,
      `Betweenness Centrality` = betweenness_centrality
    )
    
    # Calculate total influence
    centrality_df$`Total Influence` <- rowSums(centrality_df[, 2:5])
    
    # Sort by total centrality
    centrality_df <- centrality_df[order(centrality_df$`Total Influence`, decreasing = TRUE), ]
    
    return(centrality_df)
  }
  
  # Identify feedback loops
  model_env$identify_feedback_loops <- function() {
    # Find all simple cycles
    simple_cycles <- all_simple_paths(model_env$G, V(model_env$G), 
                                     V(model_env$G), mode = "out", cutoff = 8)
    
    # Filter for actual cycles (where first node equals last node)
    cycles <- list()
    for (path in simple_cycles) {
      path_nodes <- V(model_env$G)[path]$name
      if (length(path_nodes) > 2 && path_nodes[1] == path_nodes[length(path_nodes)]) {
        cycles[[length(cycles) + 1]] <- path_nodes
      }
    }
    
    # Calculate properties for each loop
    loops <- list()
    for (i in seq_along(cycles)) {
      cycle <- cycles[[i]]
      cycle_nodes <- cycle[-length(cycle)]  # Remove duplicate end node
      
      # Check if the cycle is a complete loop
      cycle_complete <- TRUE
      for (j in 1:(length(cycle_nodes))) {
        from_node <- cycle_nodes[j]
        to_node <- cycle_nodes[if (j == length(cycle_nodes)) 1 else j + 1]
        
        if (!are.connected(model_env$G, from_node, to_node)) {
          cycle_complete <- FALSE
          break
        }
      }
      
      if (!cycle_complete) {
        next
      }
      
      # Add the complete loop to results
      cycle_str <- paste(cycle_nodes, collapse = " → ")
      cycle_str <- paste0(cycle_str, " → ", cycle_nodes[1])
      
      # Calculate overall sign of the loop
      loop_sign <- 1
      for (j in 1:(length(cycle_nodes))) {
        from_node <- cycle_nodes[j]
        to_node <- cycle_nodes[if (j == length(cycle_nodes)) 1 else j + 1]
        
        edge_id <- get_edge_ids(model_env$G, c(from_node, to_node))
        edge_sign <- E(model_env$G)[edge_id]$sign
        
        loop_sign <- loop_sign * edge_sign
      }
      
      loop_type <- ifelse(loop_sign > 0, "Reinforcing", "Balancing")
      
      # Calculate strength based on the product of weights
      strength <- 1
      for (j in 1:(length(cycle_nodes))) {
        from_node <- cycle_nodes[j]
        to_node <- cycle_nodes[if (j == length(cycle_nodes)) 1 else j + 1]
        
        edge_id <- get_edge_ids(model_env$G, c(from_node, to_node))
        edge_weight <- E(model_env$G)[edge_id]$weight
        
        strength <- strength * edge_weight
      }
      
      # Store in results
      loops[[cycle_str]] <- list(
        nodes = cycle_nodes,
        length = length(cycle_nodes),
        type = loop_type,
        strength = strength
      )
    }
    
    return(loops)
  }
  
  # Run an intervention scenario
  model_env$run_intervention_scenario <- function(intervention_nodes, intervention_values, iterations = 10) {
    # Set initial values based on intervention
    intervention_dict <- setNames(intervention_values, intervention_nodes)
    model_env$set_node_values(intervention_dict)
    
    # Run propagation
    history <- model_env$propagate_effects(iterations)
    
    # Return to default values (reset)
    default_values <- setNames(rep(5.0, length(V(model_env$G)$name)), V(model_env$G)$name)
    model_env$set_node_values(default_values)
    
    return(history)
  }
  
  # Return the model environment
  return(model_env)
}

# Example usage
if (FALSE) {  # Set to TRUE to run the examples
  # Create the network model
  model <- dei_network_model()
  
  # Visualize the initial network
  network_plot <- model$visualize_network()
  print(network_plot)
  
  # Run propagation and visualize results
  history <- model$propagate_effects(iterations = 10)
  history_plots <- model$plot_propagation_history(history)
  
  # Display history plots
  library(gridExtra)
  do.call(grid.arrange, c(history_plots, ncol = 2))
  
  # Analyze centrality to find most influential factors
  centrality_df <- model$centrality_analysis()
  print("Centrality Analysis (Most Influential Factors):")
  print(centrality_df)
  
  # Identify feedback loops
  loops <- model$identify_feedback_loops()
  print("Feedback Loops Analysis:")
  for (loop_name in names(loops)) {
    loop <- loops[[loop_name]]
    cat(sprintf("%s Loop (%d nodes, strength=%.3f): %s\n", 
               loop$type, loop$length, loop$strength, loop_name))
  }
  
  # Run an intervention scenario (e.g., increasing leader support)
  intervention_history <- model$run_intervention_scenario(
    intervention_nodes = c('Leader Support', 'Organizational Messaging'),
    intervention_values = c(9.0, 8.0),
    iterations = 10
  )
}