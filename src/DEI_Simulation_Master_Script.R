#!/usr/bin/env Rscript
# DEI Computational Model - Master Script
# This script coordinates the entire DEI model simulation workflow

# Load required libraries
library(tidyverse)
library(viridis)
library(patchwork)
library(scales)

# Set the working directory to the repository root (adjust as needed)
# If running from command line in the repository root, this can be commented out
# setwd("/path/to/dei-computational-model")

# Create required directories if they don't exist
dir.create("data", showWarnings = FALSE)
dir.create("visualizations", showWarnings = FALSE)

# Source the core model components
message("Loading core model components...")
source("src/core/system_dynamics_model.R")
source("src/core/network_analysis_model.R")
source("src/core/model_integration.R")
source("src/core/dynamic_relationship_weights.r")
source("src/core/nonlinear_relationships.r")

# Run the simulation and collect data
message("Running DEI model simulations...")
# Initialize models
system_model <- initialize_system_dynamics_model()
network_model <- initialize_network_model()

# Integrate models
integrated_model <- integrate_models(system_model, network_model)

# Apply dynamic relationship weights
integrated_model <- apply_dynamic_weights(integrated_model)

# Apply nonlinear relationships
integrated_model <- apply_nonlinear_relationships(integrated_model)

# Run Monte Carlo simulations
message("Running Monte Carlo simulations and scenario analyses...")
source("src/analysis/monte_carlo_analysis.R")
# Save Monte Carlo results to data directory
write_csv(monte_carlo_results, "data/monte_carlo_results.csv")
write_csv(scenario_results, "data/scenario_results.csv")

# Run statistical analysis
message("Performing statistical analysis on simulation results...")
source("src/analysis/statistical_analysis.R")
# Save statistical results to data directory
write_csv(statistical_summary, "data/statistical_summary.csv")

# Generate interaction workflow analysis
message("Analyzing interaction workflows...")
source("src/analysis/simplified_interaction_workflow.r")
# Save interaction results
write_csv(interaction_results, "data/interaction_results.csv")

# Generate visualizations
message("Generating visualizations...")
source("src/visualization/generate_key_visualizations.r")
source("src/visualization/DEI_Enhanced_Visualizations.r")

message("DEI Computational Model simulation completed successfully.")
message("Results saved to 'data/' directory")
message("Visualizations saved to 'visualizations/' directory")