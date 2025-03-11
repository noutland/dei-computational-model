#!/usr/bin/env Rscript
# DEI Computational Model - Setup Script
# This script installs the required dependencies for the DEI computational model

# List of required packages
required_packages <- c(
  "tidyverse",  # Includes ggplot2, dplyr, tidyr, readr, etc.
  "viridis",    # For color palettes
  "patchwork",  # For combining plots
  "scales"      # For scaling functions in visualizations
)

# Check which packages are not installed yet
not_installed <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]

# Install missing packages
if (length(not_installed) > 0) {
  cat("Installing the following packages:", paste(not_installed, collapse = ", "), "\n")
  install.packages(not_installed, repos = "https://cloud.r-project.org")
} else {
  cat("All required packages are already installed.\n")
}

# Load and check all packages
cat("\nVerifying package installation and loading packages:\n")
for (pkg in required_packages) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    cat(paste("✓", pkg, "is installed and loaded\n"))
    library(pkg, character.only = TRUE)
  } else {
    cat(paste("✗ Failed to install or load", pkg, "\n"))
  }
}

cat("\nSetup completed. You are now ready to run the DEI Computational Model.\n")
cat("To run the full simulation, execute: Rscript src/DEI_Simulation_Master_Script.R\n")