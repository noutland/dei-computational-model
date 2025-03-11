# DEI Computational Model: Repository Overview

This document provides an overview of the repository structure and the purpose of key files to help you navigate the codebase effectively.

## Directory Structure

```
dei-computational-model/
├── data/                  # Simulation results and datasets
│   ├── monte_carlo_statistics.csv
│   ├── scenario_summary.csv
│   ├── threshold_Cultural_Zeitgeist.csv
│   └── threshold_Leader_Support.csv
│
├── docs/                  # Documentation
│   └── technical_documentation.md
│
├── src/                   # Source code
│   ├── core/              # Core model components
│   │   ├── system_dynamics_model.R
│   │   ├── network_analysis_model.R
│   │   ├── model_integration.r
│   │   ├── dynamic_relationship_weights.r
│   │   └── nonlinear_relationships.r
│   │
│   ├── analysis/          # Analysis scripts
│   │   ├── statistical_analysis.R
│   │   ├── monte_carlo_analysis.R
│   │   └── simplified_interaction_workflow.r
│   │
│   ├── visualization/     # Visualization scripts
│   │   ├── generate_key_visualizations.r
│   │   └── DEI_Enhanced_Visualizations.r
│   │
│   └── DEI_Simulation_Master_Script.R  # Main simulation script
│
├── visualizations/        # Generated visualization outputs
│   ├── executive_dashboard.png
│   ├── leader_support_threshold.png
│   ├── identity_safety_j_curve.png
│   ├── employee_focused_advantage.png
│   ├── implementation_funding_ratio.png
│   └── legal_action_thresholds.png
│
├── .gitignore             # Git ignore file
├── LICENSE                # MIT License
├── README.md              # Main README with usage instructions
├── GITHUB_SETUP.md        # Instructions for setting up GitHub repository
├── REPOSITORY_OVERVIEW.md # This file
├── run.sh                 # Shell script for running the model
└── setup.R                # R script for setting up dependencies
```

## Key Files

### Core Scripts

- **src/DEI_Simulation_Master_Script.R**: The main entry point that orchestrates the entire simulation workflow.
- **src/core/system_dynamics_model.R**: Implements the system dynamics approach modeling cause-and-effect relationships.
- **src/core/network_analysis_model.R**: Implements the network analysis approach modeling stakeholder interactions.
- **src/core/model_integration.r**: Integrates the system dynamics and network analysis models.

### Analysis Scripts

- **src/analysis/monte_carlo_analysis.R**: Implements Monte Carlo simulations to analyze model uncertainty.
- **src/analysis/statistical_analysis.R**: Performs statistical analysis on simulation results.
- **src/analysis/simplified_interaction_workflow.r**: Analyzes interaction workflows between model components.

### Visualization Scripts

- **src/visualization/generate_key_visualizations.r**: Generates the primary visualizations showcasing model insights.
- **src/visualization/DEI_Enhanced_Visualizations.r**: Produces enhanced visualizations with additional analysis.

### Documentation

- **README.md**: The main documentation with setup and usage instructions.
- **docs/technical_documentation.md**: Technical documentation of the model architecture and components.
- **GITHUB_SETUP.md**: Guide for setting up and pushing to a GitHub repository.

### Setup and Running

- **setup.R**: Script to install required R packages.
- **run.sh**: Convenient shell script for running different aspects of the model.

## Key Visualizations

The `visualizations/` directory contains several important visualizations that illustrate key findings:

1. **executive_dashboard.png**: A consolidated dashboard with key insights for executive-level presentations.
2. **leader_support_threshold.png**: Shows the critical threshold at which leader support becomes effective.
3. **identity_safety_j_curve.png**: Depicts the J-curve pattern of identity safety evolution.
4. **employee_focused_advantage.png**: Compares the employee-focused approach to other strategies.
5. **implementation_funding_ratio.png**: Illustrates the optimal implementation-to-funding ratio.
6. **legal_action_thresholds.png**: Shows critical risk thresholds for legal action intensity.

## Getting Started

1. If you're setting up the repository on GitHub, refer to `GITHUB_SETUP.md`.
2. For installing dependencies, run `./run.sh` and select option 1, or directly run `Rscript setup.R`.
3. To understand the model architecture, read `docs/technical_documentation.md`.
4. To run the full simulation, execute `./run.sh` and select option 2.

## Contributing

If you want to contribute to this project:

1. Follow the coding style used in existing files.
2. Add appropriate documentation for any new features.
3. Ensure any new visualizations follow the same design principles as existing ones.
4. Add tests for new functionality when applicable.