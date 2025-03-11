# DEI Computational Model

This repository contains a computational model for simulating and analyzing Diversity, Equity, and Inclusion (DEI) initiatives within organizations. The model uses a combination of system dynamics and network analysis approaches to understand the complex interrelationships between organizational culture, leadership support, and DEI program outcomes.

## Repository Structure

```
dei-computational-model/
├── src/                  # Source code for the DEI computational model
│   ├── core/             # Core model components
│   ├── analysis/         # Statistical analysis scripts
│   └── visualization/    # Visualization generation scripts
├── data/                 # Simulation results and data files
├── visualizations/       # Generated visualizations and plots
└── docs/                 # Documentation files
```

## Key Findings

The DEI computational model reveals several critical insights:

1. **Leadership Support Threshold**: A critical threshold exists at which leader support becomes effective. Half-measures can be worse than no commitment at all.

2. **Identity Safety J-Curve**: Identity safety initially declines before dramatically improving, following a J-curve pattern.

3. **Employee-Focused Approach Advantage**: Employee-focused DEI approaches show dramatic improvements compared to other implementation strategies.

4. **Implementation-to-Funding Ratio**: An optimal ratio of approximately 2:1 (implementation effort to funding) maximizes program success.

5. **Legal Action Thresholds**: Critical risk thresholds occur at specific points in legal action intensity.

## Setup Instructions

### Prerequisites

- R version 4.0.0 or higher
- Required R packages:
  - tidyverse (includes ggplot2, dplyr, tidyr, readr)
  - viridis
  - patchwork
  - scales
- Git (for GitHub repository management)

### Installation

#### Option 1: Clone from GitHub
```bash
git clone https://github.com/yourusername/dei-computational-model.git
cd dei-computational-model
```

#### Option 2: Set up from scratch
1. Download or extract this repository to your computer
2. Navigate to the repository directory
3. Run `Rscript setup.R` to install required packages

### GitHub Repository Setup

This repository includes helper scripts to manage the GitHub repository:

1. **GITHUB_SETUP.md**: Detailed instructions for setting up a GitHub repository
2. **github_push.sh**: Interactive shell script to help push to GitHub:
   ```bash
   # Make executable if needed
   chmod +x github_push.sh
   
   # Run the script
   ./github_push.sh
   ```
   This script will:
   - Prompt for your GitHub username
   - Set up the appropriate remote URL
   - Guide you through pushing to GitHub
   - Provide next steps and troubleshooting

## Usage

### Running the Full Simulation

To run the complete DEI model simulation:

```bash
Rscript src/DEI_Simulation_Master_Script.R
```

This script will:
- Initialize the system dynamics and network analysis models
- Run Monte Carlo simulations across different parameters
- Generate statistical analyses of the results
- Save data to the `data/` directory

### Generating Visualizations

To generate key visualizations from the simulation results:

```bash
Rscript src/visualization/generate_key_visualizations.R
```

This will create visualization files in the `visualizations/` directory.

### Executive Dashboard

To view the executive dashboard summarizing key findings:

```bash
open visualizations/executive_dashboard.png
```

## Key Visualization Descriptions

### Leader Support Threshold
Demonstrates that program success reaches its lowest point at a leader support level of 1.6, where "half-measures" produce worse outcomes than no commitment.

### Identity Safety J-Curve
Illustrates how identity safety initially declines before dramatically improving as cultural support increases.

### Employee-Focused Approach Comparison
Shows the dramatic improvements achieved with an employee-focused approach compared to baseline and crisis response approaches.

### Implementation-to-Funding Ratio
Identifies the optimal ratio of implementation effort to funding (approximately 2:1) that maximizes program success.

### Legal Action Thresholds
Reveals critical risk thresholds that occur at specific points of legal action intensity.

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Acknowledgments

- This model was developed as part of a comprehensive analysis of DEI program effectiveness in organizational settings
- Special thanks to contributors and organizations who provided insights and data for model validation