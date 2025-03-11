#!/bin/bash
# DEI Computational Model - Run Script
# This script provides a convenient way to run the DEI computational model

# Make this script executable
# chmod +x run.sh

# Set the working directory to the script location
cd "$(dirname "$0")"

# Print banner
echo "========================================"
echo "   DEI Computational Model Runner"
echo "========================================"
echo

# Check if R is installed
if ! command -v Rscript &> /dev/null; then
    echo "Error: R is not installed or not in the path."
    echo "Please install R from https://www.r-project.org/"
    exit 1
fi

# Offer options
echo "Select an option:"
echo "1. Install dependencies (setup)"
echo "2. Run full simulation"
echo "3. Generate visualizations only"
echo "4. View executive dashboard"
echo "5. Exit"
echo

read -p "Enter your choice (1-5): " choice

case $choice in
    1)
        echo "Installing dependencies..."
        Rscript setup.R
        ;;
    2)
        echo "Running full simulation..."
        Rscript src/DEI_Simulation_Master_Script.R
        ;;
    3)
        echo "Generating visualizations..."
        Rscript src/visualization/generate_key_visualizations.r
        ;;
    4)
        echo "Opening executive dashboard..."
        # Try different commands based on OS
        if command -v xdg-open &> /dev/null; then
            # Linux
            xdg-open visualizations/executive_dashboard.png
        elif command -v open &> /dev/null; then
            # macOS
            open visualizations/executive_dashboard.png
        elif command -v start &> /dev/null; then
            # Windows
            start visualizations/executive_dashboard.png
        else
            echo "Cannot open the visualization automatically."
            echo "Please open visualizations/executive_dashboard.png manually."
        fi
        ;;
    5)
        echo "Exiting."
        exit 0
        ;;
    *)
        echo "Invalid option. Exiting."
        exit 1
        ;;
esac

echo
echo "Done."