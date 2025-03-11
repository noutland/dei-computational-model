#!/bin/bash
# DEI Computational Model - GitHub Push Helper
# This script helps push the repository to GitHub

# Make this script executable with: chmod +x github_push.sh

# Set the working directory to the script location
cd "$(dirname "$0")"

# Print banner
echo "========================================"
echo "   DEI Computational Model GitHub Push Helper"
echo "========================================"
echo

# Check if git is available
if ! command -v git &> /dev/null; then
    echo "Error: Git is not installed or not in the path."
    echo "Please install Git from https://git-scm.com/"
    exit 1
fi

# Check if we are in a git repo
if [ ! -d ".git" ]; then
    echo "Error: This directory is not a Git repository."
    echo "Please run this script from the root of the DEI Computational Model repository."
    exit 1
fi

# Check current branch
current_branch=$(git branch --show-current)
echo "Current branch: $current_branch"
echo

# Check if GitHub username is provided
read -p "Enter your GitHub username: " github_username

if [ -z "$github_username" ]; then
    echo "Error: GitHub username cannot be empty."
    exit 1
fi

# Set the remote URL
remote_url="https://github.com/$github_username/dei-computational-model.git"
echo
echo "Setting remote URL to: $remote_url"

# Check if remote already exists
if git remote | grep -q "origin"; then
    echo "Remote 'origin' already exists. Updating URL..."
    git remote set-url origin "$remote_url"
else
    echo "Adding remote 'origin'..."
    git remote add origin "$remote_url"
fi

# Verify remote
echo
echo "Configured remote:"
git remote -v

echo
echo "IMPORTANT: Before proceeding, ensure you have created a new repository on GitHub:"
echo "1. Go to https://github.com/new"
echo "2. Name: dei-computational-model"
echo "3. Description: Computational model for analyzing DEI initiatives within organizations"
echo "4. Choose 'Public' or 'Private' visibility"
echo "5. Do NOT initialize with README, .gitignore, or license"
echo "6. Click 'Create repository'"
echo

read -p "Have you created the repository on GitHub? (y/n): " created_repo

if [ "$created_repo" != "y" ] && [ "$created_repo" != "Y" ]; then
    echo "Please create the repository on GitHub before proceeding."
    echo "Run this script again after creating the repository."
    exit 1
fi

# Push to GitHub
echo
echo "Pushing to GitHub..."
git push -u origin "$current_branch"

push_status=$?

if [ $push_status -eq 0 ]; then
    echo
    echo "Success! The DEI Computational Model has been pushed to GitHub."
    echo "Your repository is now available at: https://github.com/$github_username/dei-computational-model"
    echo
    echo "Next steps:"
    echo "1. Visit your repository to verify everything was uploaded correctly"
    echo "2. Set up GitHub Pages if you want to showcase your visualizations online"
    echo "3. Add collaborators if needed (Settings -> Manage access)"
else
    echo
    echo "Push failed. This might be due to:"
    echo "1. The repository on GitHub was not created correctly"
    echo "2. Authentication issues - you may need to set up a GitHub token"
    echo "3. Network connectivity problems"
    echo
    echo "For more help, visit: https://docs.github.com/en/authentication"
fi