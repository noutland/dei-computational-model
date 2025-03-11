# Setting Up GitHub Repository

This guide will help you create a new GitHub repository and push the DEI Computational Model code to it.

## Prerequisites

- [Git](https://git-scm.com/) installed on your machine
- A [GitHub](https://github.com/) account

## Steps to Create and Push to GitHub

1. **Create a new repository on GitHub**

   - Go to https://github.com/new
   - Name your repository `dei-computational-model`
   - Add a brief description: "Computational model for analyzing DEI initiatives within organizations"
   - Choose "Public" or "Private" visibility as appropriate
   - Do NOT initialize with a README, .gitignore, or license (we already have these files)
   - Click "Create repository"

2. **Initialize the local Git repository**

   Open a terminal in the `dei-computational-model` directory and run:

   ```bash
   # Navigate to the repository directory (if not already there)
   cd path/to/dei-computational-model

   # Initialize git repository
   git init

   # Add all files to staging
   git add .

   # Create initial commit
   git commit -m "Initial commit of DEI Computational Model"
   ```

3. **Add the GitHub repository as remote and push**

   ```bash
   # Add the GitHub repository as remote
   # Replace 'YOUR-USERNAME' with your GitHub username
   git remote add origin https://github.com/YOUR-USERNAME/dei-computational-model.git

   # Push to GitHub
   git push -u origin main
   # Note: If you're using an older Git version, you might need to use 'master' instead of 'main'
   ```

4. **Verify your repository**

   - Visit `https://github.com/YOUR-USERNAME/dei-computational-model` to see your repository
   - Ensure all files and directories were uploaded correctly

## Collaborating on the Repository

To collaborate with others:

1. Go to your repository on GitHub
2. Navigate to "Settings" > "Manage access"
3. Click "Invite a collaborator" and add their GitHub username or email

## Creating a Release

Consider creating a release once you've reached a stable version:

1. Go to your repository on GitHub
2. Navigate to "Releases" on the right side
3. Click "Create a new release"
4. Tag version (e.g., v1.0.0)
5. Add a title and description
6. Click "Publish release"

This will make it easier for users to download a specific version of the model.