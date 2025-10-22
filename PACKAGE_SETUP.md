# Ganttify Package Setup Instructions

This file contains instructions for finalizing the package before publishing.

## Steps to Complete Package Setup

### 1. Generate Test Data
Run the script to create the test_project dataset:

```r
# Install usethis if needed
install.packages("usethis")

# Run the data creation script
source("data-raw/create_test_project.R")
```

This will create the `data/test_project.rda` file.

### 2. Generate Documentation
Use roxygen2 to generate documentation and NAMESPACE:

```r
# Install devtools and roxygen2 if needed
install.packages(c("devtools", "roxygen2"))

# Generate documentation
devtools::document()
```

This will:
- Create the `man/` directory with documentation files
- Generate the `NAMESPACE` file with exports

### 3. Update DESCRIPTION File
Edit the `DESCRIPTION` file to update:
- **Authors@R**: Replace with your actual name and email
- **URL**: Replace with your GitHub repository URL
- **BugReports**: Replace with your GitHub issues URL
- **License**: Ensure you have a LICENSE file (MIT, GPL, etc.)

### 4. Check Package
Run R CMD check to ensure everything is working:

```r
devtools::check()
```

Fix any errors, warnings, or notes that appear.

### 5. Install and Test Locally
Install the package locally to test:

```r
devtools::install()

# Test it out
library(ganttify)
data <- test_project
chart <- Ganttify(
  wbs_structure = data$wbs_structure,
  activities = data$activities,
  wbs_colors = data$colors
)
chart
```

### 6. Build Package
Build the package tarball:

```r
devtools::build()
```

### 7. Publish to GitHub
Initialize git and push to GitHub:

```bash
git add .
git commit -m "Initial package release"
git branch -M main
git remote add origin https://github.com/yourusername/ganttify.git
git push -u origin main
```

### 8. (Optional) Publish to CRAN
Follow CRAN submission guidelines at: https://cran.r-project.org/submit.html

Or publish to GitHub and let users install via:

```r
# install.packages("devtools")
devtools::install_github("yourusername/ganttify")
```

## Package Structure

```
ganttify/
├── DESCRIPTION          # Package metadata
├── NAMESPACE            # Auto-generated exports (don't edit manually)
├── LICENSE              # License file
├── .gitignore           # Git ignore patterns
├── Ganttify.Rproj       # RStudio project file
├── R/
│   ├── Ganttify.R       # Main function with documentation
│   └── data.R           # Test dataset documentation
├── data/
│   └── test_project.rda # Test dataset (generated)
├── data-raw/
│   └── create_test_project.R  # Script to create test data
└── man/                 # Documentation (auto-generated)
    ├── Ganttify.Rd
    └── test_project.Rd
```

## Dependencies

The package depends on:
- plotly (>= 4.9.0)
- dplyr (>= 1.0.0)
- lubridate (>= 1.7.0)
- htmlwidgets (>= 1.5.0)

These will be automatically installed when users install the package.

## Next Steps

1. Run the setup steps above
2. Test the package thoroughly
3. Consider adding vignettes or additional examples
4. Publish to GitHub or CRAN
5. Update the README.md with installation and usage instructions
