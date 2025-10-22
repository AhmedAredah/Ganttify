# Script to create test_project dataset
# Run this script to generate the data/test_project.rda file

wbs_structure <- data.frame(
  ID = c("W1", "W2", "W3", "W4", "W5", "W6", "W7", "W8"),
  Name = c("Project Summary", "Project Details", "Phase 1", "Phase 2",
           "Design Phase", "Development Phase", "Testing Phase", "Deployment Phase"),
  Parent = c("None", "W1", "W2", "W2", "W3", "W3", "W4", "W4"),
  stringsAsFactors = FALSE
)

activities <- data.frame(
  WBS_ID = c("W5", "W5", "W5", "W6", "W6", "W6", "W6", "W7", "W7", "W7",
             "W8", "W8", "W8", "W8", "W8"),
  Activity_ID = c("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9", "A10",
                  "A11", "A12", "A13", "A14", "A15"),
  Activity_Name = c("Design UI", "Design Architecture", "Design Database",
                    "Code Frontend", "Code Backend", "Code API", "Code Database",
                    "Unit Testing", "Integration Testing", "Performance Testing",
                    "Deploy Dev", "Deploy Staging", "Deploy Production",
                    "User Training", "Documentation"),
  Start_Date = c("09/09/2024", "09/15/2024", "09/20/2024",
                 "10/01/2024", "10/01/2024", "10/15/2024", "10/20/2024",
                 "11/01/2024", "11/10/2024", "11/20/2024",
                 "12/01/2024", "12/10/2024", "12/20/2024", "12/25/2024", "12/28/2024"),
  End_Date = c("09/30/2024", "10/05/2024", "09/30/2024",
               "11/15/2024", "11/30/2024", "11/10/2024", "11/05/2024",
               "11/15/2024", "11/25/2024", "12/05/2024",
               "12/15/2024", "12/25/2024", "01/15/2025", "01/10/2025", "01/20/2025"),
  stringsAsFactors = FALSE
)

colors <- list(
  "W1" = "#FF6B6B",
  "W2" = "#4ECDC4",
  "W3" = "#45B7D1",
  "W4" = "#FFA07A",
  "W5" = "#95E1D3",
  "W6" = "#F38181",
  "W7" = "#A8E6CF",
  "W8" = "#FFD3B6"
)

# Combine into a single list
test_project <- list(
  wbs_structure = wbs_structure,
  activities = activities,
  colors = colors
)

# Save to data/ directory
usethis::use_data(test_project, overwrite = TRUE)
