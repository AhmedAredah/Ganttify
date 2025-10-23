# Ganttify

<!-- badges: start -->
[![R-CMD-check](https://github.com/AhmedAredah/Ganttify/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/AhmedAredah/Ganttify/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Create interactive Primavera-style Gantt charts with Work Breakdown Structure (WBS) hierarchy and activities. Built on top of plotly for rich, interactive visualizations.

## Features

- **WBS Hierarchy**: Organize projects with multi-level Work Breakdown Structure
- **Color-Coded Items**: Customizable colors for each WBS element
- **Interactive Charts**: Pan, zoom, and hover for detailed information
- **Dynamic Date Formatting**: Automatically adjusts time scale based on zoom level
- **Scrollable Views**: Handle large projects with vertical scrolling
- **Past Activity Dimming**: Optionally dim completed activities for better focus
- **Flexible Display**: Show/hide WBS and activity names on bars
- **Today Line**: Visual indicator for current date

## Installation

### From CRAN (Recommended)

```r
install.packages("ganttify")
```

### From GitHub (Development Version)

```r
# Install devtools if you haven't already
install.packages("devtools")

# Install ganttify from GitHub
devtools::install_github("AhmedAredah/Ganttify")
```

## Quick Start

```r
library(ganttify)

# Load the example dataset
data(test_project)

# Create a basic Gantt chart
chart <- Ganttify(
  wbs_structure = test_project$wbs_structure,
  activities = test_project$activities,
  wbs_colors = test_project$colors
)

# Display the chart
chart
```

## Usage Examples

### Basic Gantt Chart

```r
library(ganttify)
data(test_project)

# Simple chart with default settings
Ganttify(
  wbs_structure = test_project$wbs_structure,
  activities = test_project$activities,
  wbs_colors = test_project$colors,
  chart_title = "My Project Schedule"
)
```

<img width="1011" height="627" alt="image" src="https://github.com/user-attachments/assets/3f4d9465-8468-418d-86c2-f00874075152" />


### Dim Past Activities

```r
# Highlight completed work by dimming past activities
Ganttify(
  wbs_structure = test_project$wbs_structure,
  activities = test_project$activities,
  wbs_colors = test_project$colors,
  dim_past_activities = TRUE,
  dim_opacity = 0.3
)
```

### Show Names on Bars

```r
# Display WBS and activity names directly on bars
Ganttify(
  wbs_structure = test_project$wbs_structure,
  activities = test_project$activities,
  wbs_colors = test_project$colors,
  show_wbs_names_on_bars = TRUE,
  show_activity_names_on_bars = TRUE
)
```

### Custom Scrolling View

```r
# Control visible rows for large projects
Ganttify(
  wbs_structure = test_project$wbs_structure,
  activities = test_project$activities,
  wbs_colors = test_project$colors,
  max_visible_rows = 15,
  buffer_days = 60
)
```

## Data Structure

### WBS Structure Format

Your WBS data frame should have 3 columns:

```r
wbs_structure <- data.frame(
  ID = c("W1", "W2", "W3"),
  Name = c("Project", "Phase 1", "Phase 2"),
  Parent = c("None", "W1", "W1")
)
```

### Activities Format

Your activities data frame should have 5 columns:

```r
activities <- data.frame(
  WBS_ID = c("W2", "W2", "W3"),
  Activity_ID = c("A1", "A2", "A3"),
  Activity_Name = c("Design", "Development", "Testing"),
  Start_Date = c("01/01/2024", "02/01/2024", "03/01/2024"),
  End_Date = c("01/31/2024", "02/28/2024", "03/31/2024")
)
```

**Note:** Dates must be in `MM/DD/YYYY` format.

### Custom Colors (Optional)

```r
colors <- list(
  "W1" = "#FF6B6B",
  "W2" = "#4ECDC4",
  "W3" = "#45B7D1"
)
```

## Parameters

Key parameters for the `Ganttify()` function:

- `wbs_structure`: Data frame with WBS hierarchy (ID, Name, Parent)
- `activities`: Data frame with activities (WBS_ID, Activity_ID, Activity_Name, Start_Date, End_Date)
- `wbs_colors`: Named list of colors for each WBS item
- `show_today_line`: Show vertical line at current date (default: TRUE)
- `dim_past_activities`: Dim completed activities (default: FALSE)
- `dim_opacity`: Opacity for dimmed activities (default: 0.3)
- `chart_title`: Chart title (default: "Project Gantt Chart with WBS")
- `max_visible_rows`: Number of visible rows for scrolling (default: 20)
- `buffer_days`: Extra days before/after timeline (default: 30)

See `?Ganttify` for complete documentation.

## Saving Charts

```r
library(htmlwidgets)

# Create chart
chart <- Ganttify(
  wbs_structure = test_project$wbs_structure,
  activities = test_project$activities,
  wbs_colors = test_project$colors
)

# Save as HTML
saveWidget(chart, "my_gantt_chart.html")
```

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

GPL-3

## Author

Ahmed Aredah (Ahmed.Aredah@gmail.com)

## Issues

Report bugs and request features at: https://github.com/AhmedAredah/Ganttify/issues
