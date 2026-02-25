# Ganttify

<!-- badges: start -->
[![R-CMD-check](https://github.com/AhmedAredah/Ganttify/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/AhmedAredah/Ganttify/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

[![](https://cranlogs.r-pkg.org/badges/grand-total/ganttify)](https://cran.r-project.org/package=ganttify)

Create interactive Primavera-style Gantt charts with Work Breakdown Structure (WBS) hierarchy and activities. Built on top of plotly for rich, interactive visualizations.

## Features

- **WBS Hierarchy**: Organize projects with multi-level Work Breakdown Structure
- **Flexible Coloring**: Three color modes - WBS-based, uniform, or attribute-based (status, priority, etc.)
- **Interactive Charts**: Pan, zoom, and hover for detailed information
- **Dynamic Date Formatting**: Automatically adjusts time scale based on zoom level
- **Scrollable Views**: Handle large projects with vertical scrolling
- **Milestone Lines & Areas**: Add vertical date markers or shaded date-range areas with custom labels, label levels for stacking, and custom tooltips
- **Past Activity Dimming**: Optionally dim completed activities for better focus (via bar_config)
- **Dynamic Minimum Bar Width**: Automatically ensures short-duration activities remain visible at any zoom level by dynamically adjusting bar width
- **Flexible Display**: Show/hide WBS and activity names on bars (via display_config)
- **Custom Label Templates**: Customize labels with placeholders for dates, duration, IDs (via label_config)
- **Custom Tooltip Fields**: Add any column from your data to hover tooltips (via tooltip_config)
- **Planned vs Actual**: Show both planned and actual dates with visual distinction
- **Configuration-Based API**: Organized parameters into logical config objects for cleaner code

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
  color_config = list(mode = "wbs", wbs = test_project$colors)
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
  color_config = list(mode = "wbs", wbs = test_project$colors),
  chart_title = "My Project Schedule"
)
```

<img width="1011" height="627" alt="image" src="https://github.com/user-attachments/assets/3f4d9465-8468-418d-86c2-f00874075152" />


### Color Modes

#### WBS-Based Colors (Default)

```r
# Activities inherit colors from their parent WBS item
Ganttify(
  wbs_structure = test_project$wbs_structure,
  activities = test_project$activities,
  color_config = list(
    mode = "wbs",
    wbs = test_project$colors
  )
)
```

#### Uniform Colors

```r
# All activities one color, all WBS items one color
Ganttify(
  wbs_structure = test_project$wbs_structure,
  activities = test_project$activities,
  color_config = list(
    mode = "uniform",
    uniform = list(
      wbs = "#34495E",       # dark gray for WBS
      activity = "#2ECC71"   # green for activities
    )
  )
)
```

#### Attribute-Based Colors

```r
# Color activities by custom attribute (e.g., Status, Priority)
# First, add a status column to activities
activities_with_status <- test_project$activities
activities_with_status$Status <- c("completed", "in-progress", "not-started", ...)

Ganttify(
  wbs_structure = test_project$wbs_structure,
  activities = activities_with_status,
  color_config = list(
    mode = "attribute",
    attribute = list(
      column = "Status",
      mapping = list(
        "completed" = "#27AE60",     # green
        "in-progress" = "#F39C12",   # orange
        "not-started" = "#95A5A6",   # gray
        "stopped" = "#E74C3C"        # red
      ),
      wbs = "#34495E"  # dark gray for WBS
    )
  )
)
```

### Other Features

```r
# WBS-only view (hide activities using display_config)
Ganttify(
  wbs_structure = test_project$wbs_structure,
  activities = test_project$activities,
  display_config = list(activity = list(show = FALSE))
)

# Custom labels with date ranges
Ganttify(
  wbs_structure = test_project$wbs_structure,
  activities = test_project$activities,
  label_config = list(
    activity = list(yaxis = "{name} ({start} - {end})"),
    wbs = list(yaxis = "{name}")
  )
)

# Customize bar appearance and dim past activities
Ganttify(
  wbs_structure = test_project$wbs_structure,
  activities = test_project$activities,
  bar_config = list(
    wbs = list(opacity = 0.5, height = 0.4),
    activity = list(height = 0.9, dim_past_activities = TRUE, dim_opacity = 0.4)
  )
)

# Add "today" line and other milestones
milestones <- data.frame(
  date = c(Sys.Date(), "10/15/2024", "12/01/2024"),
  label = c("Today", "Review", "Release"),
  color = c("red", "blue", "green")
)

Ganttify(
  wbs_structure = test_project$wbs_structure,
  activities = test_project$activities,
  milestone_lines = milestones
)

# Add milestone areas (date ranges) - use list column for dates
milestones <- data.frame(
  label = c("Today", "Review Period", "Release"),
  color = c("red", "blue", "green"),
  fill_opacity = c(1, 0.15, 1)  # Lower opacity for areas
)
milestones$date <- list(
  Sys.Date(),                          # Single date = vertical line
  c("10/01/2024", "10/31/2024"),       # Two dates = shaded area
  "12/01/2024"                         # Single date = vertical line
)

Ganttify(
  wbs_structure = test_project$wbs_structure,
  activities = test_project$activities,
  milestone_lines = milestones
)

# Milestone label levels (prevent overlapping labels)
milestones <- data.frame(
  label = c("Kickoff", "Budget Approval", "Review Period", "Go Live"),
  color = c("blue", "green", "purple", "red"),
  label_level = c(1, 2, 1, 2)  # Level 1 appears above level 2
)
milestones$date <- list(
  "01/05/2025",
  "01/10/2025",
  c("02/10/2025", "02/20/2025"),
  "03/31/2025"
)

Ganttify(
  wbs_structure = test_project$wbs_structure,
  activities = test_project$activities,
  milestone_lines = milestones
)

# Hide level-1 milestone labels (text annotation hidden, hover tooltip still works)
milestones <- data.frame(
  label = c("Kickoff", "Budget Approval", "Review Period", "Go Live"),
  color = c("blue", "green", "purple", "red"),
  label_level = c(1, 2, 1, 2)
)
milestones$date <- list(
  "01/05/2025",
  "01/10/2025",
  c("02/10/2025", "02/20/2025"),
  "03/31/2025"
)

Ganttify(
  wbs_structure = test_project$wbs_structure,
  activities = test_project$activities,
  milestone_lines = milestones,
  display_config = list(
    milestone = list(hide_label_levels = c(1))
  )
)

# Milestone custom tooltips
milestones <- data.frame(
  label = c("Kickoff", "Review Period"),
  color = c("blue", "purple"),
  Description = c("Project kickoff meeting", "Technical design review"),
  Owner = c("Project Manager", "Tech Lead")
)
milestones$date <- list("01/05/2025", c("02/10/2025", "02/20/2025"))

Ganttify(
  wbs_structure = test_project$wbs_structure,
  activities = test_project$activities,
  milestone_lines = milestones,
  tooltip_config = list(
    milestone = c("Description", "Owner")
  )
)

# Hide y-axis labels (useful for presentations)
Ganttify(
  wbs_structure = test_project$wbs_structure,
  activities = test_project$activities,
  layout_config = list(show_yaxis_labels = FALSE)
)

# Adjust layout settings
Ganttify(
  wbs_structure = test_project$wbs_structure,
  activities = test_project$activities,
  layout_config = list(
    buffer_days = 60,
    indent_size = 4,
    max_visible_rows = 30
  )
)

# Narrow label area with automatic truncation
Ganttify(
  wbs_structure = test_project$wbs_structure,
  activities = test_project$activities,
  layout_config = list(
    yaxis_label_width = 200,
    yaxis_label_max_chars = 25
  )
)

# Control hover popup width with text wrapping
Ganttify(
  wbs_structure = test_project$wbs_structure,
  activities = test_project$activities,
  layout_config = list(
    hover_popup_max_chars = 30  # Wrap long text in popups at 30 chars
  )
)

# Add custom fields to hover tooltips
# First add custom columns to your data
activities_extended <- test_project$activities
activities_extended$Status <- c("On Track", "Delayed", "Complete", ...)
activities_extended$Agency <- "TTI"

wbs_extended <- test_project$wbs_structure
wbs_extended$Owner <- "Project Manager"

Ganttify(
  wbs_structure = wbs_extended,
  activities = activities_extended,
  tooltip_config = list(
    wbs = c("Owner"),
    activity = c("Status", "Agency")
  )
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

### Color Configuration

The `color_config` parameter controls all chart colors. Three modes available:

**1. WBS Mode (default)** - Activities inherit colors from parent WBS:
```r
color_config = list(
  mode = "wbs",
  wbs = list("W1" = "#FF6B6B", "W2" = "#4ECDC4")
)
```

**2. Uniform Mode** - All activities same color:
```r
color_config = list(
  mode = "uniform",
  uniform = list(wbs = "#34495E", activity = "#2ECC71")
)
```

**3. Attribute Mode** - Color by custom attribute:
```r
color_config = list(
  mode = "attribute",
  attribute = list(
    column = "Status",
    mapping = list("completed" = "green", "in-progress" = "orange"),
    wbs = "#34495E"
  )
)
```

## Parameters

Key parameters for the `Ganttify()` function:

### Core Data
- `wbs_structure`: Data frame with WBS hierarchy (ID, Name, Parent)
- `activities`: Data frame with activities (WBS_ID, Activity_ID, Activity_Name, Start_Date, End_Date)

### Configuration Objects
- `color_config`: List configuring chart colors (mode: "wbs", "uniform", or "attribute")
- `display_config`: List controlling visibility (WBS/activity show, labels, names on bars, milestone label hiding by level)
- `label_config`: List with label templates for y-axis and bars (supports placeholders)
- `bar_config`: List with bar styling (opacity, height, dim_opacity, dim_past_activities)
- `layout_config`: List with layout settings (buffer_days, indent_size, max_visible_rows, y_scroll_position, yaxis_label_width, yaxis_label_max_chars, hover_popup_max_chars, show_yaxis_labels)
- `tooltip_config`: List with custom tooltip fields (wbs: columns from wbs_structure, activity: columns from activities, milestone: columns from milestone_lines). Fields with NA/empty values are auto-hidden.

### Other Parameters
- `milestone_lines`: Data frame with milestone dates and labels. Use single date for vertical lines or two dates for shaded areas (via list column). Supports fill_opacity for area transparency, label_level (1 or 2) for vertical stacking, and custom columns for tooltips.
- `chart_title`: Chart title (default: "Project Gantt Chart with WBS")
- `x_range`: Date range for x-axis zoom

See `?Ganttify` for complete documentation.

## Saving Charts

```r
library(htmlwidgets)

# Create chart
chart <- Ganttify(
  wbs_structure = test_project$wbs_structure,
  activities = test_project$activities,
  color_config = list(mode = "wbs", wbs = test_project$colors)
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
