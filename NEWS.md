# ganttify 0.2.2

## Bug Fix

* **Milestone Lines Extend Full Height**: Milestone vertical lines now span the entire chart height instead of stopping at the visible viewport boundary when scrolling through rows. Lines drawn with `y = c(0.5, total_rows + 0.5)` for consistent visibility across the full chart.

## New Features

* **Hide Milestone Labels by Level**: Added `display_config$milestone$hide_label_levels` parameter to suppress text annotations for milestones at specific `label_level` values while keeping hover tooltips intact. For example, set to `c(1)` to hide all level-1 milestone labels. Default `NULL` shows all labels (fully backwards-compatible).

---

# ganttify 0.2.1

## New Features

* **Milestone Label Levels**: Added `label_level` parameter to milestone markers for vertical stacking of labels. Set to 1 or 2 to control which labels appear above others when milestones are close together.

```r
milestones <- data.frame(
  label = c("Kickoff", "Budget Approval"),
  color = c("blue", "green"),
  label_level = c(1, 2)  # Level 1 appears above level 2
)
milestones$date <- list("01/05/2025", "01/10/2025")
```

* **Milestone Custom Tooltips**: Added support for custom tooltip fields in milestones via `tooltip_config$milestone`. Add any columns to your milestone data frame and display them in hover tooltips.

```r
milestones <- data.frame(
  label = c("Kickoff", "Review Period"),
  color = c("blue", "purple"),
  Description = c("Project kickoff meeting", "Technical review"),
  Owner = c("PM", "Tech Lead")
)
milestones$date <- list("01/05/2025", c("02/10/2025", "02/20/2025"))

Ganttify(
  ...,
  milestone_lines = milestones,
  tooltip_config = list(
    milestone = c("Description", "Owner")
  )
)
```

## Improvements

* Fixed hover functionality for milestone areas (shaded date ranges) - now triggers at the label position (top/middle/bottom edge) for consistency with line milestones.

# ganttify 0.2.0

## New Features

* **Milestone Areas**: Milestone markers now support date ranges in addition to single dates. Use a list column for the `date` field to specify either a single date (vertical line) or two dates (shaded area).

```r
milestones <- data.frame(
  label = c("Deadline", "Review Period"),
  color = c("red", "blue"),
  fill_opacity = c(1, 0.15)
)
milestones$date <- list(
  "12/01/2024",                    # Single date = line
  c("10/01/2024", "10/31/2024")    # Two dates = shaded area
)
```

* **Y-Axis Label Visibility**: Added `show_yaxis_labels` to `layout_config` to hide y-axis labels. When set to `FALSE`, activity labels are hidden. If `display_config$wbs$show_labels` is `TRUE`, WBS labels will still be shown.

```r
layout_config = list(show_yaxis_labels = FALSE)
```

## Improvements

* Narrow milestone date ranges are automatically converted to vertical lines (uses same 0.3% threshold as activity bars).
* Fixed milestone area opacity by using Plotly's separate `opacity` parameter instead of rgba.

# ganttify 0.1.8

## New Features

* **Custom Tooltip Fields**: Added `tooltip_config` parameter to display custom columns from your data in hover tooltips. Specify which columns from `wbs_structure` and `activities` to show. Fields that don't exist or have empty/NA values are automatically hidden.

```r
tooltip_config = list(
  wbs = c("Owner", "Budget"),
  activity = c("Status", "Agency", "Priority")
)
```

# ganttify 0.1.7

## New Features

* **Dynamic Minimum Bar Width**: Activities and WBS items with very short durations now remain visible at any zoom level. The chart dynamically adjusts bar widths when zooming to ensure all items are visible, while preserving original dates in hover tooltips.

## Improvements

* Switched from `hovertext` to `hovertemplate` for more consistent hover behavior across all bar types.
* Removed deprecated `short_activity_indicator` parameter (replaced by automatic dynamic bar width).

## Bug Fixes

* Fixed hover tooltips showing incorrect format on dynamically resized bars.

# ganttify 0.1.6

## Bug Fixes

* Fixed column name change bug.

## Improvements

* Added `hover_popup_max_chars` parameter to control text wrapping in hover popups.

# ganttify 0.1.5

## New Features

* Added `yaxis_label_width` parameter to control y-axis label area width.
* Added `yaxis_label_max_chars` parameter for automatic label truncation.

# ganttify 0.1.4

## New Features

* Added milestone lines support with customizable labels, colors, and positions.
* Added "today" line capability using `Sys.Date()` in milestone_lines.

# ganttify 0.1.3

## New Features

* Added planned vs actual dates visualization with stacked bars.
* Added diagonal stripe pattern for actual date bars.

# ganttify 0.1.2

## New Features

* Added `color_config` parameter with three modes: "wbs", "uniform", and "attribute".
* Added `bar_config` parameter for bar styling (opacity, height, dim_past_activities).
* Added `display_config` parameter to control visibility of WBS/activities.
* Added `label_config` parameter for custom label templates.

# ganttify 0.1.1

## Improvements

* Added dynamic date formatting based on zoom level.
* Added alternating background colors for time periods.

# ganttify 0.1.0

## Initial Release

* Create interactive Gantt charts with WBS hierarchy.
* WBS-based color inheritance for activities.
* Scrollable views for large projects.
* Pan and zoom support.
* Hover tooltips with activity details.
