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
