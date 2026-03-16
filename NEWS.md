# ganttify 0.2.6

## Improvements

* **Cleaner hover tooltips**: Removed redundant "Type: WBS", "Type: Activity", and
  "Type: Milestone" lines from all hover popups. The chart element type is already
  visually apparent from bar shape and color.

* **Bold tooltip labels**: All field labels in hover popups are now bold
  for improved scanability — applies to both built-in fields (Start, End, Duration,
  Variance, Date) and user-provided custom fields via `tooltip_config`.

* **Variance spacing**: Added a blank separator line after the Variance row in
  activity tooltips that show both planned and actual bars, visually separating the
  core date fields from any custom tooltip fields that follow.

* **X-axis position control**: New `xaxis_position` key in `layout_config` lets
  users place the time axis at `"bottom"` (default), `"top"`, or `"both"`. Use
  `"top"` to keep the date axis visible when scrolling through long charts, or
  `"both"` to show tick labels on both edges simultaneously.

## Documentation

* Updated `tooltip_config` parameter description: removed stale mention of "Type"
  from the list of default tooltip fields.

* Extended `@examples` to demonstrate `tooltip_config$milestone` — showing how to
  add a custom column to `milestone_lines` and display it in hover tooltips.

## Bug Fix

* **Actual bar tooltip was missing**: Hovering over the actual (lower) bar of a
  stacked planned/actual activity pair showed no tooltip. The actual bar trace had
  `hoverinfo = "skip"` — changed to show the same full tooltip (planned + actual
  dates) as the planned bar.

---

# ganttify 0.2.5

## New Feature

* **Custom tooltip field labels**: `tooltip_config` now supports named character vectors
  to set friendly display labels for custom tooltip fields. Use
  `c(column_name = "Display Label")` instead of `c("column_name")` to control what
  label appears in the hover tooltip. Unnamed elements continue to use the column name
  as the label (fully backward-compatible).

---

# ganttify 0.2.4

## Bug Fix

* **Milestone line hover now works along the full line height**: Previously, hover tooltips on single-date milestone vertical lines only triggered near the top or bottom endpoints. The hover mechanism is now a separate invisible marker trace distributed along the full height of the line, making the hover responsive anywhere along the line. Applied to both single-date vertical lines and narrow date-range milestones that fall back to vertical lines.

* **Reduced hover sensitivity for milestone lines**: Lowered the hover detection distance from 20px to 10px, making the hover trigger only when the cursor is very close to the milestone line (within ~2.5 days at typical chart zoom), preventing accidental tooltip triggers when hovering nearby activities.

---

# ganttify 0.2.3

## Performance

* **Optimized date parsing**: Dates now default to `MM/DD/YYYY` format with single-pass parsing.
  Dramatically faster on large datasets. Users can specify alternative format via `date_format` parameter.

## Improvements

* **Flexible Date Formats**: Activity and milestone dates accept `MM/DD/YYYY` (default, e.g. `"09/15/2024"`),
  `Date` class objects, or custom formats via optional `date_format` parameter (e.g. `"%Y-%m-%d"` for ISO).
* `parse_date_flex()` helper with configurable `date_format` parameter (default: `"%m/%d/%Y"`).
* Guard in `generate_hover_points()` handles reversed dates gracefully (End < Start) without crashing.
* `format_label()` renders date placeholders in `MM/DD/YYYY`.
* Sample dataset (`test_project`) uses `MM/DD/YYYY` date strings.

---

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
