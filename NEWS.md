# ganttify 0.2.10

## Improvements

* **Long y-axis labels are now truncated with an ellipsis (instead of
  right-aligned) and reveal their full text on hover**: When
  `show_yaxis_labels = TRUE`, any tick label too long to fit the reserved left
  gutter previously fell back to plotly's native right-alignment, which kept the
  label inside the gutter but collapsed its leading indentation and hid the WBS
  hierarchy for that row.

  The `onRender` alignment handler now **left-aligns every label** and, for
  labels that do not fit, **truncates the trailing content and appends an
  ellipsis (`…`)** while preserving the leading whitespace that encodes the WBS
  depth. As a result the parent/child hierarchy stays visible for *all* rows,
  not just the ones short enough to fit — truncation is a strictly better
  fallback than right-alignment. The fit is computed with a bounded binary
  search over the rendered text width (`getComputedTextLength()`), guarded
  against the degenerate case where even `indent + …` overflows.

  The original, untruncated label text is cached in a `data-full-label`
  attribute before any mutation, so re-runs on pan/zoom relayout always truncate
  from the original rather than from already-truncated text.

  **Hover popup + accessibility**: hovering a truncated label shows a small,
  self-contained popup (a custom absolutely-positioned div appended to the
  widget) with the full untruncated text, and each truncated label also carries
  an `aria-label` with the full text for screen readers. The popup is wired
  through a single set of delegated listeners bound once on the widget container
  (guarded so the per-relayout re-run never stacks duplicate listeners), and is
  hidden cleanly when the pointer leaves a label.

# ganttify 0.2.9

## Bug Fix

* **Y-axis labels are now left-aligned to preserve the WBS hierarchy, without
  overlapping the bars**: When `show_yaxis_labels = TRUE`, the activity tick
  labels were rendering inside the plot area, to the right of the y-axis line,
  overlapping the activity bars. The `onRender` alignment handler was forcing
  SVG `text-anchor='start'` on each left-axis label without shifting its `x`
  coordinate, so the text grew rightward from the axis into the bars.

  The handler now LEFT-ALIGNS each label flush to the left edge of the reserved
  gutter (`text-anchor='start'` with a small constant left pad). Because the WBS
  hierarchy is encoded as leading indentation/whitespace in each label, starting
  every label at the same left edge makes that indentation — and therefore the
  parent/child hierarchy — visible again. (Right-aligning the labels flush to the
  axis, as a previous interim fix did, removed the overlap but collapsed the
  indentation and hid the hierarchy.)

  A per-label collision guard measures each rendered label with
  `getComputedTextLength()`. Any label that would cross the axis line
  (`x = effective_label_width`) into the plot area is degraded gracefully by
  falling back to plotly's native right-alignment (`text-anchor='end'`, anchored
  at the axis), so it stays flush to the axis and never overlaps the bars. Since
  the gutter is sized to fit the longest label, virtually all labels fit
  left-aligned; the fallback is a safety net for pathological edge cases.
  Alignment is applied on initial render and re-applied after every pan/zoom
  relayout; if text is not yet laid out (measurement returns 0) it is retried on
  the next animation frame.

# ganttify 0.2.7

## Improvements

* **Activity-bar click identification**: Each activity bar's plotly `customdata`
  now carries an additional `activity_id` field (the value of the `Activity_ID`
  column for that bar), alongside the existing `type`, `original_start`, and
  `original_end` keys. This lets consuming apps resolve a clicked bar back to its
  source record via `event_data("plotly_click")$customdata`. The change is purely
  additive — existing `customdata` keys are unchanged, so the bar-width JavaScript
  (which reads `type`/`original_start`/`original_end` by name) is unaffected.
  Applies to all three activity-bar traces: planned, actual, and single bars.

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
