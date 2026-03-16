## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

* local Windows 11, R 4.5.2
* win-builder (devel and release)

## Downstream dependencies

There are currently no downstream dependencies for this package.

## Notes

This is a patch update (v0.2.6) with hover tooltip UX improvements:

* **Removed type labels**: "Type: WBS", "Type: Activity", and "Type: Milestone"
  lines removed from all hover tooltips — the element type is visually clear from
  chart appearance.

* **Bold field labels**: All tooltip labels (Start, End, Duration, Variance, Date,
  and user-provided custom fields) are now rendered bold for improved readability.

* **Variance separator**: A blank line is added after the Variance row in
  activity tooltips that show both planned and actual dates.

* **Documentation**: Updated `tooltip_config` parameter description and extended
  `@examples` to include a milestone custom tooltip demonstration.

* **Bug fix — actual bar hover**: Hovering the actual (lower) bar in stacked
  planned/actual activity pairs now shows the tooltip. Previously suppressed by
  `hoverinfo = "skip"`.

* **X-axis position**: New `layout_config$xaxis_position` option (`"bottom"`,
  `"top"`, `"both"`) controls where the time axis appears.

All changes are fully backward-compatible — no API changes.
