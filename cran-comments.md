## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

* local Windows 11, R 4.5.2
* win-builder (devel and release)

## Downstream dependencies

There are currently no downstream dependencies for this package.

## Notes

This is a patch update (v0.2.5) adding custom display labels for tooltip fields:

* **tooltip_config now accepts named character vectors** so users can control the label
  shown for each custom field in hover tooltips. Use `c(column_name = "Display Label")`
  syntax to display "Activity Details" instead of the raw column name "activity_details".

* Fully backward-compatible — existing `tooltip_config` usage with plain character vectors
  continues to work unchanged. Named elements can be mixed with unnamed elements.
