## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

* local Windows 11, R 4.5.2
* win-builder (devel and release)

## Downstream dependencies

There are currently no downstream dependencies for this package.

## Notes

This is a patch update (v0.2.2) that includes:

* Fix: Milestone vertical lines now span the full chart height during scrolling instead of stopping at the visible viewport boundary
* New: `display_config$milestone$hide_label_levels` to suppress milestone text annotations by level while preserving hover tooltips
