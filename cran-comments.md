## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

* local Windows 11, R 4.5.2
* win-builder (devel and release)

## Downstream dependencies

There are currently no downstream dependencies for this package.

## Notes

This is a patch update (v0.2.3) that improves date parsing flexibility:

* Dates now accept both ISO YYYY-MM-DD and legacy MM/DD/YYYY formats, as well as Date
  class objects. Fully backward-compatible with existing user data.
* format_label() renders date placeholders in ISO YYYY-MM-DD.
* Sample dataset updated to use ISO date strings.
