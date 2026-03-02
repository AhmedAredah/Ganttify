## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

* local Windows 11, R 4.5.2
* win-builder (devel and release)

## Downstream dependencies

There are currently no downstream dependencies for this package.

## Notes

This is a patch update (v0.2.4) that fixes milestone hover interaction:

* **Milestone vertical lines now show their hover tooltip along the entire line height**,
  not just at the top/bottom endpoints. The hover mechanism now uses a separate invisible
  marker trace distributed along the line (same pattern as area milestones).

* **Reduced hover detection distance** from 20px to 10px for tighter, more precise hover
  behavior. The hover now triggers only when very close to the milestone line (~2.5 days
  horizontally at typical zoom), preventing accidental triggers on nearby activities.

* No API changes — fully backward-compatible with all existing code.
