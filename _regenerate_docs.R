# Full CRAN build pipeline for ganttify
# Run from within RStudio or R console with the package root as working directory

# Step 1 — Read version from DESCRIPTION (no install needed)
version <- read.dcf("DESCRIPTION", fields = "Version")[1, 1]
cat("=== ganttify", version, "— Full Build Pipeline ===\n\n")

# Step 2 — Regenerate documentation (Roxygen -> man/*.Rd + NAMESPACE)
cat("Step 1: Regenerating documentation...\n")
devtools::document()
cat("✓ Documentation regenerated.\n\n")

# Step 3 — Run full R CMD check (CRAN-style: cran = TRUE, remote checks included)
cat("Step 2: Running R CMD check (CRAN mode)...\n")
check_results <- withCallingHandlers(
  devtools::check(cran = TRUE, error_on = "warning"),
  warning = function(w) {
    # Suppress the quarto Windows system2 bug: env= is prepended as a CLI
    # argument on Windows, making quarto receive "TMPDIR=..." as a command.
    # This is a known bug in the quarto R package and does not affect the
    # package being checked (ganttify has no quarto dependency).
    if (grepl("TMPDIR", conditionMessage(w), fixed = TRUE)) {
      invokeRestart("muffleWarning")
    }
  }
)
cat("✓ R CMD check complete.\n\n")

# Step 4 — Build source tarball
cat("Step 3: Building source tarball...\n")
tarball <- devtools::build()
cat("✓ Built:", tarball, "\n\n")

# Step 5 — Summary
cat("=== Build Pipeline Complete ===\n")
cat("Package : ganttify\n")
cat("Version :", version, "\n")
cat("Tarball :", tarball, "\n\n")

# Step 6 — Optionally submit to CRAN
answer <- readline("Submit to CRAN? [y/N]: ")
if (tolower(trimws(answer)) == "y") {
  cat("\nStep 4: Submitting to CRAN...\n")
  cat("Note: Your pre-built tarball is at:", tarball, "\n\n")
  devtools::submit_cran()
  cat("✓ Submitted to CRAN. Watch your email for confirmation.\n")
} else {
  cat("Skipped CRAN submission. Tarball is ready at:", tarball, "\n")
}
