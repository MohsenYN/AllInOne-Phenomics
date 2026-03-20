# ============================================================
#  AllInOne Phenomics — Package Installer
#  Run this script ONCE before launching the app.
#  Source it: source("install_packages.R")
# ============================================================

# ── Helper ────────────────────────────────────────────────────
install_if_missing <- function(pkg, repo = "https://cloud.r-project.org") {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message("  Installing: ", pkg)
    tryCatch(
      install.packages(pkg, repos = repo, quiet = TRUE),
      error = function(e) message("  FAILED: ", pkg, " — ", e$message)
    )
  } else {
    message("  OK (already installed): ", pkg)
  }
}

# ── 1 / 5  Core Shiny ─────────────────────────────────────────
cat("\n=== 1/5  Core Shiny packages ===\n")
core <- c(
  "shiny", "shinydashboard", "shinyWidgets", "shinyjs",
  "DT", "htmltools", "shinymanager"
)
for (pkg in core) install_if_missing(pkg)

# ── 2 / 5  Spatial / Raster ───────────────────────────────────
cat("\n=== 2/5  Spatial packages ===\n")
spatial <- c(
  "terra", "sf", "stars", "raster", "sp"
)
for (pkg in spatial) install_if_missing(pkg)

# ── 3 / 5  Data & Visualisation ───────────────────────────────
cat("\n=== 3/5  Data & Visualisation packages ===\n")
viz <- c(
  "ggplot2", "plotly", "ggcorrplot", "viridis", "RColorBrewer",
  "dplyr", "tidyr", "reshape2", "gridExtra", "dendextend",
  "openxlsx", "readr", "tibble", "scales", "rlang",
  "base64enc", "digest", "httr"
)
for (pkg in viz) install_if_missing(pkg)

# ── 4 / 5  Machine Learning ───────────────────────────────────
cat("\n=== 4/5  Machine Learning packages ===\n")
ml <- c(
  "caret", "randomForest", "e1071", "gbm", "xgboost",
  "nnet", "glmnet", "kknn", "rpart", "naivebayes",
  "ipred", "MASS", "doParallel", "foreach", "pROC"
)
for (pkg in ml) install_if_missing(pkg)

# ── 5 / 5  Deployment ─────────────────────────────────────────
cat("\n=== 5/5  Deployment ===\n")
deploy <- c("rsconnect", "remotes")
for (pkg in deploy) install_if_missing(pkg)

# ── Optional: FIELDimageR from GitHub ─────────────────────────
cat("\n=== Optional: FIELDimageR (GitHub — app works without it) ===\n")
if (!requireNamespace("FIELDimageR", quietly = TRUE)) {
  tryCatch({
    remotes::install_github("OpenDroneMap/FIELDimageR",       upgrade = "never")
    remotes::install_github("OpenDroneMap/FIELDimageR.Extra", upgrade = "never")
    cat("  OK  FIELDimageR installed from GitHub\n")
  }, error = function(e) {
    cat("  SKIP  FIELDimageR not available — app runs fine without it\n")
  })
} else {
  cat("  OK  FIELDimageR already installed\n")
}

# ── Verification ───────────────────────────────────────────────
cat("\n=== Verifying core packages ===\n")
required <- c(
  "shiny", "shinydashboard", "shinyWidgets", "shinyjs", "shinymanager",
  "DT", "terra", "sf", "ggplot2", "plotly", "ggcorrplot",
  "viridis", "RColorBrewer", "dplyr", "tidyr", "openxlsx",
  "dendextend", "base64enc", "digest", "httr", "caret", "randomForest"
)
missing_v <- required[!sapply(required, requireNamespace, quietly = TRUE)]

if (length(missing_v) == 0) {
  cat("\n✅ All packages verified — ready to run!\n\n")
  cat("   shiny::runApp('app.R')\n")
  cat("   OR\n")
  cat("   shiny::runGitHub('AllInOne-Phenomics', 'MohsenYN')\n\n")
} else {
  cat(sprintf("\n⚠️  %d package(s) still missing:\n", length(missing_v)))
  for (p in missing_v) cat(sprintf("   install.packages('%s')\n", p))
  cat("\nRe-run this script or install the above packages manually.\n")
}
