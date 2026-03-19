# ============================================================
#  INSTALL ALL PACKAGES
#  Dry Bean Breeding & Computational Biology — Univ. of Guelph
#  Run this script ONCE in RStudio before launching the app.
#  Installs every dependency including all caret ML backends.
# ============================================================

repo <- "https://cloud.r-project.org"
ok   <- character(0)
fail <- character(0)

install_if_missing <- function(pkg, repos = repo) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat(sprintf("  Installing %-22s ...", pkg))
    tryCatch({
      install.packages(pkg, repos = repos, quiet = TRUE)
      if (requireNamespace(pkg, quietly = TRUE)) {
        cat(" OK\n"); ok <<- c(ok, pkg)
      } else {
        cat(" FAILED\n"); fail <<- c(fail, pkg)
      }
    }, error = function(e) {
      cat(sprintf(" ERROR: %s\n", e$message)); fail <<- c(fail, pkg)
    })
  } else {
    cat(sprintf("  OK  %-24s (already installed)\n", pkg))
    ok <<- c(ok, pkg)
  }
}

# ---- 1. Core Shiny & UI -----------------------------------------
cat("\n=== 1/5  Core UI packages ===\n")
for (pkg in c("shiny","shinydashboard","shinyWidgets","shinyjs",
              "DT","htmltools","htmlwidgets"))
  install_if_missing(pkg)

# ---- 2. Visualisation ------------------------------------------
cat("\n=== 2/5  Visualisation packages ===\n")
for (pkg in c("plotly","ggplot2","ggcorrplot","viridis",
              "RColorBrewer","scales","gridExtra","reshape2","dendextend"))
  install_if_missing(pkg)

# ---- 3. Spatial & data -----------------------------------------
cat("\n=== 3/5  Spatial & data packages ===\n")
for (pkg in c("terra","sf","dplyr","tidyr","rlang",
              "openxlsx","MASS"))
  install_if_missing(pkg)

# ---- 4. Machine Learning: caret + ALL backends -----------------
cat("\n=== 4/5  Machine Learning packages ===\n")

# caret core (installs many deps automatically)
install_if_missing("caret")

# Algorithm backends
for (pkg in c(
  "randomForest",   # rf
  "e1071",          # svmRadial, svmLinear
  "gbm",            # gbm
  "xgboost",        # xgbTree
  "Matrix",         # xgboost / glmnet dep
  "nnet",           # nnet
  "glmnet",         # glmnet
  "kknn",           # kknn
  "rpart",          # rpart
  "naivebayes",     # naive_bayes
  "ipred",          # treebag
  # caret runtime deps that may not auto-install
  "ModelMetrics",
  "recipes",
  "hardhat",
  "gower",
  "prodlim",
  "lava",
  "timeDate",
  "pROC",
  "plyr",
  "foreach",
  "iterators",
  "survival",
  "kernlab",
  "base64enc"
)) install_if_missing(pkg)

# ---- 5. Deployment ---------------------------------------------
cat("\n=== 5/5  Deployment ===\n")
install_if_missing("rsconnect")

# ---- Optional: FIELDimageR from GitHub -------------------------
cat("\n=== Optional: FIELDimageR (GitHub) ===\n")
if (!requireNamespace("FIELDimageR", quietly=TRUE)) {
  install_if_missing("remotes")
  tryCatch({
    remotes::install_github("OpenDroneMap/FIELDimageR",       upgrade="never")
    remotes::install_github("OpenDroneMap/FIELDimageR.Extra", upgrade="never")
    cat("  OK  FIELDimageR (GitHub)\n")
  }, error=function(e) cat("  SKIP FIELDimageR — app runs without it\n"))
} else {
  cat("  OK  FIELDimageR already installed\n")
}

# ---- Verification ----------------------------------------------
cat("\n=== Verification ===\n")
must_have <- c(
  "shiny","shinydashboard","shinyWidgets","shinyjs",
  "DT","plotly","ggplot2","dplyr","tidyr","viridis",
  "terra","sf","openxlsx","ggcorrplot","scales",
  "RColorBrewer","reshape2","gridExtra","rlang","dendextend",
  "caret","randomForest","e1071","gbm","xgboost",
  "nnet","glmnet","kknn","rpart","naivebayes","ipred","MASS",
  "Matrix","ModelMetrics","pROC","plyr","foreach","rsconnect"
)
missing_v <- must_have[!sapply(must_have, requireNamespace, quietly=TRUE)]

cat("\n")
if (!length(missing_v)) {
  cat("All packages verified — ready to run!\n\n")
  cat("  shiny::runApp('app.R')\n\n")
} else {
  cat(sprintf("WARNING: %d package(s) still missing:\n", length(missing_v)))
  for (p in missing_v) cat(sprintf("  install.packages('%s')\n", p))
  cat("\nRe-run this script or install above manually.\n")
}

# ---- Publishing instructions -----------------------------------
cat("
=== To publish on shinyapps.io ===

Step 1 — sign up at https://www.shinyapps.io and copy your token.

Step 2 — run once in RStudio:
  library(rsconnect)
  rsconnect::setAccountInfo(
    name   = 'YOUR_ACCOUNT_NAME',
    token  = 'YOUR_TOKEN',
    secret = 'YOUR_SECRET'
  )

Step 3 — deploy:
  rsconnect::deployApp(
    appDir  = 'C:/path/to/DrybeanDroneApp',
    appName = 'DrybeanDroneApp'
  )

Notes:
  - Free tier: 25 active hours/month, 1 GB RAM, 1 GB storage.
  - Large .tif uploads may time out on free tier; use Starter plan
    for field-scale imagery.
  - terra and sf need system GDAL libs; shinyapps.io installs
    these automatically for CRAN packages.
  - The DESCRIPTION file in the app folder tells rsconnect
    exactly which packages to install on the server.
")
