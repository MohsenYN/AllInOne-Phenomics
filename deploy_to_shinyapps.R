# ============================================================
#  DEPLOY TO SHINYAPPS.IO — AllInOne Phenomics
#  Fill in your credentials below, then source this file.
# ============================================================

library(rsconnect)

# ── Step 1: Set credentials ────────────────────────────────
# Get from: https://www.shinyapps.io → Account → Tokens
rsconnect::setAccountInfo(
  name   = "YOUR_ACCOUNT_NAME",   # <-- replace
  token  = "YOUR_TOKEN",          # <-- replace
  secret = "YOUR_SECRET"          # <-- replace
)

# ── Step 2: Deploy (only app.R + DESCRIPTION + www/) ───────
# We explicitly list files to prevent rsconnect scanning
# the helper R scripts for extra dependencies.
rsconnect::deployApp(
  appDir     = getwd(),
  appName    = "AllInOne-Phenomics",
  appTitle   = "AllInOne Phenomics",
  appFiles   = c(
    "app.R",
    "DESCRIPTION",
    "www/logo.png"
  ),
  forceUpdate    = TRUE,
  launch.browser = TRUE
)

# After deploy, your app is at:
# https://YOUR_ACCOUNT_NAME.shinyapps.io/AllInOne-Phenomics
#
# To view logs if something goes wrong:
# rsconnect::showLogs("AllInOne-Phenomics")
