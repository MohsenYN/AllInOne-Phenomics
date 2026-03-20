# ============================================================
#  AllInOne Phenomics
#  Drone Image Analysis Platform
#  University of Guelph
#  ============================================================
#  Supports: RGB & Multispectral drone imagery
#  20+ built-in vegetation indices + custom formula builder
#  Spatial field maps, stats, correlation, PCA, clustering
#  Full Excel export with multiple sheets
# ============================================================

# ── Authentication ─────────────────────────────────────────────
# ── Authentication ─────────────────────────────────────────────
# Passwords are stored as SHA-256 hashes — safe to publish on GitHub.
# Nobody can read the real passwords from the hash.
#
# TO CHANGE A PASSWORD:
#   1. Open R and run:  digest::digest("NewPassword", algo="sha256")
#   2. Copy the hash string into the password field below.
#
# TO ADD A USER: copy one of the user rows and change the username & hash.
# TO REMOVE A USER: delete that row.
# TO DISABLE AUTH: set USE_AUTH <- FALSE

# Install shinymanager if needed
if (!requireNamespace("shinymanager", quietly=TRUE))
  tryCatch(install.packages("shinymanager", repos="https://cloud.r-project.org"),
           error=function(e) NULL)

USE_AUTH <- requireNamespace("shinymanager", quietly=TRUE)

if (USE_AUTH) {
  library(shinymanager)
  if (!requireNamespace('digest',quietly=TRUE)) install.packages('digest')
  library(digest)

  # ── USER CREDENTIALS ─────────────────────────────────────────
  # Passwords are SHA-256 hashes. Current passwords are shown in comments
  # but only the hash is stored — the comment is for your reference only.
  # Delete the comment once you change the password!
  #
  # Generate a new hash in R:
  #   digest::digest("YourNewPassword", algo="sha256")

  # Passwords stored as SHA-256 hashes (safe on GitHub)
  # Generate hash: digest::digest("YourPassword", algo="sha256")
  creds <- data.frame(
    user = c(
      "MYN",
      "DBBCB",
      "Public"
    ),
    password = c(
      "01b9473f7b2196a5f4a1742590147a97421afff9f32f89bf4d0c955721db67ca",
      "4a77b160b56795298e26e4e45a9680761c750736336cb8d7880d11f68003141e",
      "fb1f19e69d1ca0a166ce8228eb11515461474ed1cd24ee96f8cf190744eec5c1"
    ),
    admin    = c(TRUE, FALSE, FALSE),
    stringsAsFactors = FALSE
  )

  message("✅ Authentication enabled — ", nrow(creds), " users configured.")
} else {
  creds    <- NULL
  USE_AUTH <- FALSE
  message("⚠️  shinymanager not available — running without authentication.")
}

# ── Upload limit (4 GB local; shinyapps.io caps at ~1 GB) ────
options(shiny.maxRequestSize = 4 * 1024^3)

# Shiny automatically serves files in www/ at the root URL.
# logo.png in www/ is accessed as src="logo.png" — no addResourcePath needed.

# ── Core packages ─────────────────────────────────────────────
# On shinyapps.io these are auto-installed from DESCRIPTION.
# Locally, run install_packages.R once first.
core_pkgs <- c(
  "shiny", "shinydashboard", "shinyWidgets", "shinyjs",
  "DT", "plotly", "ggplot2", "dplyr", "tidyr", "viridis",
  "terra", "sf", "openxlsx", "ggcorrplot", "scales",
  "RColorBrewer", "reshape2", "gridExtra", "rlang"
)
for (pkg in core_pkgs)
  suppressPackageStartupMessages(
    library(pkg, character.only = TRUE, quietly = TRUE))

# ── ML packages (loaded on demand during training) ────────────
ml_pkgs <- c(
  "caret", "randomForest", "e1071", "gbm", "xgboost",
  "nnet", "glmnet", "kknn", "rpart", "naivebayes", "ipred", "MASS",
  "base64enc"
)
for (pkg in ml_pkgs)
  suppressPackageStartupMessages(
    tryCatch(library(pkg, character.only = TRUE, quietly = TRUE),
             error = function(e) message("Optional ML pkg missing: ", pkg)))

# ── Optional extras ───────────────────────────────────────────
for (pkg in c("dendextend", "FIELDimageR", "FIELDimageR.Extra"))
  suppressPackageStartupMessages(
    tryCatch(library(pkg, character.only = TRUE, quietly = TRUE),
             error = function(e) NULL))

# ── Colour Palette ─────────────────────────────────────────────
C_RED    <- "#CC0000"
C_DARK   <- "#1a1a2e"
C_GOLD   <- "#FFC72C"
C_GREEN  <- "#2d6a4f"
C_LIGHT  <- "#f8f6f0"
C_BODY   <- "#f2f0eb"

# ── Default Index Libraries ────────────────────────────────────
RGB_IDX <- list(
  list(name="NDVI",   formula="(NIR-Red)/(NIR+Red)",              desc="NDVI (requires NIR band)"),
  list(name="GNDVI",  formula="(NIR-Green)/(NIR+Green)",          desc="Green NDVI (requires NIR band)"),
  list(name="CCI",    formula="(Red-Blue)/Red",                   desc="Color Contrast Index"),
  list(name="EGVI",   formula="2*Green-Red-Blue",                 desc="Excess Green Vegetation Index"),
  list(name="ERVI",   formula="(1.4*Red)-Green",                  desc="Excess Red Vegetation Index"),
  list(name="GBI",    formula="Green/Blue",                       desc="Green-Blue Index"),
  list(name="GD",     formula="Green-(Red+Blue)/2",               desc="Green Difference Index"),
  list(name="GLAI",   formula="(25*(Green-Red)/(Green+Red-Blue))+1.25", desc="Green Leaf Area Index"),
  list(name="GR",     formula="Green/Red",                        desc="Green-Red Index"),
  list(name="MGVRI",  formula="(Green^2-Red^2)/(Green^2+Red^2)", desc="Modified Green Vegetation Ratio Index"),
  list(name="NB",     formula="Blue/(Red+Green+Blue)",            desc="Normalized Blue Index"),
  list(name="NG",     formula="Green/(Red+Green+Blue)",           desc="Normalized Green Index"),
  list(name="NGBDI",  formula="(Green-Blue)/(Green+Blue)",        desc="Norm. Green-Blue Difference Index"),
  list(name="NR",     formula="Red/(Red+Green+Blue)",             desc="Normalized Red Index"),
  list(name="RB",     formula="Red/Blue",                         desc="Red-Blue Index"),
  list(name="RI",     formula="(Red^2)/(Blue*Green^3)",           desc="Redness Index"),
  list(name="SAVIrgb",formula="(1+0.5)*(Green-Red)/(Green+Red+0.5)", desc="Soil Adjusted VI (RGB)"),
  list(name="NGRDI",  formula="(Green-Red)/(Green+Red)",          desc="Norm. Green-Red Difference Index"),
  list(name="BGI",    formula="(Green-Blue)/(Green+Blue)",        desc="Blue-Green Index"),
  list(name="RBG",    formula="(Red-Blue)/Green",                 desc="Red-Blue / Green"),
  list(name="RGR",    formula="Red/Green",                        desc="Red-to-Green Ratio")
)

MS_IDX <- list(
  list(name="NDVI",   formula="(NIR-Red)/(NIR+Red)",                    desc="Normalized Difference Vegetation Index"),
  list(name="NDRE",   formula="(NIR-RE)/(NIR+RE)",                      desc="Normalized Difference Red Edge"),
  list(name="GNDVI",  formula="(NIR-Green)/(NIR+Green)",                desc="Green NDVI"),
  list(name="SAVI",   formula="1.5*(NIR-Red)/(NIR+Red+0.5)",            desc="Soil Adjusted Vegetation Index"),
  list(name="EVI",    formula="2.5*(NIR-Red)/(NIR+6*Red-7.5*Blue+1)",   desc="Enhanced Vegetation Index"),
  list(name="RVI",    formula="NIR/Red",                                 desc="Ratio Vegetation Index"),
  list(name="OSAVI",  formula="(NIR-Red)/(NIR+Red+0.16)",               desc="Optimized SAVI"),
  list(name="CIre",   formula="(NIR/RE)-1",                             desc="Chlorophyll Index Red Edge"),
  list(name="MSAVI",  formula="(2*NIR+1-sqrt((2*NIR+1)^2-8*(NIR-Red)))/2", desc="Modified SAVI"),
  list(name="RDVI",   formula="(NIR-Red)/sqrt(NIR+Red)",                desc="Renormalized Difference VI"),
  list(name="MCARI",  formula="((RE-Red)-0.2*(RE-Green))*(RE/Red)",     desc="Modified Chlorophyll Absorption Ratio Index"),
  list(name="WDRVI",  formula="(0.2*NIR-Red)/(0.2*NIR+Red)",           desc="Wide Dynamic Range VI")
)

idx_df_to_show <- function(lst) {
  data.frame(
    Index       = sapply(lst, `[[`, "name"),
    Formula     = sapply(lst, `[[`, "formula"),
    Description = sapply(lst, `[[`, "desc"),
    stringsAsFactors = FALSE
  )
}

# ── Custom CSS ─────────────────────────────────────────────────
APP_CSS <- "
@import url('https://fonts.googleapis.com/css2?family=Playfair+Display:wght@600;700&family=Source+Sans+Pro:wght@300;400;600&display=swap');

* { box-sizing: border-box; }
body, .content-wrapper { background: #f2f0eb !important; font-family: 'Source Sans Pro', sans-serif; }

/* ── Sidebar ── */
.main-sidebar { background: #1a1a2e !important; border-right: 1px solid rgba(255,199,44,0.15); }
.sidebar-menu > li > a {
  color: #c8c8d8 !important; font-size: 13px; font-weight: 400;
  padding: 10px 20px; transition: all 0.25s; border-left: 3px solid transparent;
}
.sidebar-menu > li > a:hover {
  background: rgba(255,199,44,0.08) !important; color: #FFC72C !important;
  border-left-color: #FFC72C;
}
.sidebar-menu > li.active > a {
  background: rgba(204,0,0,0.18) !important; color: #FFC72C !important;
  border-left-color: #CC0000 !important;
}
.sidebar-menu .fa { color: #FFC72C !important; width: 18px; }
.sidebar { padding-bottom: 20px; }

/* ── Header ── */
.main-header .navbar { background: #1a1a2e !important; border-bottom: 2px solid #CC0000; }
.main-header .logo {
  background: #CC0000 !important;
  font-family: 'Playfair Display', serif !important;
  color: #fff !important; font-size: 13px; letter-spacing: 0.5px;
  border-bottom: 2px solid #8B0000;
}
.main-header .logo:hover { background: #8B0000 !important; }
.navbar-custom-menu .navbar-nav > li > a { color: #aaa !important; }

/* ── Boxes ── */
.box {
  border-radius: 6px; border-top: none !important;
  box-shadow: 0 2px 12px rgba(0,0,0,0.08); background: #fff;
  animation: fadeSlideIn 0.35s ease both;
  overflow: visible !important;
  position: relative;
}
.box-header {
  border-radius: 6px 6px 0 0; background: #1a1a2e !important;
  color: #fff; padding: 10px 18px; border-bottom: 2px solid #CC0000;
}
.box-header .box-title {
  font-family: 'Playfair Display', serif;
  color: #FFC72C !important; font-size: 14px; font-weight: 600;
}
.box-body { padding: 18px; }

/* ── Info Boxes ── */
.info-box { border-radius: 8px; box-shadow: 0 2px 10px rgba(0,0,0,0.1); }
.info-box-icon { border-radius: 8px 0 0 8px; }

/* ── Buttons ── */
.btn { border-radius: 4px !important; font-weight: 600; font-size: 13px; letter-spacing: 0.3px; transition: all 0.2s; }
.btn-primary { background: #CC0000 !important; border: none !important; color: #fff !important; }
.btn-primary:hover { background: #8B0000 !important; transform: translateY(-2px); box-shadow: 0 6px 16px rgba(204,0,0,0.4); }
.btn-success { background: #2d6a4f !important; border: none !important; }
.btn-success:hover { background: #1b4332 !important; transform: translateY(-1px); }
.btn-warning { background: #FFC72C !important; border: none !important; color: #1a1a2e !important; }
.btn-warning:hover { background: #e6a800 !important; transform: translateY(-1px); }
.btn-info    { background: #0f3460 !important; border: none !important; }
.btn-lg { padding: 10px 22px; }

/* ── Logo container ── */
.logo-wrap {
  padding: 18px 12px 8px; text-align: center;
  animation: logoPop 0.7s cubic-bezier(0.34,1.56,0.64,1) both;
}
.logo-wrap img {
  max-width: 210px; object-fit: contain;
  filter: drop-shadow(0 3px 10px rgba(0,0,0,0.55));
  transition: transform 0.3s ease, filter 0.3s ease;
}
.logo-wrap img:hover {
  transform: scale(1.07) translateY(-2px);
  filter: drop-shadow(0 6px 18px rgba(204,0,0,0.5));
}
.app-brand {
  font-family: 'Playfair Display', serif;
  color: #FFC72C; font-size: 13.5px; font-weight: 700;
  letter-spacing: 0.6px; margin-top: 6px; text-align: center;
  text-shadow: 0 1px 4px rgba(0,0,0,0.4);
}
.app-brand-sub {
  color: rgba(255,255,255,0.5); font-size: 10.5px; text-align: center;
  margin-top: 2px; margin-bottom: 4px;
}

/* ── Animations ── */
@keyframes logoPop {
  0%   { opacity:0; transform: scale(0.6) translateY(-10px); }
  70%  { transform: scale(1.08) translateY(2px); }
  100% { opacity:1; transform: scale(1) translateY(0); }
}
@keyframes fadeSlideIn {
  from { opacity:0; transform: translateY(8px); }
  to   { opacity:1; transform: translateY(0); }
}
@keyframes shimmer {
  0%   { background-position: -400px 0; }
  100% { background-position: 400px 0; }
}
@keyframes pulse-glow {
  0%, 100% { box-shadow: 0 0 8px rgba(204,0,0,0.3); }
  50%       { box-shadow: 0 0 22px rgba(204,0,0,0.7); }
}
@keyframes spin { to { transform: rotate(360deg); } }
@keyframes progressWave {
  0%   { background-position: 0 0; }
  100% { background-position: 40px 0; }
}
@keyframes bounce {
  0%, 100% { transform: translateY(0); }
  50%       { transform: translateY(-4px); }
}

/* ── Shiny progress bar (beautiful animated) ── */
.shiny-notification {
  border-radius: 12px !important;
  box-shadow: 0 8px 32px rgba(0,0,0,0.25) !important;
  border: none !important;
  padding: 0 !important;
  overflow: hidden;
  min-width: 340px !important;
  animation: fadeSlideIn 0.3s ease both;
}
.shiny-notification-content {
  background: #1a1a2e !important;
  padding: 16px 20px 14px !important;
  color: #fff !important;
}
.shiny-notification-message {
  font-family: 'Source Sans Pro', sans-serif !important;
  font-size: 13.5px !important; font-weight: 600 !important;
  color: #FFC72C !important; margin-bottom: 10px !important;
  display: flex; align-items: center; gap: 8px;
}
.shiny-notification-message::before {
  content: '';
  display: inline-block; width: 14px; height: 14px;
  border: 2px solid #FFC72C; border-top-color: transparent;
  border-radius: 50%; animation: spin 0.7s linear infinite;
  flex-shrink: 0;
}
.shiny-notification-detail {
  font-size: 11.5px !important; color: rgba(255,255,255,0.6) !important;
  margin-bottom: 12px !important;
}
.progress {
  height: 8px !important; border-radius: 4px !important;
  background: rgba(255,255,255,0.12) !important;
  margin: 0 !important; overflow: visible !important;
}
.progress-bar {
  border-radius: 4px !important;
  background: linear-gradient(90deg, #CC0000, #FFC72C, #CC0000) !important;
  background-size: 200% 100% !important;
  animation: shimmer 1.5s linear infinite !important;
  box-shadow: 0 0 10px rgba(255,199,44,0.5) !important;
  transition: width 0.4s cubic-bezier(0.4,0,0.2,1) !important;
}
/* Bottom accent stripe */
.shiny-notification::after {
  content: '';
  display: block; height: 3px;
  background: linear-gradient(90deg, #CC0000, #FFC72C, #2d6a4f);
}

/* ── Welcome Hero ── */
.hero-banner {
  background: linear-gradient(135deg, #1a1a2e 0%, #16213e 40%, #0f3460 70%, #CC0000 100%);
  border-radius: 10px; padding: 28px 32px; margin-bottom: 24px;
  display: flex; align-items: center; gap: 24px;
  box-shadow: 0 6px 30px rgba(0,0,0,0.25);
  animation: fadeSlideIn 0.5s ease both;
}
.hero-text h2 { margin: 0; font-family: 'Playfair Display',serif; color: #fff; font-size: 22px; }
.hero-text p  { margin: 6px 0 0; color: rgba(255,255,255,0.75); font-size: 13px; }

/* ── Workflow Steps ── */
.step-card {
  background: #fff; border-radius: 8px; padding: 18px 14px;
  text-align: center; box-shadow: 0 2px 10px rgba(0,0,0,0.07);
  border-bottom: 3px solid #CC0000; transition: transform 0.2s, box-shadow 0.2s;
  margin: 4px;
}
.step-card:hover { transform: translateY(-4px); box-shadow: 0 8px 24px rgba(0,0,0,0.15); animation: pulse-glow 1.5s ease infinite; }
.step-num {
  width: 34px; height: 34px; border-radius: 50%;
  background: #CC0000; color: #fff; font-weight: 700; font-size: 16px;
  display: inline-flex; align-items: center; justify-content: center; margin-bottom: 8px;
}
.step-card h5 { margin: 6px 0 4px; font-family:'Playfair Display',serif; color:#1a1a2e; font-size:14px; }
.step-card p  { font-size: 11.5px; color: #666; margin: 0; line-height: 1.4; }

/* ── Feature Cards ── */
.feat-card {
  background: #fff; border-radius: 8px; padding: 20px;
  box-shadow: 0 2px 12px rgba(0,0,0,0.07); border-left: 4px solid #CC0000;
  margin-bottom: 16px; transition: transform 0.2s, box-shadow 0.2s;
  animation: fadeSlideIn 0.4s ease both;
}
.feat-card:hover { transform: translateY(-3px); box-shadow: 0 8px 24px rgba(0,0,0,0.12); }
.feat-card h4 { font-family:'Playfair Display',serif; color:#1a1a2e; margin:0 0 8px; font-size:15px; }
.feat-card p  { font-size: 12.5px; color: #555; margin:0; }

/* ── Pills ── */
.idx-pill {
  display: inline-block; background: #1a1a2e; color: #FFC72C;
  padding: 3px 10px; border-radius: 12px; font-size: 11px; font-weight: 600;
  margin: 2px; letter-spacing: 0.3px; transition: transform 0.15s;
}
.idx-pill:hover { transform: scale(1.08); }
.idx-pill.green { background: #2d6a4f; color: #fff; }
.idx-pill.red   { background: #CC0000; color: #fff; }

/* ── Sidebar status ── */
.stat-row { font-size: 11.5px; color: #aaa; padding: 2px 0; }
.s-ok  { color: #52b788; font-weight: 700; }
.s-pnd { color: #FFC72C; }
.s-err { color: #CC0000; }

/* ── DT tables ── */
table.dataTable thead th { background: #1a1a2e !important; color: #FFC72C !important; font-size: 12px; }
table.dataTable tbody tr:hover { background: #fff8e1 !important; transition: background 0.15s; }

/* ── Log output ── */
pre { background: #f8f8f8; border: 1px solid #ddd; border-radius: 4px; font-size: 12px; padding: 10px; }

/* ── Dropdown z-index: floats above sibling boxes ── */
/* Must override Bootstrap row, col, and shinydashboard box stacking */
.row               { overflow: visible !important; }
.col-sm-1, .col-sm-2, .col-sm-3, .col-sm-4, .col-sm-5,
.col-sm-6, .col-sm-7, .col-sm-8, .col-sm-9, .col-sm-10,
.col-sm-11, .col-sm-12 { overflow: visible !important; }
.box, .box-body    { overflow: visible !important; }
.shiny-bound-output, .shiny-html-output { overflow: visible !important; }
/* Selectize/select2 dropdown always on top */
.selectize-dropdown,
.selectize-dropdown.form-control,
.selectize-input.dropdown-active ~ .selectize-dropdown,
.selectize-control.single .selectize-dropdown {
  z-index: 100000 !important;
  position: absolute !important;
}
.selectize-control  { overflow: visible !important; position: relative; }
.shiny-input-container .selectize-control { overflow: visible !important; }
/* Bootstrap select */
.bootstrap-select .dropdown-menu { z-index: 100000 !important; }
/* native <select> rendered via selectInput */
select.form-control { position: relative; z-index: 10; }

/* ── Section divider ── */
.section-title {
  font-family: 'Playfair Display', serif; font-size: 16px; color: #1a1a2e;
  border-bottom: 2px solid #CC0000; padding-bottom: 6px; margin-bottom: 16px;
}

/* ── Tab panel override ── */
.nav-tabs-custom { border-radius: 6px; }
.nav-tabs-custom > .nav-tabs > li.active > a { border-top: 3px solid #CC0000 !important; }
"


# ── UI ─────────────────────────────────────────────────────────
# ── Wrap UI with login screen if auth enabled ─────────────────
raw_ui <- dashboardPage(
  skin = "red",

  # ─ Header ─
  dashboardHeader(
    title = tags$span(
      style = "font-family:'Playfair Display',serif; color:#fff; font-size:14px; letter-spacing:0.5px;",
      "🌱 AllInOne Phenomics"
    ),
    titleWidth = 255
  ),

  # ─ Sidebar ─
  dashboardSidebar(
    width = 255,
    tags$style(APP_CSS),
    useShinyjs(),

    tags$div(class = "logo-wrap",
      tags$a(
        href="https://www.uogbeans.com", target="_blank",
        style="display:block;",
        tags$img(
          src    = "logo.png",
          height = "78px",
          id     = "sidebar-logo",
          alt    = "AllInOne Phenomics — UoG Dry Bean Lab",
          style  = "max-width:210px;object-fit:contain;",
          onerror = "this.style.display='none';document.getElementById('logo-fallback').style.display='block';"
        ),
        tags$div(
          id    = "logo-fallback",
          style = "display:none;font-size:28px;text-align:center;padding:8px;",
          "🌱"
        )
      ),
      tags$div(class="app-brand",    "AllInOne Phenomics"),
      tags$div(class="app-brand-sub","Dry Bean Breeding & Comp. Biology")
    ),

    sidebarMenu(
      id = "sidebar",
      menuItem("🏠  Welcome",           tabName = "welcome",   icon = icon("home")),
      menuItem("🛩️  WebODM Processing",  tabName = "webodm",    icon = icon("plane")),
      menuItem("📁  Data Upload",        tabName = "upload",    icon = icon("upload")),
      menuItem("🌿  Mosaic & Soil Mask", tabName = "mosaic",   icon = icon("layer-group")),
      menuItem("🗺️  Plot Grid",          tabName = "grid",      icon = icon("th")),
      menuItem("📊  Vegetation Indices", tabName = "indices",  icon = icon("leaf")),
      menuItem("🔬  Custom Index",       tabName = "custom",   icon = icon("flask")),
      menuItem("🗺  Field Maps",         tabName = "fieldmaps",icon = icon("map")),
      menuItem("📈  Statistics",         tabName = "stats",    icon = icon("chart-bar")),
      menuItem("🔗  Correlation",        tabName = "corr",     icon = icon("project-diagram")),
      menuItem("🧬  PCA & Clustering",   tabName = "pca",      icon = icon("dna")),
      menuItem("🏆  Genotype Selection",  tabName = "geno_sel", icon = icon("trophy")),
      menuItem("🤖  Machine Learning",     tabName = "ml",       icon = icon("robot")),
      menuItem("🔍  Data QC",             tabName = "dataQC",   icon = icon("check-circle")),
      menuItem("📡  Spectral Signatures",  tabName = "spectral", icon = icon("wave-square")),
      menuItem("📑  Report Generator",     tabName = "report",   icon = icon("file-alt")),
      menuItem("💾  Export",             tabName = "export",   icon = icon("download")),
      # Admin-only menu item — hidden for non-admins via server-side UI
      uiOutput("admin_menu_item")
    ),

    tags$hr(style = "border-color:rgba(255,255,255,0.1); margin:8px 14px;"),

    tags$div(
      style = "padding:4px 18px 10px;",
      tags$p(class="stat-row", "Pipeline Status"),
      tags$p(class="stat-row", "📁 Raster ·· ", uiOutput("st_raster", inline=TRUE)),
      tags$p(class="stat-row", "🌿 Mask   ·· ", uiOutput("st_mask",   inline=TRUE)),
      tags$p(class="stat-row", "🗺️ Grid   ·· ", uiOutput("st_grid",   inline=TRUE)),
      tags$p(class="stat-row", "📊 Indices ·", uiOutput("st_idx",    inline=TRUE)),
      tags$p(class="stat-row", "📋 Data   ·· ", uiOutput("st_data",   inline=TRUE))
    )
  ),

  # ─ Body ─
  dashboardBody(
    tags$head(tags$style(APP_CSS)),

    tabItems(

      # ══════════════════════════════════════════════════════════
      # TAB: WELCOME
      # ══════════════════════════════════════════════════════════
      tabItem(tabName = "welcome",

        # ── Hero Banner ──────────────────────────────────────────
        tags$div(class="hero-banner",
          tags$img(src="logo.png", height="80px", onerror="this.src='';",
            style="flex-shrink:0; filter:drop-shadow(0 2px 10px rgba(0,0,0,0.6));"),
          tags$div(class="hero-text",
            tags$h2("AllInOne Phenomics"),
            tags$p(
              tags$b("Dry Bean Breeding & Computational Biology"),
              " · University of Guelph"
            ),
            tags$p(style="margin-top:6px;font-size:12.5px;",
              "RGB & Multispectral pipelines · 20+ vegetation indices · ",
              "Interactive field heatmaps · Machine learning · Elite plot selection"
            ),
            tags$div(style="margin-top:10px;",
              tags$a(href="https://www.uogbeans.com", target="_blank",
                style=paste0("display:inline-block;background:rgba(255,199,44,0.15);",
                  "border:1px solid #FFC72C;color:#FFC72C;padding:4px 14px;",
                  "border-radius:20px;font-size:12px;font-weight:600;text-decoration:none;",
                  "margin-right:8px;"),
                "🌐 www.uogbeans.com"
              ),
              tags$a(href="mailto:myoosefz@uoguelph.ca",
                style=paste0("display:inline-block;background:rgba(255,255,255,0.08);",
                  "border:1px solid rgba(255,255,255,0.25);color:#ddd;padding:4px 14px;",
                  "border-radius:20px;font-size:12px;font-weight:600;text-decoration:none;"),
                "✉️ Contact the Lab"
              )
            )
          )
        ),

        # ── Mission statement ─────────────────────────────────────
        tags$div(
          style=paste0("background:#fff;border-left:4px solid #CC0000;",
            "border-radius:0 6px 6px 0;padding:14px 20px;margin-bottom:18px;",
            "box-shadow:0 2px 10px rgba(0,0,0,0.06);"),
          tags$p(style="font-size:13.5px;color:#333;margin:0;line-height:1.7;font-style:italic;",
            '"Our research at the University of Guelph is centered around developing dry bean ',
            'varieties that excel in yield, stress resistance, and nutritional quality. We employ ',
            'both conventional breeding methods and modern techniques, such as genomic selection ',
            'and phenomics, to achieve superior breeding outcomes."'
          ),
          tags$p(style="font-size:11.5px;color:#999;margin:6px 0 0;",
            "— Dry Bean Breeding & Computational Biology Program"
          )
        ),

        # ── 3 feature cards ───────────────────────────────────────
        fluidRow(
          column(4, tags$div(class="feat-card",
            tags$h4("🛸 Remote Sensing Pipeline"),
            tags$p(style="font-size:12.5px;color:#555;",
              "Drone-based RGB and multispectral imagery analysis supporting ",
              "high-throughput phenotyping for breeding decisions across field trials."),
            tags$div(
              tags$span(class="idx-pill","RGB"),
              tags$span(class="idx-pill","Multispectral"),
              tags$span(class="idx-pill","MicaSense")
            )
          )),
          column(4, tags$div(class="feat-card",
            tags$h4("🌿 Vegetation Index Library"),
            tags$p(style="font-size:12.5px;color:#555;",
              "20+ built-in indices for plant health, canopy cover, chlorophyll, ",
              "and stress detection — plus an unlimited custom formula builder."),
            tags$div(
              tags$span(class="idx-pill green","NDVI"),
              tags$span(class="idx-pill green","NDRE"),
              tags$span(class="idx-pill green","GNDVI"),
              tags$span(class="idx-pill green","SAVI"),
              tags$span(class="idx-pill green","+more")
            )
          )),
          column(4, tags$div(class="feat-card",
            tags$h4("🤖 AI & Machine Learning"),
            tags$p(style="font-size:12.5px;color:#555;",
              "12 ML algorithms with automated feature selection, cross-validation, ",
              "and predictive modelling — supporting BeanGPT-inspired data-driven breeding."),
            tags$div(
              tags$span(class="idx-pill red","Random Forest"),
              tags$span(class="idx-pill red","XGBoost"),
              tags$span(class="idx-pill red","SVM"),
              tags$span(class="idx-pill red","RFE")
            )
          ))
        ),

        # ── Research areas ────────────────────────────────────────
        fluidRow(
          box(title="🔬 Research Focus Areas", width=8,
              solidHeader=TRUE, status="danger",
            fluidRow(
              column(6,
                tags$div(style="margin-bottom:12px;",
                  tags$div(style="font-weight:700;color:#CC0000;font-size:13px;margin-bottom:3px;",
                    "🌱 Dry Bean Breeding & Genetics"),
                  tags$p(style="font-size:12px;color:#555;margin:0;",
                    "Developing strategies to improve yield, biotic/abiotic stress resistance, ",
                    "and cooking quality across dry bean market classes.")
                ),
                tags$div(style="margin-bottom:12px;",
                  tags$div(style="font-weight:700;color:#CC0000;font-size:13px;margin-bottom:3px;",
                    "📡 Remote Sensing & Imaging"),
                  tags$p(style="font-size:12px;color:#555;margin:0;",
                    "Using aerial and satellite imaging to improve prediction accuracy of ",
                    "complex traits, supporting faster and more informed breeding decisions.")
                ),
                tags$div(
                  tags$div(style="font-weight:700;color:#CC0000;font-size:13px;margin-bottom:3px;",
                    "🧬 Omics-Based Selection"),
                  tags$p(style="font-size:12px;color:#555;margin:0;",
                    "Genomics, phenomics, metabolomics, and enviromics prediction ",
                    "methods to optimise the dry bean selection process.")
                )
              ),
              column(6,
                tags$div(style="margin-bottom:12px;",
                  tags$div(style="font-weight:700;color:#CC0000;font-size:13px;margin-bottom:3px;",
                    "🔬 Comparative Breeding Analysis"),
                  tags$p(style="font-size:12px;color:#555;margin:0;",
                    "Investigating conventional vs. modern breeding techniques through ",
                    "multi-omics insights to enhance breeding outcomes.")
                ),
                tags$div(style="margin-bottom:12px;",
                  tags$div(style="font-weight:700;color:#CC0000;font-size:13px;margin-bottom:3px;",
                    "💻 Computational Tools"),
                  tags$p(style="font-size:12px;color:#555;margin:0;",
                    "Building data packages, statistical pipelines, and AI tools — ",
                    "including BeanGPT — for complex crop breeding datasets.")
                ),
                tags$div(
                  tags$div(style="font-weight:700;color:#CC0000;font-size:13px;margin-bottom:3px;",
                    "🏭 Canning Quality & Yield"),
                  tags$p(style="font-size:12px;color:#555;margin:0;",
                    "AI-powered canning quality assessment pipeline enabling early-stage ",
                    "evaluation of bean lines for both yield and processing quality.")
                )
              )
            )
          ),

          box(title="🔗 Lab Resources", width=4,
              solidHeader=TRUE, status="primary",
            tags$div(style="display:flex;flex-direction:column;gap:8px;",
              lapply(list(
                list("🏠","Home",          "https://www.uogbeans.com"),
                list("📖","Research",       "https://uogbeans.com/research/"),
                list("📄","Publications",   "https://uogbeans.com/publications/"),
                list("👥","Team Members",   "https://uogbeans.com/team-members/"),
                list("📰","Lab News",       "https://uogbeans.com/updates/"),
                list("🧪","Resources",      "https://uogbeans.com/Resource/"),
                list("🌱","Seed Catalogue", "https://uogbeans.com/seed-catalogue/"),
                list("🤖","AI & Analysis Hub","https://uogbeans.com/AI%20&%20Analysis%20Hub/"),
                list("📩","Join Us",        "https://uogbeans.com/join-us/")
              ), function(item) {
                tags$a(href=item[[3]], target="_blank",
                  style=paste0("display:flex;align-items:center;gap:8px;",
                    "background:#f8f6f0;border-radius:6px;padding:7px 12px;",
                    "text-decoration:none;color:#1a1a2e;font-size:12.5px;",
                    "font-weight:600;transition:background 0.2s;",
                    "border-left:3px solid #CC0000;"),
                  tags$span(item[[1]]),
                  tags$span(item[[2]])
                )
              })
            )
          )
        ),

        # ── Pipeline workflow ─────────────────────────────────────
        box(title="🚀 Analysis Pipeline — 12 Steps", width=12,
            solidHeader=TRUE, status="danger",
          fluidRow(
            lapply(list(
              list("1","Upload Data",         "Load .tif mosaic, plot grid .rds, and metadata .csv"),
              list("2","Camera Config",        "RGB or Multispectral; assign bands; set reflectance scaling"),
              list("3","Soil Mask",            "Remove soil pixels using BGI, HUE, ExG or NDVI threshold"),
              list("4","Plot Grid",            "Click 4 field corners to define grid; set rows & columns"),
              list("5","Vegetation Indices",   "Calculate 20+ indices; add unlimited custom formulas"),
              list("6","Field Maps",           "Interactive per-plot heatmaps with metadata hover"),
              list("7","Statistics",           "Distributions, genotype comparisons, summary stats"),
              list("8","Correlation",          "Pearson/Spearman matrix + interactive scatter"),
              list("9","PCA & Clustering",     "Score biplot, scree plot, loadings, dendrogram"),
              list("10","Genotype Selection",  "Set index thresholds; rank and categorise elite plots"),
              list("11","Machine Learning",    "12 algorithms, hyperparameter tuning, RFE, RMSE/R2"),
              list("12","Export",              "CSV, Excel (6 sheets), PNG/PDF plots, rankings")
            ), function(s) {
              column(2, tags$div(class="step-card",
                tags$div(class="step-num", s[[1]]),
                tags$h5(s[[2]]), tags$p(s[[3]])
              ))
            })
          )
        )
      ),

      # ══════════════════════════════════════════════════════════
      # TAB: WEBODM
      # ══════════════════════════════════════════════════════════
      tabItem(tabName = "webodm",

        # ── Pipeline banner ──────────────────────────────────────
        fluidRow(
          column(12,
            tags$div(
              style=paste0("background:linear-gradient(135deg,#1a1a2e,#0f3460);",
                "border-radius:10px;padding:18px 24px;margin-bottom:16px;",
                "color:#fff;display:flex;align-items:center;gap:20px;"),
              tags$div(style="font-size:32px;","🛩️"),
              tags$div(
                tags$h4("WebODM — Drone Image Processing",
                  style="margin:0 0 4px;font-family:'Playfair Display',serif;color:#FFC72C;font-size:18px;"),
                tags$p(style="margin:0;font-size:13px;opacity:0.85;",
                  "WebODM converts your raw drone photos into a georeferenced orthomosaic (.tif). ",
                  "Complete the setup below, then upload your .tif to AllInOne Phenomics.")
              )
            )
          )
        ),

        # ── Pipeline flow ────────────────────────────────────────
        fluidRow(
          box(title="📋 Full Workflow", width=12, solidHeader=TRUE, status="danger",
            tags$div(
              style="display:flex;align-items:center;justify-content:center;gap:0;flex-wrap:wrap;padding:8px 0;",
              tags$div(style="text-align:center;padding:12px 16px;background:#1a1a2e;border-radius:10px;min-width:120px;",
                tags$div(style="font-size:24px;","📸"),
                tags$div(style="color:#FFC72C;font-weight:700;font-size:12px;margin-top:3px;","1. Raw Photos"),
                tags$div(style="color:rgba(255,255,255,0.6);font-size:11px;","JPG from drone")),
              tags$div(style="font-size:22px;color:#CC0000;padding:0 6px;","→"),
              tags$div(style="text-align:center;padding:12px 16px;background:#0f3460;border-radius:10px;min-width:120px;border:2px solid #FFC72C;",
                tags$div(style="font-size:24px;","🐳"),
                tags$div(style="color:#FFC72C;font-weight:700;font-size:12px;margin-top:3px;","2. Docker"),
                tags$div(style="color:rgba(255,255,255,0.6);font-size:11px;","Install once")),
              tags$div(style="font-size:22px;color:#CC0000;padding:0 6px;","→"),
              tags$div(style="text-align:center;padding:12px 16px;background:#2d4a3e;border-radius:10px;min-width:120px;",
                tags$div(style="font-size:24px;","🛩️"),
                tags$div(style="color:#FFC72C;font-weight:700;font-size:12px;margin-top:3px;","3. WebODM"),
                tags$div(style="color:rgba(255,255,255,0.6);font-size:11px;","Process photos")),
              tags$div(style="font-size:22px;color:#CC0000;padding:0 6px;","→"),
              tags$div(style="text-align:center;padding:12px 16px;background:#1b4332;border-radius:10px;min-width:120px;",
                tags$div(style="font-size:24px;","🗺️"),
                tags$div(style="color:#FFC72C;font-weight:700;font-size:12px;margin-top:3px;","4. .tif File"),
                tags$div(style="color:rgba(255,255,255,0.6);font-size:11px;","Download output")),
              tags$div(style="font-size:22px;color:#CC0000;padding:0 6px;","→"),
              tags$div(style="text-align:center;padding:12px 16px;background:#CC0000;border-radius:10px;min-width:120px;",
                tags$div(style="font-size:24px;","🌿"),
                tags$div(style="color:#fff;font-weight:700;font-size:12px;margin-top:3px;","5. AllInOne"),
                tags$div(style="color:rgba(255,255,255,0.8);font-size:11px;","Upload & analyse"))
            )
          )
        ),

        # ── Live status row ──────────────────────────────────────
        fluidRow(
          box(title="🔍 System Status", width=4, solidHeader=TRUE, status="primary",
            uiOutput("webodm_sys_status"),
            tags$br(),
            actionButton("recheck_webodm", "🔄 Recheck Status",
                         class="btn-warning btn-sm", style="width:100%;")
          ),
          box(title="⚡ Quick Actions", width=8, solidHeader=TRUE, status="success",
            uiOutput("webodm_quick_actions")
          )
        ),

        # ── Step-by-step setup wizard ────────────────────────────
        fluidRow(
          box(title="🧙 Setup Wizard — Follow These Steps in Order",
              width=12, solidHeader=TRUE, status="danger",

            # STEP 1: Install Docker
            tags$div(
              style="background:#f8f9fa;border-radius:10px;padding:16px 20px;margin-bottom:14px;border-left:5px solid #0f3460;",
              tags$div(style="display:flex;align-items:center;gap:12px;margin-bottom:10px;",
                tags$div(style="background:#0f3460;color:#FFC72C;border-radius:50%;width:32px;height:32px;display:flex;align-items:center;justify-content:center;font-weight:700;font-size:15px;flex-shrink:0;","1"),
                tags$h5(style="margin:0;color:#1a1a2e;font-size:15px;","Install Docker Desktop")
              ),
              tags$div(style="display:grid;grid-template-columns:1fr 1fr;gap:12px;",
                # Windows
                tags$div(style="background:#fff;border-radius:8px;padding:12px;border:1px solid #dee2e6;",
                  tags$p(style="margin:0 0 6px;font-weight:700;font-size:13px;","🪟 Windows"),
                  tags$ol(style="font-size:12px;color:#555;padding-left:16px;margin:0 0 8px;",
                    tags$li("Download Docker Desktop from the button below"),
                    tags$li("Run the installer — select ", tags$b("Use WSL 2")),
                    tags$li("Restart your computer"),
                    tags$li("Open Docker Desktop and wait for ", tags$b("Engine running"))
                  ),
                  tags$a(href="https://www.docker.com/products/docker-desktop/",
                    target="_blank", class="btn btn-primary btn-sm",
                    style="font-weight:600;width:100%;",
                    "⬇️ Download Docker Desktop for Windows")
                ),
                # Mac/Linux
                tags$div(style="background:#fff;border-radius:8px;padding:12px;border:1px solid #dee2e6;",
                  tags$p(style="margin:0 0 6px;font-weight:700;font-size:13px;","🍎 Mac / 🐧 Linux"),
                  tags$p(style="font-size:12px;color:#555;margin-bottom:8px;",
                    tags$b("Mac:"), " Download Docker Desktop for Mac below.", tags$br(),
                    tags$b("Linux Ubuntu:"), " Run this in terminal:"),
                  tags$pre(style="background:#1a1a2e;color:#FFC72C;padding:8px;border-radius:5px;font-size:11px;margin-bottom:8px;",
                    "sudo apt update\nsudo apt install docker.io docker-compose -y\nsudo usermod -aG docker $USER\nnewgrp docker"),
                  tags$a(href="https://www.docker.com/products/docker-desktop/",
                    target="_blank", class="btn btn-primary btn-sm",
                    style="font-weight:600;width:100%;",
                    "⬇️ Download Docker Desktop for Mac")
                )
              )
            ),

            # STEP 2: Fix Docker permissions (Linux/WSL)
            tags$div(
              style="background:#f8f9fa;border-radius:10px;padding:16px 20px;margin-bottom:14px;border-left:5px solid #856404;",
              tags$div(style="display:flex;align-items:center;gap:12px;margin-bottom:10px;",
                tags$div(style="background:#856404;color:#fff;border-radius:50%;width:32px;height:32px;display:flex;align-items:center;justify-content:center;font-weight:700;font-size:15px;flex-shrink:0;","2"),
                tags$h5(style="margin:0;color:#1a1a2e;font-size:15px;","Fix Docker Permissions (Linux / WSL only)")
              ),
              tags$p(style="font-size:12.5px;color:#555;margin-bottom:8px;",
                "If you see ", tags$code("permission denied"), " when running Docker commands, ",
                "run these commands in your Linux/WSL terminal:"),
              tags$div(style="display:grid;grid-template-columns:1fr 1fr;gap:12px;",
                tags$div(
                  tags$p(style="font-size:12px;font-weight:700;color:#555;margin-bottom:4px;","Option A — Add user to docker group:"),
                  tags$pre(style="background:#1a1a2e;color:#FFC72C;padding:10px;border-radius:6px;font-size:11.5px;",
                    "sudo groupadd docker\nsudo usermod -aG docker $USER\nnewgrp docker")
                ),
                tags$div(
                  tags$p(style="font-size:12px;font-weight:700;color:#555;margin-bottom:4px;","Option B — WSL Docker Desktop integration:"),
                  tags$ol(style="font-size:12px;color:#555;padding-left:16px;",
                    tags$li("Open Docker Desktop"),
                    tags$li("Settings → Resources → WSL Integration"),
                    tags$li("Enable your Ubuntu distro"),
                    tags$li("Click Apply & Restart")
                  )
                )
              ),
              tags$div(style="background:#fff3cd;border-radius:6px;padding:8px 12px;margin-top:8px;font-size:12px;color:#856404;",
                tags$b("Windows users:"), " Skip this step — Docker Desktop handles permissions automatically.")
            ),

            # STEP 3: Install & start WebODM
            tags$div(
              style="background:#f8f9fa;border-radius:10px;padding:16px 20px;margin-bottom:14px;border-left:5px solid #2d6a4f;",
              tags$div(style="display:flex;align-items:center;gap:12px;margin-bottom:10px;",
                tags$div(style="background:#2d6a4f;color:#fff;border-radius:50%;width:32px;height:32px;display:flex;align-items:center;justify-content:center;font-weight:700;font-size:15px;flex-shrink:0;","3"),
                tags$h5(style="margin:0;color:#1a1a2e;font-size:15px;","Install & Start WebODM")
              ),
              tags$div(style="display:grid;grid-template-columns:1fr 1fr;gap:12px;",
                tags$div(
                  tags$p(style="font-size:12px;font-weight:700;color:#555;margin-bottom:4px;","🪟 Windows — use Git Bash or WSL terminal:"),
                  tags$pre(style="background:#1a1a2e;color:#FFC72C;padding:10px;border-radius:6px;font-size:11.5px;",
                    "# Clone WebODM (only needed once)\ngit clone https://github.com/OpenDroneMap/WebODM\ncd WebODM\n\n# Start WebODM (run every time)\n./webodm.sh start"),
                  tags$p(style="font-size:11.5px;color:#888;margin-top:4px;",
                    "💡 Need Git Bash? Download from: https://git-scm.com")
                ),
                tags$div(
                  tags$p(style="font-size:12px;font-weight:700;color:#555;margin-bottom:4px;","🍎 Mac / 🐧 Linux terminal:"),
                  tags$pre(style="background:#1a1a2e;color:#FFC72C;padding:10px;border-radius:6px;font-size:11.5px;",
                    "# Clone WebODM (only needed once)\ngit clone https://github.com/OpenDroneMap/WebODM\ncd WebODM\n\n# Start WebODM (run every time)\n./webodm.sh start"),
                  tags$p(style="font-size:11.5px;color:#888;margin-top:4px;",
                    "⏱️ First start takes 5–10 min to download images.")
                )
              ),
              tags$div(style="background:#e8f5e9;border-radius:6px;padding:8px 12px;margin-top:8px;font-size:12px;color:#1b4332;",
                tags$b("To stop WebODM:"), tags$code("./webodm.sh down"), " &nbsp;|&nbsp; ",
                tags$b("To update:"), tags$code("./webodm.sh update"))
            ),

            # STEP 4: Use WebODM
            tags$div(
              style="background:#f8f9fa;border-radius:10px;padding:16px 20px;margin-bottom:14px;border-left:5px solid #CC0000;",
              tags$div(style="display:flex;align-items:center;gap:12px;margin-bottom:10px;",
                tags$div(style="background:#CC0000;color:#fff;border-radius:50%;width:32px;height:32px;display:flex;align-items:center;justify-content:center;font-weight:700;font-size:15px;flex-shrink:0;","4"),
                tags$h5(style="margin:0;color:#1a1a2e;font-size:15px;","Process Your Photos in WebODM")
              ),
              tags$div(style="display:grid;grid-template-columns:repeat(4,1fr);gap:10px;",
                tags$div(style="background:#fff;border-radius:8px;padding:10px;border:1px solid #dee2e6;text-align:center;",
                  tags$div(style="font-size:22px;","🌐"),
                  tags$p(style="font-size:12px;margin:4px 0;font-weight:700;","Open WebODM"),
                  tags$p(style="font-size:11px;color:#888;margin:0;","Go to localhost:8000")),
                tags$div(style="background:#fff;border-radius:8px;padding:10px;border:1px solid #dee2e6;text-align:center;",
                  tags$div(style="font-size:22px;","📁"),
                  tags$p(style="font-size:12px;margin:4px 0;font-weight:700;","Create Task"),
                  tags$p(style="font-size:11px;color:#888;margin:0;","Add Project → New Task")),
                tags$div(style="background:#fff;border-radius:8px;padding:10px;border:1px solid #dee2e6;text-align:center;",
                  tags$div(style="font-size:22px;","📸"),
                  tags$p(style="font-size:12px;margin:4px 0;font-weight:700;","Upload Photos"),
                  tags$p(style="font-size:11px;color:#888;margin:0;","Drag & drop JPGs")),
                tags$div(style="background:#fff;border-radius:8px;padding:10px;border:1px solid #dee2e6;text-align:center;",
                  tags$div(style="font-size:22px;","⬇️"),
                  tags$p(style="font-size:12px;margin:4px 0;font-weight:700;","Download .tif"),
                  tags$p(style="font-size:11px;color:#888;margin:0;","Assets → Orthophoto"))
              ),
              tags$div(style="background:#fff3cd;border-radius:6px;padding:8px 12px;margin-top:10px;font-size:12px;color:#856404;",
                tags$b("Recommended WebODM settings for field trials:"),
                " Resolution: 2–3 cm/px | Quality: High | Enable: Orthophoto | ",
                "Multispectral: Enable radiometric calibration | GCPs: highly recommended for accuracy")
            ),

            # STEP 5: Back to AllInOne
            tags$div(
              style="background:#e8f5e9;border-radius:10px;padding:16px 20px;border-left:5px solid #1b4332;",
              tags$div(style="display:flex;align-items:center;gap:12px;margin-bottom:10px;",
                tags$div(style="background:#1b4332;color:#FFC72C;border-radius:50%;width:32px;height:32px;display:flex;align-items:center;justify-content:center;font-weight:700;font-size:15px;flex-shrink:0;","5"),
                tags$h5(style="margin:0;color:#1a1a2e;font-size:15px;","Upload .tif to AllInOne Phenomics")
              ),
              tags$p(style="font-size:13px;color:#1b4332;margin-bottom:8px;",
                "After downloading the orthomosaic .tif from WebODM, go to the ",
                tags$b("📁 Data Upload"), " tab and upload it. The app will auto-detect bands and scaling."),
              tags$div(style="display:flex;gap:10px;flex-wrap:wrap;",
                actionButton("goto_upload_from_webodm", "📁 Go to Data Upload →",
                             class="btn-success btn-lg", style="font-weight:700;"),
                tags$a(href="https://docs.opendronemap.org/", target="_blank",
                       class="btn btn-default btn-lg", "📖 WebODM Full Docs")
              )
            )
          )
        ),

        # ── WebODM interface embedded ────────────────────────────
        fluidRow(
          box(title="🖥️ WebODM Interface (auto-loads when WebODM is running)",
              width=12, solidHeader=TRUE, status="primary",
            uiOutput("webodm_iframe_ui")
          )
        ),

        # ── Cloud alternative ────────────────────────────────────
        fluidRow(
          box(title="☁️ No Installation? Use WebODM Cloud",
              width=12, solidHeader=TRUE, status="success",
            tags$div(
              style="display:flex;align-items:center;gap:24px;flex-wrap:wrap;",
              tags$div(style="flex:1;min-width:280px;",
                tags$p(style="font-size:13.5px;margin:0 0 8px;",
                  tags$b("WebODM Lightning"), " is the cloud version — no Docker, no installation, ",
                  "no permissions issues. Upload photos and download your .tif in minutes."),
                tags$p(style="font-size:12.5px;color:#555;margin:0;",
                  "💰 Cost: ~$0.01–$0.15 per image | ⚡ Faster than local processing | ",
                  "🔒 Your data is private")
              ),
              tags$div(
                tags$a(href="https://webodm.net", target="_blank",
                  class="btn btn-success btn-lg",
                  style="font-weight:700;min-width:200px;text-align:center;",
                  "🌐 Open WebODM Lightning"),
                tags$br(), tags$br(),
                tags$a(href="https://github.com/OpenDroneMap/WebODM",
                  target="_blank", class="btn btn-default",
                  "📂 WebODM GitHub")
              )
            )
          )
        )
      ),


      # ══════════════════════════════════════════════════════════
      # TAB: UPLOAD
      # ══════════════════════════════════════════════════════════
      tabItem(tabName = "upload",

        # z-index wrapper: Camera Config row must be above Metadata Preview row
        tags$div(style="position:relative; z-index:10;",
          fluidRow(
            box(title="🎛️ Camera Configuration", width=4, solidHeader=TRUE, status="danger",
              radioGroupButtons("camera_type","Camera Type:",
                choices=c("RGB"="rgb","Multispectral"="ms"),
                selected="rgb", status="danger", justified=TRUE),

              tags$hr(),

              conditionalPanel("input.camera_type=='rgb'",
                tags$p(class="section-title","Band Assignment"),
                splitLayout(
                  numericInput("rgb_r","Red",   value=1,min=1,max=10),
                  numericInput("rgb_g","Green", value=2,min=1,max=10),
                  numericInput("rgb_b","Blue",  value=3,min=1,max=10)
                )
              ),
              conditionalPanel("input.camera_type=='ms'",
                tags$p(class="section-title","Band Assignment"),
                splitLayout(
                  numericInput("ms_b", "Blue",     value=1,min=1,max=10),
                  numericInput("ms_g", "Green",    value=2,min=1,max=10),
                  numericInput("ms_r", "Red",      value=3,min=1,max=10)
                ),
                splitLayout(
                  numericInput("ms_re","Red Edge", value=4,min=1,max=10),
                  numericInput("ms_nir","NIR",     value=5,min=1,max=10)
                )
              ),
              tags$hr(),
              tags$p(class="section-title","📡 Band Scaling to Reflectance"),
              selectInput("band_scale","Scale raw values by:",
                choices=c(
                  "Auto-detect (recommended)"   = "auto",
                  "None — already 0-1"          = "none",
                  "÷ 255  (8-bit RGB camera)"   = "255",
                  "÷ 10000 (MicaSense / ENVI)"  = "10000",
                  "÷ 65535 (16-bit sensor)"     = "65535",
                  "Custom divisor"              = "custom"
                ), selected="auto"),
              conditionalPanel("input.band_scale=='custom'",
                numericInput("band_scale_custom","Custom divisor:", value=10000, min=1)
              ),
              tags$small(style="color:#888;font-size:11px;",
                "Reflectance range 0–1 is required for accurate vegetation indices.")
            ),

            box(title="📁 File Uploads", width=8, solidHeader=TRUE, status="danger",
              tags$div(
                style="background:#fff8e1;border-left:4px solid #FFC72C;padding:10px 14px;border-radius:4px;margin-bottom:14px;font-size:12.5px;color:#555;",
                tags$b("⚠️ Large files supported (up to 4 GB)."),
                " Only the drone mosaic (.tif) is required. Plot grid and metadata are optional — ",
                "you can still compute indices and view field maps without them."
              ),
              fluidRow(
                column(6,
                  fileInput("tif_file",
                    tags$span("🗺️ Drone Mosaic", tags$span(" — required", style="color:#CC0000;font-weight:600;font-size:11px;")),
                    accept=c(".tif",".tiff"), buttonLabel="Browse"),
                  uiOutput("raster_info_ui")
                ),
                column(6,
                  fileInput("shape_file",
                    tags$span("📐 Plot Grid (.rds)", tags$span(" — optional", style="color:#888;font-size:11px;")),
                    accept=".rds", buttonLabel="Browse"),
                  fileInput("csv_file",
                    tags$span("📋 Plot Metadata (.csv)", tags$span(" — optional", style="color:#888;font-size:11px;")),
                    accept=".csv", buttonLabel="Browse")
                )
              ),
              tags$hr(),
              actionButton("load_data","⚡ Load All Data", class="btn-primary btn-lg"),
              tags$br(),tags$br(),
              verbatimTextOutput("load_log")
            )
          )
        ), # end z-index:10 wrapper

        tags$div(style="position:relative; z-index:1;",
          fluidRow(
            box(title="📋 Metadata Preview", width=12, solidHeader=TRUE, status="primary",
              DTOutput("meta_preview")
            )
          )
        )
      ),

      # ══════════════════════════════════════════════════════════
      # TAB: MOSAIC & MASK
      # ══════════════════════════════════════════════════════════
      tabItem(tabName = "mosaic",

        fluidRow(
          box(title="🌿 Soil Mask Settings", width=3, solidHeader=TRUE, status="success",
            selectInput("mask_idx","Masking Index:",
              choices=c("BGI","HUE","ExG","NDVI_approx"), selected="BGI"),
            sliderInput("crop_val","Crop Threshold:",
              min=0, max=2, value=0.7, step=0.05),
            radioButtons("crop_above","Remove pixels:",
              choices=c("Above threshold"="TRUE","Below threshold"="FALSE"), selected="TRUE"),
            tags$hr(),
            actionButton("apply_mask","🌿 Apply Mask", class="btn-success btn-lg"),
            verbatimTextOutput("mask_log")
          ),

          box(title="🌄 Mosaic View", width=9, solidHeader=TRUE, status="danger",
            plotOutput("mosaic_plot", height="460px"),
            tags$br(),
            actionButton("refresh_mosaic","🔄 Refresh", class="btn-info")
          )
        ),

        fluidRow(
          box(title="Original Mosaic", width=6, solidHeader=TRUE, status="warning",
            plotOutput("pre_mask", height="320px")
          ),
          box(title="After Soil Removal", width=6, solidHeader=TRUE, status="success",
            plotOutput("post_mask", height="320px")
          )
        )
      ),

      # ══════════════════════════════════════════════════════════
      # TAB: PLOT GRID
      # ══════════════════════════════════════════════════════════
      tabItem(tabName = "grid",

        fluidRow(
          box(title="📐 Grid Settings", width=4, solidHeader=TRUE, status="danger",

            tags$div(
              style="background:#e8f5e9;border-left:4px solid #2d6a4f;padding:10px 14px;border-radius:4px;margin-bottom:14px;font-size:12.5px;",
              tags$b("🖱️ How to place the grid:"),
              tags$ol(style="margin:6px 0 0 16px;padding:0;",
                tags$li("Click 4 corner points on the field image (any order)"),
                tags$li("The app sorts them into a proper quadrilateral"),
                tags$li("Set rows/cols, then click Generate Grid")
              )
            ),

            uiOutput("pts_status_ui"),

            tags$hr(),
            numericInput("ncols","Columns:", value=14, min=1, max=200),
            numericInput("nrows","Rows:",    value=20, min=1, max=1000),
            tags$br(),
            actionButton("gen_grid",   "🗺️ Generate Grid",  class="btn-primary btn-lg"),
            actionButton("clear_pts",  "🗑️ Clear Points",   class="btn-danger",
                         style="margin-left:8px;"),
            tags$hr(),
            verbatimTextOutput("grid_info"),
            tags$small(style="color:#999;",
              "Tip: Upload a saved .rds grid (from fieldShape_render) via the Upload tab to skip manual placement.")
          ),

          box(title="🗺️ Field Layout — click to set corner points", width=8,
              solidHeader=TRUE, status="danger",
            plotOutput("grid_plot", height="500px",
                       click = clickOpts(id="grid_click", clip=FALSE))
          )
        )
      ),

      # ══════════════════════════════════════════════════════════
      # TAB: INDICES
      # ══════════════════════════════════════════════════════════
      tabItem(tabName = "indices",

        fluidRow(
          box(title="📊 Index Selection", width=4, solidHeader=TRUE, status="danger",
            uiOutput("idx_checkboxes"),
            tags$hr(),
            actionButton("calc_idx","⚡ Calculate Indices", class="btn-primary btn-lg"),
            tags$br(),tags$br(),
            verbatimTextOutput("idx_log")
          ),
          box(title="📚 Index Reference Library", width=8, solidHeader=TRUE, status="primary",
            DTOutput("idx_ref_tbl")
          )
        ),

        fluidRow(
          box(title="✅ Extracted Values Preview", width=12, solidHeader=TRUE, status="success",
            DTOutput("extract_preview")
          )
        )
      ),

      # ══════════════════════════════════════════════════════════
      # TAB: CUSTOM INDEX
      # ══════════════════════════════════════════════════════════
      tabItem(tabName = "custom",

        fluidRow(
          box(title="🔬 Custom Index Builder", width=6, solidHeader=TRUE, status="danger",
            textInput("ci_name","Index Name (e.g. MYIDX):", placeholder="MYIDX"),
            textInput("ci_formula","Formula:", placeholder="(Red-Blue)/(Red+Blue)"),
            textAreaInput("ci_desc","Description:", rows=2),
            tags$div(
              style="background:#f0f4ff; border-radius:6px; padding:12px; margin:10px 0;",
              tags$b("Available variables:"),
              tags$br(),
              tags$span(class="idx-pill","Red"), tags$span(class="idx-pill","Green"),
              tags$span(class="idx-pill","Blue"),
              uiOutput("ms_pills"),
              tags$br(),tags$br(),
              tags$small(style="color:#555;",
                "Operators: + - * / ^ sqrt() log() abs() exp()\n",
                "Example NDVI: (NIR-Red)/(NIR+Red)")
            ),
            actionButton("add_ci","➕ Add Index", class="btn-primary"),
            tags$br(),tags$br(),
            verbatimTextOutput("ci_log")
          ),
          box(title="📋 My Custom Indices", width=6, solidHeader=TRUE, status="success",
            DTOutput("ci_table"),
            tags$br(),
            actionButton("clr_ci","🗑️ Clear All", class="btn-danger")
          )
        )
      ),

      # ══════════════════════════════════════════════════════════
      # TAB: FIELD MAPS
      # ══════════════════════════════════════════════════════════
      tabItem(tabName = "fieldmaps",

        fluidRow(
          box(title="⚙️ Map Settings", width=3, solidHeader=TRUE, status="danger",
            uiOutput("map_idx_sel"),
            selectInput("map_pal","Color Palette:",
              choices=c("RdYlGn","viridis","plasma","inferno","magma","Spectral","YlOrRd","BrBG"),
              selected="RdYlGn"),
            sliderInput("map_alpha","Opacity:", min=0.3, max=1, value=0.9, step=0.05),
            sliderInput("map_pts","Max pixels (k):", min=10, max=200, value=80, step=10),
            tags$div(style="display:flex;gap:8px;",
              actionButton("render_map",   "🗺️ Render",  class="btn-primary btn-lg"),
              actionButton("cancel_map",   "⏹ Stop",     class="btn-danger")
            ),
            tags$br(),
            uiOutput("map_status_ui")
          ),
          box(title="📍 Spatial Index Heatmap", width=9, solidHeader=TRUE, status="danger",
            tags$div(style="font-size:11px;color:#888;margin-bottom:6px;",
              "🖱️ Hover = plot info  ·  Scroll = zoom  ·  Drag = pan  ·  📷 top-right = save PNG"),
            plotlyOutput("field_map", height="540px")
          )
        ),

        fluidRow(
          box(title="🖼️ Multi-Index Gallery (interactive heatmaps)", width=12,
              solidHeader=TRUE, status="primary",
            fluidRow(
              column(9, uiOutput("gallery_sel")),
              column(3, tags$br(),
                selectInput("gal_pal","Gallery palette:",
                  choices=c("RdYlGn","viridis","plasma","Spectral","YlOrRd"),
                  selected="RdYlGn"))
            ),
            tags$div(style="display:flex;gap:8px;align-items:center;",
              actionButton("render_gallery","🎨 Generate Gallery", class="btn-info"),
              actionButton("cancel_gallery","⏹ Stop",              class="btn-danger")
            ),
            tags$br(),
            uiOutput("gallery_status_ui"),
            plotlyOutput("gallery_plot", height="620px")
          )
        )
      ),

      # ══════════════════════════════════════════════════════════
      # TAB: STATISTICS
      # ══════════════════════════════════════════════════════════
      tabItem(tabName = "stats",

        uiOutput("val_boxes"),

        fluidRow(
          box(title="📊 Distribution Explorer", width=6, solidHeader=TRUE, status="danger",
            fluidRow(
              column(5, uiOutput("stat_idx_sel")),
              column(4, uiOutput("grp_sel")),
              column(3, checkboxInput("dist_outliers","Flag outliers", value=TRUE))
            ),
            radioGroupButtons("dist_type","Plot type:",
              choices=c("Violin"="violin","Box"="box","Hist"="hist","Density"="density"),
              selected="violin", status="danger", size="sm", justified=TRUE),
            plotlyOutput("dist_plot", height="360px")
          ),
          box(title="📋 Summary Statistics", width=6, solidHeader=TRUE, status="primary",
            DTOutput("sum_stats_tbl")
          )
        ),

        fluidRow(
          box(title="🌿 Genotype / Entry Comparison", width=12, solidHeader=TRUE, status="success",
            fluidRow(
              column(4, uiOutput("geno_idx_sel")),
              column(4, uiOutput("geno_grp_sel")),
              column(4, radioGroupButtons("geno_plot_type","Type:",
                choices=c("Bar"="bar","Dot"="dot","Lollipop"="lollipop"),
                selected="lollipop", status="success", size="sm"))
            ),
            plotlyOutput("geno_plot", height="420px")
          )
        )
      ),

      # ══════════════════════════════════════════════════════════
      # TAB: CORRELATION
      # ══════════════════════════════════════════════════════════
      tabItem(tabName = "corr",

        fluidRow(
          box(title="⚙️ Settings", width=3, solidHeader=TRUE, status="danger",
            selectInput("corr_meth","Method:",
              choices=c("pearson","spearman","kendall"), selected="pearson"),
            sliderInput("corr_sig","Sig. level:", min=0.01, max=0.1, value=0.05, step=0.01),
            checkboxInput("corr_blank","Hide non-significant", value=FALSE),
            selectInput("corr_pal","Palette:",
              choices=c("RdBu","PRGn","PiYG","RdYlGn"), selected="RdBu"),
            actionButton("run_corr","🔗 Run", class="btn-primary btn-lg")
          ),
          box(title="📊 Correlation Matrix", width=9, solidHeader=TRUE, status="danger",
            plotOutput("corr_plot", height="530px")
          )
        ),

        fluidRow(
          box(title="📈 X-Y Scatter Plot", width=12, solidHeader=TRUE, status="primary",
            uiOutput("scatter_sel"),
            plotlyOutput("scatter_plot", height="380px")
          )
        )
      ),

      # ══════════════════════════════════════════════════════════
      # TAB: PCA
      # ══════════════════════════════════════════════════════════
      tabItem(tabName = "pca",

        fluidRow(
          box(title="⚙️ PCA Settings", width=3, solidHeader=TRUE, status="danger",
            selectInput("pca_scale","Scale variables:",
              choices=c("Yes"="TRUE","No"="FALSE"), selected="TRUE"),
            uiOutput("pca_col_sel"),
            splitLayout(
              numericInput("pca_pc1","PC X:", value=1, min=1, max=20),
              numericInput("pca_pc2","PC Y:", value=2, min=1, max=20)
            ),
            actionButton("run_pca","🧬 Run PCA", class="btn-primary btn-lg")
          ),
          box(title="🔬 PCA Score Biplot", width=9, solidHeader=TRUE, status="danger",
            plotlyOutput("pca_biplot", height="460px")
          )
        ),

        fluidRow(
          column(6, box(title="📊 Scree Plot", width=12, solidHeader=TRUE, status="primary",
            plotOutput("scree_plot", height="320px")
          )),
          column(6, box(title="📌 Variable Loadings", width=12, solidHeader=TRUE, status="success",
            plotOutput("load_plot", height="320px")
          ))
        ),

        fluidRow(
          box(title="🌳 Hierarchical Clustering Dendrogram", width=12, solidHeader=TRUE, status="warning",
            fluidRow(
              column(8, uiOutput("clust_vars_sel")),
              column(2, numericInput("n_clust","# Clusters:", value=3, min=2, max=10)),
              column(2, tags$br(), actionButton("run_clust","🌳 Cluster", class="btn-warning"))
            ),
            plotOutput("dendro_plot", height="380px")
          )
        )
      ),

      # ══════════════════════════════════════════════════════════
      # TAB: GENOTYPE SELECTION
      # ══════════════════════════════════════════════════════════
      tabItem(tabName = "geno_sel",

        fluidRow(
          box(title="⚙️ Selection Settings", width=4, solidHeader=TRUE, status="danger",
            tags$div(
              style="background:#e8f5e9;border-left:4px solid #2d6a4f;padding:10px 14px;border-radius:4px;margin-bottom:12px;font-size:12px;",
              tags$b("How it works:"), tags$br(),
              "1. Set how many indices to use for selection.", tags$br(),
              "2. For each index: choose direction, weight, and an optional threshold.", tags$br(),
              "3. A 0-1 score is computed and plots ranked.", tags$br(),
              "4. Threshold lines appear on the chart to flag elite plots."
            ),
            uiOutput("gs_id_col_sel"),
            tags$hr(),
            tags$p(class="section-title", "Select Indices for Ranking"),
            uiOutput("gs_idx_picker"),
            tags$hr(),
            uiOutput("gs_criteria_ui"),
            tags$hr(),
            actionButton("run_gs", "🏆 Rank & Categorize", class="btn-primary btn-lg"),
            tags$br(), tags$br(),
            downloadButton("dl_gs", "⬇️ Download Rankings (.xlsx)", class="btn-success")
          ),

          box(title="🏆 Ranked Plots", width=8, solidHeader=TRUE, status="danger",
            uiOutput("gs_category_summary"),
            tags$br(),
            DTOutput("gs_rank_tbl")
          )
        ),

        fluidRow(
          box(title="📊 Index Value Chart with Thresholds", width=8, solidHeader=TRUE, status="success",
            fluidRow(
              column(4, numericInput("gs_top_n", "Show top N plots:", value=30, min=5, max=500)),
              column(4, uiOutput("gs_plot_idx_sel")),
              column(4, tags$br(), actionButton("gs_render_chart", "📊 Update", class="btn-info"))
            ),
            tags$br(),
            plotlyOutput("gs_bar_plot", height="480px")
          ),
          box(title="🌐 Multi-Index Radar / Parallel Plot", width=4, solidHeader=TRUE, status="primary",
            uiOutput("gs_radar_sel"),
            actionButton("gs_render_radar", "🌐 Render", class="btn-info"),
            tags$br(), tags$br(),
            plotlyOutput("gs_radar_plot", height="400px")
          )
        )
      ),


      # ══════════════════════════════════════════════════════════
      # TAB: MACHINE LEARNING
      # ══════════════════════════════════════════════════════════
      tabItem(tabName = "ml",

        fluidRow(
          box(title="⚙️ ML Configuration", width=4, solidHeader=TRUE, status="danger",

            tags$div(
              style="background:#e8f5e9;border-left:4px solid #2d6a4f;padding:10px 14px;border-radius:4px;margin-bottom:12px;font-size:12px;",
              tags$b("Workflow:"), tags$br(),
              "1. Select features (vegetation indices / bands)", tags$br(),
              "2. Select the target variable (from metadata)", tags$br(),
              "3. Choose task type, algorithms & CV settings", tags$br(),
              "4. Click Train — results, importance & predictions appear"
            ),

            tags$p(class="section-title", "Data"),
            uiOutput("ml_feature_sel"),
            uiOutput("ml_target_sel"),
            radioGroupButtons("ml_task", "Task type:",
              choices=c("Regression"="reg","Classification"="cls"),
              selected="reg", status="danger", justified=TRUE),

            tags$hr(),
            tags$p(class="section-title", "Algorithms"),
            checkboxGroupInput("ml_algos", NULL,
              choices=c(
                "Random Forest"         = "rf",
                "Gradient Boosting"     = "gbm",
                "XGBoost"               = "xgbTree",
                "SVM (Radial)"          = "svmRadial",
                "SVM (Linear)"          = "svmLinear",
                "Neural Network"        = "nnet",
                "Elastic Net"           = "glmnet",
                "k-Nearest Neighbours"  = "kknn",
                "Decision Tree"         = "rpart",
                "Naive Bayes"           = "naive_bayes",
                "Bagging (treebag)"     = "treebag",
                "LDA"                   = "lda"
              ),
              selected = c("rf","gbm","svmRadial","glmnet")
            ),

            tags$hr(),
            tags$p(class="section-title", "Cross-Validation & Split"),
            splitLayout(
              numericInput("ml_cv_k",   "Folds:",   value=5, min=2, max=20),
              numericInput("ml_cv_rep", "Repeats:", value=3, min=1, max=10)
            ),
            sliderInput("ml_train_pct", "Training %:", min=50, max=90, value=75, step=5),

            tags$hr(),
            tags$p(class="section-title", "Algorithm Hyperparameters"),
            tags$small(style="color:#888;font-size:11px;",
              "Leave blank / 0 to use automatic tuning (tuneLength=5)."),
            tags$br(),tags$br(),

            # RF
            conditionalPanel("input.ml_algos.indexOf('rf') >= 0",
              tags$b(style="font-size:11px;color:#CC0000;","Random Forest"),
              splitLayout(
                numericInput("hp_rf_ntree", "ntree:", value=500, min=50, max=5000, step=50),
                numericInput("hp_rf_mtry",  "mtry (0=auto):", value=0, min=0, max=50)
              )
            ),
            # GBM
            conditionalPanel("input.ml_algos.indexOf('gbm') >= 0",
              tags$b(style="font-size:11px;color:#CC0000;","GBM"),
              splitLayout(
                numericInput("hp_gbm_trees",  "n.trees:", value=200, min=50, max=2000, step=50),
                numericInput("hp_gbm_depth",  "depth:",   value=3,   min=1,  max=10)
              ),
              numericInput("hp_gbm_shrink","shrinkage:", value=0.05, min=0.001, max=0.5, step=0.01)
            ),
            # XGBoost
            conditionalPanel("input.ml_algos.indexOf('xgbTree') >= 0",
              tags$b(style="font-size:11px;color:#CC0000;","XGBoost"),
              splitLayout(
                numericInput("hp_xgb_rounds","nrounds:", value=150, min=10, max=1000, step=10),
                numericInput("hp_xgb_depth", "max_depth:", value=4, min=1, max=15)
              ),
              numericInput("hp_xgb_eta","eta (learning rate):", value=0.1, min=0.001, max=0.5, step=0.01)
            ),
            # SVM Radial
            conditionalPanel("input.ml_algos.indexOf('svmRadial') >= 0",
              tags$b(style="font-size:11px;color:#CC0000;","SVM Radial"),
              splitLayout(
                numericInput("hp_svmR_C",     "Cost (C):",   value=1,    min=0.01, max=1000),
                numericInput("hp_svmR_sigma", "Sigma (0=auto):", value=0, min=0,   max=10, step=0.01)
              )
            ),
            # SVM Linear
            conditionalPanel("input.ml_algos.indexOf('svmLinear') >= 0",
              tags$b(style="font-size:11px;color:#CC0000;","SVM Linear"),
              numericInput("hp_svmL_C","Cost (C):", value=1, min=0.01, max=1000)
            ),
            # nnet
            conditionalPanel("input.ml_algos.indexOf('nnet') >= 0",
              tags$b(style="font-size:11px;color:#CC0000;","Neural Network"),
              splitLayout(
                numericInput("hp_nnet_size",  "Hidden units:", value=5,   min=1, max=100),
                numericInput("hp_nnet_decay", "Decay:",        value=0.1, min=0, max=1, step=0.01)
              )
            ),
            # glmnet
            conditionalPanel("input.ml_algos.indexOf('glmnet') >= 0",
              tags$b(style="font-size:11px;color:#CC0000;","Elastic Net"),
              splitLayout(
                numericInput("hp_glm_alpha","Alpha (0=Ridge,1=Lasso):", value=0.5, min=0, max=1, step=0.1),
                numericInput("hp_glm_lambda","Lambda (0=auto):", value=0, min=0, step=0.001)
              )
            ),
            # kknn
            conditionalPanel("input.ml_algos.indexOf('kknn') >= 0",
              tags$b(style="font-size:11px;color:#CC0000;","k-Nearest Neighbours"),
              numericInput("hp_kknn_k","k (neighbours):", value=7, min=1, max=50)
            ),
            # rpart
            conditionalPanel("input.ml_algos.indexOf('rpart') >= 0",
              tags$b(style="font-size:11px;color:#CC0000;","Decision Tree"),
              numericInput("hp_rpart_cp","Complexity (cp):", value=0.01, min=0, max=0.5, step=0.001)
            ),

            tags$hr(),
            tags$p(class="section-title", "Feature Selection"),
            checkboxInput("ml_rfe", "Run RFE (Recursive Feature Elimination)", value=FALSE),
            numericInput("ml_rfe_sizes", "Max features to test:", value=10, min=2, max=30),

            tags$br(),
            actionButton("run_ml", "🤖 Train Models", class="btn-primary btn-lg",
                         style="width:100%;"),
            tags$br(), tags$br(),
            uiOutput("ml_progress_ui")
          ),

          box(title="📊 Model Performance", width=8, solidHeader=TRUE, status="danger",
            tabsetPanel(
              tabPanel("📈 Comparison",
                tags$br(),
                plotlyOutput("ml_perf_plot", height="380px"),
                tags$br(),
                DTOutput("ml_perf_tbl")
              ),
              tabPanel("🌳 Feature Importance",
                tags$br(),
                uiOutput("ml_imp_model_sel"),
                plotlyOutput("ml_imp_plot", height="420px")
              ),
              tabPanel("🎯 Predictions",
                tags$br(),
                uiOutput("ml_pred_model_sel"),
                plotlyOutput("ml_pred_plot", height="380px"),
                tags$br(),
                DTOutput("ml_pred_tbl")
              ),
              tabPanel("🔍 RFE Results",
                tags$br(),
                plotlyOutput("ml_rfe_plot", height="360px"),
                tags$br(),
                uiOutput("ml_rfe_vars_ui")
              )
            )
          )
        ),

        fluidRow(
          box(title="🥇 Best Model Selection", width=6, solidHeader=TRUE, status="warning",
            fluidRow(
              column(6,
                radioGroupButtons("ml_best_metric","Rank models by:",
                  choices=c("RMSE (↓)"="RMSE","R² (↑)"="R2",
                            "MAE (↓)"="MAE","Accuracy (↑)"="Accuracy",
                            "Kappa (↑)"="Kappa"),
                  selected="RMSE", status="warning", justified=TRUE)
              ),
              column(6, tags$br(), uiOutput("ml_best_model_ui"))
            )
          ),
          box(title="📥 Export ML Results", width=6, solidHeader=TRUE, status="success",
            tags$div(style="display:flex;gap:12px;flex-wrap:wrap;align-items:center;",
              downloadButton("dl_ml_xlsx", "⬇️ Full Results (.xlsx)", class="btn-success"),
              downloadButton("dl_ml_preds","⬇️ Predictions (.csv)",   class="btn-info")
            )
          )
        )
      ),


      # ══════════════════════════════════════════════════════════
      # TAB: DATA QC
      # ══════════════════════════════════════════════════════════
      tabItem(tabName = "dataQC",

        fluidRow(
          box(title="📊 Data Completeness Heatmap", width=8,
              solidHeader=TRUE, status="danger",
            tags$div(style="font-size:12px;color:#666;margin-bottom:8px;",
              "Each cell shows the % of valid (non-NA, non-Inf) values per index.",
              " Red = many missing values, green = complete."),
            plotlyOutput("qc_completeness", height="380px")
          ),
          box(title="⚙️ QC Settings", width=4, solidHeader=TRUE, status="primary",
            sliderInput("qc_outlier_k","IQR multiplier for outliers:",
              min=1.0, max=4.0, value=1.5, step=0.1),
            actionButton("run_qc","🔍 Run QC Analysis", class="btn-primary btn-lg"),
            tags$hr(),
            uiOutput("qc_summary_ui")
          )
        ),

        fluidRow(
          box(title="📦 Outlier Summary per Index", width=6,
              solidHeader=TRUE, status="warning",
            plotlyOutput("qc_outlier_bar", height="340px")
          ),
          box(title="📋 Flagged Plots", width=6,
              solidHeader=TRUE, status="warning",
            tags$p(style="font-size:12px;color:#666;",
              "Plots flagged as outliers in ≥ 1 index:"),
            DTOutput("qc_outlier_tbl")
          )
        ),

        fluidRow(
          box(title="📈 Value Range Checker", width=12,
              solidHeader=TRUE, status="success",
            tags$div(style="font-size:12px;color:#666;margin-bottom:8px;",
              "Box ranges show the distribution of each index.",
              " Expected reflectance range 0–1 shown as dashed lines."),
            plotlyOutput("qc_range_plot", height="360px")
          )
        )
      ),

      # ══════════════════════════════════════════════════════════
      # TAB: SPECTRAL SIGNATURES
      # ══════════════════════════════════════════════════════════
      tabItem(tabName = "spectral",

        fluidRow(
          box(title="⚙️ Settings", width=3, solidHeader=TRUE, status="danger",
            tags$div(
              style="background:#e8f5e9;border-left:4px solid #2d6a4f;padding:10px;border-radius:4px;margin-bottom:12px;font-size:12px;",
              tags$b("Spectral Signature Chart"), tags$br(),
              "Shows mean reflectance of each band per genotype/group.",
              " Requires raw band data (Red, Green, Blue, RedEdge, NIR)."
            ),
            uiOutput("spec_grp_sel"),
            uiOutput("spec_id_sel"),
            sliderInput("spec_top_n","Show top N groups:",
              min=2, max=30, value=10, step=1),
            radioGroupButtons("spec_err","Error bars:",
              choices=c("SD"="sd","SE"="se","None"="none"),
              selected="se", status="danger", size="sm"),
            actionButton("render_spec","📡 Render Signatures", class="btn-primary btn-lg")
          ),
          box(title="📡 Mean Spectral Reflectance by Group", width=9,
              solidHeader=TRUE, status="danger",
            tags$div(style="font-size:11.5px;color:#888;margin-bottom:6px;",
              "Each line = mean reflectance across spectral bands. ",
              "Hover for values. Zoom/pan/save with toolbar."),
            plotlyOutput("spec_plot", height="480px")
          )
        ),

        fluidRow(
          box(title="🌡️ Band Heatmap — All Plots", width=12,
              solidHeader=TRUE, status="primary",
            uiOutput("spec_band_sel"),
            actionButton("render_spec_heat","🌡️ Render Heatmap", class="btn-info"),
            tags$br(),tags$br(),
            plotlyOutput("spec_heatmap", height="400px")
          )
        )
      ),

      # ══════════════════════════════════════════════════════════
      # TAB: REPORT GENERATOR
      # ══════════════════════════════════════════════════════════
      tabItem(tabName = "report",

        fluidRow(
          box(title="📑 Report Settings", width=4, solidHeader=TRUE, status="danger",
            tags$div(
              style="background:#e8f5e9;border-left:4px solid #2d6a4f;padding:10px;border-radius:4px;margin-bottom:14px;font-size:12px;",
              tags$b("Auto-generate a full HTML report"), tags$br(),
              "Includes summary stats, field maps, distribution plots, ",
              "correlation matrix, PCA, and genotype rankings — all in one downloadable document."
            ),
            textInput("rep_title","Report title:",
              value="Dry Bean Drone Analysis Report"),
            textInput("rep_author","Author / PI:",
              value="Dry Bean Breeding & Computational Biology, University of Guelph"),
            textInput("rep_exp","Experiment / Season:", placeholder="e.g. Guelph 2024 Trial 1"),
            checkboxGroupInput("rep_sections","Include sections:",
              choices=c(
                "Executive Summary"          = "summary",
                "Drone Mosaic & Field Layout" = "mosaic",
                "Data Overview"              = "overview",
                "Field Heatmaps"             = "maps",
                "Index Distributions"        = "dists",
                "Summary Statistics"         = "stats",
                "Correlation Matrix"         = "corr",
                "PCA Analysis"               = "pca",
                "Genotype Rankings"          = "rankings",
                "ML Results"                 = "ml"
              ),
              selected = c("summary","mosaic","overview","maps","dists","stats","corr","rankings")
            ),
            tags$hr(),
            tags$p(style="font-size:12px;color:#666;",
              "Output format: self-contained HTML (no internet needed to view)"),
            actionButton("gen_report","📄 Generate Report", class="btn-primary btn-lg",
                         style="width:100%;"),
            tags$br(),tags$br(),
            uiOutput("report_status_ui")
          ),

          box(title="📋 Report Preview", width=8, solidHeader=TRUE, status="primary",
            tags$div(style="font-size:12px;color:#666;margin-bottom:10px;",
              "After generating, download and open the HTML file in any browser."),
            uiOutput("report_preview_ui"),
            tags$br(),
            downloadButton("dl_report","⬇️ Download HTML Report",
                           class="btn-success btn-lg", style="width:100%;")
          )
        )
      ),

      # ══════════════════════════════════════════════════════════
      # TAB: ADMIN DASHBOARD (admin only)
      # ══════════════════════════════════════════════════════════
      tabItem(tabName = "admin",

        fluidRow(
          box(title="👥 Active Sessions & Login History", width=8,
              solidHeader=TRUE, status="danger",
            tags$div(style="font-size:12px;color:#666;margin-bottom:8px;",
              "Live view of who has logged in and what they are doing.",
              " Refreshes every 10 seconds automatically."),
            actionButton("refresh_log", "🔄 Refresh Now",
                         class="btn-warning btn-sm", style="margin-bottom:10px;"),
            downloadButton("dl_usage_log", "⬇️ Download Full Log",
                           class="btn-info btn-sm", style="margin-bottom:10px;margin-left:6px;"),
            DTOutput("admin_log_tbl")
          ),
          box(title="📊 Usage Summary", width=4,
              solidHeader=TRUE, status="primary",
            uiOutput("admin_summary_ui"),
            tags$hr(),
            plotlyOutput("admin_tab_chart", height="220px")
          )
        ),

        fluidRow(
          box(title="📈 Activity Timeline — Logins per Day", width=8,
              solidHeader=TRUE, status="success",
            plotlyOutput("admin_timeline", height="260px")
          ),
          box(title="⚙️ Log Management", width=4,
              solidHeader=TRUE, status="warning",
            tags$div(style="background:#fff3cd;border-left:4px solid #e6a800;
                            padding:10px;border-radius:4px;margin-bottom:12px;font-size:12px;",
              tags$b("⚠️ Warning:"), " Clearing the log is permanent and cannot be undone."
            ),
            actionButton("clear_log", "🗑️ Clear Usage Log",
                         class="btn-danger", style="width:100%;"),
            tags$hr(),
            tags$p(style="font-size:11.5px;color:#888;",
              "Log file: usage_log.csv (saved in app working directory)"),
            tags$p(style="font-size:11.5px;color:#888;",
              "Each row = one tab visit. Timestamps in server local time.")
          )
        )
      ),

      # ══════════════════════════════════════════════════════════
      # TAB: EXPORT
      # ══════════════════════════════════════════════════════════
      tabItem(tabName = "export",

        fluidRow(
          box(title="💾 Download Options", width=5, solidHeader=TRUE, status="danger",

            tags$p(class="section-title","📊 Data Downloads"),
            tags$div(style="display:flex; gap:10px; flex-wrap:wrap;",
              downloadButton("dl_csv",  "⬇️ CSV",          class="btn-success"),
              downloadButton("dl_xlsx", "⬇️ Excel (.xlsx)", class="btn-success")
            ),
            tags$small(style="color:#888;",
              "Excel includes: Index Data · Summary Statistics · Index Definitions · Correlation Matrix · Raw Bands · PCA Loadings"),
            tags$hr(),

            tags$p(class="section-title","📈 Export Current Plot"),
            uiOutput("plot_export_sel"),
            splitLayout(
              numericInput("exp_w","Width (in):",  value=10, min=4, max=24),
              numericInput("exp_h","Height (in):", value=7,  min=3, max=18),
              selectInput("exp_fmt","Format:", choices=c("png","pdf","svg"), selected="png")
            ),
            downloadButton("dl_plot","⬇️ Download Plot", class="btn-info")
          ),

          box(title="📋 Full Data Table", width=7, solidHeader=TRUE, status="primary",
            DTOutput("full_data_tbl")
          )
        )
      )

    ) # end tabItems
  ) # end dashboardBody
) # end dashboardPage

# Apply login wrapper only when credentials are configured
ui <- if (USE_AUTH) {
  shinymanager::secure_app(
    raw_ui,
    enable_admin    = TRUE,
    choose_language = FALSE,
    background      = "linear-gradient(135deg,#0d0d1a 0%,#1a1a2e 35%,#0f3460 65%,#8B0000 100%)",

    tags_top = tags$div(
      # Inject login page custom CSS
      tags$style(HTML("
        @import url('https://fonts.googleapis.com/css2?family=Playfair+Display:wght@700&family=Source+Sans+Pro:wght@300;400;600&display=swap');

        /* Full-page background */
        body { margin:0; padding:0;
          background: linear-gradient(135deg,#0d0d1a 0%,#1a1a2e 35%,#0f3460 65%,#8B0000 100%) !important;
          min-height: 100vh;
        }

        /* Animated floating particles */
        body::before {
          content:''; position:fixed; top:0; left:0; width:100%; height:100%;
          background-image:
            radial-gradient(circle at 20% 50%, rgba(204,0,0,0.08) 0%, transparent 50%),
            radial-gradient(circle at 80% 20%, rgba(255,199,44,0.06) 0%, transparent 40%),
            radial-gradient(circle at 50% 80%, rgba(15,52,96,0.4) 0%, transparent 50%);
          pointer-events:none; z-index:0;
        }

        /* Login card */
        #auth-panel,
        .panel-auth {
          background: rgba(255,255,255,0.97) !important;
          border-radius: 18px !important;
          box-shadow: 0 30px 80px rgba(0,0,0,0.5), 0 0 0 1px rgba(255,199,44,0.2) !important;
          border: none !important;
          overflow: hidden;
          animation: cardIn 0.6s cubic-bezier(0.34,1.56,0.64,1) both;
          position: relative; z-index:1;
        }
        @keyframes cardIn {
          from { opacity:0; transform: translateY(40px) scale(0.95); }
          to   { opacity:1; transform: translateY(0)    scale(1); }
        }

        /* Top accent bar on card */
        #auth-panel::before, .panel-auth::before {
          content:'';
          display:block; height:5px; width:100%;
          background: linear-gradient(90deg, #CC0000, #FFC72C, #2d6a4f);
        }

        /* Input fields */
        #auth-panel input, .panel-auth input {
          border-radius: 8px !important;
          border: 2px solid #e0e0e0 !important;
          padding: 10px 14px !important;
          font-size: 14px !important;
          transition: border-color 0.2s, box-shadow 0.2s !important;
          font-family: 'Source Sans Pro', sans-serif !important;
        }
        #auth-panel input:focus, .panel-auth input:focus {
          border-color: #CC0000 !important;
          box-shadow: 0 0 0 3px rgba(204,0,0,0.12) !important;
          outline: none !important;
        }

        /* Login button */
        #auth-panel .btn-primary,
        .panel-auth .btn-primary,
        #go_auth {
          background: linear-gradient(135deg, #CC0000, #8B0000) !important;
          border: none !important;
          border-radius: 8px !important;
          padding: 11px 28px !important;
          font-size: 14px !important;
          font-weight: 700 !important;
          font-family: 'Source Sans Pro', sans-serif !important;
          letter-spacing: 0.5px !important;
          transition: all 0.25s !important;
          box-shadow: 0 4px 15px rgba(204,0,0,0.35) !important;
          width: 100% !important;
        }
        #auth-panel .btn-primary:hover,
        .panel-auth .btn-primary:hover,
        #go_auth:hover {
          background: linear-gradient(135deg, #8B0000, #CC0000) !important;
          transform: translateY(-2px) !important;
          box-shadow: 0 8px 25px rgba(204,0,0,0.45) !important;
        }

        /* Labels */
        #auth-panel label, .panel-auth label {
          font-family: 'Source Sans Pro', sans-serif !important;
          font-weight: 600 !important;
          color: #1a1a2e !important;
          font-size: 13px !important;
          letter-spacing: 0.3px !important;
        }

        /* Hide default shinymanager logo/title */
        #auth-panel > .panel > .panel-heading,
        .panel-auth > .panel > .panel-heading { display:none !important; }
      ")),

      # Custom header inside the card
      tags$div(
        style = paste0(
          "text-align:center;padding:28px 20px 8px;",
          "background:linear-gradient(135deg,#1a1a2e,#0f3460);",
          "margin:-1px -1px 0;"
        ),
        # Logo — using base64 embed so it works from GitHub download
        tags$img(
          src   = "www/logo.png",
          height= "90px",
          style = paste0(
            "max-width:280px;object-fit:contain;",
            "filter:drop-shadow(0 3px 12px rgba(0,0,0,0.6));",
            "animation:logoPop 0.7s cubic-bezier(0.34,1.56,0.64,1) both;"
          ),
          onerror = "this.style.display='none';"
        ),
        tags$h2("AllInOne Phenomics",
          style = paste0(
            "font-family:'Playfair Display',serif;font-size:22px;",
            "color:#FFC72C;margin:12px 0 4px;letter-spacing:0.5px;",
            "text-shadow:0 2px 8px rgba(0,0,0,0.4);"
          )
        ),
        tags$p("Dry Bean Breeding & Computational Biology",
          style="color:rgba(255,255,255,0.8);font-size:12.5px;margin:0 0 2px;"),
        tags$p("University of Guelph",
          style="color:rgba(255,199,44,0.9);font-size:11.5px;font-weight:600;margin:0 0 18px;"),
        tags$div(
          style="display:flex;justify-content:center;gap:10px;margin-bottom:16px;",
          tags$a(href="https://www.uogbeans.com", target="_blank",
            style=paste0("color:#FFC72C;font-size:11px;font-weight:600;",
                         "text-decoration:none;background:rgba(255,199,44,0.1);",
                         "padding:3px 12px;border-radius:12px;border:1px solid rgba(255,199,44,0.3);"),
            "🌐 uogbeans.com")
        )
      )
    )
  )
} else {
  raw_ui
}

# ── SERVER ─────────────────────────────────────────────────────
server <- function(input, output, session) {

  # ── Authenticate session if auth is enabled ──────────────────
  if (USE_AUTH) {
    # Custom checker: compares SHA-256 hash of entered password to stored hash
    sha256_check <- function(user, password) {
      row <- creds[creds$user == user, , drop = FALSE]
      if (nrow(row) == 0) return(list(result = FALSE))
      entered_hash <- tryCatch(
        digest::digest(password, algo = "sha256"),
        error = function(e) ""
      )
      if (nchar(entered_hash) > 0 && entered_hash == row$password[1]) {
        list(result = TRUE, admin = isTRUE(row$admin[1]))
      } else {
        list(result = FALSE)
      }
    }

    res_auth <- shinymanager::secure_server(
      check_credentials = sha256_check
    )
    # ── Full usage tracking ─────────────────────────────────────
    current_user <- reactive({
      tryCatch({
        u <- res_auth()$user
        if (is.null(u) || length(u) == 0 || !nzchar(u)) return("")
        as.character(u[1])
      }, error=function(e) "")
    })

    is_admin <- reactive({
      tryCatch({
        adm <- res_auth()$admin
        isTRUE(adm)
      }, error=function(e) FALSE)
    })

    # Show/hide Admin menu item based on role
    output$admin_menu_item <- renderUI({
      req(is_admin())
      if (is_admin())
        menuItem("🛡️  Admin Dashboard", tabName="admin",
                 icon=icon("user-shield"))
      else NULL
    })

    # Log every tab change
    write_log <- function(user, action, detail="") {
      if (!nzchar(user)) return()
      tryCatch({
        log_file <- "usage_log.csv"
        log_row  <- data.frame(
          timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          user      = user,
          action    = action,
          detail    = detail,
          session   = session$token,
          stringsAsFactors = FALSE
        )
        write.table(log_row, log_file, append=TRUE, sep=",",
                    col.names=!file.exists(log_file),
                    row.names=FALSE, quote=TRUE)
      }, error=function(e) NULL)
    }

    # Log login event
    observeEvent(current_user(), {
      u <- current_user()
      if (nzchar(u)) write_log(u, "LOGIN", "Session started")
    }, once=TRUE, ignoreInit=FALSE)

    # Log every tab navigation
    observeEvent(input$sidebar, {
      u <- current_user()
      if (nzchar(u) && !is.null(input$sidebar))
        write_log(u, "TAB_VISIT", input$sidebar)
    })

    # Log key actions
    observeEvent(input$load_data,    { write_log(current_user(), "ACTION", "Load All Data") })
    observeEvent(input$run_indices,  { write_log(current_user(), "ACTION", "Calculate Indices") })
    observeEvent(input$render_maps,  { write_log(current_user(), "ACTION", "Render Field Maps") })
    observeEvent(input$run_ml,       { write_log(current_user(), "ACTION", "Run ML Models") })
    observeEvent(input$gen_report,   { write_log(current_user(), "ACTION", "Generate Report") })
    observeEvent(input$dl_csv,       { write_log(current_user(), "EXPORT", "Download CSV") })
    observeEvent(input$dl_excel,     { write_log(current_user(), "EXPORT", "Download Excel") })
  }


  rv <- reactiveValues(
    mosaic      = NULL,
    masked      = NULL,
    shape       = NULL,
    meta        = NULL,
    idx_ras     = NULL,
    extracted   = NULL,
    band_data   = NULL,
    merged      = NULL,
    custom      = list(),
    pca_obj     = NULL,
    corner_pts  = list(),
    cancel_map  = FALSE,   # cancel flag for field map rendering
    map_busy    = FALSE    # TRUE while rendering
  )

  # ── Pipeline Status ──────────────────────────────────────────
  mk_st <- function(ok) if (ok) tags$span(class="s-ok","✓") else tags$span(class="s-pnd","…")
  output$st_raster <- renderUI(mk_st(!is.null(rv$mosaic)))
  output$st_mask   <- renderUI(mk_st(!is.null(rv$masked)))
  output$st_grid   <- renderUI(mk_st(!is.null(rv$shape)))
  output$st_idx    <- renderUI(mk_st(!is.null(rv$idx_ras)))
  output$st_data   <- renderUI(mk_st(!is.null(rv$meta)))

  # ── Get band numbers ─────────────────────────────────────────
  get_bands <- function() {
    if (input$camera_type == "rgb") {
      list(r=as.integer(input$rgb_r), g=as.integer(input$rgb_g), b=as.integer(input$rgb_b))
    } else {
      list(b=as.integer(input$ms_b), g=as.integer(input$ms_g), r=as.integer(input$ms_r),
           re=as.integer(input$ms_re), nir=as.integer(input$ms_nir))
    }
  }

  # ── Load Data ────────────────────────────────────────────────
  observeEvent(input$load_data, {
    msgs <- c()
    withProgress(message="Loading...", value=0, {

      # ── Reset ALL previous session data before loading new image ──
      incProgress(0.05, "Clearing previous data…")
      rv$mosaic       <- NULL
      rv$masked       <- NULL
      rv$shape        <- NULL
      rv$meta         <- NULL
      rv$merged       <- NULL
      rv$idx_ras      <- NULL
      rv$extracted    <- NULL
      rv$pca_obj      <- NULL
      rv$ml_results   <- NULL
      rv$corner_pts   <- list()
      rv$cancel_map   <- FALSE
      rv$report_path  <- NULL
      rv$report_ready <- FALSE

      if (!is.null(input$tif_file)) {
        incProgress(0.3, "Loading raster…")
        tryCatch({
          rv$mosaic <- terra::rast(input$tif_file$datapath)
          msgs <- c(msgs, sprintf("✅ Raster: %d bands  ·  %d × %d px",
            terra::nlyr(rv$mosaic), nrow(rv$mosaic), ncol(rv$mosaic)))
          # Auto-update band scaling dropdown based on detected range
          tryCatch({
            samp_v <- as.vector(terra::values(rv$mosaic[[1]]))
            samp_v <- samp_v[is.finite(samp_v)]
            mx_v   <- max(samp_v, na.rm=TRUE)
            suggested <- if (mx_v <= 1.01)    "none"
                         else if (mx_v <= 256) "255"
                         else if (mx_v <= 10001)"10000"
                         else                  "65535"
            shiny::updateSelectInput(session, "band_scale", selected=suggested)
          }, error=function(e) NULL)
        }, error=function(e) msgs <<- c(msgs, paste("❌ Raster:", e$message)))
      }

      if (!is.null(input$shape_file)) {
        incProgress(0.3, "Loading plot grid…")
        tryCatch({
          rv$shape <- readRDS(input$shape_file$datapath)
          msgs <- c(msgs, sprintf("✅ Shape: %d plots", nrow(rv$shape)))
        }, error=function(e) msgs <<- c(msgs, paste("❌ Shape:", e$message)))
      }

      if (!is.null(input$csv_file)) {
        incProgress(0.3, "Loading metadata…")
        tryCatch({
          rv$meta <- read.csv(input$csv_file$datapath, header=TRUE, stringsAsFactors=FALSE)
          msgs <- c(msgs, sprintf("✅ Metadata: %d rows  ·  %d columns",
            nrow(rv$meta), ncol(rv$meta)))
        }, error=function(e) msgs <<- c(msgs, paste("❌ CSV:", e$message)))
      }

      incProgress(0.1, "Done")
    })
    output$load_log <- renderText(paste(msgs, collapse="\n"))
  })

  output$raster_info_ui <- renderUI({
    req(rv$mosaic)
    # Sample a quick range from band 1
    samp <- tryCatch({
      v <- as.vector(terra::values(rv$mosaic[[1]]))
      v <- v[is.finite(v)]
      c(min(v), max(v))
    }, error=function(e) c(NA,NA))
    raw_range <- if (all(is.finite(samp)))
      sprintf("%.1f – %.1f", samp[1], samp[2]) else "unknown"
    # Suggest divisor
    mx <- samp[2]
    suggest <- if (!is.finite(mx))  "unknown"
               else if (mx <= 1)    "Already 0-1 (none)"
               else if (mx <= 255)  "÷ 255 (8-bit)"
               else if (mx <= 10000)"÷ 10000 (MicaSense)"
               else                 "÷ 65535 (16-bit)"
    tags$div(style="background:#e8f5e9;border-radius:5px;padding:8px;font-size:12px;",
      tags$p(style="margin:2px 0;font-weight:700;", paste("Bands:", terra::nlyr(rv$mosaic))),
      tags$p(style="margin:2px 0;", paste("Size:", nrow(rv$mosaic), "×", ncol(rv$mosaic))),
      tags$p(style="margin:2px 0;", paste("Raw range (band 1):", raw_range)),
      tags$p(style="margin:2px 0;color:#2d6a4f;font-weight:600;",
             paste("Suggested scaling:", suggest)),
      tags$p(style="margin:2px 0;", paste("CRS:",
        tryCatch(terra::crs(rv$mosaic, describe=TRUE)$name[1], error=function(e)"unknown")))
    )
  })

  output$meta_preview <- renderDT({
    req(rv$meta)
    datatable(rv$meta, options=list(pageLength=8, scrollX=TRUE), rownames=FALSE)
  })

  # ── Mosaic Plot ──────────────────────────────────────────────
  plot_mosaic <- function(r, title="Mosaic") {
    b <- get_bands()
    nb <- terra::nlyr(r)
    if (nb >= 3) {
      terra::plotRGB(r, r=b$r, g=b$g, b=b$b, main=title, stretch="lin")
    } else {
      terra::plot(r[[1]], main=title, col=viridis::viridis(256))
    }
  }

  output$mosaic_plot <- renderPlot({
    req(rv$mosaic); input$refresh_mosaic
    r <- if (!is.null(rv$masked)) rv$masked else rv$mosaic
    plot_mosaic(r, "Current Mosaic")
  })

  output$pre_mask  <- renderPlot({ req(rv$mosaic); plot_mosaic(rv$mosaic,  "Original") })
  output$post_mask <- renderPlot({ req(rv$masked); plot_mosaic(rv$masked, "After Soil Removal") })

  # ── Apply Soil Mask ──────────────────────────────────────────
  observeEvent(input$apply_mask, {
    req(rv$mosaic)
    withProgress(message="Applying soil mask…", {
      tryCatch({
        b <- get_bands()
        r <- rv$mosaic
        d <- get_scale_divisor(r)
        R <- terra::clamp(r[[b$r]]/d, 0, 1)
        G <- terra::clamp(r[[b$g]]/d, 0, 1)
        B <- terra::clamp(r[[b$b]]/d, 0, 1)

        mask_lyr <- switch(input$mask_idx,
          "BGI"        = (G-B)/(G+B),
          "HUE"        = atan2(sqrt(3)*(G-B), (R-G)+(R-B)),
          "ExG"        = 2*G - R - B,
          "NDVI_approx"= (G-R)/(G+R),
          (G-B)/(G+B)
        )

        if (as.logical(input$crop_above)) {
          mk <- terra::ifel(mask_lyr > input$crop_val, 1, NA)
        } else {
          mk <- terra::ifel(mask_lyr < input$crop_val, 1, NA)
        }

        rv$masked <- terra::mask(r, mk)
        output$mask_log <- renderText("✅ Mask applied")
      }, error=function(e) output$mask_log <- renderText(paste("❌", e$message)))
    })
  })

  # ── Plot Grid ────────────────────────────────────────────────

  # Show how many corner points have been selected
  output$pts_status_ui <- renderUI({
    n <- length(rv$corner_pts)
    cols <- c("#CC0000","#e6a800","#2d6a4f","#0f3460")
    labels <- c("TL / P1","TR / P2","BR / P3","BL / P4")
    dots <- lapply(1:4, function(i) {
      bg <- if (i <= n) cols[i] else "#ccc"
      lbl <- if (i <= n) {
        pt <- rv$corner_pts[[i]]
        sprintf("%s\n(%.1f, %.1f)", labels[i], pt[1], pt[2])
      } else labels[i]
      tags$div(
        style=sprintf("display:inline-block;background:%s;color:#fff;border-radius:6px;
          padding:4px 8px;margin:3px;font-size:11px;font-weight:600;", bg),
        lbl
      )
    })
    tags$div(
      tags$b(style="font-size:12px;", sprintf("Corner points: %d / 4", n)),
      tags$br(),
      do.call(tags$div, dots),
      if (n == 4)
        tags$p(style="color:#2d6a4f;font-weight:700;font-size:12px;margin-top:6px;",
               "✅ All 4 corners set — ready to generate grid!")
      else
        tags$p(style="color:#e6a800;font-size:11.5px;margin-top:4px;",
               sprintf("Click %d more point(s) on the map →", 4-n))
    )
  })

  # Collect clicks — up to 4 points
  observeEvent(input$grid_click, {
    if (length(rv$corner_pts) < 4) {
      rv$corner_pts <- c(rv$corner_pts,
                         list(c(input$grid_click$x, input$grid_click$y)))
    } else {
      showNotification("Already have 4 points. Click 'Clear Points' to reset.", type="warning", duration=3)
    }
  })

  observeEvent(input$clear_pts, {
    rv$corner_pts <- list()
    rv$shape <- NULL
    showNotification("Points cleared.", type="message", duration=2)
  })

  output$grid_info <- renderText({
    if (!is.null(rv$shape)) {
      n <- if (inherits(rv$shape,"SpatVector")) nrow(rv$shape) else length(rv$shape)
      paste("✅ Grid ready:", n, "plots")
    } else if (length(rv$corner_pts) == 4) {
      "4 corners selected — click Generate Grid"
    } else {
      paste0("Select ", 4 - length(rv$corner_pts), " more corner point(s) on the map.")
    }
  })

  # Sort 4 points into convex-hull order (CCW: BL, BR, TR, TL)
  sort_corners <- function(pts_list) {
    m  <- do.call(rbind, pts_list)          # 4×2
    cx <- mean(m[,1]); cy <- mean(m[,2])
    ang <- atan2(m[,2]-cy, m[,1]-cx)
    m[order(ang), ]                         # CCW order
  }

  observeEvent(input$gen_grid, {
    req(rv$mosaic)

    # ── If 4 corners were selected, use quadrilateral grid ──
    if (length(rv$corner_pts) == 4) {
      withProgress(message="Generating grid from 4 corners…", {
        tryCatch({
          pts <- sort_corners(rv$corner_pts)  # CCW: P0,P1,P2,P3
          # P0=BL, P1=BR, P2=TR, P3=TL
          P00 <- pts[1,]; P10 <- pts[2,]; P11 <- pts[3,]; P01 <- pts[4,]

          nc <- input$ncols; nr <- input$nrows

          bilerp <- function(u, v)
            (1-u)*(1-v)*P00 + u*(1-v)*P10 + u*v*P11 + (1-u)*v*P01

          polys <- lapply(seq_len(nr * nc), function(k) {
            row <- ceiling(k / nc); col <- ((k-1) %% nc) + 1
            u0 <- (col-1)/nc; u1 <- col/nc
            v0 <- (row-1)/nr; v1 <- row/nr
            coords <- rbind(bilerp(u0,v0), bilerp(u1,v0),
                            bilerp(u1,v1), bilerp(u0,v1),
                            bilerp(u0,v0))
            terra::vect(coords, "polygons", crs=terra::crs(rv$mosaic))
          })
          rv$shape <- do.call(rbind, polys)
          rv$shape$PlotID <- seq_len(nrow(rv$shape))   # 1-based
          showNotification(paste("✅ Grid:", nr*nc, "plots within selected area"),
                           type="message", duration=4)
        }, error=function(e) showNotification(paste("❌", e$message), type="error"))
      })

    } else {
      # ── Fallback: fill entire raster extent ──
      withProgress(message="Generating full-extent grid…", {
        tryCatch({
          e    <- terra::ext(rv$mosaic)
          xmin <- e$xmin; xmax <- e$xmax
          ymin <- e$ymin; ymax <- e$ymax
          nc <- input$ncols; nr <- input$nrows
          xs <- (xmax-xmin)/nc; ys <- (ymax-ymin)/nr
          polys <- lapply(seq_len(nr*nc), function(k) {
            row <- ceiling(k/nc); col <- ((k-1)%%nc)+1
            x1 <- xmin+(col-1)*xs; x2 <- x1+xs
            y1 <- ymin+(row-1)*ys; y2 <- y1+ys
            terra::vect(matrix(c(x1,y1,x2,y1,x2,y2,x1,y2,x1,y1),
                                ncol=2,byrow=TRUE), "polygons",
                        crs=terra::crs(rv$mosaic))
          })
          rv$shape <- do.call(rbind, polys)
          rv$shape$PlotID <- seq_len(nrow(rv$shape))   # 1-based
          showNotification(paste("✅ Grid (full extent):", nr*nc, "plots"),
                           type="message", duration=4)
        }, error=function(e) showNotification(paste("❌", e$message), type="error"))
      })
    }
  })

  output$grid_plot <- renderPlot({
    req(rv$mosaic)
    r <- rv$mosaic; b <- get_bands()
    main_title <- if (is.null(rv$shape))
      "Click 4 corners of the field  (or Generate Grid to fill extent)"
    else
      paste0("Field Grid — ", nrow(rv$shape), " plots  |  Plot 1 = top-left,  numbered left→right, top→bottom")

    if (terra::nlyr(r) >= 3) {
      terra::plotRGB(r, r=b$r, g=b$g, b=b$b, main=main_title, stretch="lin")
    } else {
      terra::plot(r[[1]], main=main_title, col=viridis::viridis(256))
    }

    # ── Draw generated grid with numbered labels ──────────────
    if (!is.null(rv$shape)) {
      shp <- rv$shape
      terra::plot(shp, add=TRUE, border="#FFC72C", lwd=0.7, col=NA)

      # Compute centroids and label each plot
      n_plots <- nrow(shp)
      # Only label if not too many plots (performance)
      max_labels <- 300
      label_idx  <- if (n_plots <= max_labels) seq_len(n_plots)
                    else c(1, seq(5, n_plots, by=max(1, floor(n_plots/50))), n_plots)

      cents <- tryCatch(terra::centroids(shp[label_idx,]), error=function(e) NULL)
      if (!is.null(cents)) {
        cx <- terra::crds(cents)[,1]
        cy <- terra::crds(cents)[,2]
        ids <- if ("PlotID" %in% names(shp)) shp$PlotID[label_idx] else label_idx
        cex_lbl <- if (n_plots > 200) 0.45 else if (n_plots > 100) 0.55 else 0.7

        # White halo then coloured text
        text(cx, cy, labels=ids, col="white",  cex=cex_lbl+0.12, font=2)
        text(cx, cy, labels=ids, col="#FFC72C", cex=cex_lbl,      font=2)

        # Highlight plot 1 with a red dot + bigger label
        if ("PlotID" %in% names(shp)) {
          p1_idx <- which(shp$PlotID == 1)
        } else {
          p1_idx <- 1
        }
        if (length(p1_idx)) {
          c1 <- tryCatch(terra::crds(terra::centroids(shp[p1_idx,])), error=function(e) NULL)
          if (!is.null(c1)) {
            points(c1[1], c1[2], pch=21, bg=C_RED, col="white", cex=3, lwd=2)
            text(c1[1], c1[2], labels="1", col="white", cex=0.85, font=2)
          }
        }
      }

      # Legend note
      legend("bottomright", inset=0.01,
        legend=c("Plot #1 (start)", "Other plots"),
        pch=c(21, NA), pt.bg=c(C_RED, NA),
        lty=c(NA, 1), col=c("white","#FFC72C"),
        pt.cex=c(1.5,NA), lwd=c(NA,1.5),
        bg=adjustcolor("black", alpha.f=0.55), text.col="white",
        cex=0.75, box.col=NA)
    }

    # ── Draw clicked corner points ────────────────────────────
    pts <- rv$corner_pts
    if (length(pts) > 0) {
      m <- do.call(rbind, pts)
      pt_cols <- c("#CC0000","#e6a800","#2d6a4f","#0f3460")[seq_len(nrow(m))]
      points(m[,1], m[,2], pch=21, bg=pt_cols, col="white", cex=2.5, lwd=2)
      text(m[,1], m[,2], labels=seq_len(nrow(m)),
           pos=3, offset=0.7, col="white", font=2, cex=1.1)
      if (length(pts) == 4) {
        ord <- sort_corners(pts)
        polygon(ord[,1], ord[,2], border="#FFC72C", lwd=2.5, lty=2)
      }
    }
  })

  # ── Index UI ─────────────────────────────────────────────────
  output$idx_checkboxes <- renderUI({
    lst <- if (input$camera_type=="rgb") RGB_IDX else MS_IDX
    base_ch <- setNames(
      sapply(lst,`[[`,"name"),
      sapply(lst, function(x) paste0(x$name," — ",substr(x$desc,1,35)))
    )
    custom_ch <- if (length(rv$custom)>0) {
      setNames(sapply(rv$custom,`[[`,"name"),
               sapply(rv$custom, function(x) paste0("⭐ ",x$name)))
    } else character(0)
    all_ch <- c(base_ch, custom_ch)
    def <- if (input$camera_type=="rgb") c("NGRDI","BGI","SAVIrgb") else c("NDVI","NDRE","GNDVI")
    checkboxGroupInput("sel_idx","Select indices:", choices=all_ch, selected=def)
  })

  output$idx_ref_tbl <- renderDT({
    lst <- if (input$camera_type=="rgb") RGB_IDX else MS_IDX
    df <- idx_df_to_show(lst)
    datatable(df, options=list(pageLength=15,scrollX=TRUE), rownames=FALSE,
              class="table-striped table-hover compact") %>%
      formatStyle("Index", fontWeight="bold", color=C_RED)
  })

  # ── Band scaling helper ──────────────────────────────────────
  get_scale_divisor <- function(r) {
    sel <- input$band_scale
    if (is.null(sel)) sel <- "auto"
    if (sel == "none")   return(1)
    if (sel == "255")    return(255)
    if (sel == "10000")  return(10000)
    if (sel == "65535")  return(65535)
    if (sel == "custom") return(max(1, as.numeric(input$band_scale_custom)))
    # auto: sample max value to guess bit depth
    samp <- tryCatch(as.vector(terra::values(r[[1]])), error=function(e) c(0,1))
    mx <- max(samp, na.rm=TRUE)
    if      (mx <= 1)     return(1)
    else if (mx <= 255)   return(255)
    else if (mx <= 10000) return(10000)
    else                  return(65535)
  }

  # ── Calculate Indices ────────────────────────────────────────
  observeEvent(input$calc_idx, {
    req(rv$mosaic, input$sel_idx)

    mosaic_use <- if (!is.null(rv$masked)) rv$masked else rv$mosaic
    b <- get_bands()

    withProgress(message="Calculating indices…", value=0.1, {
      tryCatch({
        # Scale bands to reflectance 0-1
        divisor <- get_scale_divisor(mosaic_use)
        R     <- mosaic_use[[b$r]]   / divisor
        G     <- mosaic_use[[b$g]]   / divisor
        B     <- mosaic_use[[b$b]]   / divisor
        NIR   <- if (!is.null(b$nir) && terra::nlyr(mosaic_use)>=b$nir) mosaic_use[[b$nir]]/divisor else NULL
        RE    <- if (!is.null(b$re)  && terra::nlyr(mosaic_use)>=b$re)  mosaic_use[[b$re]] /divisor else NULL
        # Clamp to 0-1 reflectance
        R   <- terra::clamp(R,   0, 1)
        G   <- terra::clamp(G,   0, 1)
        B   <- terra::clamp(B,   0, 1)
        if (!is.null(NIR)) NIR <- terra::clamp(NIR, 0, 1)
        if (!is.null(RE))  RE  <- terra::clamp(RE,  0, 1)
        # Full-name aliases used in index formulas
        Red   <- R;  Green <- G;  Blue <- B

        all_idx <- c(if (input$camera_type=="rgb") RGB_IDX else MS_IDX, rv$custom)
        stack_list <- list()

        for (idx_name in input$sel_idx) {
          def <- Filter(function(x) x$name==idx_name, all_idx)
          if (!length(def)) next
          f <- def[[1]]$formula
          lyr <- tryCatch({
            layer <- eval(parse(text=f))
            names(layer) <- idx_name
            layer
          }, error=function(e) { message("Skip ",idx_name,": ",e$message); NULL })
          if (!is.null(lyr)) stack_list[[idx_name]] <- lyr
          incProgress(0.5/length(input$sel_idx))
        }

        if (length(stack_list) > 0) {
          rv$idx_ras <- terra::rast(stack_list)
          incProgress(0.3, "Extracting by plot…")

          if (!is.null(rv$shape)) {
            # ── Per-plot extraction ──
            shp_v <- if (!inherits(rv$shape,"SpatVector")) terra::vect(rv$shape) else rv$shape
            ras <- rv$idx_ras
            if (is.na(terra::crs(ras)))   terra::crs(ras)   <- "EPSG:4326"
            if (is.na(terra::crs(shp_v))) terra::crs(shp_v) <- terra::crs(ras)
            ext_df <- terra::extract(ras, shp_v, fun=mean, na.rm=TRUE)
            names(ext_df)[1] <- "PlotID"
            rv$extracted <- ext_df

            # ── Extract raw band means (in reflectance units) ──
            band_names <- c()
            band_lyrs  <- list()
            if (!is.null(b$r))   { band_lyrs[["Red"]]     <- terra::clamp(mosaic_use[[b$r]]  /divisor,0,1); band_names <- c(band_names,"Red") }
            if (!is.null(b$g))   { band_lyrs[["Green"]]   <- terra::clamp(mosaic_use[[b$g]]  /divisor,0,1); band_names <- c(band_names,"Green") }
            if (!is.null(b$b))   { band_lyrs[["Blue"]]    <- terra::clamp(mosaic_use[[b$b]]  /divisor,0,1); band_names <- c(band_names,"Blue") }
            if (!is.null(b$re)  && terra::nlyr(mosaic_use)>=b$re)
              { band_lyrs[["RedEdge"]] <- terra::clamp(mosaic_use[[b$re]] /divisor,0,1); band_names <- c(band_names,"RedEdge") }
            if (!is.null(b$nir) && terra::nlyr(mosaic_use)>=b$nir)
              { band_lyrs[["NIR"]]     <- terra::clamp(mosaic_use[[b$nir]]/divisor,0,1); band_names <- c(band_names,"NIR") }
            if (length(band_lyrs) > 0) {
              band_stk  <- terra::rast(band_lyrs)
              band_ext  <- terra::extract(band_stk, shp_v, fun=mean, na.rm=TRUE)
              names(band_ext)[1] <- "PlotID"
              rv$band_data <- band_ext
              ext_df <- merge(ext_df, band_ext, by="PlotID", all.x=TRUE)
            }

            rv$merged <- if (!is.null(rv$meta)) {
              # find the ID column in meta (first column, or "PLOT" if present)
              id_col <- if ("PLOT" %in% names(rv$meta)) "PLOT" else names(rv$meta)[1]
              merge(rv$meta, ext_df, by.x=id_col, by.y="PlotID", all.x=TRUE)
            } else ext_df
            extra_msg <- sprintf("  📋 Extracted means for %d plots. Raw bands included.", nrow(ext_df))
          } else {
            # ── No grid: sample random pixels so stats/PCA still work ──
            samp <- tryCatch(
              terra::spatSample(rv$idx_ras, size=5000, method="random",
                                na.rm=TRUE, as.df=TRUE),
              error = function(e) as.data.frame(rv$idx_ras, xy=FALSE)[1:min(5000,.Machine$integer.max),]
            )
            if (!is.null(rv$meta)) samp <- cbind(samp, rv$meta[rep(1,nrow(samp)),,drop=FALSE])
            rv$merged <- samp
            extra_msg <- sprintf("  ⚠️ No plot grid — using %d sampled pixels for stats/PCA.", nrow(samp))
          }

          output$idx_log <- renderText(sprintf("✅ %d indices calculated: %s\n%s",
            length(names(rv$idx_ras)),
            paste(names(rv$idx_ras), collapse=", "),
            extra_msg))
        }
      }, error=function(e) output$idx_log <- renderText(paste("❌", e$message)))
    })
  })

  output$extract_preview <- renderDT({
    req(rv$merged)
    datatable(head(rv$merged,50), options=list(pageLength=10,scrollX=TRUE), rownames=FALSE) %>%
      formatRound(which(sapply(rv$merged,is.numeric)), digits=4)
  })

  # ── Custom Index ─────────────────────────────────────────────
  output$ms_pills <- renderUI({
    if (input$camera_type=="ms")
      tags$div(tags$span(class="idx-pill","RE"), tags$span(class="idx-pill","NIR"))
  })

  observeEvent(input$add_ci, {
    req(nchar(trimws(input$ci_name))>0, nchar(trimws(input$ci_formula))>0)
    nm <- toupper(trimws(input$ci_name))
    if (nm %in% sapply(rv$custom,`[[`,"name")) {
      output$ci_log <- renderText("⚠️ Name already exists"); return()
    }
    rv$custom <- c(rv$custom, list(list(
      name=nm, formula=trimws(input$ci_formula),
      desc=if(nchar(trimws(input$ci_desc))>0) trimws(input$ci_desc) else "Custom index"
    )))
    output$ci_log <- renderText(paste("✅ Added:", nm))
    updateTextInput(session,"ci_name",""); updateTextInput(session,"ci_formula","")
    updateTextAreaInput(session,"ci_desc","")
  })

  observeEvent(input$clr_ci, { rv$custom <- list() })

  output$ci_table <- renderDT({
    df <- if (length(rv$custom)>0)
      data.frame(Name=sapply(rv$custom,`[[`,"name"),
                 Formula=sapply(rv$custom,`[[`,"formula"),
                 Description=sapply(rv$custom,`[[`,"desc"))
    else data.frame(Name=character(),Formula=character(),Description=character())
    datatable(df, options=list(pageLength=8), rownames=FALSE)
  })

  # ── Field Maps ───────────────────────────────────────────────
  output$map_idx_sel <- renderUI({
    req(rv$idx_ras)
    selectInput("map_idx","Index:", choices=names(rv$idx_ras), selected=names(rv$idx_ras)[1])
  })

  output$gallery_sel <- renderUI({
    req(rv$idx_ras)
    nms <- names(rv$idx_ras)
    checkboxGroupInput("gal_idx","Indices for gallery:", choices=nms,
      selected=nms[1:min(6,length(nms))], inline=TRUE)
  })

  # ── Colour-scale lookup for plotly ──────────────────────────
  plotly_colorscale <- function(name) {
    switch(name,
      "viridis"  = "Viridis",
      "plasma"   = "Plasma",
      "inferno"  = "Inferno",
      "magma"    = "Magma",
      "RdYlGn"   = list(c(0,"#d73027"),c(0.25,"#fc8d59"),c(0.5,"#ffffbf"),
                        c(0.75,"#91cf60"),c(1,"#1a9850")),
      "Spectral" = list(c(0,"#9e0142"),c(0.25,"#f46d43"),c(0.5,"#ffffbf"),
                        c(0.75,"#66c2a5"),c(1,"#5e4fa2")),
      "YlOrRd"   = list(c(0,"#ffffcc"),c(0.33,"#feb24c"),c(0.67,"#f03b20"),c(1,"#bd0026")),
      "BrBG"     = list(c(0,"#543005"),c(0.25,"#bf812d"),c(0.5,"#f5f5f5"),
                        c(0.75,"#80cdc1"),c(1,"#003c30")),
      "Viridis"
    )
  }

  # ── Build rich hover text including all metadata ──────────────
  build_hover <- function(plot_ids, idx_name, idx_vals) {
    base <- sprintf("<b>Plot %s</b><br>%s: <b>%.4f</b>",
                    as.character(plot_ids), idx_name,
                    round(as.numeric(idx_vals), 4))
    # Attach any metadata columns present in rv$merged
    if (!is.null(rv$merged) && !is.null(plot_ids)) {
      id_col <- names(rv$merged)[1]   # first col = plot ID
      meta_cols <- setdiff(names(rv$merged),
                           c(id_col,
                             if (!is.null(rv$idx_ras)) names(rv$idx_ras) else NULL,
                             c("Red","Green","Blue","RedEdge","NIR")))
      meta_cols <- meta_cols[meta_cols %in% names(rv$merged)]
      meta_cols <- head(meta_cols, 8)   # cap at 8 extra fields

      for (col in meta_cols) {
        matched <- rv$merged[[col]][match(as.character(plot_ids),
                                          as.character(rv$merged[[id_col]]))]
        base <- paste0(base, sprintf("<br>%s: %s", col,
                                     as.character(matched)))
      }
    }
    base
  }

  # ── Helper: sf polygon → plotly filled traces ─────────────────
  sf_to_plotly_choropleth <- function(df_sf, val_col, hover_text,
                                       colorscale, title_txt, idx_name) {
    coords_list <- lapply(seq_len(nrow(df_sf)), function(i) {
      tryCatch({
        geom <- df_sf$geometry[[i]]
        if (inherits(geom, "POLYGON")) {
          m <- as.matrix(geom[[1]])[, 1:2, drop=FALSE]
        } else if (inherits(geom, "MULTIPOLYGON")) {
          m <- as.matrix(geom[[1]][[1]])[, 1:2, drop=FALSE]
        } else return(NULL)
        list(x=c(m[,1], NA), y=c(m[,2], NA))
      }, error=function(e) NULL)
    })

    vals <- as.numeric(df_sf[[val_col]])
    vmin <- min(vals, na.rm=TRUE)
    vmax <- max(vals, na.rm=TRUE)
    if (vmin == vmax) { vmin <- vmin - 1; vmax <- vmax + 1 }

    # Normalise value → colour using viridis as fallback
    norm_v <- (vals - vmin) / (vmax - vmin)

    fig <- plot_ly()
    for (i in seq_along(coords_list)) {
      if (is.null(coords_list[[i]])) next
      v <- vals[i]
      nv <- norm_v[i]
      col_hex <- if (is.na(nv)) "#cccccc"
        else grDevices::colorRamp(
          if (is.character(colorscale) && colorscale=="Viridis")
            viridis::viridis(11)
          else if (is.character(colorscale))
            viridis::viridis(11)  # fallback for named scales handled via colorscale arg
          else
            sapply(colorscale, function(x) x[[2]])
        )(nv) |> (\(m) grDevices::rgb(m[1]/255, m[2]/255, m[3]/255))()

      fig <- fig %>% add_trace(
        type="scatter", mode="lines",
        x=coords_list[[i]]$x, y=coords_list[[i]]$y,
        fill="toself",
        fillcolor=col_hex,
        line=list(color="white", width=0.4),
        hovertext=hover_text[i],
        hoverinfo="text",
        showlegend=FALSE,
        name=""
      )
    }

    # Add invisible scatter for proper colorbar
    fig <- fig %>% add_trace(
      type="scatter", mode="markers",
      x=rep(mean(unlist(lapply(coords_list, function(x) if(!is.null(x)) x$x[1])),na.rm=TRUE), 2),
      y=rep(mean(unlist(lapply(coords_list, function(x) if(!is.null(x)) x$y[1])),na.rm=TRUE), 2),
      marker=list(
        color=c(vmin, vmax),
        colorscale=colorscale,
        showscale=TRUE,
        size=0.01,
        colorbar=list(title=list(text=idx_name, font=list(size=12)),
                      thickness=15, len=0.6)
      ),
      hoverinfo="skip", showlegend=FALSE, name=""
    )

    fig %>% layout(
      title=list(text=title_txt, font=list(size=14, color=C_DARK, family="sans")),
      xaxis=list(title="Easting",  showgrid=FALSE, zeroline=FALSE),
      yaxis=list(title="Northing", showgrid=FALSE, zeroline=FALSE, scaleanchor="x"),
      plot_bgcolor="#f5f5f5",
      paper_bgcolor="white",
      hovermode="closest",
      margin=list(l=60,r=20,t=50,b=50)
    ) %>%
      config(
        displaylogo=FALSE,
        toImageButtonOptions=list(format="png", filename=paste0("FieldMap_",idx_name),
                                  width=1200, height=900, scale=2),
        modeBarButtonsToAdd=list("pan2d","zoomIn2d","zoomOut2d","resetScale2d")
      )
  }

  # ── Raster fallback → plotly heatmap ─────────────────────────
  raster_plotly <- function(layer, title_txt, idx_name, colorscale,
                             max_pts=60000) {
    df <- as.data.frame(layer, xy=TRUE)
    names(df)[3] <- "val"
    df <- df[!is.na(df$val), ]
    if (nrow(df) > max_pts) df <- df[sample(nrow(df), max_pts), ]

    # Pivot to matrix for heatmap
    xs <- sort(unique(round(df$x, 8)))
    ys <- sort(unique(round(df$y, 8)), decreasing=TRUE)
    if (length(xs) * length(ys) > 500000) {
      # Too many cells — use scatter instead
      plot_ly(df, x=~x, y=~y, color=~val, type="scatter", mode="markers",
              marker=list(size=3, colorscale=colorscale, showscale=TRUE,
                          colorbar=list(title=list(text=idx_name))),
              hovertemplate=paste0("X: %{x:.2f}<br>Y: %{y:.2f}<br>",
                                   idx_name,": %{marker.color:.4f}<extra></extra>")) %>%
        layout(title=list(text=title_txt, font=list(size=13, color=C_DARK)),
               xaxis=list(title="Easting"),
               yaxis=list(title="Northing", scaleanchor="x"),
               paper_bgcolor="white") %>%
        config(displaylogo=FALSE,
               toImageButtonOptions=list(format="png",
                 filename=paste0("FieldMap_",idx_name), width=1200, height=900, scale=2))
    } else {
      # Build z matrix
      mat <- matrix(NA, nrow=length(ys), ncol=length(xs))
      xi  <- match(round(df$x,8), xs)
      yi  <- match(round(df$y,8), ys)
      for (k in seq_len(nrow(df))) if (!is.na(xi[k])&&!is.na(yi[k]))
        mat[yi[k], xi[k]] <- df$val[k]

      plot_ly(x=xs, y=ys, z=mat, type="heatmap",
              colorscale=colorscale, showscale=TRUE,
              colorbar=list(title=list(text=idx_name, font=list(size=12))),
              hovertemplate=paste0("X: %{x:.2f}<br>Y: %{y:.2f}<br>",
                idx_name,": %{z:.4f}<extra></extra>")) %>%
        layout(title=list(text=title_txt, font=list(size=13, color=C_DARK)),
               xaxis=list(title="Easting",  showgrid=FALSE),
               yaxis=list(title="Northing", showgrid=FALSE),
               paper_bgcolor="white") %>%
        config(displaylogo=FALSE,
               toImageButtonOptions=list(format="png",
                 filename=paste0("FieldMap_",idx_name), width=1200, height=900, scale=2))
    }
  }

  # ── Cancel observers ─────────────────────────────────────────
  observeEvent(input$cancel_map,     { rv$cancel_map  <- TRUE  })
  observeEvent(input$cancel_gallery, { rv$cancel_map  <- TRUE  })
  observeEvent(input$render_map,     { rv$cancel_map  <- FALSE })
  observeEvent(input$render_gallery, { rv$cancel_map  <- FALSE })

  # Shared status helper — used by both map and gallery status outputs
  render_map_status <- function() {
    if (isTRUE(rv$map_busy))
      tags$div(style="background:#fff3cd;border-radius:5px;padding:7px 10px;font-size:12px;color:#856404;",
        tags$b("⏳ Rendering…"), " Processing plot polygons.",
        tags$br(), tags$small("Click ⏹ Stop to cancel."))
    else if (isTRUE(rv$cancel_map))
      tags$div(style="background:#f8d7da;border-radius:5px;padding:7px 10px;font-size:12px;color:#721c24;",
        "⏹ Cancelled.")
    else NULL
  }

  output$map_status_ui     <- renderUI({ render_map_status() })
  output$gallery_status_ui <- renderUI({ render_map_status() })

  # ── Field Map (plotly) ────────────────────────────────────────
  # ── Field Map — only fires on button click ───────────────────
  field_map_data <- eventReactive(input$render_map, {
    req(rv$idx_ras, input$map_idx)
    rv$cancel_map <- FALSE

    idx  <- isolate(input$map_idx)
    cs   <- plotly_colorscale(isolate(input$map_pal))
    mpts <- isolate(input$map_pts) * 1000

    withProgress(message="Rendering field map…", value=0, {

      if (!is.null(rv$shape) && !is.null(rv$extracted) && idx %in% names(rv$extracted)) {
        incProgress(0.2, detail="Converting polygons…")
        shp_v <- if (!inherits(rv$shape,"SpatVector")) terra::vect(rv$shape) else rv$shape
        vals  <- rv$extracted[[idx]]
        plot_ids <- if ("PlotID" %in% names(shp_v))
                      as.integer(shp_v$PlotID) else seq_len(length(vals))
        shp_v$PlotID   <- plot_ids
        shp_v$plot_val <- vals

        incProgress(0.3, detail="Building hover text…")
        df_sf <- tryCatch(sf::st_as_sf(shp_v), error=function(e) NULL)

        if (!is.null(df_sf)) {
          if (isTRUE(rv$cancel_map)) return(NULL)
          hover <- build_hover(plot_ids, idx, vals)
          incProgress(0.4, detail="Drawing choropleth…")
          result <- sf_to_plotly_choropleth(
            df_sf, "plot_val", hover, cs,
            title_txt = paste0("<b>", idx, " — Per-Plot Heatmap</b><br>",
              "<sup>Dry Bean Breeding & Computational Biology, University of Guelph</sup>"),
            idx_name = idx
          )
          incProgress(0.1, detail="Done")
          return(result)
        }
      }

      # Raster fallback
      incProgress(0.3, detail="Sampling raster…")
      if (isTRUE(rv$cancel_map)) return(NULL)
      incProgress(0.5, detail="Building heatmap…")
      result <- raster_plotly(rv$idx_ras[[idx]],
        title_txt = paste0("<b>", idx, " — Raster Heatmap</b><br>",
          "<sup>Load a plot grid to see per-plot view with metadata</sup>"),
        idx_name = idx, colorscale = cs, max_pts = mpts)
      incProgress(0.2, detail="Done")
      result
    })
  }, ignoreNULL=TRUE)

  output$field_map <- renderPlotly({
    req(field_map_data())
    field_map_data()
  })

  # ── Multi-Index Gallery — only fires on button click ──────────
  gallery_data <- eventReactive(input$render_gallery, {
    req(rv$idx_ras, input$gal_idx)
    rv$cancel_map <- FALSE

    indices <- isolate(input$gal_idx)
    cs      <- plotly_colorscale(isolate(
                 if (!is.null(input$gal_pal)) input$gal_pal else "RdYlGn"))
    n       <- length(indices)
    ncols   <- min(3, n)
    nrows   <- ceiling(n / ncols)

    withProgress(message="Rendering gallery…", value=0, {
      sub_figs <- vector("list", n)

      for (i in seq_along(indices)) {
        if (isTRUE(rv$cancel_map)) return(NULL)
        nm <- indices[i]
        incProgress(1/n, detail=paste("Index", i, "of", n, "—", nm))

        has_shape <- !is.null(rv$shape) && !is.null(rv$extracted) &&
                     nm %in% names(rv$extracted)

        if (has_shape) {
          shp_v <- if (!inherits(rv$shape,"SpatVector")) terra::vect(rv$shape) else rv$shape
          vals  <- rv$extracted[[nm]]
          plot_ids <- if ("PlotID" %in% names(shp_v))
                         as.integer(shp_v$PlotID) else seq_len(length(vals))
          shp_v$PlotID   <- plot_ids
          shp_v$plot_val <- vals
          df_sf <- tryCatch(sf::st_as_sf(shp_v), error=function(e) NULL)

          if (!is.null(df_sf)) {
            hover <- sprintf("<b>Plot %d</b><br>%s: %.4f",
                             plot_ids, nm, round(vals, 4))
            sub_figs[[i]] <- sf_to_plotly_choropleth(
              df_sf, "plot_val", hover, cs, title_txt=nm, idx_name=nm)
            next
          }
        }
        # Raster fallback
        sub_figs[[i]] <- raster_plotly(rv$idx_ras[[nm]], title_txt=nm,
                                        idx_name=nm, colorscale=cs, max_pts=15000)
      }

      # Remove any NULLs from cancelled steps
      sub_figs <- Filter(Negate(is.null), sub_figs)
      if (!length(sub_figs)) return(NULL)

      subplot(sub_figs, nrows=nrows, shareX=FALSE, shareY=FALSE,
              titleX=FALSE, titleY=FALSE, margin=0.04) %>%
        layout(
          title=list(text="<b>Multi-Index Field Heatmap Gallery</b>",
                     font=list(size=14, color=C_DARK)),
          paper_bgcolor="white", showlegend=FALSE
        ) %>%
        config(
          displaylogo=FALSE,
          toImageButtonOptions=list(format="png", filename="MultiIndex_Gallery",
                                    width=1600, height=1200, scale=2)
        )
    })
  }, ignoreNULL=TRUE)

  output$gallery_plot <- renderPlotly({
    req(gallery_data())
    gallery_data()
  })

  # ── Statistics ───────────────────────────────────────────────
  num_cols_merged <- reactive({
    req(rv$merged)
    names(rv$merged)[sapply(rv$merged, is.numeric)]
  })

  output$val_boxes <- renderUI({
    req(rv$merged)
    nc <- num_cols_merged()
    if (!length(nc)) return(NULL)
    show <- nc[1:min(4,length(nc))]
    fluidRow(lapply(show, function(col) {
      x <- rv$merged[[col]]
      column(3, infoBox(
        title=col, value=round(mean(x,na.rm=TRUE),3),
        subtitle=paste("SD:", round(sd(x,na.rm=TRUE),3)),
        icon=icon("leaf"), color="red", fill=TRUE, width=NULL
      ))
    }))
  })

  output$stat_idx_sel <- renderUI({
    req(rv$merged); selectInput("stat_idx","Index:", choices=num_cols_merged(), selected=num_cols_merged()[1])
  })
  output$grp_sel <- renderUI({
    req(rv$merged)
    idx_cols  <- if (!is.null(rv$idx_ras)) names(rv$idx_ras) else character(0)
    band_cols <- c("Red","Green","Blue","RedEdge","NIR")
    # Only show metadata columns (not index or band values) for grouping
    meta_candidates <- names(rv$merged)[!names(rv$merged) %in% c(idx_cols, band_cols)]
    # Prioritise columns with few unique values (genotype, treatment, rep, etc.)
    grp_cols <- meta_candidates[sapply(meta_candidates, function(col) {
      x <- rv$merged[[col]]
      n_u <- length(unique(x[!is.na(x)]))
      n_u >= 2 && n_u <= 60
    })]
    cats <- c("None", grp_cols)
    selectInput("grp_var","Group by:", choices=cats, selected="None")
  })

  output$dist_plot <- renderPlotly({
    req(rv$merged, input$stat_idx)
    df   <- rv$merged
    col  <- input$stat_idx
    x    <- as.numeric(df[[col]])
    x[!is.finite(x)] <- NA

    grp <- if (!is.null(input$grp_var) && input$grp_var != "None") input$grp_var else NULL
    show_outliers <- isTRUE(input$dist_outliers)

    # Outlier bounds (IQR method)
    q1 <- quantile(x, 0.25, na.rm=TRUE)
    q3 <- quantile(x, 0.75, na.rm=TRUE)
    iqr <- q3 - q1
    out_lo <- q1 - 1.5 * iqr
    out_hi <- q3 + 1.5 * iqr
    is_out <- !is.na(x) & (x < out_lo | x > out_hi)
    n_out  <- sum(is_out)
    df$.outlier <- is_out

    p <- if (input$dist_type == "violin") {
      if (!is.null(grp)) {
        ggplot(df, aes(x=.data[[grp]], y=.data[[col]], fill=.data[[grp]])) +
          geom_violin(alpha=0.65, trim=FALSE) +
          geom_boxplot(width=0.1, fill="white", outlier.shape=NA, linewidth=0.6) +
          { if (show_outliers) geom_jitter(data=df[df$.outlier,],
              aes(x=.data[[grp]], y=.data[[col]]),
              color=C_RED, size=2.5, alpha=0.8, width=0.05) } +
          scale_fill_viridis_d(option="D")
      } else {
        ggplot(df, aes(x="All plots", y=.data[[col]])) +
          geom_violin(fill=C_RED, alpha=0.65, trim=FALSE) +
          geom_boxplot(width=0.1, fill="white", outlier.shape=NA, linewidth=0.7) +
          geom_hline(yintercept=mean(x,na.rm=TRUE), linetype="dashed",
                     color=C_DARK, linewidth=0.8) +
          { if (show_outliers && n_out > 0)
              geom_jitter(data=df[df$.outlier,], aes(x="All plots", y=.data[[col]]),
                color=C_RED, size=2.5, alpha=0.9, width=0.05) }
      }
    } else if (input$dist_type == "box") {
      if (!is.null(grp)) {
        ggplot(df, aes(x=.data[[grp]], y=.data[[col]], fill=.data[[grp]])) +
          geom_boxplot(outlier.shape=if(show_outliers) 16 else NA,
                       outlier.color=C_RED, outlier.size=2) +
          scale_fill_viridis_d()
      } else {
        ggplot(df, aes(x="All plots", y=.data[[col]])) +
          geom_boxplot(fill=C_GOLD, outlier.shape=if(show_outliers) 16 else NA,
                       outlier.color=C_RED, outlier.size=2.5) +
          geom_hline(yintercept=mean(x,na.rm=TRUE), linetype="dashed",
                     color=C_DARK, linewidth=0.8)
      }
    } else if (input$dist_type == "hist") {
      bin_n <- max(10, min(60, round(sqrt(sum(!is.na(x))))))
      if (!is.null(grp)) {
        ggplot(df, aes(x=.data[[col]], fill=.data[[grp]])) +
          geom_histogram(bins=bin_n, color="white", alpha=0.8, position="identity") +
          scale_fill_viridis_d()
      } else {
        ggplot(df, aes(x=.data[[col]])) +
          geom_histogram(bins=bin_n, fill=C_RED, color="white", alpha=0.85) +
          geom_vline(xintercept=mean(x,na.rm=TRUE), linetype="dashed",
                     color=C_DARK, linewidth=0.9) +
          geom_vline(xintercept=c(q1,q3), linetype="dotted",
                     color="#555", linewidth=0.7)
      }
    } else {
      if (!is.null(grp)) {
        ggplot(df, aes(x=.data[[col]], fill=.data[[grp]], color=.data[[grp]])) +
          geom_density(alpha=0.45, linewidth=0.8) +
          scale_fill_viridis_d() + scale_color_viridis_d()
      } else {
        ggplot(df, aes(x=.data[[col]])) +
          geom_density(fill=C_RED, alpha=0.55, linewidth=0.9) +
          geom_vline(xintercept=mean(x,na.rm=TRUE), linetype="dashed",
                     color=C_DARK, linewidth=0.9) +
          geom_vline(xintercept=c(out_lo, out_hi), linetype="dotted",
                     color=C_RED, linewidth=0.7)
      }
    }

    subtitle_txt <- if (show_outliers && n_out > 0)
      paste0("⚠️  ", n_out, " outlier(s) flagged  |  IQR fence: [",
             round(out_lo,3), ", ", round(out_hi,3), "]")
    else paste0("n = ", sum(!is.na(x)), "  |  mean = ", round(mean(x,na.rm=TRUE),4),
                "  |  SD = ", round(sd(x,na.rm=TRUE),4))

    p <- p +
      labs(title=paste0(col, " — ", input$dist_type),
           subtitle=subtitle_txt, x=col, y=NULL) +
      theme_minimal() +
      theme(plot.title=element_text(face="bold", color=C_DARK, size=13),
            plot.subtitle=element_text(size=10, color="#666"),
            legend.position="bottom",
            panel.grid.minor=element_blank())

    ggplotly(p, tooltip=c("x","y","fill","colour")) %>%
      layout(showlegend=TRUE)
  })

  output$sum_stats_tbl <- renderDT({
    req(rv$merged)
    nc <- num_cols_merged()
    df <- do.call(rbind, lapply(nc, function(col) {
      x <- rv$merged[[col]]
      data.frame(Index=col, N=sum(!is.na(x)), Mean=round(mean(x,na.rm=T),4),
        SD=round(sd(x,na.rm=T),4), Min=round(min(x,na.rm=T),4),
        Median=round(median(x,na.rm=T),4), Max=round(max(x,na.rm=T),4),
        CV_pct=round(sd(x,na.rm=T)/abs(mean(x,na.rm=T))*100,2),
        stringsAsFactors=FALSE)
    }))
    datatable(df, options=list(pageLength=12,scrollX=TRUE), rownames=FALSE) %>%
      formatStyle("CV_pct",
        color=styleInterval(c(10,30), c(C_GREEN,"#e6a800",C_RED)))
  })

  output$geno_idx_sel <- renderUI({
    req(rv$merged); selectInput("geno_idx","Index:", choices=num_cols_merged(), selected=num_cols_merged()[1])
  })
  output$geno_grp_sel <- renderUI({
    req(rv$merged)
    # Include all non-pure-numeric columns PLUS any metadata columns
    idx_cols <- if (!is.null(rv$idx_ras)) names(rv$idx_ras) else character(0)
    band_cols <- c("Red","Green","Blue","RedEdge","NIR")
    # Everything that isn't an index or band value = potential grouping column
    all_cols  <- names(rv$merged)
    grp_cols  <- all_cols[!all_cols %in% c(idx_cols, band_cols)]
    if (!length(grp_cols)) grp_cols <- all_cols[1]
    selectInput("geno_grp","Group by:", choices=grp_cols, selected=grp_cols[1])
  })

  output$geno_plot <- renderPlotly({
    req(rv$merged, input$geno_idx, input$geno_grp)
    df <- rv$merged
    sm <- df %>%
      dplyr::group_by(.data[[input$geno_grp]]) %>%
      dplyr::summarise(mean_v=mean(.data[[input$geno_idx]],na.rm=TRUE),
                se_v=sd(.data[[input$geno_idx]],na.rm=TRUE)/sqrt(dplyr::n()),
                n=dplyr::n(), .groups="drop") %>%
      dplyr::arrange(dplyr::desc(mean_v))
    if (nrow(sm)>40) sm <- head(sm,40)
    grp_col <- input$geno_grp

    p <- if (input$geno_plot_type=="lollipop") {
      ggplot(sm, aes(x=reorder(.data[[grp_col]],mean_v), y=mean_v)) +
        geom_segment(aes(xend=.data[[grp_col]],yend=0), color="#ddd", size=0.8) +
        geom_point(aes(color=mean_v, size=n)) +
        scale_color_gradientn(colors=c(C_DARK,C_GREEN,"#FFC72C",C_RED)) +
        scale_size_continuous(range=c(3,8))
    } else if (input$geno_plot_type=="bar") {
      ggplot(sm, aes(x=reorder(.data[[grp_col]],mean_v), y=mean_v, fill=mean_v)) +
        geom_col(color="white", size=0.2) +
        geom_errorbar(aes(ymin=mean_v-se_v,ymax=mean_v+se_v), width=0.3, color="#333") +
        scale_fill_gradientn(colors=c(C_DARK,C_GREEN,"#FFC72C",C_RED))
    } else {
      ggplot(sm, aes(x=reorder(.data[[grp_col]],mean_v), y=mean_v)) +
        geom_point(aes(color=mean_v), size=4) +
        geom_errorbar(aes(ymin=mean_v-se_v,ymax=mean_v+se_v), width=0.3) +
        scale_color_gradientn(colors=c(C_DARK,C_GREEN,"#FFC72C",C_RED))
    }

    p <- p + coord_flip() +
      labs(title=paste("Comparison:", input$geno_idx),
           subtitle="Mean ± SE — top 40 entries",
           x=grp_col, y=input$geno_idx) +
      theme_minimal() +
      theme(plot.title=element_text(face="bold",color=C_DARK),
            legend.position="right", panel.grid.major.y=element_blank())
    ggplotly(p)
  })

  # ── Correlation ──────────────────────────────────────────────
  observeEvent(input$run_corr, {
    req(rv$merged)
    output$corr_plot <- renderPlot({
      nd <- rv$merged[, sapply(rv$merged,is.numeric), drop=FALSE]
      nd <- nd[, colSums(!is.na(nd))>5, drop=FALSE]
      if (ncol(nd)<2) { plot.new(); text(0.5,0.5,"Need ≥ 2 numeric columns"); return() }
      cm <- cor(nd, method=input$corr_meth, use="pairwise.complete.obs")
      ggcorrplot::ggcorrplot(cm,
        method="square", type="lower", lab=TRUE, lab_size=2.8,
        sig.level=input$corr_sig,
        insig=if(input$corr_blank)"blank" else "pch",
        colors=RColorBrewer::brewer.pal(3,input$corr_pal),
        outline.col="white",
        title=paste("Correlation Matrix —", input$corr_meth),
        ggtheme=theme_minimal()
      ) + theme(plot.title=element_text(face="bold",color=C_DARK))
    })
  })

  output$scatter_sel <- renderUI({
    req(rv$merged)
    nc <- num_cols_merged()
    fluidRow(
      column(4, selectInput("sc_x","X axis:", choices=nc, selected=nc[1])),
      column(4, selectInput("sc_y","Y axis:", choices=nc, selected=if(length(nc)>1)nc[2] else nc[1])),
      column(4, uiOutput("sc_col_sel"))
    )
  })
  output$sc_col_sel <- renderUI({
    req(rv$merged)
    cats <- c("None", names(rv$merged)[sapply(rv$merged,function(x)is.character(x)||is.factor(x))])
    selectInput("sc_col","Color by:", choices=cats, selected="None")
  })

  output$scatter_plot <- renderPlotly({
    req(rv$merged, input$sc_x, input$sc_y)
    df <- rv$merged
    grp <- if (!is.null(input$sc_col) && input$sc_col!="None") input$sc_col else NULL
    p <- if (!is.null(grp)) {
      ggplot(df, aes(x=.data[[input$sc_x]], y=.data[[input$sc_y]], color=.data[[grp]])) +
        geom_point(alpha=0.7, size=2.5) + scale_color_viridis_d()
    } else {
      ggplot(df, aes(x=.data[[input$sc_x]], y=.data[[input$sc_y]])) +
        geom_point(color=C_RED, alpha=0.7, size=2.5)
    }
    p <- p +
      geom_smooth(method="lm", color=C_DARK, fill=C_GOLD, alpha=0.2, se=TRUE) +
      labs(title=paste(input$sc_x,"vs",input$sc_y),
           x=input$sc_x, y=input$sc_y) +
      theme_minimal() +
      theme(plot.title=element_text(face="bold",color=C_DARK))
    ggplotly(p)
  })

  # ── PCA ─────────────────────────────────────────────────────
  pca_res <- eventReactive(input$run_pca, {
    req(rv$merged)
    nd <- rv$merged[, sapply(rv$merged, is.numeric), drop=FALSE]
    # Remove columns with Inf, all-NA, or zero variance
    nd <- nd[, sapply(nd, function(x) {
      x2 <- x[is.finite(x)]; length(x2) > 2 && var(x2) > 0
    }), drop=FALSE]
    nd[] <- lapply(nd, function(x) { x[!is.finite(x)] <- NA; x })
    nd   <- nd[complete.cases(nd), , drop=FALSE]
    if (ncol(nd) < 2 || nrow(nd) < 3) {
      showNotification("PCA needs >= 2 valid numeric columns and >= 3 complete rows.", type="warning")
      return(NULL)
    }
    rv$pca_obj <- prcomp(nd, scale. = as.logical(input$pca_scale))
    rv$pca_obj
  })

  output$pca_col_sel <- renderUI({
    req(rv$merged)
    all_cols <- c("None",names(rv$merged))
    selectInput("pca_col","Color by:", choices=all_cols, selected="None")
  })

  output$pca_biplot <- renderPlotly({
    req(pca_res())
    pca <- pca_res()
    p1 <- input$pca_pc1; p2 <- input$pca_pc2
    scores <- as.data.frame(pca$x)
    pct <- round(summary(pca)$importance[2,]*100,1)

    color_v <- NULL
    if (!is.null(input$pca_col) && input$pca_col!="None" &&
        input$pca_col %in% names(rv$merged)) {
      color_v <- rv$merged[[input$pca_col]][seq_len(nrow(scores))]
    }

    plot_ly(scores,
      x=~get(paste0("PC",p1)), y=~get(paste0("PC",p2)),
      type="scatter", mode="markers",
      marker=list(size=8, color=if(!is.null(color_v)) color_v else C_RED,
                  colorscale="Viridis", showscale=!is.null(color_v), opacity=0.75),
      text=rownames(scores),
      hovertemplate="%{text}<br>PC%{xaxis.title.text}: %{x:.3f}<br>PC%{yaxis.title.text}: %{y:.3f}<extra></extra>"
    ) %>% layout(
      title=list(text="PCA Score Plot",font=list(family="sans",size=16,color=C_DARK)),
      xaxis=list(title=paste0("PC",p1," (",pct[p1],"% var)")),
      yaxis=list(title=paste0("PC",p2," (",pct[p2],"% var)")),
      plot_bgcolor="#f9f9f9"
    )
  })

  output$scree_plot <- renderPlot({
    req(pca_res())
    pca <- pca_res()
    imp <- summary(pca)$importance
    df <- data.frame(PC=seq_len(ncol(imp)),
                     Var=imp[2,]*100, Cum=imp[3,]*100)
    ggplot(df, aes(x=PC)) +
      geom_col(aes(y=Var), fill=C_RED, alpha=0.85) +
      geom_line(aes(y=Cum), color=C_GOLD, size=1.3) +
      geom_point(aes(y=Cum), color=C_GOLD, size=3.5) +
      geom_hline(yintercept=80, linetype="dashed", color=C_GREEN) +
      scale_x_continuous(breaks=seq_len(ncol(imp))) +
      labs(title="Scree Plot", x="PC", y="% Variance Explained") +
      theme_minimal() +
      theme(plot.title=element_text(face="bold",color=C_DARK))
  })

  output$load_plot <- renderPlot({
    req(pca_res())
    pca <- pca_res(); p1 <- input$pca_pc1
    df <- data.frame(Variable=rownames(pca$rotation),
                     Loading=pca$rotation[,p1]) %>%
      dplyr::arrange(Loading)
    ggplot(df, aes(x=reorder(Variable,Loading), y=Loading, fill=Loading)) +
      geom_col(color="white", size=0.3) +
      scale_fill_gradient2(low=C_DARK, mid="white", high=C_RED, midpoint=0) +
      coord_flip() +
      labs(title=paste("PC",p1,"Loadings"), x=NULL, y="Loading") +
      theme_minimal() +
      theme(plot.title=element_text(face="bold",color=C_DARK), legend.position="none")
  })

  output$clust_vars_sel <- renderUI({
    req(rv$merged)
    nc <- num_cols_merged()
    checkboxGroupInput("clust_vars","Variables:",choices=nc,
      selected=nc[1:min(5,length(nc))], inline=TRUE)
  })

  observeEvent(input$run_clust, {
    req(rv$merged, input$clust_vars)
    output$dendro_plot <- renderPlot({
      df_c <- rv$merged[,input$clust_vars,drop=FALSE]
      df_c <- scale(df_c[complete.cases(df_c),,drop=FALSE])
      if (nrow(df_c)<3) { plot.new(); text(0.5,0.5,"Too few observations"); return() }
      if (nrow(df_c)>120) df_c <- df_c[sample(nrow(df_c),120),]
      hc <- hclust(dist(df_c), method="ward.D2")

      if (requireNamespace("dendextend",quietly=TRUE)) {
        dend <- dendextend::color_branches(as.dendrogram(hc), k=input$n_clust)
        par(mar=c(5,4,4,1))
        plot(dend, main=paste("Hierarchical Clustering —",input$n_clust,"clusters"),
             ylab="Height", cex=0.65)
      } else {
        par(mar=c(5,4,4,1))
        plot(hc, main=paste("Hierarchical Clustering —",input$n_clust,"clusters"),
             cex=0.65, xlab="", sub="")
        rect.hclust(hc, k=input$n_clust,
          border=c(C_RED,C_GOLD,C_GREEN,C_DARK,"#6b3a2a"))
      }
    })
  })

  # ── Export ───────────────────────────────────────────────────
  output$full_data_tbl <- renderDT({
    req(rv$merged)
    datatable(rv$merged, options=list(pageLength=15,scrollX=TRUE), rownames=FALSE) %>%
      formatRound(which(sapply(rv$merged,is.numeric)), digits=4)
  })

  output$dl_csv <- downloadHandler(
    filename=function() paste0("DryBean_DroneAnalysis_",Sys.Date(),".csv"),
    content=function(file) { req(rv$merged); write.csv(rv$merged, file, row.names=FALSE) }
  )

  output$dl_xlsx <- downloadHandler(
    filename=function() paste0("DryBean_DroneAnalysis_",Sys.Date(),".xlsx"),
    content=function(file) {
      req(rv$merged)
      wb <- openxlsx::createWorkbook()
      hdr <- openxlsx::createStyle(fontColour="white", fgFill=C_RED,
        fontName="Calibri", fontSize=11, textDecoration="bold",
        halign="center", border="Bottom")
      alt <- openxlsx::createStyle(fgFill="#fff8f8")

      # Sheet 1: Data
      openxlsx::addWorksheet(wb,"Index Data")
      openxlsx::writeData(wb,"Index Data",rv$merged)
      openxlsx::addStyle(wb,"Index Data",hdr,rows=1,cols=seq_len(ncol(rv$merged)))
      openxlsx::setColWidths(wb,"Index Data",cols=seq_len(ncol(rv$merged)),widths="auto")

      # Sheet 2: Summary Stats
      nd <- rv$merged[,sapply(rv$merged,is.numeric),drop=FALSE]
      stats_df <- do.call(rbind, lapply(names(nd), function(col) {
        x <- nd[[col]]
        data.frame(Index=col, N=sum(!is.na(x)),
          Mean=round(mean(x,na.rm=T),4), SD=round(sd(x,na.rm=T),4),
          Min=round(min(x,na.rm=T),4), Median=round(median(x,na.rm=T),4),
          Max=round(max(x,na.rm=T),4),
          CV_pct=round(sd(x,na.rm=T)/abs(mean(x,na.rm=T))*100,2))
      }))
      openxlsx::addWorksheet(wb,"Summary Statistics")
      openxlsx::writeData(wb,"Summary Statistics",stats_df)
      openxlsx::addStyle(wb,"Summary Statistics",hdr,rows=1,cols=seq_len(ncol(stats_df)))
      openxlsx::setColWidths(wb,"Summary Statistics",cols=seq_len(ncol(stats_df)),widths="auto")

      # Sheet 3: Index Definitions
      all_def <- c(RGB_IDX, MS_IDX, rv$custom)
      idx_ref <- data.frame(
        Name=sapply(all_def,`[[`,"name"),
        Formula=sapply(all_def,`[[`,"formula"),
        Description=sapply(all_def,`[[`,"desc"),
        Type=c(rep("RGB",length(RGB_IDX)), rep("Multispectral",length(MS_IDX)),
               rep("Custom",length(rv$custom))))
      openxlsx::addWorksheet(wb,"Index Definitions")
      openxlsx::writeData(wb,"Index Definitions",idx_ref)
      openxlsx::addStyle(wb,"Index Definitions",hdr,rows=1,cols=seq_len(ncol(idx_ref)))
      openxlsx::setColWidths(wb,"Index Definitions",cols=seq_len(ncol(idx_ref)),widths="auto")

      # Sheet 4: Correlation
      if (ncol(nd)>=2) {
        cm <- cor(nd,use="pairwise.complete.obs")
        cm_df <- cbind(Variable=rownames(cm), as.data.frame(round(cm,4)))
        openxlsx::addWorksheet(wb,"Correlation Matrix")
        openxlsx::writeData(wb,"Correlation Matrix",cm_df)
        openxlsx::addStyle(wb,"Correlation Matrix",hdr,rows=1,cols=seq_len(ncol(cm_df)))
        openxlsx::setColWidths(wb,"Correlation Matrix",cols=seq_len(ncol(cm_df)),widths="auto")
      }

      # Sheet 5: Raw Band Values (if available)
      if (!is.null(rv$band_data)) {
        openxlsx::addWorksheet(wb,"Raw Band Values")
        openxlsx::writeData(wb,"Raw Band Values", rv$band_data)
        openxlsx::addStyle(wb,"Raw Band Values",hdr,rows=1,cols=seq_len(ncol(rv$band_data)))
        openxlsx::setColWidths(wb,"Raw Band Values",cols=seq_len(ncol(rv$band_data)),widths="auto")
      }

      # Sheet 6: PCA Loadings (if available)
      if (!is.null(rv$pca_obj)) {
        load_df <- as.data.frame(round(rv$pca_obj$rotation,4))
        load_df <- cbind(Variable=rownames(load_df), load_df)
        openxlsx::addWorksheet(wb,"PCA Loadings")
        openxlsx::writeData(wb,"PCA Loadings",load_df)
        openxlsx::addStyle(wb,"PCA Loadings",hdr,rows=1,cols=seq_len(ncol(load_df)))
        openxlsx::setColWidths(wb,"PCA Loadings",cols=seq_len(ncol(load_df)),widths="auto")
      }

      openxlsx::saveWorkbook(wb, file)
    }
  )

  # (R script download handlers removed — not used in this version)

  output$plot_export_sel <- renderUI({
    selectInput("exp_plot","Plot to export:",
      choices=c("Field Map"="field","Distribution"="dist",
                "Genotype Comparison"="geno",
                "Correlation Matrix"="corr",
                "Scree Plot"="scree",
                "PCA Loadings"="loads"),
      selected="field")
  })

  output$dl_plot <- downloadHandler(
    filename=function() paste0("DryBean_",input$exp_plot,"_",Sys.Date(),".",input$exp_fmt),
    content=function(file) {
      # Re-generate the selected plot and save
      gg <- tryCatch({
        switch(input$exp_plot,
          "scree" = {
            req(pca_res())
            pca <- pca_res()
            imp <- summary(pca)$importance
            df <- data.frame(PC=seq_len(ncol(imp)),Var=imp[2,]*100,Cum=imp[3,]*100)
            ggplot(df,aes(x=PC))+geom_col(aes(y=Var),fill=C_RED,alpha=0.85)+
              geom_line(aes(y=Cum),color=C_GOLD,size=1.3)+theme_minimal()+
              labs(title="Scree Plot",x="PC",y="% Variance Explained")
          },
          "loads" = {
            req(pca_res())
            pca <- pca_res()
            data.frame(Variable=rownames(pca$rotation),Loading=pca$rotation[,1]) %>%
              dplyr::arrange(Loading) %>%
              ggplot(aes(x=reorder(Variable,Loading),y=Loading,fill=Loading))+
              geom_col(color="white")+
              scale_fill_gradient2(low=C_DARK,mid="white",high=C_RED,midpoint=0)+
              coord_flip()+theme_minimal()+labs(title="PC1 Loadings",x=NULL)
          },
          ggplot() + annotate("text",x=0.5,y=0.5,label="Render the plot in-app first") + theme_void()
        )
      }, error=function(e) ggplot()+theme_void())
      ggsave(file, plot=gg, width=input$exp_w, height=input$exp_h,
             device=input$exp_fmt, dpi=300)
    }
  )

  # ── Genotype Selection (enhanced) ───────────────────────────

  gs_idx_cols <- reactive({
    req(rv$merged)
    idx_nms <- if (!is.null(rv$idx_ras)) names(rv$idx_ras) else character(0)
    avail   <- intersect(idx_nms, names(rv$merged))
    if (!length(avail)) names(rv$merged)[sapply(rv$merged, is.numeric)] else avail
  })

  output$gs_id_col_sel <- renderUI({
    req(rv$merged)
    all_cols  <- names(rv$merged)
    idx_cols  <- gs_idx_cols()
    band_cols <- c("Red","Green","Blue","RedEdge","NIR")
    id_cands  <- all_cols[!all_cols %in% c(idx_cols, band_cols)]
    if (!length(id_cands)) id_cands <- all_cols[1]
    selectInput("gs_id_col","Plot ID / Label column:", choices=id_cands, selected=id_cands[1])
  })

  output$gs_idx_picker <- renderUI({
    cols_all <- gs_idx_cols()
    if (!length(cols_all)) return(tags$p("Calculate indices first.", style="color:#888;font-size:12px;"))
    checkboxGroupInput("gs_sel_idx", NULL,
      choices  = cols_all,
      selected = cols_all[seq_len(min(4, length(cols_all)))])
  })

  output$gs_criteria_ui <- renderUI({
    cols <- if (!is.null(input$gs_sel_idx) && length(input$gs_sel_idx)) input$gs_sel_idx else character(0)
    if (!length(cols)) return(tags$p("Select at least one index above.", style="color:#888;"))
    tagList(
      tags$p(class="section-title","Index  |  Direction  |  Weight  |  Threshold"),
      lapply(cols, function(idx) {
        x_vals <- if (!is.null(rv$merged) && idx %in% names(rv$merged))
          rv$merged[[idx]][is.finite(rv$merged[[idx]])] else c(0,1)
        mn <- round(min(x_vals, na.rm=TRUE), 4)
        mx <- round(max(x_vals, na.rm=TRUE), 4)
        med <- round(median(x_vals, na.rm=TRUE), 4)
        tags$div(
          style="background:#f8f8f8;border-radius:6px;padding:8px 10px;margin-bottom:10px;",
          tags$b(style="color:#CC0000;font-size:12px;", idx),
          tags$br(),
          tags$div(style="display:flex;gap:6px;flex-wrap:wrap;align-items:flex-end;margin-top:4px;",
            selectInput(paste0("gs_dir_",idx), "Direction:",
              choices=c("High=Good"="high","Low=Good"="low"), selected="high", width="120px"),
            numericInput(paste0("gs_wt_",idx), "Weight:",
              value=1, min=0, max=10, step=0.5, width="70px"),
            numericInput(paste0("gs_thr_",idx), paste0("Threshold (",mn," – ",mx,"):"),
              value=med, min=mn, max=mx,
              step=round((mx-mn)/20, 5), width="110px")
          )
        )
      })
    )
  })

  gs_ranks <- eventReactive(input$run_gs, {
    req(rv$merged)
    cols_all <- gs_idx_cols()
    cols     <- if (!is.null(input$gs_sel_idx) && length(input$gs_sel_idx))
                  intersect(input$gs_sel_idx, cols_all) else cols_all[1]
    df       <- rv$merged
    id_col   <- if (!is.null(input$gs_id_col)) input$gs_id_col else names(df)[1]

    scores <- sapply(cols, function(idx) {
      if (!idx %in% names(df)) return(rep(NA_real_, nrow(df)))
      dir <- input[[paste0("gs_dir_",idx)]]
      wt  <- as.numeric(input[[paste0("gs_wt_", idx)]]); if (is.null(wt)||is.na(wt)) wt <- 1
      x   <- df[[idx]]; x[!is.finite(x)] <- NA
      rng <- range(x, na.rm=TRUE)
      if (rng[1]==rng[2]) return(rep(0.5*wt, nrow(df)))
      norm <- (x - rng[1]) / (rng[2] - rng[1])
      if (dir=="low") norm <- 1 - norm
      norm * wt
    })

    total_wt <- sum(sapply(cols, function(idx) {
      wt <- as.numeric(input[[paste0("gs_wt_",idx)]]); if (is.null(wt)||is.na(wt)) 1 else wt
    }), na.rm=TRUE)
    score_vec <- rowSums(scores, na.rm=TRUE) / max(total_wt, 1)

    # Categorize: ELITE if ALL thresholds met
    elite_vec <- rep(TRUE, nrow(df))
    for (idx in cols) {
      thr <- as.numeric(input[[paste0("gs_thr_",idx)]])
      dir <- input[[paste0("gs_dir_",idx)]]
      if (is.null(thr)||is.na(thr)) next
      x <- df[[idx]]; x[!is.finite(x)] <- NA
      if (dir=="high") elite_vec <- elite_vec & (!is.na(x) & x >= thr)
      else             elite_vec <- elite_vec & (!is.na(x) & x <= thr)
    }

    keep_cols <- unique(c(id_col, cols[cols %in% names(df)]))
    result_df <- df[, keep_cols, drop=FALSE]
    result_df$Score    <- round(score_vec, 4)
    result_df$Rank     <- rank(-score_vec, ties.method="min")
    result_df$Category <- ifelse(elite_vec, "Elite", "Standard")
    result_df[order(result_df$Rank), ]
  })

  output$gs_category_summary <- renderUI({
    req(gs_ranks())
    df <- gs_ranks()
    n_elite <- sum(df$Category=="Elite")
    n_total <- nrow(df)
    tags$div(style="display:flex;gap:12px;flex-wrap:wrap;margin-bottom:8px;",
      tags$div(style="background:#c7f9cc;border-radius:8px;padding:10px 18px;font-weight:700;color:#1b4332;",
        sprintf("🏆 Elite: %d plots (%.1f%%)", n_elite, 100*n_elite/n_total)),
      tags$div(style="background:#f0f0f0;border-radius:8px;padding:10px 18px;font-weight:600;color:#555;",
        sprintf("📋 Standard: %d plots", n_total - n_elite)),
      tags$div(style="background:#fff3cd;border-radius:8px;padding:10px 18px;font-weight:600;color:#856404;",
        sprintf("📊 Total: %d plots", n_total))
    )
  })

  output$gs_rank_tbl <- renderDT({
    req(gs_ranks())
    df <- gs_ranks()
    datatable(df, options=list(pageLength=20, scrollX=TRUE), rownames=FALSE,
              class="table-striped table-hover compact") %>%
      formatRound(which(sapply(df,is.numeric) & !names(df) %in% "Rank"), digits=4) %>%
      formatStyle("Score",
        background=styleColorBar(c(0,1),"#2d6a4f"),
        backgroundSize="98% 70%", backgroundRepeat="no-repeat", backgroundPosition="center") %>%
      formatStyle("Rank",
        color=styleInterval(c(5,10,20),c(C_RED,C_GOLD,"#2d6a4f","#555")), fontWeight="bold") %>%
      formatStyle("Category",
        backgroundColor=styleEqual(c("Elite","Standard"),c("#c7f9cc","#f8f9fa")),
        color=styleEqual(c("Elite","Standard"),c("#1b4332","#555")), fontWeight="bold")
  })

  output$gs_plot_idx_sel <- renderUI({
    cols <- gs_idx_cols()
    selectInput("gs_chart_idx","Index for chart:", choices=cols, selected=cols[1])
  })

  output$gs_bar_plot <- renderPlotly({
    req(gs_ranks(), input$gs_chart_idx); input$gs_render_chart
    df     <- gs_ranks()
    n      <- min(input$gs_top_n, nrow(df))
    top_df <- head(df, n)
    id_col <- names(df)[1]
    idx    <- input$gs_chart_idx
    if (!idx %in% names(top_df)) return(NULL)

    # Get threshold for this index
    thr_val <- as.numeric(input[[paste0("gs_thr_",idx)]])

    p <- ggplot(top_df,
           aes(x=reorder(.data[[id_col]], .data[[idx]]),
               y=.data[[idx]], fill=Category)) +
      geom_col(color="white", linewidth=0.15) +
      scale_fill_manual(values=c("Elite"="#2d6a4f","Standard"="#cccccc")) +
      coord_flip() +
      labs(title=paste("Top", n, "Plots —", idx),
           subtitle="Green = Elite (all thresholds met)",
           x=id_col, y=idx, fill="Category") +
      theme_minimal() +
      theme(plot.title=element_text(face="bold",color=C_DARK,size=13),
            plot.subtitle=element_text(size=10,color="#666"),
            panel.grid.major.y=element_blank())

    if (!is.null(thr_val) && is.finite(thr_val))
      p <- p + geom_hline(yintercept=thr_val, color=C_RED, linetype="dashed",
                           linewidth=1.1) +
               annotate("text", x=1.2, y=thr_val, label=paste("Threshold:", round(thr_val,4)),
                        color=C_RED, size=3.2, hjust=-0.05)
    ggplotly(p, tooltip=c("x","y","fill"))
  })

  output$gs_radar_sel <- renderUI({
    req(gs_ranks())
    df    <- gs_ranks()
    id_col <- names(df)[1]
    ids   <- df[[id_col]]
    n_show <- min(10, length(ids))
    checkboxGroupInput("gs_radar_ids","Select plots (≤ 10):",
      choices=ids[1:n_show], selected=ids[1:min(5,n_show)])
  })

  output$gs_radar_plot <- renderPlotly({
    req(gs_ranks(), input$gs_radar_ids); input$gs_render_radar
    df     <- gs_ranks()
    id_col <- names(df)[1]
    cols   <- gs_idx_cols()
    cols   <- cols[cols %in% names(df)]
    sel    <- df[df[[id_col]] %in% input$gs_radar_ids, ]
    if (!nrow(sel) || !length(cols)) return(NULL)

    # Normalise each index 0-1 for radar
    norm_df <- sel
    for (col in cols) {
      x <- sel[[col]]; rng <- range(x, na.rm=TRUE)
      if (rng[1]!=rng[2]) norm_df[[col]] <- (x-rng[1])/(rng[2]-rng[1])
    }

    fig <- plot_ly(type="scatterpolar", fill="toself")
    colors_pal <- viridis::viridis(nrow(sel))
    for (i in seq_len(nrow(sel))) {
      fig <- fig %>% add_trace(
        r    = unlist(norm_df[i, cols]),
        theta= cols,
        name = as.character(sel[[id_col]][i]),
        line = list(color=colors_pal[i])
      )
    }
    fig %>% layout(
      polar=list(radialaxis=list(visible=TRUE, range=c(0,1))),
      title=list(text="Normalised Index Radar", font=list(size=13, color=C_DARK)),
      showlegend=TRUE
    )
  })

  output$dl_gs <- downloadHandler(
    filename=function() paste0("DryBean_PlotRankings_",Sys.Date(),".xlsx"),
    content =function(file) {
      req(gs_ranks())
      df  <- gs_ranks()
      wb  <- openxlsx::createWorkbook()
      hdr <- openxlsx::createStyle(fontColour="white",fgFill=C_RED,
               fontName="Calibri",fontSize=11,textDecoration="bold",
               halign="center",border="Bottom")
      openxlsx::addWorksheet(wb,"Plot Rankings")
      openxlsx::writeData(wb,"Plot Rankings",df)
      openxlsx::addStyle(wb,"Plot Rankings",hdr,rows=1,cols=seq_len(ncol(df)))
      openxlsx::setColWidths(wb,"Plot Rankings",cols=seq_len(ncol(df)),widths="auto")
      score_col <- which(names(df)=="Score")
      hi_rows <- which(df$Category=="Elite")+1
      lo_rows <- tail(order(df$Score),5)+1
      if (length(hi_rows)) openxlsx::addStyle(wb,"Plot Rankings",
        openxlsx::createStyle(fgFill="#c7f9cc"),rows=hi_rows,cols=score_col,stack=TRUE)
      if (length(lo_rows)) openxlsx::addStyle(wb,"Plot Rankings",
        openxlsx::createStyle(fgFill="#ffccd5"),rows=lo_rows,cols=score_col,stack=TRUE)
      openxlsx::saveWorkbook(wb,file)
    }
  )

  # ── Machine Learning ─────────────────────────────────────────

  output$ml_feature_sel <- renderUI({
    req(rv$merged)
    idx_cols <- if (!is.null(rv$idx_ras)) names(rv$idx_ras) else character(0)
    num_cols <- names(rv$merged)[sapply(rv$merged,is.numeric)]
    feat_ch  <- unique(c(idx_cols[idx_cols %in% num_cols], num_cols))
    checkboxGroupInput("ml_features","Feature columns (X):",
      choices=feat_ch, selected=feat_ch[seq_len(min(6,length(feat_ch)))],
      inline=FALSE)
  })

  output$ml_target_sel <- renderUI({
    req(rv$merged)
    all_cols <- names(rv$merged)
    feat     <- if (!is.null(input$ml_features)) input$ml_features else character(0)
    tgt_ch   <- setdiff(all_cols, feat)
    if (!length(tgt_ch)) tgt_ch <- all_cols
    # Build label hints for each target
    tgt_labels <- sapply(tgt_ch, function(col) {
      x <- rv$merged[[col]]
      x2 <- suppressWarnings(as.numeric(as.character(x[!is.na(x)])))
      n_u <- length(unique(x[!is.na(x)]))
      is_num <- all(!is.na(x2))
      hint <- if (!is_num)                 paste0(" [categorical, ", n_u, " classes]")
              else if (n_u <= 10)          paste0(" [", n_u, " levels — Classification]")
              else                         paste0(" [continuous — Regression]")
      paste0(col, hint)
    })
    tagList(
      selectInput("ml_target","Target variable (Y):",
        choices  = setNames(tgt_ch, tgt_labels),
        selected = tgt_ch[length(tgt_ch)]),
      uiOutput("ml_target_hint")
    )
  })

  output$ml_target_hint <- renderUI({
    req(rv$merged, input$ml_target)
    tgt <- input$ml_target
    if (!tgt %in% names(rv$merged)) return(NULL)
    x   <- rv$merged[[tgt]]
    x_c <- suppressWarnings(as.numeric(as.character(x[!is.na(x)])))
    n_u <- length(unique(x[!is.na(x)]))
    is_num <- all(!is.na(x_c))
    will_cls <- (!is_num) || (n_u <= 10)
    col <- if (will_cls) "#1b4332" else "#0f3460"
    bg  <- if (will_cls) "#c7f9cc" else "#dbeafe"
    lbl <- if (will_cls)
      paste0("🔵 Will use Classification (", n_u, " classes)")
    else
      paste0("🟢 Will use Regression (", n_u, " unique numeric values)")
    tags$div(style=paste0("background:",bg,";border-radius:5px;padding:7px 10px;",
                          "font-size:11.5px;font-weight:600;color:",col,";margin-top:-8px;"),
      lbl,
      if (will_cls && is_num)
        tags$div(style="font-weight:400;margin-top:3px;",
          "Tip: numeric targets with ≤10 unique values are treated as class labels.")
      else if (!will_cls && n_u < 20)
        tags$div(style="font-weight:400;color:#CC0000;margin-top:3px;",
          paste0("⚠️ Only ", n_u, " unique values — consider Classification instead."))
    )
  })

  output$ml_progress_ui <- renderUI({
    if (is.null(rv$ml_log)) return(NULL)
    tags$div(style="background:#f8f8f8;border-radius:4px;padding:10px;font-size:11.5px;",
      pre(rv$ml_log))
  })

  rv$ml_results <- NULL
  rv$ml_log     <- NULL

  # ── Helper: build tuneGrid per algo ──────────────────────────
  build_tune_grid <- function(algo) {
    tryCatch({
      g <- switch(algo,
        "rf" = {
          mtry_v <- as.integer(input$hp_rf_mtry)
          if (is.na(mtry_v) || mtry_v <= 0) return(NULL)
          expand.grid(mtry=mtry_v)
        },
        "gbm" = expand.grid(
          n.trees=as.integer(input$hp_gbm_trees),
          interaction.depth=as.integer(input$hp_gbm_depth),
          shrinkage=as.numeric(input$hp_gbm_shrink),
          n.minobsinnode=10),
        "xgbTree" = expand.grid(
          nrounds=as.integer(input$hp_xgb_rounds),
          max_depth=as.integer(input$hp_xgb_depth),
          eta=as.numeric(input$hp_xgb_eta),
          gamma=0, colsample_bytree=0.8,
          min_child_weight=1, subsample=0.8),
        "svmRadial" = {
          sv <- as.numeric(input$hp_svmR_sigma)
          if (is.na(sv)||sv<=0) return(NULL)
          expand.grid(C=as.numeric(input$hp_svmR_C), sigma=sv)
        },
        "svmLinear"  = expand.grid(C=as.numeric(input$hp_svmL_C)),
        "nnet"       = expand.grid(size=as.integer(input$hp_nnet_size), decay=as.numeric(input$hp_nnet_decay)),
        "glmnet"     = {
          lv <- as.numeric(input$hp_glm_lambda)
          expand.grid(alpha=as.numeric(input$hp_glm_alpha),
                      lambda=if(is.na(lv)||lv<=0) 10^seq(-4,0,length=8) else lv)
        },
        "kknn"  = expand.grid(kmax=as.integer(input$hp_kknn_k), distance=2, kernel="optimal"),
        "rpart" = expand.grid(cp=as.numeric(input$hp_rpart_cp)),
        NULL
      )
      if (!is.null(g) && any(!is.finite(unlist(g)))) return(NULL)
      g
    }, error=function(e) NULL)
  }

  observeEvent(input$run_ml, {
    req(rv$merged, input$ml_features, input$ml_target, input$ml_algos)

    df     <- rv$merged
    feat   <- input$ml_features
    tgt    <- input$ml_target
    algos  <- input$ml_algos
    task   <- input$ml_task
    k_fold <- as.integer(input$ml_cv_k)
    n_rep  <- as.integer(input$ml_cv_rep)
    tr_pct <- input$ml_train_pct / 100

    # Ensure packages loaded
    pkg_map <- list(rf="randomForest",gbm="gbm",xgbTree="xgboost",
      svmRadial="e1071",svmLinear="e1071",nnet="nnet",glmnet="glmnet",
      kknn="kknn",rpart="rpart",naive_bayes="naivebayes",treebag="ipred",lda="MASS")
    for (a in algos) {
      pkg <- pkg_map[[a]]
      if (!is.null(pkg)) suppressPackageStartupMessages(
        tryCatch(library(pkg,character.only=TRUE,quietly=TRUE), error=function(e) NULL))
    }

    # ── Auto-detect task type from target ──
    tgt_raw     <- df[[tgt]]
    n_unique    <- length(unique(tgt_raw[!is.na(tgt_raw)]))
    tgt_numeric <- suppressWarnings(all(!is.na(as.numeric(as.character(tgt_raw[!is.na(tgt_raw)])))))
    auto_cls    <- (!tgt_numeric) || (n_unique <= 10 && n_unique >= 2)

    if (task == "reg" && auto_cls) {
      task <- "cls"
      showNotification(paste0("Target '",tgt,"' has ",n_unique,
        " unique values — auto-switched to Classification."), type="warning", duration=8)
    }
    if (task == "cls" && tgt_numeric && n_unique > 20) {
      task <- "reg"
      showNotification(paste0("Target '",tgt,"' has ",n_unique,
        " numeric values — auto-switched to Regression."), type="warning", duration=8)
    }

    # ── Build model data ──────────────────────────────────────────
    # Only keep columns that actually exist in merged
    feat_use <- feat[feat %in% names(df)]
    model_df <- df[, c(feat_use, tgt), drop=FALSE]

    # Coerce feature columns to numeric (preserve values, just ensure type)
    for (col in feat_use) {
      x <- model_df[[col]]
      if (!is.numeric(x)) x <- suppressWarnings(as.numeric(as.character(x)))
      x[!is.finite(x)] <- NA   # replace Inf / NaN with NA
      model_df[[col]] <- x
    }

    # Coerce / factor target
    if (task == "cls") {
      model_df[[tgt]] <- as.factor(as.character(model_df[[tgt]]))
    } else {
      x <- suppressWarnings(as.numeric(as.character(model_df[[tgt]])))
      x[!is.finite(x)] <- NA
      model_df[[tgt]] <- x
    }

    # ── Drop columns with > 50% NA (bad indices like RI near zero) ──
    na_frac <- sapply(model_df[feat_use], function(x) mean(is.na(x)))
    bad_cols <- names(na_frac[na_frac > 0.50])
    if (length(bad_cols)) {
      model_df  <- model_df[, !names(model_df) %in% bad_cols, drop=FALSE]
      feat_use  <- feat_use[!feat_use %in% bad_cols]
      showNotification(paste0("Dropped ", length(bad_cols),
        " feature(s) with >50% NA: ", paste(bad_cols, collapse=", ")),
        type="warning", duration=7)
    }

    # ── Median-impute remaining NAs in features (keeps all rows) ──
    for (col in feat_use) {
      x <- model_df[[col]]
      if (any(is.na(x))) {
        med <- median(x, na.rm=TRUE)
        x[is.na(x)] <- if (is.finite(med)) med else 0
        model_df[[col]] <- x
      }
    }

    # Drop rows where TARGET is still NA
    model_df <- model_df[!is.na(model_df[[tgt]]), , drop=FALSE]

    # ── Diagnostics ──
    n_rows <- nrow(model_df)
    n_feat <- length(feat_use)

    if (n_rows < 10) {
      na_report <- paste(sapply(c(feat_use, tgt), function(col)
        sprintf("  %s: %d NA", col,
          if (col %in% names(df)) sum(is.na(df[[col]])) else NA_integer_)),
        collapse="
")
      rv$ml_log <- paste0(
        "ERROR: Only ", n_rows, " usable rows. Need >= 10.
",
        "  Target: '", tgt, "' (", n_unique, " unique values, task=", task, ")
",
        "  Features available: ", n_feat, "
",
        "  NA counts per column:
", na_report, "

",
        "Suggestions:
",
        "  1. Deselect features with many NAs/Inf (e.g. RI, GLAI)
",
        "  2. Make sure target column has real values
",
        "  3. Try fewer features")
      return()
    }

    clean_tgt <- make.names(tgt)
    safe_feat <- make.names(feat_use)
    # Rename model_df columns safely
    col_map <- setNames(c(feat_use, tgt), c(safe_feat, clean_tgt))
    names(model_df)[names(model_df) %in% names(col_map)] <-
      col_map[names(model_df)[names(model_df) %in% names(col_map)]]
    feat <- feat_use

    withProgress(message="Training models...", value=0, {
      logs <- c(
        sprintf("Data: %d rows x %d features | target: %s | task: %s | unique_Y: %d",
                nrow(model_df), length(feat), tgt, task, n_unique))
      results <- list(); preds_list <- list()

      set.seed(42)
      train_idx <- sample(seq_len(nrow(model_df)), floor(tr_pct*nrow(model_df)))
      train_df  <- model_df[ train_idx,]
      test_df   <- model_df[-train_idx,]

      cv_method <- if (n_rep>1) "repeatedcv" else "cv"
      ctrl <- tryCatch(
        caret::trainControl(
          method=cv_method, number=k_fold,
          repeats=if(n_rep>1) n_rep else NA,
          savePredictions="final", verboseIter=FALSE, allowParallel=FALSE
        ), error=function(e) {
          rv$ml_log <<- paste("ERROR: caret unavailable —", e$message,
            "\nRun install_packages.R first.")
          NULL
        }
      )
      if (is.null(ctrl)) return()

      formula_str <- as.formula(paste0(clean_tgt," ~ ."))

      for (i in seq_along(algos)) {
        algo <- algos[i]
        incProgress(i/length(algos), message=paste("Training:", algo))
        tryCatch({
          tg <- build_tune_grid(algo)
          fa <- list(formula_str, data=train_df, method=algo,
                     trControl=ctrl, preProcess=c("center","scale"))
          if (!is.null(tg)) fa$tuneGrid <- tg else fa$tuneLength <- 5
          if (algo=="rf") {
            nt <- suppressWarnings(as.integer(input$hp_rf_ntree))
            if (!is.na(nt)&&nt>0) fa$ntree <- nt
          }
          if (algo=="nnet") fa$maxit <- 500

          fit        <- do.call(caret::train, fa)
          pred_test  <- predict(fit, newdata=test_df)

          if (task=="reg") {
            # Retrieve actual values — test_df columns were renamed to make.names
            actual_col <- make.names(tgt)
            actual <- as.numeric(test_df[[actual_col]])
            pred_n <- as.numeric(as.character(pred_test))

            # Diagnostic: check lengths match
            if (length(actual) != length(pred_n))
              pred_n <- pred_n[seq_along(actual)]

            n_pairs <- sum(!is.na(actual) & !is.na(pred_n))
            rmse_v  <- if (n_pairs < 2) NA_real_ else
                         sqrt(mean((pred_n - actual)^2, na.rm=TRUE))
            mae_v   <- if (n_pairs < 2) NA_real_ else
                         mean(abs(pred_n - actual), na.rm=TRUE)
            rsq_v   <- tryCatch({
              sd_p <- sd(pred_n, na.rm=TRUE); sd_a <- sd(actual, na.rm=TRUE)
              if (n_pairs < 3 || is.na(sd_p)||sd_p<1e-10||is.na(sd_a)||sd_a<1e-10)
                NA_real_
              else cor(pred_n, actual, use="complete.obs")^2
            }, error=function(e) NA_real_)

            metrics <- data.frame(Algorithm=algo,
              RMSE=round(rmse_v,4),
              R2  =ifelse(is.na(rsq_v), NA_real_, round(rsq_v,4)),
              MAE =round(mae_v,4), stringsAsFactors=FALSE)
            log_line <- sprintf("  OK  %-14s n=%d  RMSE=%.4f  R2=%s  MAE=%.4f",
              algo, n_pairs, ifelse(is.na(rmse_v),0,rmse_v),
              ifelse(is.na(rsq_v),"NA",sprintf("%.4f",rsq_v)),
              ifelse(is.na(mae_v),0,mae_v))
          } else {
            cm    <- table(Pred=pred_test, Act=test_df[[clean_tgt]])
            acc_v <- sum(diag(cm))/sum(cm)
            pe    <- tryCatch(sum(rowSums(cm)/sum(cm)*colSums(cm)/sum(cm)), error=function(e) NA_real_)
            kap_v <- tryCatch((acc_v-pe)/(1-pe), error=function(e) NA_real_)
            metrics <- data.frame(Algorithm=algo,
              Accuracy=round(acc_v,4),
              Kappa   =round(kap_v,4), stringsAsFactors=FALSE)
            log_line <- sprintf("  OK  %-14s Acc=%.4f  Kappa=%s",
              algo, acc_v, ifelse(is.na(kap_v),"NA",sprintf("%.4f",kap_v)))
          }
          results[[algo]]    <- list(fit=fit, metrics=metrics)
          preds_list[[algo]] <- data.frame(
            Actual=as.character(test_df[[clean_tgt]]),
            Predicted=as.character(pred_test),
            Algorithm=algo, stringsAsFactors=FALSE)
          logs <- c(logs, log_line)
        }, error=function(e) {
          logs <<- c(logs, sprintf("  SKIP %-12s %s", algo, conditionMessage(e)))
        })
      }

      # RFE
      rfe_result <- NULL
      if (isTRUE(input$ml_rfe)) {
        incProgress(0.95, "Running RFE...")
        tryCatch({
          rfe_ctrl   <- caret::rfeControl(functions=caret::rfFuncs, method="cv", number=min(k_fold,5))
          sizes_v    <- unique(sort(c(1, seq(2, min(input$ml_rfe_sizes,length(feat)), by=2))))
          rfe_result <- caret::rfe(x=train_df[,safe_feat,drop=FALSE],
            y=train_df[[clean_tgt]], sizes=sizes_v, rfeControl=rfe_ctrl)
          logs <- c(logs, sprintf("  RFE optimal=%d top: %s", rfe_result$optsize,
            paste(head(caret::predictors(rfe_result),5),collapse=", ")))
        }, error=function(e) logs <<- c(logs, paste("  RFE failed:", e$message)))
      }

      rv$ml_results <- list(models=results, task=task, target=tgt,
        features=feat, safe_feat=safe_feat, preds=preds_list, rfe=rfe_result,
        train_df=train_df, test_df=test_df)
      rv$ml_log <- paste(logs, collapse="\n")
    })
  })

  ml_metrics_df <- reactive({
    req(rv$ml_results)
    do.call(rbind, lapply(rv$ml_results$models, function(x) x$metrics))
  })

  output$ml_perf_tbl <- renderDT({
    req(ml_metrics_df())
    df   <- ml_metrics_df()
    task <- rv$ml_results$task
    dt <- datatable(df, options=list(pageLength=15, scrollX=TRUE), rownames=FALSE,
              class="table-striped table-hover compact")
    if (task == "reg") {
      if ("RMSE" %in% names(df))
        dt <- dt %>% formatStyle("RMSE",
          color=styleInterval(quantile(df$RMSE,c(0.33,0.66),na.rm=TRUE),
                              c(C_GREEN,"#e6a800",C_RED)), fontWeight="bold")
      if ("R2" %in% names(df))
        dt <- dt %>% formatStyle("R2",
          color=styleInterval(c(0.5,0.8), c(C_RED,"#e6a800",C_GREEN)), fontWeight="bold")
    } else {
      if ("Accuracy" %in% names(df))
        dt <- dt %>% formatStyle("Accuracy",
          color=styleInterval(c(0.6,0.8), c(C_RED,"#e6a800",C_GREEN)), fontWeight="bold")
    }
    dt
  })

  output$ml_perf_plot <- renderPlotly({
    req(ml_metrics_df())
    df   <- ml_metrics_df()
    task <- rv$ml_results$task

    if (task == "reg") {
      # Side-by-side: RMSE (lower=better) and R2 (higher=better)
      has_r2 <- "R2" %in% names(df)
      p1 <- ggplot(df, aes(x=reorder(Algorithm, -RMSE), y=RMSE, fill=RMSE)) +
        geom_col(color="white", linewidth=0.2) +
        scale_fill_gradient(low=C_GREEN, high=C_RED) +
        coord_flip() +
        labs(title="RMSE (lower = better)", x=NULL, y="RMSE") +
        theme_minimal() +
        theme(plot.title=element_text(face="bold",color=C_DARK,size=11),
              panel.grid.major.y=element_blank(), legend.position="none")
      fig <- subplot(ggplotly(p1), nrows=1, shareY=FALSE, titleX=TRUE)
      if (has_r2) {
        p2 <- ggplot(df, aes(x=reorder(Algorithm, R2), y=R2, fill=R2)) +
          geom_col(color="white", linewidth=0.2) +
          scale_fill_gradient(low="#ccc", high=C_GREEN) +
          coord_flip() +
          labs(title="R² (higher = better)", x=NULL, y="R²") +
          theme_minimal() +
          theme(plot.title=element_text(face="bold",color=C_DARK,size=11),
                panel.grid.major.y=element_blank(), legend.position="none")
        fig <- subplot(ggplotly(p1), ggplotly(p2), nrows=1, shareY=FALSE,
                       titleX=TRUE, margin=0.06)
      }
      fig %>% layout(showlegend=FALSE,
               title=list(text="Model Comparison", font=list(size=14,color=C_DARK)))
    } else {
      metric_col <- if ("Accuracy" %in% names(df)) "Accuracy" else names(df)[2]
      p <- ggplot(df, aes(x=reorder(Algorithm, .data[[metric_col]]),
                          y=.data[[metric_col]], fill=.data[[metric_col]])) +
        geom_col(color="white", linewidth=0.2) +
        scale_fill_gradient(low="#ccc", high=C_GREEN) +
        coord_flip() +
        labs(title=paste("Model Comparison —", metric_col),
             x="Algorithm", y=metric_col) +
        theme_minimal() +
        theme(plot.title=element_text(face="bold",color=C_DARK,size=13),
              panel.grid.major.y=element_blank(), legend.position="none")
      ggplotly(p)
    }
  })

  output$ml_imp_model_sel <- renderUI({
    req(rv$ml_results)
    nms <- names(rv$ml_results$models)
    selectInput("ml_imp_model","Model:", choices=nms, selected=nms[1])
  })

  output$ml_imp_plot <- renderPlotly({
    req(rv$ml_results, input$ml_imp_model)
    fit <- rv$ml_results$models[[input$ml_imp_model]]$fit
    imp <- tryCatch(caret::varImp(fit, scale=TRUE)$importance, error=function(e) NULL)
    if (is.null(imp)) return(NULL)
    if (ncol(imp) > 1) imp <- data.frame(Overall=rowMeans(imp))
    df_imp <- data.frame(Variable=rownames(imp), Importance=imp[[1]])
    df_imp <- df_imp[order(df_imp$Importance, decreasing=TRUE),]
    df_imp <- head(df_imp, 20)

    p <- ggplot(df_imp, aes(x=reorder(Variable,Importance), y=Importance, fill=Importance)) +
      geom_col(color="white", linewidth=0.2) +
      scale_fill_gradientn(colors=c("#ccc",C_GREEN,C_GOLD,C_RED)) +
      coord_flip() +
      labs(title=paste("Feature Importance —", input$ml_imp_model),
           x=NULL, y="Scaled Importance") +
      theme_minimal() +
      theme(plot.title=element_text(face="bold",color=C_DARK,size=13), legend.position="none")
    ggplotly(p)
  })

  output$ml_pred_model_sel <- renderUI({
    req(rv$ml_results)
    nms <- names(rv$ml_results$preds)
    selectInput("ml_pred_model","Model:", choices=nms, selected=nms[1])
  })

  output$ml_pred_plot <- renderPlotly({
    req(rv$ml_results, input$ml_pred_model)
    preds <- rv$ml_results$preds[[input$ml_pred_model]]
    task  <- rv$ml_results$task
    if (task=="reg") {
      df <- preds
      df$Actual    <- as.numeric(df$Actual)
      df$Predicted <- as.numeric(df$Predicted)
      p <- ggplot(df, aes(x=Actual, y=Predicted)) +
        geom_point(color=C_RED, alpha=0.65, size=2.5) +
        geom_abline(slope=1,intercept=0,linetype="dashed",color=C_DARK,linewidth=0.9) +
        geom_smooth(method="lm",color=C_GREEN,fill=C_GOLD,alpha=0.15,se=TRUE) +
        labs(title=paste("Predicted vs Actual —", input$ml_pred_model),
             x="Actual", y="Predicted") +
        theme_minimal() +
        theme(plot.title=element_text(face="bold",color=C_DARK,size=13))
      ggplotly(p)
    } else {
      cm_df <- as.data.frame(table(Predicted=preds$Predicted, Actual=preds$Actual))
      p <- ggplot(cm_df, aes(x=Actual, y=Predicted, fill=Freq)) +
        geom_tile(color="white") +
        geom_text(aes(label=Freq), size=4, fontface="bold") +
        scale_fill_gradientn(colors=c("white",C_GOLD,C_RED)) +
        labs(title=paste("Confusion Matrix —", input$ml_pred_model)) +
        theme_minimal() +
        theme(plot.title=element_text(face="bold",color=C_DARK,size=13))
      ggplotly(p)
    }
  })

  output$ml_pred_tbl <- renderDT({
    req(rv$ml_results, input$ml_pred_model)
    preds <- rv$ml_results$preds[[input$ml_pred_model]]
    datatable(preds, options=list(pageLength=10,scrollX=TRUE), rownames=FALSE,
              class="table-striped compact")
  })

  output$ml_rfe_plot <- renderPlotly({
    req(rv$ml_results)
    rfe <- rv$ml_results$rfe
    if (is.null(rfe)) return(NULL)
    df  <- rfe$results
    p <- ggplot(df, aes(x=Variables, y=RMSE)) +
      geom_line(color=C_GREEN, linewidth=1.2) +
      geom_point(color=C_RED, size=3.5) +
      geom_vline(xintercept=rfe$optsize, linetype="dashed", color=C_GOLD, linewidth=1) +
      labs(title="RFE: Model Performance vs Number of Features",
           x="Number of Features", y="CV RMSE") +
      theme_minimal() +
      theme(plot.title=element_text(face="bold",color=C_DARK,size=13))
    ggplotly(p)
  })

  output$ml_rfe_vars_ui <- renderUI({
    req(rv$ml_results)
    rfe <- rv$ml_results$rfe
    if (is.null(rfe)) return(tags$p("Run RFE to see selected variables.", style="color:#888;"))
    top_vars <- caret::predictors(rfe)
    tags$div(
      tags$b(sprintf("Optimal feature set (%d variables):", rfe$optsize)),
      tags$br(),
      lapply(seq_along(top_vars), function(i)
        tags$span(class="idx-pill", if (i<=rfe$optsize) "green" else "",
                  style=if(i<=rfe$optsize)"background:#2d6a4f;color:#fff;" else "",
                  top_vars[i])
      )
    )
  })

  output$ml_best_model_ui <- renderUI({
    req(ml_metrics_df())
    df   <- ml_metrics_df()
    mc   <- if (!is.null(input$ml_best_metric)) input$ml_best_metric else "RMSE"
    # Only use metrics that exist
    mc   <- if (mc %in% names(df)) mc else names(df)[2]
    # Lower is better: RMSE, MAE; higher is better: R2, Accuracy, Kappa
    lower_better <- mc %in% c("RMSE","MAE")
    valid <- df[!is.na(df[[mc]]), ]
    if (!nrow(valid)) return(tags$p("No valid metric values yet.", style="color:#888;"))
    best_row <- if (lower_better) valid[which.min(valid[[mc]]),] else valid[which.max(valid[[mc]]),]
    best     <- best_row$Algorithm
    val      <- round(best_row[[mc]], 4)
    direction <- if (lower_better) "↓ lower=better" else "↑ higher=better"
    col <- if (lower_better) "#1b4332" else "#0f3460"
    bg  <- if (lower_better) "#c7f9cc"  else "#dbeafe"
    tags$div(style=paste0("background:",bg,";border-radius:8px;padding:10px 14px;"),
      tags$div(style=paste0("font-weight:700;color:",col,";font-size:13px;"),
        sprintf("🥇 Best: %s", best)),
      tags$div(style="font-size:12px;color:#555;margin-top:3px;",
        sprintf("%s = %s  (%s)", mc, val, direction))
    )
  })

  output$dl_ml_xlsx <- downloadHandler(
    filename=function() paste0("DryBean_ML_Results_",Sys.Date(),".xlsx"),
    content =function(file) {
      req(rv$ml_results)
      wb  <- openxlsx::createWorkbook()
      hdr <- openxlsx::createStyle(fontColour="white",fgFill=C_RED,
               fontName="Calibri",fontSize=11,textDecoration="bold",
               halign="center",border="Bottom")
      add_sheet <- function(wb, name, df) {
        openxlsx::addWorksheet(wb, name)
        openxlsx::writeData(wb, name, df)
        openxlsx::addStyle(wb, name, hdr, rows=1, cols=seq_len(ncol(df)))
        openxlsx::setColWidths(wb, name, cols=seq_len(ncol(df)), widths="auto")
      }
      add_sheet(wb, "Performance", ml_metrics_df())
      all_preds <- do.call(rbind, rv$ml_results$preds)
      if (!is.null(all_preds)) add_sheet(wb, "Predictions", all_preds)
      openxlsx::saveWorkbook(wb, file)
    }
  )

  output$dl_ml_preds <- downloadHandler(
    filename=function() paste0("DryBean_ML_Predictions_",Sys.Date(),".csv"),
    content =function(file) {
      req(rv$ml_results)
      all_preds <- do.call(rbind, rv$ml_results$preds)
      write.csv(all_preds, file, row.names=FALSE)
    }
  )


  # ═══════════════════════════════════════════════════════════
  # DATA QC SERVER
  # ═══════════════════════════════════════════════════════════

  qc_results <- eventReactive(input$run_qc, {
    req(rv$merged)
    k   <- input$qc_outlier_k
    df  <- rv$merged
    nc  <- names(df)[sapply(df, is.numeric)]
    nc  <- nc[sapply(df[nc], function(x) sum(is.finite(x)) > 3)]

    # Completeness
    completeness <- sapply(nc, function(col) {
      x <- df[[col]]; round(100 * sum(is.finite(x)) / length(x), 1)
    })

    # Outlier counts per column
    outlier_counts <- sapply(nc, function(col) {
      x <- df[[col]]; x[!is.finite(x)] <- NA
      q1 <- quantile(x, 0.25, na.rm=TRUE); q3 <- quantile(x, 0.75, na.rm=TRUE)
      iqr <- q3 - q1
      sum(!is.na(x) & (x < q1 - k*iqr | x > q3 + k*iqr), na.rm=TRUE)
    })

    # Flag rows that are outliers in at least 1 index
    flag_mat <- sapply(nc, function(col) {
      x <- df[[col]]; x[!is.finite(x)] <- NA
      q1 <- quantile(x, 0.25, na.rm=TRUE); q3 <- quantile(x, 0.75, na.rm=TRUE)
      iqr <- q3 - q1
      !is.na(x) & (x < q1 - k*iqr | x > q3 + k*iqr)
    })
    flagged_rows <- rowSums(flag_mat) >= 1

    list(completeness=completeness, outlier_counts=outlier_counts,
         flagged_rows=flagged_rows, nc=nc, k=k)
  })

  output$qc_summary_ui <- renderUI({
    req(qc_results())
    r  <- qc_results()
    n_flagged  <- sum(r$flagged_rows)
    n_total    <- nrow(rv$merged)
    low_comp   <- sum(r$completeness < 90)
    tags$div(
      tags$div(style="background:#c7f9cc;border-radius:6px;padding:8px 12px;margin-bottom:6px;",
        tags$b(sprintf("Total plots: %d", n_total))),
      tags$div(style=paste0("background:",if(n_flagged>0)"#fff3cd" else "#c7f9cc",
                            ";border-radius:6px;padding:8px 12px;margin-bottom:6px;"),
        tags$b(sprintf("Outlier plots: %d (%.1f%%)", n_flagged, 100*n_flagged/n_total))),
      tags$div(style=paste0("background:",if(low_comp>0)"#f8d7da" else "#c7f9cc",
                            ";border-radius:6px;padding:8px 12px;"),
        tags$b(sprintf("Indices < 90%% complete: %d", low_comp)))
    )
  })

  output$qc_completeness <- renderPlotly({
    req(rv$merged)
    df  <- rv$merged
    nc  <- names(df)[sapply(df, is.numeric)]
    completeness <- sapply(nc, function(col) {
      x <- df[[col]]; round(100 * sum(is.finite(x)) / length(x), 1)
    })
    df_c <- data.frame(Index=nc, Completeness=completeness)
    df_c <- df_c[order(df_c$Completeness), ]
    plot_ly(df_c, x=~Completeness, y=~reorder(Index,Completeness),
            type="bar", orientation="h",
            marker=list(color=~Completeness, colorscale="RdYlGn",
                        showscale=TRUE, cmin=0, cmax=100,
                        colorbar=list(title=list(text="%"))),
            hovertemplate="%{y}: %{x:.1f}%<extra></extra>") %>%
      add_segments(x=90, xend=90, y=0, yend=nrow(df_c)+0.5,
                   line=list(color="red",dash="dash",width=2), name="90% threshold") %>%
      layout(title=list(text="Data Completeness per Index",font=list(size=13,color=C_DARK)),
             xaxis=list(title="% Valid Values",range=c(0,101)),
             yaxis=list(title=""), margin=list(l=120),
             paper_bgcolor="white") %>%
      config(displaylogo=FALSE, toImageButtonOptions=list(format="png",filename="DataQC_Completeness"))
  })

  output$qc_outlier_bar <- renderPlotly({
    req(qc_results())
    r <- qc_results()
    df_o <- data.frame(Index=r$nc, Outliers=r$outlier_counts)
    df_o <- df_o[order(-df_o$Outliers), ]
    df_o <- head(df_o, 20)
    plot_ly(df_o, x=~reorder(Index,-Outliers), y=~Outliers, type="bar",
            marker=list(color=~Outliers, colorscale=list(c(0,"#c7f9cc"),c(1,C_RED)),
                        showscale=FALSE),
            hovertemplate="%{x}: %{y} outliers<extra></extra>") %>%
      layout(title=list(text=paste0("Outlier Count per Index (IQR×",r$k,")"),
                        font=list(size=13,color=C_DARK)),
             xaxis=list(title="", tickangle=-40),
             yaxis=list(title="# Outlier Plots"),
             paper_bgcolor="white") %>%
      config(displaylogo=FALSE)
  })

  output$qc_outlier_tbl <- renderDT({
    req(qc_results(), rv$merged)
    flagged <- rv$merged[qc_results()$flagged_rows, , drop=FALSE]
    if (!nrow(flagged)) return(datatable(data.frame(Message="No outliers detected")))
    datatable(head(flagged, 100), options=list(pageLength=10, scrollX=TRUE),
              rownames=FALSE, class="table-striped compact")
  })

  output$qc_range_plot <- renderPlotly({
    req(rv$merged)
    df  <- rv$merged
    nc  <- names(df)[sapply(df, is.numeric)]
    nc  <- nc[sapply(df[nc], function(x) sum(is.finite(x)) > 3)]

    # Compute 5-number summary per index
    summ <- do.call(rbind, lapply(nc, function(col) {
      x <- df[[col]]; x[!is.finite(x)] <- NA
      data.frame(Index=col, Min=min(x,na.rm=T), Q1=quantile(x,.25,na.rm=T),
                 Median=median(x,na.rm=T), Q3=quantile(x,.75,na.rm=T),
                 Max=max(x,na.rm=T), stringsAsFactors=FALSE)
    }))

    plot_ly() %>%
      add_trace(data=summ, x=~Index, type="box",
                lowerfence=~Min, q1=~Q1, median=~Median, q3=~Q3, upperfence=~Max,
                marker=list(color=C_RED),
                hovertemplate="<b>%{x}</b><br>Min: %{lowerfence:.4f}<br>Q1: %{q1:.4f}<br>Median: %{median:.4f}<br>Q3: %{q3:.4f}<br>Max: %{upperfence:.4f}<extra></extra>") %>%
      add_segments(x=0, xend=length(nc)+1, y=0, yend=0,
                   line=list(color="#2d6a4f",dash="dash",width=1.5)) %>%
      add_segments(x=0, xend=length(nc)+1, y=1, yend=1,
                   line=list(color="#2d6a4f",dash="dash",width=1.5)) %>%
      layout(title=list(text="Index Value Ranges — green dashed = reflectance [0,1]",
                        font=list(size=13,color=C_DARK)),
             xaxis=list(title="",tickangle=-40),
             yaxis=list(title="Value"),
             paper_bgcolor="white") %>%
      config(displaylogo=FALSE, toImageButtonOptions=list(format="png",filename="DataQC_Ranges"))
  })

  # ═══════════════════════════════════════════════════════════
  # SPECTRAL SIGNATURES SERVER
  # ═══════════════════════════════════════════════════════════

  # Determine available band columns
  avail_bands <- reactive({
    req(rv$merged)
    intersect(c("Blue","Green","Red","RedEdge","NIR"), names(rv$merged))
  })

  # Band wavelength centres (nm) for x-axis
  band_wavelengths <- c(Blue=475, Green=560, Red=668, RedEdge=717, NIR=840)

  output$spec_grp_sel <- renderUI({
    req(rv$merged)
    idx_cols  <- if (!is.null(rv$idx_ras)) names(rv$idx_ras) else character(0)
    band_cols <- c("Red","Green","Blue","RedEdge","NIR")
    meta_cols <- names(rv$merged)[!names(rv$merged) %in% c(idx_cols, band_cols)]
    grp_cols  <- meta_cols[sapply(meta_cols, function(col) {
      n_u <- length(unique(rv$merged[[col]][!is.na(rv$merged[[col]])]))
      n_u >= 2 && n_u <= 100
    })]
    if (!length(grp_cols)) grp_cols <- meta_cols[1]
    selectInput("spec_grp","Group plots by:", choices=c("None (all plots)"="none", grp_cols),
                selected=if(length(grp_cols)) grp_cols[1] else "none")
  })

  output$spec_id_sel <- renderUI({
    req(rv$merged)
    nc <- names(rv$merged)[1]  # plot ID col
    selectInput("spec_id_col","Plot ID column:", choices=names(rv$merged), selected=nc)
  })

  spec_plot_data <- eventReactive(input$render_spec, {
    req(rv$merged)
    bands <- avail_bands()
    if (!length(bands)) return(NULL)

    df    <- rv$merged
    grp   <- isolate(input$spec_grp)
    top_n <- isolate(input$spec_top_n)
    err_t <- isolate(input$spec_err)

    wl <- band_wavelengths[bands]

    if (grp == "none" || !grp %in% names(df)) {
      # Single mean signature for all plots
      means <- sapply(bands, function(b) mean(df[[b]], na.rm=TRUE))
      sds   <- sapply(bands, function(b) sd(df[[b]], na.rm=TRUE))
      ns    <- sapply(bands, function(b) sum(!is.na(df[[b]])))
      ses   <- sds / sqrt(ns)
      list(type="single", bands=bands, wl=wl, means=means,
           err=if(err_t=="sd") sds else if(err_t=="se") ses else rep(0,length(bands)))
    } else {
      groups <- unique(df[[grp]][!is.na(df[[grp]])])
      # Compute mean per group per band
      result <- lapply(groups, function(g) {
        sub <- df[df[[grp]]==g & !is.na(df[[grp]]), , drop=FALSE]
        means <- sapply(bands, function(b) mean(sub[[b]], na.rm=TRUE))
        sds   <- sapply(bands, function(b) sd(sub[[b]], na.rm=TRUE))
        ns    <- sapply(bands, function(b) sum(!is.na(sub[[b]])))
        ses   <- sds / sqrt(pmax(ns,1))
        list(group=as.character(g), means=means,
             err=if(err_t=="sd") sds else if(err_t=="se") ses else rep(0,length(bands)))
      })
      # Limit to top_n by mean NIR or first band
      sort_band <- if ("NIR" %in% bands) "NIR" else bands[length(bands)]
      ord <- order(sapply(result, function(r) r$means[which(bands==sort_band)]), decreasing=TRUE)
      result <- result[ord[seq_len(min(top_n, length(result)))]]
      list(type="group", bands=bands, wl=wl, groups=result)
    }
  })

  output$spec_plot <- renderPlotly({
    req(spec_plot_data())
    d    <- spec_plot_data()
    pal  <- viridis::viridis(if(d$type=="group") length(d$groups) else 1)
    wl   <- d$wl
    bnm  <- d$bands
    fig  <- plot_ly()

    if (d$type == "single") {
      fig <- fig %>%
        add_trace(x=wl, y=d$means, type="scatter", mode="lines+markers",
                  line=list(color=C_RED, width=2.5),
                  marker=list(size=8, color=C_RED),
                  error_y=if(isTRUE(any(d$err > 0, na.rm=TRUE))) list(array=d$err, color="#aaa") else NULL,
                  name="All plots",
                  hovertemplate=paste0("<b>%{text}</b><br>Wavelength: %{x} nm<br>",
                    "Reflectance: %{y:.4f}<extra></extra>"),
                  text=bnm)
    } else {
      for (i in seq_along(d$groups)) {
        g <- d$groups[[i]]
        fig <- fig %>%
          add_trace(x=wl, y=g$means, type="scatter", mode="lines+markers",
                    line=list(color=pal[i], width=2),
                    marker=list(size=7, color=pal[i]),
                    error_y=if(isTRUE(any(g$err > 0, na.rm=TRUE))) list(array=g$err, color=pal[i], opacity=0.4) else NULL,
                    name=g$group,
                    hovertemplate=paste0("<b>",g$group,"</b><br>%{text}: %{x} nm<br>",
                      "Reflectance: %{y:.4f}<extra></extra>"),
                    text=bnm)
      }
    }

    fig %>% layout(
      title=list(text="<b>Mean Spectral Reflectance Signatures</b>",
                 font=list(size=14,color=C_DARK)),
      xaxis=list(title="Wavelength (nm)", showgrid=TRUE,
                 tickvals=wl, ticktext=paste0(bnm,"\n(",wl,"nm)")),
      yaxis=list(title="Reflectance (0–1)", range=c(0, NA)),
      paper_bgcolor="white", plot_bgcolor="#f9f9f9",
      legend=list(orientation="v"),
      hovermode="x unified"
    ) %>%
      config(displaylogo=FALSE,
             toImageButtonOptions=list(format="png",filename="SpectralSignatures",
                                       width=1200,height=700,scale=2))
  })

  output$spec_band_sel <- renderUI({
    bands <- avail_bands()
    if (!length(bands)) return(tags$p("No band data available.", style="color:#888;"))
    selectInput("spec_heat_band","Band for plot heatmap:",
                choices=bands, selected=bands[1])
  })

  spec_heatmap_data <- eventReactive(input$render_spec_heat, {
    req(rv$merged, input$spec_heat_band)
    list(band=isolate(input$spec_heat_band), df=rv$merged)
  })

  output$spec_heatmap <- renderPlotly({
    req(spec_heatmap_data())
    d <- spec_heatmap_data()
    band <- d$band; df <- d$df
    if (!band %in% names(df)) return(NULL)
    vals <- as.numeric(df[[band]]); vals[!is.finite(vals)] <- NA
    id_col <- names(df)[1]

    wl <- band_wavelengths[band]
    plot_ly(x=seq_along(vals), y=vals, type="bar",
            marker=list(color=vals, colorscale="RdYlGn", showscale=TRUE,
                        colorbar=list(title=list(text="Reflectance"))),
            hovertemplate=paste0(id_col,": %{x}<br>",band," (",wl,"nm): %{y:.4f}<extra></extra>")) %>%
      layout(title=list(text=paste0("<b>",band," (", wl,"nm) — Per Plot</b>"),
                        font=list(size=13,color=C_DARK)),
             xaxis=list(title=paste("Plot",id_col)),
             yaxis=list(title="Reflectance"),
             paper_bgcolor="white") %>%
      config(displaylogo=FALSE,
             toImageButtonOptions=list(format="png",filename=paste0("SpectralHeatmap_",band)))
  })

  # ═══════════════════════════════════════════════════════════
  # REPORT GENERATOR SERVER
  # ═══════════════════════════════════════════════════════════

  rv$report_path  <- NULL
  rv$report_ready <- FALSE

  output$report_status_ui <- renderUI({
    if (isTRUE(rv$report_ready))
      tags$div(style="background:#c7f9cc;border-radius:5px;padding:8px 12px;font-size:12px;font-weight:600;color:#1b4332;",
        "✅ Report ready — click Download below.")
    else if (!is.null(rv$report_path) && !isTRUE(rv$report_ready))
      tags$div(style="background:#fff3cd;border-radius:5px;padding:8px 12px;font-size:12px;color:#856404;",
        "⏳ Generating report…")
    else NULL
  })

  # ── Helper: ggplot → base64 PNG string ──────────────────────
  gg_to_b64 <- function(gg_obj, w=8, h=5, dpi=120) {
    tryCatch({
      tmp <- tempfile(fileext=".png")
      ggplot2::ggsave(tmp, plot=gg_obj, width=w, height=h, dpi=dpi, bg="white")
      b64 <- base64enc::base64encode(tmp)
      unlink(tmp)
      paste0("data:image/png;base64,", b64)
    }, error=function(e) NULL)
  }

  # ── Helper: base plot → base64 PNG ───────────────────────────
  base_to_b64 <- function(expr, w=800, h=500) {
    tryCatch({
      tmp <- tempfile(fileext=".png")
      grDevices::png(tmp, width=w, height=h, res=100, bg="white")
      tryCatch(expr, error=function(e) NULL)
      grDevices::dev.off()
      b64 <- base64enc::base64encode(tmp)
      unlink(tmp)
      paste0("data:image/png;base64,", b64)
    }, error=function(e) { try(grDevices::dev.off(), silent=TRUE); NULL })
  }

  # ── Helper: embed image in HTML ──────────────────────────────
  img_html <- function(b64, caption="", width="100%") {
    if (is.null(b64)) return('<p style="color:#888;font-style:italic;">[Plot not available — generate it in the app first]</p>')
    paste0(
      sprintf('<div style="text-align:center;margin:16px 0;">'),
      sprintf('<img src="%s" style="max-width:%s;border-radius:6px;box-shadow:0 2px 10px rgba(0,0,0,.12);" />', b64, width),
      if(nchar(caption)>0) sprintf('<p style="font-size:11.5px;color:#666;margin-top:6px;font-style:italic;">%s</p>', caption) else "",
      '</div>'
    )
  }

  observeEvent(input$gen_report, {
    req(requireNamespace("base64enc", quietly=TRUE) || {
      tryCatch({ install.packages("base64enc", repos="https://cloud.r-project.org"); TRUE },
               error=function(e) FALSE)
    })
    suppressPackageStartupMessages(
      tryCatch(library("base64enc", character.only=TRUE, quietly=TRUE), error=function(e) NULL))

    rv$report_ready <- FALSE
    rv$report_path  <- NULL

    withProgress(message="Building full report...", value=0, {
      secs <- input$rep_sections
      df   <- rv$merged
      nc   <- if (!is.null(df)) names(df)[sapply(df, is.numeric)] else character(0)

      # ── CSS ───────────────────────────────────────────────────
      css <- '
        @import url("https://fonts.googleapis.com/css2?family=Playfair+Display:wght@700&family=Source+Sans+Pro:wght@300;400;600&display=swap");
        *{box-sizing:border-box;}
        body{font-family:"Source Sans Pro",sans-serif;max-width:1200px;margin:auto;
             padding:32px 28px;background:#f5f4f0;color:#222;line-height:1.7;}
        .cover{background:linear-gradient(135deg,#0d0d1a 0%,#1a1a2e 40%,#0f3460 70%,#8B0000 100%);
               border-radius:14px;padding:40px 44px;color:#fff;margin-bottom:32px;
               box-shadow:0 8px 40px rgba(0,0,0,0.35);}
        .cover h1{font-family:"Playfair Display",serif;font-size:30px;margin:0 0 10px;letter-spacing:0.3px;}
        .cover p{margin:5px 0;font-size:13.5px;opacity:0.85;}
        .cover a{color:#FFC72C;text-decoration:none;font-weight:700;}
        .cover .meta-grid{display:grid;grid-template-columns:1fr 1fr;gap:8px;margin-top:18px;}
        .cover .meta-item{background:rgba(255,255,255,0.1);border-radius:8px;padding:8px 14px;font-size:12.5px;}
        .cover .meta-item b{color:#FFC72C;display:block;font-size:10px;opacity:0.8;text-transform:uppercase;letter-spacing:0.5px;margin-bottom:2px;}
        h2{font-family:"Playfair Display",serif;color:#1a1a2e;border-left:5px solid #CC0000;
           padding-left:14px;margin-top:40px;font-size:20px;}
        h3{color:#0f3460;font-size:15px;margin-top:24px;font-weight:700;}
        h4{color:#2d6a4f;font-size:13.5px;margin-top:16px;font-weight:700;}
        p{margin:8px 0;font-size:13.5px;}
        .note{background:#fffde7;border-left:4px solid #FFC72C;border-radius:0 8px 8px 0;
              padding:13px 18px;margin:16px 0;font-size:13px;color:#555;}
        .note b{color:#856404;}
        .info{background:#e8f5e9;border-left:4px solid #2d6a4f;border-radius:0 8px 8px 0;
              padding:13px 18px;margin:16px 0;font-size:13px;color:#1b4332;}
        .warn{background:#fff3cd;border-left:4px solid #e6a800;border-radius:0 8px 8px 0;
              padding:13px 18px;margin:16px 0;font-size:13px;color:#856404;}
        .card{background:#fff;border-radius:10px;padding:20px 24px;
              margin:16px 0;box-shadow:0 3px 14px rgba(0,0,0,0.09);}
        .stat-badge{display:inline-block;background:#1a1a2e;color:#FFC72C;
                    border-radius:20px;padding:4px 14px;font-size:11.5px;
                    font-weight:600;margin:3px;}
        .three-col{display:grid;grid-template-columns:1fr 1fr 1fr;gap:14px;margin:16px 0;}
        .four-col{display:grid;grid-template-columns:1fr 1fr 1fr 1fr;gap:12px;margin:16px 0;}
        .two-col{display:grid;grid-template-columns:1fr 1fr;gap:16px;margin:16px 0;}
        .metric-box{background:#fff;border-radius:10px;padding:16px;
                    box-shadow:0 2px 10px rgba(0,0,0,.08);text-align:center;
                    border-top:3px solid #CC0000;}
        .metric-box .val{font-size:26px;font-weight:700;color:#CC0000;}
        .metric-box .lbl{font-size:11px;color:#888;margin-top:4px;text-transform:uppercase;letter-spacing:0.3px;}
        table{border-collapse:collapse;width:100%;font-size:13px;background:#fff;
              border-radius:8px;overflow:hidden;box-shadow:0 2px 8px rgba(0,0,0,.08);margin:12px 0;}
        th{background:#1a1a2e;color:#FFC72C;padding:10px 14px;text-align:left;font-weight:700;font-size:12px;letter-spacing:0.3px;}
        td{padding:8px 14px;border-bottom:1px solid #eee;font-size:13px;}
        tr:nth-child(even) td{background:#fafaf8;}
        tr:hover td{background:#fff8e1;}
        .good{color:#1b4332;font-weight:700;} .caution{color:#856404;font-weight:700;}
        .bad{color:#CC0000;font-weight:700;}
        .section-divider{border:none;border-top:2px solid #e0ddd8;margin:36px 0;}
        .toc{background:#fff;border-radius:10px;padding:16px 24px;box-shadow:0 2px 10px rgba(0,0,0,.07);}
        .toc a{display:inline-block;padding:5px 12px;margin:3px;color:#0f3460;
               text-decoration:none;font-size:13.5px;border-radius:6px;
               background:#f0f0f0;transition:background 0.2s;}
        .toc a:hover{background:#CC0000;color:#fff;}
        .fig-wrap{text-align:center;margin:18px 0;}
        .fig-wrap img{max-width:100%;border-radius:8px;box-shadow:0 4px 16px rgba(0,0,0,.15);}
        .fig-cap{font-size:11.5px;color:#666;margin-top:6px;font-style:italic;text-align:center;}
        .idx-desc{font-size:12px;color:#888;font-style:italic;}
        .footer{margin-top:56px;padding-top:18px;border-top:2px solid #ddd;
                font-size:11.5px;color:#999;text-align:center;}
        @media print{.cover,.metric-box,.card{-webkit-print-color-adjust:exact;print-color-adjust:exact;}}
      '

      lines <- c(
        '<!DOCTYPE html><html lang="en"><head>',
        '<meta charset="UTF-8"><meta name="viewport" content="width=device-width,initial-scale=1">',
        sprintf('<title>%s</title>', htmltools::htmlEscape(input$rep_title)),
        sprintf('<style>%s</style>', css),
        '</head><body>'
      )

      # ── COVER PAGE ────────────────────────────────────────────
      incProgress(0.03, "Building cover page...")
      has_raster <- !is.null(rv$mosaic)
      has_grid   <- !is.null(rv$shape)
      has_idx    <- !is.null(rv$idx_ras)
      has_data   <- !is.null(df)
      n_plots    <- if (has_grid) nrow(rv$shape) else 0
      n_idx      <- if (has_idx)  length(names(rv$idx_ras)) else 0
      n_rows_dat <- if (has_data) nrow(df) else 0
      exp_txt    <- if (nchar(trimws(input$rep_exp)) > 0) input$rep_exp else "Field Trial Analysis"
      cam_txt    <- tryCatch(if (input$camera_type=="rgb") "RGB (3-band)" else "Multispectral (5-band)", error=function(e)"—")

      lines <- c(lines,
        '<div class="cover">',
        sprintf('<h1>%s</h1>', htmltools::htmlEscape(input$rep_title)),
        '<p style="font-size:15px;opacity:0.9;margin-bottom:4px;">',
        'Drone Image Analysis Report &mdash; High-Throughput Plant Phenotyping</p>',
        '<div class="meta-grid">',
        sprintf('<div class="meta-item"><b>Experiment</b>%s</div>', htmltools::htmlEscape(exp_txt)),
        sprintf('<div class="meta-item"><b>Author / PI</b>%s</div>', htmltools::htmlEscape(input$rep_author)),
        sprintf('<div class="meta-item"><b>Generated</b>%s</div>', format(Sys.time(),"%B %d, %Y at %H:%M")),
        sprintf('<div class="meta-item"><b>Camera Type</b>%s</div>', cam_txt),
        sprintf('<div class="meta-item"><b>Total Plots</b>%s</div>', if(n_plots>0) as.character(n_plots) else "—"),
        sprintf('<div class="meta-item"><b>Indices Calculated</b>%s</div>', if(n_idx>0) as.character(n_idx) else "—"),
        '</div>',
        '<p style="margin-top:18px;font-size:12px;opacity:0.7;">',
        '<a href="https://www.uogbeans.com">www.uogbeans.com</a>',
        ' &nbsp;&middot;&nbsp; Dry Bean Breeding &amp; Computational Biology',
        ' &nbsp;&middot;&nbsp; University of Guelph, Canada</p>',
        '</div>',
        # Key metric strip
        '<div class="four-col">',
        sprintf('<div class="metric-box"><div class="val">%s</div><div class="lbl">Plots Analysed</div></div>',
          if(n_plots>0) n_plots else "—"),
        sprintf('<div class="metric-box"><div class="val">%s</div><div class="lbl">Indices Calculated</div></div>',
          if(n_idx>0) n_idx else "—"),
        sprintf('<div class="metric-box"><div class="val">%s</div><div class="lbl">Data Records</div></div>',
          if(has_data) nrow(df) else "—"),
        sprintf('<div class="metric-box"><div class="val">%s</div><div class="lbl">Bands</div></div>',
          if(has_raster) terra::nlyr(rv$mosaic) else "—"),
        '</div>'
      )

      # ── TABLE OF CONTENTS ────────────────────────────────────
      sec_map <- c(
        summary="1. Executive Summary", mosaic="2. Drone Mosaic & Field Layout",
        overview="3. Data Overview", maps="4. Field Heatmaps",
        dists="5. Index Distributions", stats="6. Summary Statistics",
        corr="7. Correlation Analysis", pca="8. PCA & Variance",
        rankings="9. Genotype Rankings", ml="10. Machine Learning"
      )
      lines <- c(lines, '<h2 style="margin-top:28px;">Contents</h2>',
        '<div class="toc">',
        sapply(intersect(names(sec_map), secs), function(s)
          sprintf('<a href="#sec_%s">%s</a>', s, sec_map[s])),
        '</div>')

      # ════════════════════════════════════════════════════════
      # 1. EXECUTIVE SUMMARY
      # ════════════════════════════════════════════════════════
      if ("summary" %in% secs) {
        incProgress(0.05, "Executive summary...")
        lines <- c(lines,
          '<hr class="section-divider">',
          '<h2 id="sec_summary">1. Executive Summary</h2>',
          '<div class="info">',
          '<b>About this report:</b> This document presents results from drone-based high-throughput ',
          'phenotyping of a dry bean field trial, conducted using the <b>AllInOne Phenomics</b> platform ',
          'developed by the Dry Bean Breeding &amp; Computational Biology Program, University of Guelph. ',
          'The platform processes RGB and multispectral drone imagery to extract vegetation indices, ',
          'generate spatial field heatmaps, perform statistical analysis, rank genotypes, and apply ',
          'machine learning models for trait prediction.</div>',

          '<div class="card"><h3>Pipeline Status</h3>',
          '<table><tr><th>Component</th><th>Status</th><th>Details</th><th>Notes</th></tr>',
          sprintf('<tr><td><b>Drone Mosaic</b></td><td class="%s">%s</td><td>%s</td><td>%s</td></tr>',
            if(has_raster)"good" else "bad",
            if(has_raster)"✅ Loaded" else "❌ Not loaded",
            if(has_raster) paste0(terra::nlyr(rv$mosaic)," bands · ",
              format(nrow(rv$mosaic)*ncol(rv$mosaic),big.mark=",")," total pixels") else "—",
            if(has_raster) tryCatch(
              paste0("CRS: ", terra::crs(rv$mosaic,describe=TRUE)$name[1]),
              error=function(e)"CRS unknown") else "Upload .tif in Data Upload tab"),
          sprintf('<tr><td><b>Camera Type</b></td><td class="good">%s</td><td>Band scaling applied</td><td>Reflectance values 0–1 required for index accuracy</td></tr>', cam_txt),
          sprintf('<tr><td><b>Plot Grid</b></td><td class="%s">%s</td><td>%s</td><td>%s</td></tr>',
            if(has_grid)"good" else "caution",
            if(has_grid)"✅ Defined" else "⚠️ Not set",
            if(has_grid) paste0(n_plots," plots") else "—",
            if(has_grid) paste0(tryCatch(input$nrows,error=function(e)"?")," rows × ",
              tryCatch(input$ncols,error=function(e)"?")," cols") else "Generate grid in Plot Grid tab"),
          sprintf('<tr><td><b>Vegetation Indices</b></td><td class="%s">%s</td><td>%s</td><td>%s</td></tr>',
            if(has_idx)"good" else "bad",
            if(has_idx)"✅ Calculated" else "❌ Not calculated",
            if(has_idx) paste0(n_idx," indices calculated") else "—",
            if(has_idx) paste(head(names(rv$idx_ras),5),collapse=", ") else "Use Vegetation Indices tab"),
          sprintf('<tr><td><b>Merged Dataset</b></td><td class="%s">%s</td><td>%s</td><td>%s</td></tr>',
            if(has_data)"good" else "bad",
            if(has_data)"✅ Ready" else "❌ Not available",
            if(has_data) paste0(nrow(df)," rows × ",ncol(df)," columns") else "—",
            if(has_data) {
              meta_c <- names(df)[!names(df) %in% c(nc, "PlotID")]
              if(length(meta_c)>0) paste("Metadata:",paste(head(meta_c,3),collapse=", ")) else "Index data only"
            } else "Load data and calculate indices"),
          '</table></div>',
          '<div class="note"><b>📝 Methodology note:</b> Vegetation indices are calculated on ',
          'reflectance-scaled band values (range 0–1). Band scaling is auto-detected from the ',
          'maximum pixel value: ÷255 for 8-bit RGB cameras, ÷10,000 for MicaSense/ENVI format, ',
          'or ÷65,535 for 16-bit sensors. Always verify this matches your sensor specification. ',
          'Plot-level values represent the mean of all pixels within each plot polygon.</div>'
        )
      }

      # ════════════════════════════════════════════════════════
      # 2. DRONE MOSAIC & FIELD LAYOUT (with images!)
      # ════════════════════════════════════════════════════════
      if ("mosaic" %in% secs) {
        incProgress(0.07, "Rendering field images...")
        lines <- c(lines,
          '<hr class="section-divider">',
          '<h2 id="sec_mosaic">2. Drone Mosaic &amp; Field Layout</h2>',
          '<p>The drone mosaic is the georeferenced orthophoto produced from drone imagery. ',
          'It serves as the spatial foundation for all analyses. Each plot polygon is overlaid ',
          'on the mosaic to define the area from which index values are extracted.</p>'
        )

        # Original mosaic image
        if (has_raster) {
          b64_mosaic <- tryCatch({
            base_to_b64({
              r  <- rv$mosaic
              b  <- get_bands()
              if (terra::nlyr(r) >= 3) {
                terra::plotRGB(r, r=b$r, g=b$g, b=b$b,
                               main="Drone Mosaic — Original Image", stretch="lin",
                               mar=c(2,2,2,2))
              } else {
                terra::plot(r[[1]], main="Drone Mosaic (Band 1)",
                            col=viridis::viridis(256), mar=c(2,2,2,2))
              }
            }, w=1100, h=700)
          }, error=function(e) NULL)
          lines <- c(lines,
            '<h3>Original Drone Mosaic</h3>',
            img_html(b64_mosaic,
              paste0("Figure 2.1: Drone mosaic orthophoto. ",
                     terra::nlyr(rv$mosaic),"-band image · ",
                     format(nrow(rv$mosaic)*ncol(rv$mosaic), big.mark=","), " pixels. ",
                     "RGB composite displayed. Spatial coordinate reference: ",
                     tryCatch(terra::crs(rv$mosaic,describe=TRUE)$name[1],error=function(e)"unknown")))
          )
        }

        # Mosaic with grid overlay
        if (has_raster && has_grid) {
          b64_grid <- tryCatch({
            base_to_b64({
              r   <- rv$mosaic
              b   <- get_bands()
              shp <- rv$shape
              if (terra::nlyr(r) >= 3) {
                terra::plotRGB(r, r=b$r, g=b$g, b=b$b,
                               main=paste0("Field Layout — ", n_plots, " Plots  |  Plot 1 = top-left, left→right, top→bottom"),
                               stretch="lin", mar=c(2,2,3,2))
              } else {
                terra::plot(r[[1]], main=paste0("Field Layout — ",n_plots," Plots"),
                            col=viridis::viridis(256), mar=c(2,2,3,2))
              }
              # Draw plot grid
              terra::plot(shp, add=TRUE, border="#FFC72C", lwd=0.7, col=NA)
              # Label plots (limit to 300 for performance)
              n_p  <- nrow(shp)
              lidx <- if(n_p<=300) seq_len(n_p) else c(1, seq(5,n_p,by=max(1,floor(n_p/60))),n_p)
              cents <- tryCatch(terra::centroids(shp[lidx,]), error=function(e) NULL)
              if (!is.null(cents)) {
                cx <- terra::crds(cents)[,1]; cy <- terra::crds(cents)[,2]
                ids <- if("PlotID" %in% names(shp)) shp$PlotID[lidx] else lidx
                cex_l <- if(n_p>200) 0.4 else if(n_p>100) 0.55 else 0.7
                text(cx, cy, labels=ids, col="white",  cex=cex_l+0.1, font=2)
                text(cx, cy, labels=ids, col="#FFC72C", cex=cex_l,    font=2)
              }
              # Highlight plot 1
              p1 <- if("PlotID" %in% names(shp)) which(shp$PlotID==1) else 1
              if (length(p1)) {
                c1 <- tryCatch(terra::crds(terra::centroids(shp[p1,])),error=function(e)NULL)
                if (!is.null(c1)) points(c1[1],c1[2],pch=21,bg="#CC0000",col="white",cex=2.5,lwd=2)
              }
              legend("bottomright", inset=0.01,
                legend=c("Plot #1","Grid lines"),
                pch=c(21,NA), lty=c(NA,1),
                pt.bg=c("#CC0000",NA), col=c("white","#FFC72C"),
                pt.cex=c(1.4,NA), lwd=c(NA,1.5),
                bg="rgba(0,0,0,0.5)", text.col="white", cex=0.75, box.col=NA)
            }, w=1100, h=700)
          }, error=function(e) NULL)
          lines <- c(lines,
            '<h3>Field Grid Overlay</h3>',
            '<p>The plot grid divides the field mosaic into individual experimental plots. ',
            'Each numbered cell corresponds to one plot. Values are extracted by computing ',
            'the mean pixel value within each polygon.</p>',
            img_html(b64_grid,
              paste0("Figure 2.2: Drone mosaic with plot grid overlay. ",
                     n_plots, " plots defined. Gold grid lines = plot boundaries. ",
                     "Red dot = Plot #1 (numbering starts top-left, proceeds left→right then top→bottom)."))
          )

          # Grid statistics
          lines <- c(lines,
            '<div class="card"><h4>Plot Grid Information</h4>',
            '<div class="two-col">',
            '<div>',
            sprintf('<p><b>Total plots:</b> %d</p>', n_plots),
            sprintf('<p><b>Rows × Columns:</b> %s × %s</p>',
              tryCatch(input$nrows,error=function(e)"?"),
              tryCatch(input$ncols,error=function(e)"?")),
            sprintf('<p><b>Numbering:</b> Left to right, top to bottom (Plot 1 = top-left)</p>'),
            '</div><div>',
            sprintf('<p><b>Coordinate system:</b> %s</p>',
              tryCatch(terra::crs(rv$mosaic,describe=TRUE)$name[1],error=function(e)"unknown")),
            sprintf('<p><b>Mosaic resolution:</b> %s × %s pixels</p>',
              nrow(rv$mosaic), ncol(rv$mosaic)),
            sprintf('<p><b>Approx. pixels per plot:</b> %s</p>',
              format(round(nrow(rv$mosaic)*ncol(rv$mosaic)/max(n_plots,1)),big.mark=",")),
            '</div></div></div>'
          )
        }

        # NDVI or first index raster view
        if (has_idx) {
          first_idx <- names(rv$idx_ras)[1]
          b64_idx_r <- tryCatch({
            base_to_b64({
              lyr <- rv$idx_ras[[first_idx]]
              terra::plot(lyr, main=paste("Vegetation Index Raster:", first_idx),
                          col=RColorBrewer::brewer.pal(11,"RdYlGn"), mar=c(2,2,3,2))
              if (has_grid) terra::plot(rv$shape, add=TRUE, border="white", lwd=0.4, col=NA)
            }, w=900, h=600)
          }, error=function(e) NULL)
          lines <- c(lines,
            sprintf('<h3>Vegetation Index Raster — %s</h3>', first_idx),
            '<p>The index raster shows the spatial distribution of the first calculated index ',
            'at full pixel resolution. White lines show the plot grid boundaries.</p>',
            img_html(b64_idx_r,
              paste0("Figure 2.3: ", first_idx, " raster with plot grid overlay. ",
                     "Red = low values, Green = high values (RdYlGn colour scale)."))
          )
        }
      }

      # ════════════════════════════════════════════════════════
      # 3. DATA OVERVIEW
      # ════════════════════════════════════════════════════════
      if ("overview" %in% secs && has_data) {
        incProgress(0.04, "Data overview...")
        meta_cols <- names(df)[!names(df) %in% nc]
        lines <- c(lines,
          '<hr class="section-divider">',
          '<h2 id="sec_overview">3. Data Overview</h2>',
          sprintf('<p>The merged dataset contains <b>%d plots</b>, <b>%d numeric indices/bands</b>, ',
            nrow(df), length(nc)),
          sprintf('and <b>%d metadata columns</b>. Each row represents one experimental plot.</p>', length(meta_cols)),

          '<div class="card"><h4>Numeric indices available:</h4><p>',
          paste(sapply(nc, function(col) sprintf('<span class="stat-badge">%s</span>', col)), collapse=" "),
          '</p></div>',

          if (length(meta_cols)>0) paste0(
            '<div class="card"><h4>Metadata columns:</h4>',
            '<table><tr><th>Column</th><th>Type</th><th>Unique Values</th><th>Example Values</th></tr>',
            paste(sapply(meta_cols, function(col) {
              x    <- df[[col]]
              nu   <- length(unique(x[!is.na(x)]))
              ex   <- paste(head(unique(x[!is.na(x)]),3), collapse=", ")
              typ  <- if(is.numeric(x)) "Numeric" else "Character"
              sprintf('<tr><td><b>%s</b></td><td>%s</td><td>%d</td><td>%s</td></tr>',col,typ,nu,ex)
            }), collapse=""),
            '</table></div>') else "",

          '<div class="note"><b>📝 Data interpretation:</b>',
          '<ul style="margin:6px 0 0 16px;font-size:12.5px;">',
          '<li><b>CV% &lt; 10%</b>: Low variability — index is stable across plots (possibly low discrimination power)</li>',
          '<li><b>CV% 10–30%</b>: Moderate variability — typical range for vegetation indices in field trials</li>',
          '<li><b>CV% &gt; 30%</b>: High variability — inspect for soil contamination, shadows, or sensor artefacts</li>',
          '<li>Normalised indices (NDVI, NDRE, GNDVI) should be in range −1 to +1; values outside this range indicate scaling errors</li>',
          '<li>RGB-derived indices (NGRDI, BGI, etc.) typically range from −1 to +1 depending on the formula</li>',
          '</ul></div>'
        )
      }

      # ════════════════════════════════════════════════════════
      # 4. FIELD HEATMAPS (per-plot)
      # ════════════════════════════════════════════════════════
      if ("maps" %in% secs && has_idx && has_grid) {
        incProgress(0.08, "Rendering field heatmaps...")
        lines <- c(lines,
          '<hr class="section-divider">',
          '<h2 id="sec_maps">4. Field Heatmaps</h2>',
          '<p>Field heatmaps show the spatial distribution of each vegetation index across all plots. ',
          'Each polygon is coloured by its mean index value within the plot boundary. ',
          'Spatial patterns can reveal gradients caused by irrigation, soil variation, disease pressure, ',
          'or genuine genotypic differences. Comparing multiple indices helps distinguish biological ',
          'signal from spatial artefacts.</p>',
          '<div class="note"><b>📝 Interpretation:</b> Spatial clusters of high or low values that persist ',
          'across multiple indices suggest genuine field gradients (soil, water, sunlight). ',
          'Patterns that appear in only one index may reflect that index\'s sensitivity to a specific ',
          'trait (e.g., NDRE responds strongly to chlorophyll content). Random scatter indicates ',
          'high within-trial variability — desirable for detecting genotypic differences.</div>'
        )

        idx_names <- names(rv$idx_ras)
        show_idx  <- head(idx_names, 8)

        for (i_nm in show_idx) {
          incProgress(0.01, paste("Heatmap:", i_nm))
          b64 <- tryCatch({
            if (!is.null(rv$extracted) && i_nm %in% names(rv$extracted)) {
              shp_v <- if(!inherits(rv$shape,"SpatVector")) terra::vect(rv$shape) else rv$shape
              vals  <- rv$extracted[[i_nm]]
              shp_v$val <- vals
              df_sf <- tryCatch(sf::st_as_sf(shp_v), error=function(e) NULL)
              req(df_sf)
              gg <- ggplot(df_sf) +
                geom_sf(aes(fill=val), color="white", linewidth=0.15) +
                scale_fill_gradientn(colors=RColorBrewer::brewer.pal(11,"RdYlGn"),
                                     na.value="grey85", name=i_nm) +
                labs(title=paste("Field Heatmap:", i_nm),
                     subtitle=sprintf("Mean per plot | n=%d plots | range: [%.4f, %.4f]",
                       sum(!is.na(vals)), min(vals,na.rm=TRUE), max(vals,na.rm=TRUE))) +
                theme_minimal() +
                theme(plot.title=element_text(face="bold",size=14,color="#1a1a2e"),
                      plot.subtitle=element_text(size=10,color="#555"),
                      axis.text=element_text(size=7), legend.position="right")
              gg_to_b64(gg, w=10, h=6.5, dpi=110)
            } else NULL
          }, error=function(e) NULL)
          lines <- c(lines,
            sprintf('<h3>%s — Field Heatmap</h3>', i_nm),
            img_html(b64,
              paste0("Figure 4.", which(show_idx==i_nm), ": ", i_nm,
                     " spatial distribution. Red = low, Yellow = medium, Green = high values. ",
                     "White outlines = individual plot boundaries."))
          )
        }
        if (length(idx_names)>8)
          lines <- c(lines, sprintf('<div class="note">Showing first 8 of %d indices. ',
            length(idx_names)), 'Use the Field Maps tab for all indices interactively.</div>')
      }

      # ════════════════════════════════════════════════════════
      # 5. INDEX DISTRIBUTIONS
      # ════════════════════════════════════════════════════════
      if ("dists" %in% secs && has_data && length(nc)>0) {
        incProgress(0.07, "Distribution plots...")
        idx_nc <- if (!is.null(rv$idx_ras)) nc[nc %in% names(rv$idx_ras)] else nc
        lines <- c(lines,
          '<hr class="section-divider">',
          '<h2 id="sec_dists">5. Index Distributions</h2>',
          '<p>Distribution plots show how each vegetation index is distributed across all plots. ',
          'The violin shape shows the probability density at each value — wide sections indicate ',
          'many plots with those values. The inner box shows the interquartile range (Q1–Q3), ',
          'the horizontal line marks the median, and red dots mark outlier plots (IQR × 1.5 rule).</p>',
          '<p>Wide, symmetric distributions are ideal for genotype discrimination. ',
          'Narrow distributions indicate low variability (the index may not be useful for ranking). ',
          'Skewed distributions may indicate sensor artefacts or non-normal trait distributions.</p>'
        )

        # Combined violin plot for all indices
        show_nc <- head(idx_nc, 10)
        b64_v <- tryCatch({
          df_long <- tidyr::pivot_longer(df[,show_nc,drop=FALSE],
                                          cols=tidyr::everything(),
                                          names_to="Index", values_to="Value")
          df_long$Value[!is.finite(df_long$Value)] <- NA
          gg <- ggplot(df_long[!is.na(df_long$Value),],
                       aes(x=reorder(Index,Value,FUN=median,na.rm=TRUE), y=Value)) +
            geom_violin(fill="#CC0000", alpha=0.50, trim=FALSE) +
            geom_boxplot(width=0.1, fill="white", outlier.shape=16,
                         outlier.color="#CC0000", outlier.size=1.8, linewidth=0.6) +
            coord_flip() +
            labs(title="Vegetation Index Distributions — All Plots",
                 subtitle="Ordered by median value. Red dots = outlier plots (IQR × 1.5)",
                 x=NULL, y="Index Value") +
            theme_minimal(base_size=12) +
            theme(plot.title=element_text(face="bold",color="#1a1a2e",size=13),
                  plot.subtitle=element_text(size=10,color="#666"),
                  axis.text.y=element_text(size=10,face="bold"),
                  panel.grid.minor=element_blank())
          gg_to_b64(gg, w=11, h=max(5,length(show_nc)*0.75), dpi=110)
        }, error=function(e) NULL)
        lines <- c(lines,
          img_html(b64_v,
            "Figure 5.1: Violin + box plots for all calculated indices. Sorted by median. Outlier plots shown in red."))

        # Per-index histogram with normal curve overlay
        if (length(idx_nc) >= 2) {
          b64_hist <- tryCatch({
            show4 <- head(idx_nc, 4)
            df_l  <- tidyr::pivot_longer(df[,show4,drop=FALSE],
                                          cols=tidyr::everything(),
                                          names_to="Index", values_to="Value")
            df_l$Value[!is.finite(df_l$Value)] <- NA
            gg <- ggplot(df_l[!is.na(df_l$Value),], aes(x=Value)) +
              geom_histogram(aes(y=after_stat(density)),
                             bins=25, fill="#CC0000", alpha=0.65, color="white") +
              geom_density(color="#1a1a2e", linewidth=1.1, alpha=0) +
              geom_vline(aes(xintercept=Value), stat="summary", fun=mean,
                         linetype="dashed", color="#0f3460", linewidth=0.9) +
              facet_wrap(~Index, scales="free", ncol=2) +
              labs(title="Index Histograms with Density Curves",
                   subtitle="Dashed line = mean. Curve = kernel density estimate.",
                   x="Value", y="Density") +
              theme_minimal(base_size=11) +
              theme(plot.title=element_text(face="bold",color="#1a1a2e"),
                    strip.text=element_text(face="bold",size=11))
            gg_to_b64(gg, w=10, h=7, dpi=110)
          }, error=function(e) NULL)
          lines <- c(lines,
            img_html(b64_hist,
              "Figure 5.2: Histograms for top 4 indices. Bars = frequency density. Curve = smoothed distribution. Dashed = mean."))
        }
      }

      # ════════════════════════════════════════════════════════
      # 6. SUMMARY STATISTICS TABLE
      # ════════════════════════════════════════════════════════
      if ("stats" %in% secs && has_data && length(nc)>0) {
        incProgress(0.05, "Statistics table...")
        idx_nc <- if (!is.null(rv$idx_ras)) nc[nc %in% names(rv$idx_ras)] else nc
        lines <- c(lines,
          '<hr class="section-divider">',
          '<h2 id="sec_stats">6. Summary Statistics</h2>',
          '<p>Descriptive statistics for each vegetation index across all ', as.character(nrow(df)), ' plots. ',
          'The Coefficient of Variation (CV%) measures relative variability — higher values indicate ',
          'greater spread across plots, which generally means better discriminating power for genotype selection. ',
          'Skewness measures asymmetry: positive skew (right tail) may indicate a few plots with unusually high values.</p>',

          '<div class="card"><table>',
          '<tr><th>Index</th><th>N</th><th>Mean</th><th>SD</th><th>Min</th>',
          '<th>Q1</th><th>Median</th><th>Q3</th><th>Max</th><th>CV%</th><th>Skewness</th></tr>'
        )
        for (col in idx_nc) {
          x  <- df[[col]]; x[!is.finite(x)] <- NA
          n  <- sum(!is.na(x))
          if (n < 2) next
          mn <- mean(x,na.rm=T); s <- sd(x,na.rm=T)
          q1 <- quantile(x,.25,na.rm=T); q3 <- quantile(x,.75,na.rm=T)
          cv <- round(s/abs(mn)*100,1)
          sk <- if(n>3) round((sum((x[!is.na(x)]-mn)^3,na.rm=T)/n)/s^3,3) else NA
          cls <- if(!is.finite(cv))"" else if(cv<10)"good" else if(cv<30)"caution" else "bad"
          lines <- c(lines, sprintf(
            '<tr><td><b>%s</b></td><td>%d</td><td>%.4f</td><td>%.4f</td><td>%.4f</td><td>%.4f</td><td>%.4f</td><td>%.4f</td><td>%.4f</td><td class="%s">%.1f</td><td>%s</td></tr>',
            col, n, mn, s, min(x,na.rm=T), q1, median(x,na.rm=T), q3, max(x,na.rm=T),
            cls, cv, if(is.na(sk)) "—" else sk))
        }
        lines <- c(lines, '</table></div>',
          '<div class="note">',
          '<b>Column guide:</b> N = valid plot count | SD = standard deviation | ',
          'Q1/Q3 = first/third quartile | CV% = coefficient of variation (SD/Mean×100) | ',
          'Skewness: 0 = symmetric, &gt;1 = right-skewed, &lt;−1 = left-skewed.<br>',
          '<b>Colour coding:</b> <span class="good">Green CV%</span> = low variability (&lt;10%) | ',
          '<span class="caution">Orange</span> = moderate (10–30%) | ',
          '<span class="bad">Red</span> = high variability (&gt;30%)</div>'
        )
      }

      # ════════════════════════════════════════════════════════
      # 7. CORRELATION ANALYSIS
      # ════════════════════════════════════════════════════════
      if ("corr" %in% secs && has_data && length(nc)>=2) {
        incProgress(0.07, "Correlation analysis...")
        idx_nc <- if (!is.null(rv$idx_ras)) nc[nc %in% names(rv$idx_ras)] else nc
        lines <- c(lines,
          '<hr class="section-divider">',
          '<h2 id="sec_corr">7. Correlation Analysis</h2>',
          '<p>The Pearson correlation matrix shows pairwise linear relationships between all indices. ',
          'Values range from −1 (perfect negative) to +1 (perfect positive). ',
          'Strong correlations (|r| &gt; 0.8) indicate <b>redundant indices</b> — selecting just one ',
          'of a highly correlated pair is sufficient for analysis and reduces multicollinearity in models. ',
          'Moderate correlations (0.4–0.8) suggest related but distinct traits. ',
          'Near-zero correlations indicate independent information.</p>'
        )
        b64_cm <- tryCatch({
          nd <- df[, idx_nc, drop=FALSE]
          nd <- nd[, sapply(nd, function(x) sum(is.finite(x))>5), drop=FALSE]
          if (ncol(nd) < 2) stop("too few")
          cm <- cor(nd, use="pairwise.complete.obs")
          gg <- ggcorrplot::ggcorrplot(cm, method="square", type="lower",
            lab=TRUE, lab_size=2.8,
            colors=c("#0f3460","white","#CC0000"),
            outline.col="white",
            title="Pearson Correlation Matrix",
            ggtheme=theme_minimal()) +
            theme(plot.title=element_text(face="bold",size=13,color="#1a1a2e"),
                  axis.text.x=element_text(size=8,angle=45,hjust=1),
                  axis.text.y=element_text(size=8))
          gg_to_b64(gg, w=max(8,ncol(nd)*0.75), h=max(7,ncol(nd)*0.7), dpi=110)
        }, error=function(e) NULL)
        lines <- c(lines,
          img_html(b64_cm,
            "Figure 7.1: Pearson correlation matrix. Blue = negative, Red = positive. Number in each cell = r value."))

        # Top correlations table
        tryCatch({
          nd2 <- df[,idx_nc,drop=FALSE]; nd2 <- nd2[,sapply(nd2,function(x) sum(is.finite(x))>5),drop=FALSE]
          if (ncol(nd2)>=2) {
            cm2 <- cor(nd2,use="pairwise.complete.obs"); ut <- upper.tri(cm2)
            df_c <- data.frame(Index1=rownames(cm2)[row(cm2)[ut]],
                               Index2=colnames(cm2)[col(cm2)[ut]], r=round(cm2[ut],4))
            df_c <- df_c[order(abs(df_c$r),decreasing=TRUE),]
            top15 <- head(df_c,15)
            lines <- c(lines,'<h3>Top 15 Strongest Correlations</h3>',
              '<div class="card"><table>',
              '<tr><th>Index 1</th><th>Index 2</th><th>r</th><th>|r|</th><th>Strength</th><th>Implication</th></tr>')
            for(i in seq_len(nrow(top15))) {
              rv2 <- top15$r[i]; ar2 <- abs(rv2)
              strength <- if(ar2>0.9)"Very strong" else if(ar2>0.7)"Strong" else "Moderate"
              impl <- if(ar2>0.85) "Consider using only one" else "Both may be informative"
              cls2 <- if(rv2>0)"good" else "bad"
              lines <- c(lines, sprintf('<tr><td>%s</td><td>%s</td><td class="%s">%.4f</td><td>%.4f</td><td>%s</td><td>%s</td></tr>',
                top15$Index1[i],top15$Index2[i],cls2,rv2,ar2,strength,impl))
            }
            lines <- c(lines,'</table></div>')
          }
        }, error=function(e) NULL)
      }

      # ════════════════════════════════════════════════════════
      # 8. PCA
      # ════════════════════════════════════════════════════════
      if ("pca" %in% secs) {
        incProgress(0.06, "PCA plots...")
        if (!is.null(rv$pca_obj)) {
          pca  <- rv$pca_obj
          imp  <- summary(pca)$importance
          pct  <- round(imp[2,]*100,1); cpct <- round(imp[3,]*100,1)
          n_pc <- min(length(pct),10)
          lines <- c(lines,
            '<hr class="section-divider">',
            '<h2 id="sec_pca">8. PCA &amp; Variance Analysis</h2>',
            sprintf('<p>Principal Component Analysis (PCA) was performed on <b>%d variables</b>. ',nrow(pca$rotation)),
            sprintf('The first two components together explain <b>%.1f%%</b> of the total variance ',pct[1]+pct[2]),
            sprintf('(PC1: %.1f%%, PC2: %.1f%%). ',pct[1],pct[2]),
            'PCA reduces correlated indices into uncorrelated principal components, ',
            'enabling visualisation of overall trial variation and identifying which ',
            'indices drive the most variability across plots.</p>'
          )
          # Scree plot
          b64_sc <- tryCatch({
            df_sc <- data.frame(PC=paste0("PC",1:n_pc), Var=pct[1:n_pc], Cum=cpct[1:n_pc])
            df_sc$PC <- factor(df_sc$PC, levels=df_sc$PC)
            gg <- ggplot(df_sc, aes(x=PC)) +
              geom_col(aes(y=Var), fill="#CC0000", alpha=0.85, width=0.65) +
              geom_line(aes(y=Cum, group=1), color="#FFC72C", linewidth=1.4) +
              geom_point(aes(y=Cum), color="#FFC72C", size=4) +
              geom_text(aes(y=Var, label=paste0(Var,"%")), vjust=-0.4, size=3.2, fontface="bold") +
              geom_hline(yintercept=80, linetype="dashed", color="#2d6a4f", linewidth=0.9) +
              annotate("text", x=n_pc*0.9, y=82, label="80% threshold", color="#2d6a4f", size=3) +
              labs(title="Scree Plot — Variance Explained per Principal Component",
                   subtitle="Bars = individual variance %; Line = cumulative %",
                   x="Principal Component", y="% Variance Explained") +
              theme_minimal(base_size=12) +
              theme(plot.title=element_text(face="bold",color="#1a1a2e"))
            gg_to_b64(gg, w=9, h=5.5, dpi=110)
          }, error=function(e) NULL)
          lines <- c(lines,
            img_html(b64_sc,
              paste0("Figure 8.1: Scree plot. Bars = variance explained per PC. ",
                     "Yellow line = cumulative variance. Green dashed = 80% threshold. ",
                     sprintf("PC1+PC2 = %.1f%% of total variance.", pct[1]+pct[2]))))
          # PC1 loadings
          b64_ld <- tryCatch({
            df_ld <- data.frame(Var=rownames(pca$rotation), PC1=pca$rotation[,1], PC2=pca$rotation[,min(2,ncol(pca$rotation))])
            df_ld <- df_ld[order(df_ld$PC1),]
            gg <- ggplot(df_ld, aes(x=reorder(Var,PC1), y=PC1, fill=PC1)) +
              geom_col(color="white", linewidth=0.25) +
              scale_fill_gradient2(low="#0f3460",mid="white",high="#CC0000",midpoint=0) +
              coord_flip() +
              labs(title="PC1 Variable Loadings",
                   subtitle="Positive (red) = increases with PC1 | Negative (blue) = decreases with PC1",
                   x=NULL, y="Loading") +
              theme_minimal(base_size=12) +
              theme(plot.title=element_text(face="bold",color="#1a1a2e"), legend.position="none")
            gg_to_b64(gg, w=9, h=max(4.5,nrow(df_ld)*0.42), dpi=110)
          }, error=function(e) NULL)
          lines <- c(lines,
            img_html(b64_ld,
              "Figure 8.2: PC1 loadings. Indices with large absolute loadings are the primary drivers of variation across plots."),
            '<div class="note"><b>📝 PCA interpretation:</b> If NDVI, NDRE, and canopy-related ',
            'indices load strongly on PC1, that component likely represents overall plant vigour. ',
            'PC2 often captures a secondary dimension such as stress response or canopy architecture. ',
            'Plots scoring high on both PC1 and PC2 are likely the most vigorous and resilient genotypes.</div>'
          )
          # PC loadings table
          df_ld_t <- as.data.frame(round(pca$rotation[,1:min(5,ncol(pca$rotation))],4))
          lines <- c(lines,
            '<h3>PC Loadings Table (top 5 PCs)</h3>',
            '<div class="card"><table>',
            paste0('<tr><th>Index</th>',
              paste(sprintf('<th>PC%d (%.1f%%)</th>',1:ncol(df_ld_t),pct[1:ncol(df_ld_t)]),collapse=""),
              '</tr>'),
            paste(sapply(seq_len(nrow(df_ld_t)), function(i) {
              paste0('<tr><td><b>',rownames(df_ld_t)[i],'</b></td>',
                paste(sapply(df_ld_t[i,], function(v) {
                  cls3 <- if(v>0.3)"good" else if(v< -0.3)"bad" else ""
                  sprintf('<td class="%s">%.4f</td>',cls3,v)
                }),collapse=""),'</tr>')
            }),collapse=""),
            '</table></div>'
          )
        } else {
          lines <- c(lines,
            '<hr class="section-divider">',
            '<h2 id="sec_pca">8. PCA</h2>',
            '<div class="warn">⚠️ PCA not run yet. Go to the PCA &amp; Clustering tab and click "Run PCA".</div>')
        }
      }

      # ════════════════════════════════════════════════════════
      # 9. GENOTYPE RANKINGS
      # ════════════════════════════════════════════════════════
      if ("rankings" %in% secs && has_data && length(nc)>0) {
        incProgress(0.06, "Genotype rankings...")
        idx_nc <- if (!is.null(rv$idx_ras)) nc[nc %in% names(rv$idx_ras)] else nc
        lines <- c(lines,
          '<hr class="section-divider">',
          '<h2 id="sec_rankings">9. Genotype Rankings</h2>',
          '<p>Plots are ranked by their vegetation index values. Higher NDVI, NDRE, GNDVI, and ',
          'canopy-cover-related indices generally indicate better-performing genotypes under ',
          'the measured conditions. Rankings should always be interpreted in the context of ',
          'the experimental design — replication structure, blocking, and environmental gradients ',
          'should be accounted for before making selection decisions.</p>',
          '<div class="info"><b>Selection guidance:</b> Elite plots (top quartile) are candidates ',
          'for advancement. Consider multiple indices and repeated measurements before final selection. ',
          'Plots ranking consistently high across several independent indices are the most reliable candidates.</div>'
        )

        for (rank_idx in head(idx_nc, 3)) {
          x    <- df[[rank_idx]]; x[!is.finite(x)] <- NA
          id_c <- names(df)[1]
          df_r <- data.frame(Plot=as.character(df[[id_c]]), Val=x)
          df_r <- df_r[!is.na(df_r$Val),]
          df_r <- df_r[order(-df_r$Val),]
          df_r$Rank <- seq_len(nrow(df_r))
          df_r$Category <- ifelse(df_r$Rank <= ceiling(nrow(df_r)*0.25),"Elite (top 25%)","Standard")

          # Bar chart
          b64_rank <- tryCatch({
            top30 <- head(df_r,30)
            top30$col <- ifelse(top30$Category=="Elite (top 25%)","#CC0000","#0f3460")
            gg <- ggplot(top30, aes(x=reorder(Plot,Val), y=Val, fill=Category)) +
              geom_col(color="white", linewidth=0.15, width=0.75) +
              scale_fill_manual(values=c("Elite (top 25%)"="#CC0000","Standard"="#0f3460")) +
              geom_hline(yintercept=quantile(df_r$Val,0.75,na.rm=TRUE),
                         linetype="dashed",color="#e6a800",linewidth=0.9) +
              annotate("text",x=3,y=quantile(df_r$Val,0.75,na.rm=TRUE),
                       label="Elite threshold (Q3)",vjust=-0.5,size=3,color="#e6a800") +
              coord_flip() +
              labs(title=paste0("Top 30 Plots — ", rank_idx),
                   subtitle=paste0("Red = Elite (top 25%) | Blue = Standard | ",
                     "Dashed = Q3 threshold"),
                   x="Plot", y=rank_idx, fill="Category") +
              theme_minimal(base_size=12) +
              theme(plot.title=element_text(face="bold",color="#1a1a2e"),
                    legend.position="bottom")
            gg_to_b64(gg, w=10, h=max(6,min(30,nrow(top30))*0.32+2), dpi=110)
          }, error=function(e) NULL)
          lines <- c(lines,
            sprintf('<h3>Rankings by %s</h3>', rank_idx),
            img_html(b64_rank,
              paste0("Figure 9.", which(head(idx_nc,3)==rank_idx),
                     ": Top 30 plots ranked by ", rank_idx, ". ",
                     "Red = elite (top 25%). Dashed line = Q3 threshold.")))

          # Top 20 table
          lines <- c(lines,
            sprintf('<h4>Top 20 Plots — %s</h4>', rank_idx),
            '<div class="card"><table>',
            '<tr><th>Rank</th><th>Plot</th><th>Value</th><th>Category</th><th>Percentile</th></tr>')
          for (i in seq_len(min(20,nrow(df_r)))) {
            r_cls <- if(df_r$Category[i]=="Elite (top 25%)")"good" else ""
            pct_v <- round(100*(1-df_r$Rank[i]/nrow(df_r)),1)
            lines <- c(lines, sprintf(
              '<tr><td class="%s">%d</td><td><b>%s</b></td><td>%.4f</td><td class="%s">%s</td><td>%.1f%%</td></tr>',
              r_cls, df_r$Rank[i], df_r$Plot[i], df_r$Val[i],
              r_cls, df_r$Category[i], pct_v))
          }
          lines <- c(lines,'</table></div>')
        }
      }

      # ════════════════════════════════════════════════════════
      # 10. MACHINE LEARNING
      # ════════════════════════════════════════════════════════
      if ("ml" %in% secs) {
        incProgress(0.05, "ML results...")
        lines <- c(lines,
          '<hr class="section-divider">',
          '<h2 id="sec_ml">10. Machine Learning Results</h2>')
        if (!is.null(rv$ml_results)) {
          ml_df <- tryCatch(
            do.call(rbind, lapply(rv$ml_results$models, function(x) x$metrics)),
            error=function(e) NULL)
          task <- rv$ml_results$task
          tgt  <- rv$ml_results$target
          feats <- rv$ml_results$features

          lines <- c(lines,
            sprintf('<p>Machine learning models were trained to predict <b>%s</b> ', tgt),
            sprintf('using <b>%d features</b> (vegetation indices). ',length(feats)),
            sprintf('Task type: <b>%s</b>. ', if(task=="reg") "Regression (continuous target)" else "Classification (categorical target)"),
            'Models were evaluated using repeated cross-validation to estimate generalisation performance.</p>',
            '<div class="card"><h4>Features used:</h4><p>',
            paste(sapply(feats, function(f) sprintf('<span class="stat-badge">%s</span>',f)), collapse=" "),
            '</p></div>'
          )

          if (!is.null(ml_df)) {
            lines <- c(lines,
              '<h3>Model Comparison Table</h3>',
              '<div class="card"><table>',
              paste0('<tr>',paste(sprintf('<th>%s</th>',names(ml_df)),collapse=""),'</tr>'))
            for (i in seq_len(nrow(ml_df))) {
              vals <- unlist(ml_df[i,])
              lines <- c(lines, paste0('<tr>',
                paste(sapply(seq_along(vals), function(j) {
                  v   <- vals[j]; nm <- names(vals)[j]
                  cls4 <- if(nm=="R2"||nm=="Accuracy") {
                    vn <- suppressWarnings(as.numeric(v))
                    if(!is.na(vn)&&vn>0.8)"good" else if(!is.na(vn)&&vn>0.5)"caution" else "bad"
                  } else if (nm=="RMSE"||nm=="MAE") {
                    "caution"
                  } else ""
                  sprintf('<td class="%s">%s</td>',cls4,v)
                }),collapse=""),'</tr>'))
            }
            lines <- c(lines,'</table></div>')

            # Model comparison chart
            b64_ml <- tryCatch({
              metric <- if(task=="reg" && "RMSE" %in% names(ml_df)) "RMSE"
                        else if("Accuracy" %in% names(ml_df)) "Accuracy"
                        else names(ml_df)[2]
              ml_df[[metric]] <- suppressWarnings(as.numeric(ml_df[[metric]]))
              lower_is_better <- metric %in% c("RMSE","MAE")
              ml_df2 <- ml_df[!is.na(ml_df[[metric]]),]
              gg <- ggplot(ml_df2,
                aes(x=reorder(Algorithm, if(lower_is_better) -.data[[metric]] else .data[[metric]]),
                    y=.data[[metric]], fill=.data[[metric]])) +
                geom_col(color="white",linewidth=0.2,width=0.7) +
                scale_fill_gradient(low=if(lower_is_better)"#CC0000" else "#ccc",
                                    high=if(lower_is_better)"#ccc" else "#CC0000") +
                coord_flip() +
                labs(title=paste("Model Comparison —", metric),
                     subtitle=if(lower_is_better)"Lower is better" else "Higher is better",
                     x="Algorithm", y=metric) +
                theme_minimal(base_size=12) +
                theme(plot.title=element_text(face="bold",color="#1a1a2e"),
                      legend.position="none")
              gg_to_b64(gg, w=9, h=5.5, dpi=110)
            }, error=function(e) NULL)
            lines <- c(lines,
              img_html(b64_ml,
                paste0("Figure 10.1: Algorithm comparison by ", metric, ". ",
                       if(task=="reg") "Lower RMSE / Higher R² = better." else "Higher Accuracy / Kappa = better.")),
              '<div class="note"><b>📝 ML interpretation:</b>',
              '<ul style="margin:6px 0 0 16px;font-size:12.5px;">',
              '<li>R² &gt; 0.8: Strong model — index values explain &gt;80% of variance in the target trait</li>',
              '<li>R² 0.5–0.8: Moderate — indices are partially predictive; consider adding more features</li>',
              '<li>R² &lt; 0.5: Weak — indices alone may not reliably predict this trait</li>',
              '<li>RMSE should be interpreted relative to the target variable range</li>',
              '<li>For classification: Kappa &gt; 0.6 = substantial agreement beyond chance</li>',
              '</ul></div>'
            )
          }
        } else {
          lines <- c(lines, '<div class="warn">⚠️ No ML results. Run models in the Machine Learning tab first.</div>')
        }
      }

      # ── FOOTER ───────────────────────────────────────────────
      incProgress(0.04, "Finalising report...")
      lines <- c(lines,
        '<hr class="section-divider">',
        '<div class="footer">',
        sprintf('<p><b>%s</b></p>', htmltools::htmlEscape(input$rep_title)),
        sprintf('<p>Experiment: %s &nbsp;·&nbsp; Generated: %s</p>',
          htmltools::htmlEscape(exp_txt), format(Sys.time(),"%Y-%m-%d %H:%M")),
        '<p>Generated by <b>AllInOne Phenomics</b> &mdash; Dry Bean Breeding &amp; ',
        'Computational Biology Program &mdash; University of Guelph</p>',
        '<p><a href="https://www.uogbeans.com">www.uogbeans.com</a> &nbsp;·&nbsp; ',
        '<a href="mailto:myoosefz@uoguelph.ca">myoosefz@uoguelph.ca</a></p>',
        '<p style="font-size:10.5px;margin-top:8px;color:#bbb;">',
        'This report was automatically generated by AllInOne Phenomics. ',
        'All analyses should be reviewed by a qualified researcher before making ',
        'breeding or agronomic decisions. Vegetation index values depend on correct ',
        'sensor calibration and image quality.</p>',
        '</div></body></html>'
      )

      incProgress(0.03, "Writing HTML file...")
      tmp <- tempfile(fileext=".html")
      writeLines(lines, tmp, useBytes=FALSE)
      rv$report_path  <- tmp
      rv$report_ready <- TRUE
    })
  })


  output$report_preview_ui <- renderUI({
    if (isTRUE(rv$report_ready)) {
      tags$div(
        style="background:#fff;border:1px solid #ddd;border-radius:8px;padding:20px;",
        tags$div(style="font-size:13px;font-weight:600;color:#1a1a2e;margin-bottom:8px;",
          "✅ Report generated successfully"),
        tags$ul(style="font-size:12px;color:#555;",
          lapply(input$rep_sections, function(s)
            tags$li(switch(s,
              summary="Executive Summary", overview="Data Overview",
              maps="Field Heatmaps", dists="Index Distributions",
              stats="Summary Statistics", corr="Correlation Matrix",
              pca="PCA Biplot", rankings="Genotype Rankings", ml="ML Results", s))
          )
        ),
        tags$p(style="font-size:11.5px;color:#888;margin-top:8px;",
          "Open the HTML file in any browser — no internet connection needed.")
      )
    } else {
      tags$div(style="background:#f8f9fa;border-radius:8px;padding:20px;text-align:center;color:#aaa;",
        tags$p("Click 'Generate Report' to create your analysis report."),
        tags$p(style="font-size:11px;","Report will include all data loaded in the current session.")
      )
    }
  })

  output$dl_report <- downloadHandler(
    filename = function() paste0("DryBean_Report_", Sys.Date(), ".html"),
    content  = function(file) {
      req(rv$report_ready, rv$report_path)
      file.copy(rv$report_path, file)
    }
  )


  # ═══════════════════════════════════════════════════════════
  # ADMIN DASHBOARD SERVER
  # ═══════════════════════════════════════════════════════════

  # Reactive log reader — auto-refreshes every 10s or on button click
  log_data <- reactivePoll(
    intervalMillis = 10000,
    session        = session,
    checkFunc = function() {
      lf <- "usage_log.csv"
      if (file.exists(lf)) file.info(lf)$mtime else Sys.time()
    },
    valueFunc = function() {
      lf <- "usage_log.csv"
      if (!file.exists(lf)) return(data.frame(
        timestamp="—", user="—", action="—", detail="—", session="—",
        stringsAsFactors=FALSE))
      tryCatch({
        df <- read.csv(lf, stringsAsFactors=FALSE)
        df <- df[order(df$timestamp, decreasing=TRUE), ]
        df
      }, error=function(e) data.frame(
        timestamp="Error reading log", user="", action="", detail="", session="",
        stringsAsFactors=FALSE))
    }
  )

  # Force refresh on button click
  observeEvent(input$refresh_log, { log_data() })

  # Main log table
  output$admin_log_tbl <- renderDT({
    df <- log_data()
    # Colour-code actions
    datatable(
      df[, intersect(c("timestamp","user","action","detail"), names(df)), drop=FALSE],
      options  = list(pageLength=20, scrollX=TRUE, order=list(list(0,"desc"))),
      rownames = FALSE,
      class    = "table-striped compact"
    ) %>%
      formatStyle("user",
        backgroundColor = styleEqual(
          c("MYN","DBBCB","Public"),
          c("rgba(204,0,0,0.08)","rgba(15,52,96,0.08)","rgba(45,106,79,0.08)")
        ),
        fontWeight = "bold"
      ) %>%
      formatStyle("action",
        color = styleEqual(
          c("LOGIN","TAB_VISIT","ACTION","EXPORT"),
          c("#2d6a4f",   "#0f3460",    "#CC0000",  "#e6a800")
        ),
        fontWeight = "600"
      )
  })

  # Summary boxes
  output$admin_summary_ui <- renderUI({
    df <- log_data()
    if (nrow(df) == 0 || df$timestamp[1] == "—") {
      return(tags$p("No log data yet.", style="color:#888;font-size:12px;"))
    }

    n_sessions  <- length(unique(df$session[df$action=="LOGIN"]))
    n_users     <- length(unique(df$user))
    n_actions   <- sum(df$action == "ACTION", na.rm=TRUE)
    n_exports   <- sum(df$action == "EXPORT", na.rm=TRUE)
    last_active <- if(nrow(df)>0) df$timestamp[1] else "—"

    # Most active user
    user_counts <- sort(table(df$user), decreasing=TRUE)
    top_user    <- if(length(user_counts)>0) names(user_counts)[1] else "—"

    tags$div(
      tags$div(style="display:grid;grid-template-columns:1fr 1fr;gap:8px;margin-bottom:8px;",
        tags$div(style="background:#1a1a2e;color:#FFC72C;border-radius:8px;padding:10px;text-align:center;",
          tags$div(style="font-size:22px;font-weight:700;", n_sessions),
          tags$div(style="font-size:10px;opacity:0.8;", "Total Sessions")),
        tags$div(style="background:#0f3460;color:#fff;border-radius:8px;padding:10px;text-align:center;",
          tags$div(style="font-size:22px;font-weight:700;", n_users),
          tags$div(style="font-size:10px;opacity:0.8;", "Unique Users")),
        tags$div(style="background:#CC0000;color:#fff;border-radius:8px;padding:10px;text-align:center;",
          tags$div(style="font-size:22px;font-weight:700;", n_actions),
          tags$div(style="font-size:10px;opacity:0.8;", "Actions Run")),
        tags$div(style="background:#2d6a4f;color:#fff;border-radius:8px;padding:10px;text-align:center;",
          tags$div(style="font-size:22px;font-weight:700;", n_exports),
          tags$div(style="font-size:10px;opacity:0.8;", "Exports"))
      ),
      tags$div(style="font-size:11.5px;color:#555;margin-top:4px;",
        tags$b("Most active: "), top_user, tags$br(),
        tags$b("Last activity: "), last_active)
    )
  })

  # Tab usage chart
  output$admin_tab_chart <- renderPlotly({
    df <- log_data()
    if (nrow(df) == 0 || !"action" %in% names(df)) return(NULL)
    tabs <- df[df$action == "TAB_VISIT" & nzchar(df$detail), ]
    if (nrow(tabs) == 0) return(NULL)
    tc  <- as.data.frame(sort(table(tabs$detail), decreasing=TRUE))
    names(tc) <- c("Tab","Count")
    tc <- head(tc, 12)
    plot_ly(tc, x=~Count, y=~reorder(Tab,Count), type="bar",
            orientation="h",
            marker=list(color=C_RED),
            hovertemplate="%{y}: %{x} visits<extra></extra>") %>%
      layout(title=list(text="Tab Visits",font=list(size=12,color=C_DARK)),
             xaxis=list(title=""),yaxis=list(title=""),
             margin=list(l=100), paper_bgcolor="white") %>%
      config(displaylogo=FALSE)
  })

  # Activity timeline
  output$admin_timeline <- renderPlotly({
    df <- log_data()
    if (nrow(df) == 0 || df$timestamp[1] == "—") return(NULL)
    logins <- df[df$action == "LOGIN", ]
    if (nrow(logins) == 0) return(NULL)
    logins$date <- as.Date(substr(logins$timestamp, 1, 10))
    daily <- as.data.frame(table(logins$date, logins$user))
    names(daily) <- c("Date","User","Count")
    daily$Date  <- as.Date(as.character(daily$Date))
    daily <- daily[daily$Count > 0, ]
    pal  <- c(MYN="#CC0000", DBBCB="#0f3460", Public="#2d6a4f")

    fig <- plot_ly()
    for (u in unique(daily$User)) {
      sub <- daily[daily$User == u, ]
      fig <- fig %>% add_trace(
        x=sub$Date, y=sub$Count, type="scatter", mode="lines+markers",
        name=u, line=list(width=2.5),
        marker=list(size=7),
        hovertemplate=paste0(u,": %{y} login(s) on %{x}<extra></extra>")
      )
    }
    fig %>% layout(
      title=list(text="Daily Logins by User", font=list(size=12,color=C_DARK)),
      xaxis=list(title=""), yaxis=list(title="Logins"),
      legend=list(orientation="h"),
      paper_bgcolor="white"
    ) %>% config(displaylogo=FALSE)
  })

  # Download full log
  # WebODM system status panel
  output$webodm_sys_status <- renderUI({
    ok <- webodm_ok()
    os_type <- tryCatch(Sys.info()["sysname"], error=function(e) "Unknown")
    docker_ok <- suppressWarnings(tryCatch({
      system2("docker", "--version", stdout=TRUE, stderr=FALSE)
      TRUE
    }, error=function(e) FALSE, warning=function(w) FALSE))

    tags$div(
      # Docker status
      tags$div(
        style=paste0("border-radius:6px;padding:8px 12px;margin-bottom:8px;font-size:12.5px;font-weight:600;",
          if(isTRUE(docker_ok)) "background:#c7f9cc;color:#1b4332;" else "background:#f8d7da;color:#721c24;"),
        if(isTRUE(docker_ok)) "🐳 Docker: Detected" else "🐳 Docker: Not found — see Step 1 below"
      ),
      # WebODM status
      tags$div(
        style=paste0("border-radius:6px;padding:8px 12px;margin-bottom:8px;font-size:12.5px;font-weight:600;",
          if(ok) "background:#c7f9cc;color:#1b4332;" else "background:#f8d7da;color:#721c24;"),
        if(ok) "🟢 WebODM: Running at localhost:8000 — ready!"
        else    "🔴 WebODM: Not running — follow Steps 3 below"
      ),
      # OS info
      tags$div(
        style="border-radius:6px;padding:8px 12px;font-size:12px;background:#e9ecef;color:#555;",
        paste0("💻 OS: ", os_type)
      )
    )
  })

  # Quick actions panel
  output$webodm_quick_actions <- renderUI({
    ok <- webodm_ok()
    if (ok) {
      tags$div(
        tags$div(style="background:#c7f9cc;border-radius:8px;padding:12px 16px;margin-bottom:12px;",
          tags$h5(style="margin:0 0 4px;color:#1b4332;","🟢 WebODM is running!"),
          tags$p(style="margin:0;font-size:12.5px;color:#2d4a3e;",
            "Click below to open WebODM, process your photos, download the .tif, then come back to upload it here.")
        ),
        tags$div(style="display:flex;gap:10px;flex-wrap:wrap;",
          tags$a(href="http://localhost:8000", target="_blank",
            class="btn btn-success btn-lg",
            style="font-weight:700;",
            "🖥️ Open WebODM Now"),
          actionButton("goto_upload_from_webodm",
            "📁 Go to Data Upload →",
            class="btn-primary btn-lg",
            style="font-weight:700;")
        )
      )
    } else {
      tags$div(
        tags$div(style="background:#fff3cd;border-radius:8px;padding:12px 16px;margin-bottom:12px;",
          tags$h5(style="margin:0 0 4px;color:#856404;","⚠️ WebODM not running yet"),
          tags$p(style="margin:0;font-size:12.5px;color:#6d4c00;",
            "Follow the Setup Wizard below. Once WebODM is running, come back and click Recheck Status.")
        ),
        tags$div(style="display:flex;gap:10px;flex-wrap:wrap;",
          tags$a(href="https://webodm.net", target="_blank",
            class="btn btn-success btn-lg",
            style="font-weight:700;",
            "☁️ Skip installation — use WebODM Cloud"),
          tags$a(href="https://www.docker.com/products/docker-desktop/",
            target="_blank",
            class="btn btn-primary",
            style="font-weight:600;",
            "⬇️ Install Docker Desktop")
        )
      )
    }
  })

    output$dl_usage_log <- downloadHandler(
    filename = function() paste0("AllInOne_UsageLog_", Sys.Date(), ".csv"),
    content  = function(file) {
      lf <- "usage_log.csv"
      if (file.exists(lf)) file.copy(lf, file)
      else writeLines("timestamp,user,action,detail,session", file)
    }
  )

  # Clear log (admin only)
  observeEvent(input$clear_log, {
    req(is_admin())
    tryCatch({
      if (file.exists("usage_log.csv")) file.remove("usage_log.csv")
      showNotification("✅ Usage log cleared.", type="message", duration=3)
    }, error=function(e)
      showNotification(paste("❌", e$message), type="error"))
  })


  # ═══════════════════════════════════════════════════════════
  # WEBODM SERVER
  # ═══════════════════════════════════════════════════════════

  # Navigate to upload tab when "Go to Data Upload" button clicked
  observeEvent(input$goto_upload_from_webodm, {
    updateTabItems(session, "sidebar", "upload")
  })

  # Manual recheck button
  webodm_manual_check <- reactiveVal(0)
  observeEvent(input$recheck_webodm, {
    webodm_manual_check(webodm_manual_check() + 1)
  })

  # Check if local WebODM is reachable — silent on failure
  webodm_reachable <- reactive({
    webodm_manual_check()  # depend on manual recheck too
    suppressWarnings(tryCatch({
      con <- url("http://localhost:8000", open="r", blocking=FALSE)
      close(con)
      TRUE
    }, warning=function(w) FALSE,
       error  =function(e) FALSE))
  })

  # Also auto-poll every 30 seconds
  webodm_poll <- reactivePoll(
    intervalMillis = 30000,
    session        = session,
    checkFunc = function() Sys.time(),
    valueFunc = function() {
      suppressWarnings(tryCatch({
        con <- url("http://localhost:8000", open="r", blocking=FALSE)
        close(con)
        TRUE
      }, warning=function(w) FALSE,
         error  =function(e) FALSE))
    }
  )

  # Combined status
  webodm_ok <- reactive({
    isTRUE(webodm_reachable()) || isTRUE(tryCatch(webodm_poll(), error=function(e) FALSE))
  })

  output$webodm_local_status <- renderUI({
    ok <- webodm_ok()
    if (isTRUE(ok)) {
      tags$div(
        style="background:#c7f9cc;border-radius:6px;padding:8px 12px;margin-bottom:10px;font-size:12.5px;color:#1b4332;font-weight:600;",
        "🟢 WebODM detected at localhost:8000 — ready to open!"
      )
    } else {
      tags$div(
        style="background:#f8d7da;border-radius:6px;padding:8px 12px;margin-bottom:10px;font-size:12.5px;color:#721c24;",
        "🔴 WebODM not detected at localhost:8000.",
        tags$br(),
        tags$span(style="font-weight:400;font-size:11.5px;",
          "Start WebODM with: ", tags$code("./webodm.sh start"),
          " then refresh this page.")
      )
    }
  })

  output$webodm_iframe_ui <- renderUI({
    ok <- webodm_ok()
    if (isTRUE(ok)) {
      tags$div(
        tags$div(
          style="background:#c7f9cc;padding:8px 14px;border-radius:6px;margin-bottom:10px;font-size:12.5px;color:#1b4332;font-weight:600;",
          "🟢 WebODM is running — loaded below."
        ),
        tags$iframe(
          src     = "http://localhost:8000",
          width   = "100%",
          height  = "750px",
          style   = "border:none;border-radius:8px;box-shadow:0 3px 16px rgba(0,0,0,0.15);"
        )
      )
    } else {
      tags$div(
        style=paste0(
          "background:#f8f9fa;border:2px dashed #dee2e6;border-radius:10px;",
          "padding:40px;text-align:center;color:#888;"
        ),
        tags$div(style="font-size:48px;margin-bottom:12px;","🛩️"),
        tags$h4(style="color:#555;margin-bottom:8px;","WebODM not running"),
        tags$p(style="font-size:13px;max-width:500px;margin:0 auto 16px;",
          "Start WebODM locally using Docker, then refresh this page. ",
          "The interface will appear here automatically once WebODM is running at ",
          tags$code("http://localhost:8000"), "."),
        tags$div(style="display:flex;justify-content:center;gap:12px;flex-wrap:wrap;",
          tags$a(href="https://webodm.net", target="_blank",
            class="btn btn-success",
            style="font-weight:600;",
            "☁️ Use WebODM Cloud instead"),
          tags$a(href="https://github.com/OpenDroneMap/WebODM#getting-started",
            target="_blank", class="btn btn-primary",
            style="font-weight:600;",
            "📖 WebODM Installation Guide")
        )
      )
    }
  })

} # end server

# ── Launch ──────────────────────────────────────────────────────
# Resource path registered at startup (works locally AND on shinyapps.io)
shinyApp(ui = ui, server = server)

