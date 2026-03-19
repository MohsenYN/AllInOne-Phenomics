# Dry Bean Drone Analytics Platform
**Dry Bean Breeding & Computational Biology — University of Guelph**

---

## Quick Start (Local)

```r
source("install_packages.R")   # run ONCE
shiny::runApp("app.R")
```

## Publish to shinyapps.io

```r
# 1. Run install_packages.R (includes rsconnect)
source("install_packages.R")

# 2. Edit deploy_to_shinyapps.R — fill in your credentials
#    from https://www.shinyapps.io -> Account -> Tokens

# 3. Deploy
source("deploy_to_shinyapps.R")
# App live at: https://YOUR_ACCOUNT.shinyapps.io/DrybeanDroneApp
```

---

## Files

| File | Purpose |
|------|---------|
| `app.R` | Main Shiny application |
| `install_packages.R` | Installs ALL dependencies (run once locally) |
| `deploy_to_shinyapps.R` | One-click deploy to shinyapps.io |
| `DESCRIPTION` | Package manifest for rsconnect |
| `www/logo.png` | Lab logo |

---

## Workflow

1. Upload — .tif mosaic, plot grid .rds, metadata .csv
2. Camera Setup — RGB or Multispectral; set band scaling
3. Soil Mask — BGI, HUE, ExG, NDVI
4. Plot Grid — Click 4 field corners; set rows/columns
5. Vegetation Indices — 20+ built-in + custom formulas
6. Field Maps — Interactive plotly per-plot heatmaps
7. Statistics — Distributions, genotype comparisons
8. Correlation — Matrix + scatter
9. PCA & Clustering — Biplot, scree, dendrogram
10. Genotype Selection — Threshold-based ranking of elite plots
11. Machine Learning — 12 algorithms, tuning, RFE, RMSE/R2
12. Export — CSV, Excel (6 sheets), PNG/PDF

---

## shinyapps.io Notes

- Free tier: 25 active hours/month, 1 GB RAM
- Large .tif uploads may be slow — consider Starter plan
- DESCRIPTION tells rsconnect which packages to install on the server
