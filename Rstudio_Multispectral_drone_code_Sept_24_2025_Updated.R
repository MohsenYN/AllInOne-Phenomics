library(FIELDimageR)
library(FIELDimageR.Extra)
library(raster)
library(terra)
library(mapview)
library(sf)
library(stars)

## Step 1: Load the raster (orthomosaic image)
EX1 <- rast(file.choose())
fieldView(EX1)

## Step 2: Visualization Option-02 (raster):
plotRGB(EX1, r = 1, g = 2, b = 3)

## Step 3: remove soil
##(HUE and BGI are different index options)
EX1.RemSoil <- fieldMask(mosaic = EX1, Red = 1, Green = 2, Blue = 3, 
                         index = "BGI", cropValue = 0.7, cropAbove = T)

## Step 4: create plot grid and visualize shape
#use'draw marker' button to click 4 corners (starting at bottom left depending on map)
##(if you already saved a rectangle shape go to Load shape step below ▶️)
#*******************************change ncols and nrows for proper grid size**********
EX1.Shape <- fieldShape_render(mosaic = EX1, ncols = 4, nrows = 55)
fieldView(mosaic = EX1, fieldShape = EX1.Shape, type = 2, alpha = 0.2)

##############to reuse plot grid#######################################
##💾 saves your current plot grid ** can choose different name for "EX1_Shape"
#saveRDS(EX1.Shape, file = "EX1_Shape.rds")
#######################################################################

##▶️ Load saved plot grid #############################################
#EX1.Shape <- readRDS(file.choose())
#fieldView(mosaic = EX1, fieldShape = EX1.Shape, type = 2, alpha = 0.2)
#######################################################################

######Editing plot grid shapefile:#####################################
## 🔁 Step A: Launch the interactive shape editor
#editShape <- fieldShape_edit(mosaic = EX1, fieldShape = EX1.Shape)

## 🧠 Inside the viewer: Make edits by clicking edit tool on left sidebar
###***make sure to hit save beside edit tool before pressing done***###

## ✅ Step B:  replace old shape file with new file
#EX1.Shape <- editShape

## 💾 Step C: (optional) Save the edited shape for future use
#saveRDS(EX1.Shape, file = "EX1_Shape_Edited.rds")

## Step D: Render Shape
#fieldView(mosaic = EX1, fieldShape = EX1.Shape, type = 2, alpha = 0.2)

###################################################################
##can check shape files if problems occur check if you edited plot
# plot(st_geometry(editShape), col = 'blue', main = "Edited Shape")
# plot(st_geometry(EX1.Shape), border = 'red', add = TRUE)
###################################################################

## Step 5: Load your plot data (should include a 'PLOT' column)
DataTable <- read.csv(file.choose(), header = TRUE)

## Step 6: Calculate vegetation indices
EX1.Indices.myIndex <- fieldIndex(
  mosaic = EX1.RemSoil$newMosaic,
  Red = 3,        # Set according to your multispectral band order
  Green = 2,
  Blue = 1,
  RedEdge = 4,
  NIR = 5,
  index = c("NDVI", "NDRE"),  # Common multispectral indices
  myIndex = c(
    "(NIR - Red) / (NIR + Red)",                    # NDVI (Normalized Difference Vegetation Index)
    "(NIR - RedEdge) / (NIR + RedEdge)"             # NDRE (Normalized Difference Red Edge)
  ),
  plot = TRUE
)

# Step 7: Use output directly as raster stack
indices_result <- EX1.Indices.myIndex

# Step 8: Extract index values by plot
extracted_vals <- terra::extract(indices_result, EX1.Shape, fun = mean, na.rm = TRUE)

# Step 9: Rename ID column for merge
names(extracted_vals)[1] <- "PlotID"

# Step 10: Merge with plot metadata (PLOT must match PlotID)
final_merged <- merge(DataTable, extracted_vals, by.x = "PLOT", by.y = "PlotID", all.x = TRUE)

# Step 11: Save final result *****rename file, Name.csv****
write.csv(final_merged, "exportedindices.csv", row.names = FALSE)


#to see where files are saved use:
#getwd()


#Done



##To bypass errors with Tif file coordinates use this code between step 7 and 8:
##these errors can happen if you edit the file in paint

# if (!inherits(EX1.Shape, "SpatVector")) {
#   EX1.Shape <- vect(EX1.Shape)
# }

# if (is.na(crs(indices_result))) {
#   crs(indices_result) <- "EPSG:4326"
# }
# if (is.na(crs(EX1.Shape))) {
#   crs(EX1.Shape) <- crs(indices_result)
# }







#bonus checks that aren't necessary
#    Add a check for CRS in your CSV or metadata: Make sure your CSV rows align with polygons (Plot IDs match).

#Explicitly convert your shapefile to terra’s SpatVector only once and early. Your code does this fine.

#Add some diagnostic prints/logs: To confirm sizes and IDs before merging, e.g.,

cat("Number of polygons:", nrow(EX1.Shape), "\n")
cat("Number of extracted rows:", nrow(extracted_vals), "\n")
cat("Metadata rows:", nrow(DataTable), "\n")






# Check raster extent and polygon extent
print(ext(indices_result))
print(ext(EX1.Shape))

# If polygon extent fits inside raster extent, that's good sign they align spatially.

