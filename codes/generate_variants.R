### MPA EUROPE PROJECT - OBIS ###
### Generate alternative shapefiles of the study area ###

# April 2023 - s.principe@unesco.org #

# Load packages ----
# Spatial manipulation
library(sf)
library(terra)
library(tidyverse)
# Other/visualization
library(ggplot2)
library(mapview)
# Settings
sf_use_s2(FALSE)

# Define version
version <- 2


# Load study area shapefile (see study_area.R) ----
st.area <- read_sf(paste0("data/shapefiles/mpa_europe_starea_v", version, ".shp"))


# Produce alternative shapefiles ----
# Simplified shapefile for simpler maps, illustrations, etc ----
st.area.simp <- st_simplify(st.area, dTolerance = 0.1)
plot(st.area.simp)

st_write(st.area.simp, paste0("data/shapefiles/mpa_europe_starea_simple_v", version, ".shp"))


# Reproject for Lambert Azimuthal Equal Area projection
st.area.rep <- st_transform(st.area, "EPSG:3035")
plot(st.area.rep)

st_write(st.area.rep,  paste0("data/shapefiles/mpa_europe_starea_laea3035_v", version, ".shp"))


# Generate extended study area (for SDMs) -----
# Get GSHHS shapefile (https://www.soest.hawaii.edu/pwessel/gshhg/)
if (!file.exists("data/gshhg-shp-2.3.7.zip") & !dir.exists("data/gshhg-shp-2.3.7")) {
  download.file("http://www.soest.hawaii.edu/pwessel/gshhg/gshhg-shp-2.3.7.zip",
                "data/gshhg-shp-2.3.7.zip")
  unzip("data/gshhg-shp-2.3.7.zip", exdir = "data/gshhg-shp-2.3.7/")
}

borders <- vect("data/gshhg-shp-2.3.7/GSHHS_shp/f/GSHHS_f_L1.shp")

# Get North Atlantic shapefile
natlantic <- mregions::mr_shp("MarineRegions:goas")
natlantic <- natlantic[natlantic$name == "North Atlantic Ocean",]

# Get a base raster in the resolution that will be used on the project
base <- rast(resolution = 0.05)
base[] <- 1
base <- mask(base, borders, inverse = T)
plot(base)

# Crop to the region
ext.area <- crop(base, ext(
  c(xmin = st_bbox(natlantic)$xmin, xmax = st_bbox(st.area)$xmax,
    ymin = st_bbox(natlantic)$ymin, ymax = 90) #or st_bbox(st.area)$ymax
))

# Remove areas not of interest
to.remove <- mregions::mr_shp("MarineRegions:goas")
to.remove <- to.remove[to.remove$name %in% c("North Pacific Ocean", "South Pacific Ocean",
                                             "Indian Ocean"),]

ext.area <- mask(ext.area, vect(to.remove), inverse = T)

ext.area <- as.polygons(ext.area)
ext.area <- st_as_sf(ext.area)

# Save
st_write(ext.area,  paste0("data/shapefiles/mpa_europe_extarea_v", version, ".shp"))


# Generate extended study area (for SDMs) - option 2, including whole Atlantic ----
# Get GSHHS shapefile (https://www.soest.hawaii.edu/pwessel/gshhg/)
if (!file.exists("data/gshhg-shp-2.3.7.zip") & !dir.exists("data/gshhg-shp-2.3.7")) {
  download.file("http://www.soest.hawaii.edu/pwessel/gshhg/gshhg-shp-2.3.7.zip",
                "data/gshhg-shp-2.3.7.zip")
  unzip("data/gshhg-shp-2.3.7.zip", exdir = "data/gshhg-shp-2.3.7/")
}

borders <- vect("data/gshhg-shp-2.3.7/GSHHS_shp/f/GSHHS_f_L1.shp")

# Get Atlantic shapefile
atlantic <- mregions::mr_shp("MarineRegions:goas")
atlantic <- atlantic[grepl("Atlantic", atlantic$name),]

# Get a base raster in the resolution that will be used on the project
base <- rast(resolution = 0.05)
base[] <- 1
base <- mask(base, borders, inverse = T)
plot(base)

# Crop to the region
ext.area <- crop(base, ext(
  c(xmin = st_bbox(atlantic)$xmin, xmax = st_bbox(st.area)$xmax,
    ymin = st_bbox(atlantic)$ymin, ymax = 90) #or st_bbox(st.area)$ymax
))

# Remove areas not of interest
to.remove <- mregions::mr_shp("MarineRegions:goas")
to.remove <- to.remove[to.remove$name %in% c("North Pacific Ocean", "South Pacific Ocean",
                                             "Indian Ocean"),]

ext.area <- mask(ext.area, vect(to.remove), inverse = T)

ext.area <- as.polygons(ext.area)
ext.area <- st_as_sf(ext.area)

# Save
st_write(ext.area,  paste0("data/shapefiles/mpa_europe_extarea_allatl_v", version, ".shp"))


# 12 nautical miles study area ----
nm12 <- mregions::mr_shp("MarineRegions:eez_12nm", maxFeatures = 3000)

# Crop to study area
nm12 <- st_crop(nm12, st.area)

# Intersects with study area
nm12.int <- st_intersects(nm12, st.area, sparse = F)
nm12.area <- nm12[nm12.int,]

# Plot to verify
mapview()+st.area+nm12.area

nm12.area <- nm12.area %>%
  filter(!mrgid %in% c(49031, 49091))

# There are still two areas that need to be removed, which we do manually drawing two
# polygons covering the regions
nm12.remove.1 <- st_as_sfc(st_bbox(ext(32, 36, 23, 30)))
st_crs(nm12.remove.1) <- st_crs(st.area)

nm12.remove.2 <- st_as_sfc(st_bbox(ext(34, 39, 45, 48)))
st_crs(nm12.remove.2) <- st_crs(st.area)
nm12.remove.2 <- st_difference(nm12.remove.2, st.area)

# Put both together
nm12.remove.c <- st_union(nm12.remove.1, nm12.remove.2)

# Plot to verify before removing
mapview()+st.area+nm12.area+nm12.remove.c

# Remove areas
nm12.area <- st_difference(nm12.area, nm12.remove.c)

# Plot to verify final result
mapview()+st.area+nm12.area

# Save final result
st_write(nm12.area,  paste0("data/shapefiles/mpa_europe_starea_12nm_v", version, ".shp"))
