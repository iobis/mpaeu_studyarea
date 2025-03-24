### MPA EUROPE PROJECT - OBIS ###
### Define the study area for the project ###

# April 2023 - s.principe@unesco.org #

# Load packages ----
# Obtaining marine regions
library(mregions2)
# Spatial manipulation
library(sf)
library(tidyverse)
# Other/visualization
library(rnaturalearth)
library(ggplot2)
library(mapview)
# Settings
sf_use_s2(FALSE)

# Define version
version <- 3


# Load shapefiles ----
# Load IHO borders
iho <- mrp_get("eez_iho")

# Load Europe shapefile
europe <- ne_countries(scale = "large", continent = "europe")
europe <- st_as_sf(europe)

# Load world shapefile (for ploting)
world <- ne_countries(scale = "large")
world <- st_as_sf(world)



# Select relevant area ----
# We start by removing areas out of the project scope from the shapefile
europe.sel <- europe %>% 
  filter(!str_detect("Russia", admin)) %>%
  select(admin)

# We crop for the relevant area
europe.sel <- st_crop(europe.sel, c(xmin = -68.4, ymin = 20, xmax = 57, ymax = 80.7))

plot(europe.sel)

# Intersect with IHO borders
# We add a small buffer to ensure all areas are covered
inter <- st_intersects(iho,
                       st_buffer(europe.sel, 0.3), sparse = F)

inter <- apply(inter, 1, any)

inter.iho <- iho[inter,]

# Plot to see
ggplot()+
  geom_sf(data = world, fill = "grey40", color = NA) +
  geom_sf(data = inter.iho, fill = "grey70", color = "orange") +
  geom_sf(data = europe.sel, fill = NA, color = "blue") +
  coord_sf(st_bbox(inter.iho)[c(1,3)], st_bbox(inter.iho)[c(2,4)])

# We manually add/remove some areas to reach the final study area
inter.iho <- inter.iho %>%
  filter(sovereign1 != "Russia")

join.iho <- iho %>%
  filter(sovereign1 == "France" |
           str_detect(iho_sea, "Mediterranean|Black Sea|Baltic Sea|Sea of Marmara|Gulf of Finland")) %>%
  st_crop(st_bbox(europe.sel)+c(0,0,10,0))

study.area <- bind_rows(inter.iho, join.iho)

study.area <- study.area %>%
  filter(iho_sea != "Sea of Azov")

study.area <- study.area[!duplicated(study.area$mrgid),]

to.remove <- "POLYGON((30.2728 59.7577, 30.2728 60.248, 28.316 63.47, 38 63.47, 39.77 59.40, 30.2728 59.7577))"
to.remove <- st_as_sfc(to.remove, crs = 4326)

study.area <- st_difference(study.area, to.remove)

# Plot to see
ggplot()+
  geom_sf(data = world, fill = "grey40", color = NA) +
  geom_sf(data = study.area, fill = "grey70", color = "orange") +
  geom_sf(data = europe.sel, fill = NA, color = "blue") +
  coord_sf(st_bbox(study.area)[c(1,3)], st_bbox(study.area)[c(2,4)])

mapView(study.area)

# Unify in a single polygon ----
starea.un <- st_union(study.area)
# Apply a small buffer to correct inconsistencies
# This step can take a while.
starea.un <- st_buffer(starea.un, dist = 0.005)

terra::plot(terra::vect(starea.un))

# Save shapefile
fs::dir_create("data/shapefiles")
study.area %>%
  select(mrgid, marregion, iho_mrgid, eez, eez_mrgid) %>%
  st_simplify(dTolerance = 0.001) %>%
  st_write(paste0("data/shapefiles/mpa_europe_starea_eez_v", version, ".gpkg"))
starea.un %>%
  st_simplify(dTolerance = 0.001) %>%
  st_write(paste0("data/shapefiles/mpa_europe_starea_hr_v", version, ".gpkg"))


# Now, we need to ensure this study area aligns with Bio-ORACLE v3, our
# main source of environmental information.
# For that we first obtain the landmass raster from Bio-ORACLE
landmass <- terra::rast("https://erddap.bio-oracle.org/erddap/griddap/terrain_characteristics.nc?landmass%5B(1970-01-01T00:00:00Z):1:(1970-01-01T00:00:00Z)%5D%5B(-89.975):1:(89.975)%5D%5B(-179.975):1:(179.975)%5D")

landmass.croped <- terra::crop(landmass, starea.un)
landmass.croped[is.na(landmass.croped)] <- 2
landmass.croped[landmass.croped == 1] <- NA
landmass.croped <- terra::as.polygons(landmass.croped)

# This was prepared manually using QGIS. Basically, using the previously saved
# shapefile, we overlayed the landmass and the shapefile and created simple
# polygons covering the study area border and the landmass, so we could
# later get the difference.
manual.outline <- sf::st_read("data/manual_mpaeu_outline.geojson")
starea.un.sum <- st_simplify(starea.un, dTolerance = 0.001) %>% 
  st_union(manual.outline)

starea.un.sum <- terra::aggregate(terra::vect(starea.un.sum))
starea.un.final <- terra::erase(starea.un.sum, landmass.croped)

st_as_sf(starea.un.final) %>%
  st_write(paste0("data/shapefiles/mpa_europe_starea_v", version, ".gpkg"))
