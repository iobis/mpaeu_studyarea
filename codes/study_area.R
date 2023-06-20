### MPA EUROPE PROJECT - OBIS ###
### Define the study area for the project ###

# April 2023 - s.principe@unesco.org #

# Load packages ----
# Obtaining marine regions
library(mregions)
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
version <- 2


# Load shapefiles ----
# Load IHO borders
iho <- mr_shp(key = "MarineRegions:eez_iho_union_v2", maxFeatures = 1000)

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
  filter(sovereign != "Russia")

join.iho <- iho %>%
  filter(sovereign == "France" |
           str_detect(iho_sea, "Mediterranean|Black Sea|Baltic Sea|Sea of Marmara|Gulf of Finland")) %>%
  st_crop(st_bbox(europe.sel)+c(0,0,10,0))

study.area <- bind_rows(inter.iho, join.iho)

study.area <- study.area %>%
  filter(iho_sea != "Sea of Azov")

study.area <- study.area[!duplicated(study.area$objectid_1),]

# Plot to see
ggplot()+
  geom_sf(data = world, fill = "grey40", color = NA) +
  geom_sf(data = study.area, fill = "grey70", color = "orange") +
  geom_sf(data = europe.sel, fill = NA, color = "blue") +
  coord_sf(st_bbox(study.area)[c(1,3)], st_bbox(study.area)[c(2,4)])

mapView(study.area)

# Unify in a single polygon and save ----
starea.un <- st_union(study.area)

plot(starea.un)



# Save shapefile ----
fs::dir_create("data/shapefiles")
st_write(study.area, paste0("data/shapefiles/mpa_europe_starea_eez_v", version, ".shp"))
st_write(starea.un, paste0("data/shapefiles/mpa_europe_starea_v", version, ".shp"))
