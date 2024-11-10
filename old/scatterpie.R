library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggforce)
library(scatterpie)
library(dplyr)

sscs <- readRDS("_targets/objects/sscs_final")
cell_data <- readRDS("_targets/objects/obs_data_gridded") %>% select(cell,cell_lat,cell_lon) %>% distinct()
sscs_coords <- merge(sscs,cell_data,by="cell")

sscs_sum <- sscs_coords %>% 
  group_by(cell_lat, cell_lon) %>% 
  summarise(n=n(),
            nspecies = length(unique(species)),
            Winter = sum(season == "1"),
            Spring = sum(season == "2"),
            Summer = sum(season == "3"),
            Fall = sum(season == "4"))

# Assuming 'cell_lat' and 'cell_lon' are the columns with latitude and longitude information
# Convert the data frame to an sf object with WGS 84 (EPSG:4326) CRS
sscs_sum_sf <- st_as_sf(sscs_sum, coords = c("cell_lon", "cell_lat"), crs = 4326, agr = "constant")

# If you don't have the 'agr' argument, it's okay to exclude it, it's just to specify the geometry type

# Get North America map
north_america <- ne_countries(scale = "medium", returnclass = "sf", continent = "north america")

lon_min <- min(sscs_sum$cell_lon)
lon_max <- max(sscs_sum$cell_lon)
lat_min <- min(sscs_sum$cell_lat)
lat_max <- max(sscs_sum$cell_lat)


world <- map_data('world')
p <- ggplot(world, aes(long, lat)) +
  geom_map(map=world, aes(map_id=region), fill=NA, color="black") +
  coord_quickmap()
p + geom_scatterpie(aes(x=cell_lon, y=cell_lat,r=nspecies/60), data=sscs_sum,
                           cols=c("Winter","Spring","Summer","Fall")) + coord_equal() +
    geom_scatterpie_legend(sscs_sum$nspecies/60, x=-70, y=30) +
    xlim(-130,lon_max) + ylim(lat_min,50)

# Plot
ggplot() +
  geom_sf(data = north_america, fill = 'lightgrey') + 
  geom_sf(data = sscs_sum_sf, color = 'red', aes(size = n)) +
  geom_pointpie(data = sscs_sum_sf, aes(x = cell_lon, y = cell_lat, group = group, r = proportion, fill = group), color = 'white', size = 0) +
  
  coord_sf(xlim = c(lon_min, lon_max), ylim = c(lat_min, lat_max), expand = FALSE) + # Set dynamic limits based on your data
  theme_minimal()


ggplot() +
  geom_sf(data = north_america, fill = 'lightgrey') + 
  geom_scatterpie(aes(x=cell_lon, y=cell_lat, r=3),
                  data=sscs_sum_sf)