library(tidyverse)
library(sf)

## read in data
mdf <- read.csv("mdf_removeOutliersResiduals_wSeasonalityTrait.csv") %>% 
  dplyr::filter(overwinteringStage != "None")

mdf_sum <- mdf %>% 
  group_by(lon, lat, id_cells, decade) %>% 
  summarise(pheno_estimates = n(),
            species = length(unique(validName)))

mdf_sum2 <- mdf_sum %>% 
  group_by(lon, lat, id_cells) %>% 
  summarise(pheno_estimates = sum(pheno_estimates),
            species = sum(species),
            decades = as.numeric(length(unique(decade))))

# read in NA sf
na <- rnaturalearth::ne_countries(continent = "North America",
                                  returnclass = "sf",
                                  scale = 10)

na <- st_transform(na, sp::CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 
                     +units=m +no_defs"))

ggplot() +  
  geom_sf(na, mapping = aes(), fill = NA) +
  geom_tile(mdf_sum2, mapping = aes(x = lon, y = lat, fill = pheno_estimates, alpha = decades), 
            color = "grey55") +
  coord_sf(xlim = c(min(mdf_sum$lon) - 10000, max(mdf_sum$lon) + 10000),
           ylim = c(min(mdf_sum$lat) - 10000, max(mdf_sum$lat) + 10000)) +
  scale_fill_gradient(low = "green", high = "pink", trans = "log") +
  scale_alpha_continuous()+
  theme_void()

ggplot() +  
  geom_sf(na, mapping = aes(), fill = NA) +
  geom_point(mdf_sum2, mapping = aes(x = lon, y = lat, size = decades, fill = pheno_estimates),  shape = 21) +
  coord_sf(xlim = c(min(mdf_sum$lon) - 10000, max(mdf_sum$lon) + 10000),
           ylim = c(min(mdf_sum$lat) - 10000, max(mdf_sum$lat) + 10000)) +
  scale_fill_viridis_c(option = "rocket", trans = "log",
                       breaks = c(10,50,400)) +
  scale_size_continuous(breaks = c(1,3,5,7,9)) +
  labs(size = "Decades", fill = "Pheno-estimates") +
  theme_void() +
  theme(legend.position = c(0.1, 0.4))

ggsave("figures/studyArea.png", dpi = 400, width = 8, height = 6)