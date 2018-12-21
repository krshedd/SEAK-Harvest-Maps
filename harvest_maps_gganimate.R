library(tidyverse)
library(sf)
library(ggmap)
library(gganimate)

stat_area <- st_read("../GIS Data/Stat Area/pvs_stat.shp")
str(stat_area, max.level = 1)

# Plot stat_area for D101-103, no basemap
stat_area_D123 <- stat_area %>% 
  filter(DISTRICT %in% c("101", "102", "103"))

stat_area_D123 %>% 
  ggplot() +
  geom_sf()

# Read harvest data
(harvest <- read_csv("../../D101to103 Seine/2018/Harvest Data/ft - Harvest by Year, Gear, Harvest type, Stat Area, Stat Week.csv"))

harvest_D123 <- harvest %>% 
  filter(`Gear Type` == "01 - Purse seine",
         District %in% 101:103) %>% 
  mutate(STAT_AREA = as.character(`Stat Area`))

# Merge
harvest_map <- left_join(stat_area_D123, harvest_D123, by = "STAT_AREA")


# Plot
harvest_map %>% 
  # filter(!is.na(`Stat Week`)) %>%
  ggplot() +
  geom_sf(data = stat_area_D123) +
  geom_sf(aes(fill = Pink)) +
  scale_fill_gradient(low = "white", high = "darkgreen") +
  coord_sf(crs = st_crs(stat_area_D123)) +
  labs(title = 'D101-103 Purse Seine Pink Harvest for Stat Week: {current_frame}') +
  transition_manual(`Stat Week`)

anim_save(filename = "../Figures/2018_pink_seine_d123_harvest.gif")


(harvest <- read_csv("../2018 SEAK ft - Harvest by Year, Gear, Harvest type, Stat Area, Stat Week.csv"))
unique(harvest$`Gear Type`)
stat_area <- st_read("../GIS Data/Stat Area/pvs_stat.shp")


harvest_map.f <- function(district, gear, species){
  stat_area_district <- stat_area %>% 
    filter(DISTRICT %in% as.character(district))
  
  h_plot <- harvest %>% 
    gather(Species, Harvest, -Year, -`Harvest Type`, -`Gear Type`, - District, -`Stat Area`, -`Stat Week`, -`Number of Permits`) %>% 
    mutate(STAT_AREA = as.character(`Stat Area`)) %>% 
    filter(`Gear Type` %in% gear,
           District %in% district,
           Species == species) %>% 
    right_join(stat_area_district, by = "STAT_AREA") %>% 
    ggplot() +
    geom_sf(data = stat_area_district, fill = "white") +
    geom_sf(aes(fill = Harvest)) +
    scale_fill_gradient(low = "white", high = "darkgreen", na.value = "white") +
    coord_sf(crs = st_crs(stat_area_district)) +
    labs(title = paste('Districts:', paste(district, collapse = ", "), '\nGear:', gear, '\nSpecies:', species, '\nHarvest for Stat Week: {current_frame}')) +
    transition_manual(`Stat Week`)
  
  animate(h_plot, fps = 3)
}


harvest_map.f(district = 113, gear = c("03 - Drift gillnet"), species = "Chum")


# Trying to get a basemap via ggmap, doesn't work
seak_basemap <- get_map("Ketchikan, AK", zoom = 11, maptype = "terrain")
ph_basemap <- get_map(location="Philadelphia, PA", zoom=11, maptype = 'satellite')
map.tokyo <- get_map("Tokyo")
ggmap(map.tokyo)
