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
  geom_sf(aes(fill = Sockeye)) +
  coord_sf(crs = st_crs(stat_area_D123)) +
  labs(title = 'D101-103 Purse Seine Sockeye Harvest for Stat Week: {current_frame}') +
  transition_manual(`Stat Week`)

