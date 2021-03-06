#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This script is meant to create maps showing harvest by stat area for SEAK
# 
# Packages:
#   There are two mapping packages that can be used
# 
# Requirements:
#   This script requires a shapefile of ADF&G salmon stat areas. Note it may not
#   have all the stat areas listed on FishTickets. This is important for Chinook.
#   This script also requires the harvest data from OceanAK 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(list = ls(all = TRUE))
setwd("V:/Analysis/1_SEAK/Sockeye/Mixture/Harvest Data")
# This sources all of the new GCL functions to this workspace
# source("C:/Users/krshedd/Documents/R/Functions.GCL.R")
# source("H:/R Source Scripts/Functions.GCL_KS.R")

# Get OceanAK Harvest data
# So far I just have Sockeye 2014-2016
sapply(2013:2017, function(year) {
  
  file <- paste0("Southeast Sockeye Salmon Catch by Day and Stat Area ", year, ".csv")
  harvest <- read.csv(file = file, as.is = TRUE)
  str(harvest)
  
  # Simplify fishery names
  unique(harvest$Fishery.Name)
  fishery.sub <- cbind(c("SALMON, DRIFT GILLNET, SOUTHEAST", 
                         "SALMON, SPECIAL HARVEST AREA <HATCHERY>, SOUTHEAST",
                         "SALMON, PURSE SEINE, SOUTHEAST",
                         "SALMON, POWER TROLL, STATEWIDE",
                         "SALMON, HAND TROLL, STATEWIDE",
                         "SALMON, SET GILLNET, YAKUTAT"),
                       c("Drift", "SHA", "Seine", "Power Troll", "Hand Troll", "Setnet"))
  harvest$Fishery.Simple <- harvest$Fishery.Name
  for(i in 1:6) {
    harvest$Fishery.Simple <- gsub(pattern = fishery.sub[i, 1], replacement = fishery.sub[i, 2], x = harvest$Fishery.Simple)
  }
  
  # Remove random longline, etc. harvest
  harvest <- subset(harvest, Fishery.Simple %in% c("Drift", "SHA", "Seine", "Power Troll", "Hand Troll", "Setnet"))
  str(harvest)
  
  # View harvest summaries
  # aggregate(Number ~ Fishery.Name, data = harvest, sum)
  aggregate(Number ~ Fishery.Simple, data = harvest, sum)
  aggregate(Number ~ District, data = harvest, sum)
  aggregate(Number ~ District + Fishery.Simple, data = harvest, sum)
  
  
  aggregate(Number ~ Stat.Area, data = harvest, sum)
  aggregate(Number ~ Stat.Area + Fishery.Simple, data = harvest, sum)
  
  require(reshape)
  cast(aggregate(Number ~ District + Fishery.Simple, data = harvest, sum), District ~ Fishery.Simple, value = "Number")
  cast(aggregate(Number ~ Stat.Area + Fishery.Simple, data = harvest, sum), Stat.Area ~ Fishery.Simple, value = "Number")
  
  
  require(lattice)
  new.colors <- colorRampPalette(c("white", "black"))
  SEAK_District_Harvest_Fishery.df <- cast(aggregate(Number ~ District + Fishery.Simple, data = harvest, sum), District ~ Fishery.Simple, value = "Number")
  SEAK_District_Harvest_Fishery.df[is.na(SEAK_District_Harvest_Fishery.df)] <- 0
  SEAK_District_Harvest_Fishery.mat <- data.matrix(frame = SEAK_District_Harvest_Fishery.df[, -1])
  rownames(SEAK_District_Harvest_Fishery.mat) <- SEAK_District_Harvest_Fishery.df[, 1]
  SEAK_District_Harvest_Fishery.mat <- cbind(SEAK_District_Harvest_Fishery.mat, "Commercial" = rowSums(SEAK_District_Harvest_Fishery.mat))
  levelplot(t(SEAK_District_Harvest_Fishery.mat), col.regions = new.colors, xlab = "Fishery", ylab = "District", scales = list(x = list(rot = 45)), aspect = "fill")
  
  
  SEAK_StatArea_Harvest_Fishery.df <- cast(aggregate(Number ~ Stat.Area + Fishery.Simple, data = harvest, sum), Stat.Area ~ Fishery.Simple, value = "Number")
  SEAK_StatArea_Harvest_Fishery.df[is.na(SEAK_StatArea_Harvest_Fishery.df)] <- 0
  SEAK_StatArea_Harvest_Fishery.mat <- data.matrix(frame = SEAK_StatArea_Harvest_Fishery.df[, -1])
  rownames(SEAK_StatArea_Harvest_Fishery.mat) <- SEAK_StatArea_Harvest_Fishery.df[, 1]
  SEAK_StatArea_Harvest_Fishery.mat <- cbind(SEAK_StatArea_Harvest_Fishery.mat, "Commercial" = rowSums(SEAK_StatArea_Harvest_Fishery.mat))
  str(SEAK_StatArea_Harvest_Fishery.mat)
  max(SEAK_StatArea_Harvest_Fishery.mat)  # 2013: 117,291; 2014: 265,810; 2015: 323,013; 2016: 212,374; 2017: 134,053
  
  assign(x = paste0("SEAK_StatArea_Harvest_Fishery", year, ".mat"), value = SEAK_StatArea_Harvest_Fishery.mat, pos = 1)
})




## Compute five year average for 2013-2017
# Save years separately
SEAK_StatArea_Harvest_Fishery2013.mat <- SEAK_StatArea_Harvest_Fishery.mat
SEAK_StatArea_Harvest_Fishery2014.mat <- SEAK_StatArea_Harvest_Fishery.mat
SEAK_StatArea_Harvest_Fishery2015.mat <- SEAK_StatArea_Harvest_Fishery.mat
SEAK_StatArea_Harvest_Fishery2016.mat <- SEAK_StatArea_Harvest_Fishery.mat
SEAK_StatArea_Harvest_Fishery2017.mat <- SEAK_StatArea_Harvest_Fishery.mat

rm(SEAK_StatArea_Harvest_Fishery.mat)

# Do all years have the same dimensions? No :(
sapply(paste0("SEAK_StatArea_Harvest_Fishery", 2013:2017, ".mat"), function(yr) {dim(get(yr))} )
yr.stat.areas <- sapply(paste0("SEAK_StatArea_Harvest_Fishery", 2013:2017, ".mat"), function(yr) {rownames(get(yr))} )
names(yr.stat.areas) <- 2013:2017
all.stat.areas <- unique(unlist(sapply(paste0("SEAK_StatArea_Harvest_Fishery", 2013:2017, ".mat"), function(yr) {rownames(get(yr))} )))

# Prepare empty matrix
SEAK_StatArea_Harvest_Fishery13to17.mat <- matrix(data = NA, nrow = length(all.stat.areas), 
                                                  ncol = ncol(SEAK_StatArea_Harvest_Fishery2013.mat), 
                                                  dimnames = list(all.stat.areas, colnames(SEAK_StatArea_Harvest_Fishery2013.mat)))
# Fill in five year average harvest by stat area
for(stat.area in all.stat.areas) {
  yrs <- names(which(sapply(yr.stat.areas, function(yr) {stat.area %in% yr} )))
  SEAK_StatArea_Harvest_Fishery13to17.mat[stat.area, ] <- colMeans(t(sapply(yrs, function(yr) {
    get(paste0("SEAK_StatArea_Harvest_Fishery", yr, ".mat"))[stat.area, ]} 
  )))
}

str(SEAK_StatArea_Harvest_Fishery13to17.mat)
SEAK_StatArea_Harvest_Fishery.mat <- SEAK_StatArea_Harvest_Fishery13to17.mat
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Maps ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
while(!require(maps)) {install.packages("maps")}
while(!require(mapdata)) {install.packages("mapdata")}
while(!require(maptools)) {install.packages("maptools")}
while(!require(GISTools)) {install.packages("GISTools")}
while(!require(rgeos)) {install.packages("rgeos")}
while(!require(sp)) {install.packages("sp")}
while(!require(RColorBrewer)) {install.packages("RColorBrewer")}
while(!require(plotly)) {install.packages("plotly")}
while(!require(PBSmapping)) {install.packages("PBSmapping")}
while(!require(devEMF)) {install.packages("devEMF")}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Read in Raw StatArea Shapefile from the shared drive <U:\Boundaries\CFStats\AllAkCFStats\pvs_stat.shp>, which was out of date (i.e. missing stat areas)
StatAreas.shp <- readShapePoly(fn = "GIS Data/Stat Area/pvs_stat.shp", verbose = TRUE)
str(StatAreas.shp, max.level = 2)
str(StatAreas.shp@data)
StatAreas.shp@proj4string
str(StatAreas.shp@polygons[[1]])

str(StatAreas.shp@data$STAT_AREA)

# Subset shapefile for just SEAK
SEAK_StatAreas <- StatAreas.shp@data$STAT_AREA[StatAreas.shp@data$STAT_AREA %in% as.character(10000:11599)]
SEAKStatAreas.shp <- subset(StatAreas.shp, StatAreas.shp@data$STAT_AREA %in% SEAK_StatAreas)
str(SEAKStatAreas.shp, max.level = 2)
writePolyShape(x = SEAKStatAreas.shp, fn = "GIS Data/Stat Area/SEAKStatAreas.shp")


# Subset shapefile for just D1-4
D1to4_StatAreas <- StatAreas.shp@data$STAT_AREA[StatAreas.shp@data$STAT_AREA %in% as.character(10000:10499)]
D1to4StatAreas.shp <- subset(StatAreas.shp, StatAreas.shp@data$STAT_AREA %in% D1to4_StatAreas)
str(D1to4StatAreas.shp, max.level = 2)
writePolyShape(x = D1to4StatAreas.shp, fn = "GIS Data/Stat Area/D1to4StatAreas.shp")


# Are all the stat areas in SEAK harvest present in the shapefile?
table(unique(harvest$Stat.Area) %in% StatAreas.shp@data$STAT_AREA)
sort(setdiff(unique(harvest$Stat.Area), StatAreas.shp@data$STAT_AREA))

SEAK_StatArea_Harvest_Fishery.mat[as.character(sort(setdiff(unique(harvest$Stat.Area), StatAreas.shp@data$STAT_AREA))), ]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Add stat area level harvest data to shapefile@data
StatAreas.shp@data <- cbind(StatAreas.shp@data, SEAK_StatArea_Harvest_Fishery.mat[match(as.character(StatAreas.shp@data$STAT_AREA), rownames(SEAK_StatArea_Harvest_Fishery.mat)), ])

# Replace NA with 0
table(is.na(StatAreas.shp@data[, 8:14]))
StatAreas.shp@data[, 8:14][is.na(StatAreas.shp@data[, 8:14])] <- 0
str(StatAreas.shp@data)

# writePolyShape(x = StatAreas.shp, fn = "GIS Data/Stat Area/pvs_stat_harvest.shp")

# StatAreas.shp <- readShapePoly("GIS Data/Stat Area/pvs_stat_harvest.shp")
str(StatAreas.shp, max.level = 2)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Plot_StatArea_SEAK_Harvest_Map.f <- function(fishery, max.col = NULL) {
  if(is.null(max.col)) {max.col <- max(StatAreas.shp@data[, fishery])}
  
  map("worldHires", "usa", xlim = c(-137.5, -130), ylim = c(54.5, 59.75), col = "gray90", fill = TRUE)
  color.ramp <- colorRampPalette(c("white", "green", "darkgreen", "black"))(101)
  # color.ramp <- colorRampPalette(c("white", brewer.pal(9, "Greys")))(101)
  plot(StatAreas.shp, add = TRUE, 
       col = color.ramp[round(StatAreas.shp@data[, fishery] / (max.col/100)) + 1],
       border = TRUE)
  legend("bottomleft", 
         legend = c(max.col, rep("", 99), 0),
         fill = rev(color.ramp), 
         border = NA,
         bty = 'n', x.intersp = 0.5, y.intersp = 0.07, lty = NULL)
  text(x = -137.4, y = 55.55, labels = "Harvest", cex = 1.3, adj = c(0, 0.5))
  text(x = -137.4, y = 55.7, labels = fishery, cex = 1.3, adj = c(0, 0.5))
  maps::map.scale(x = -136.8, y = 54.65, ratio = FALSE, relwidth = 0.2)
  north.arrow(xb = -136.7, yb = 54.85, len = 0.05, lab = "N")
}

Plot_StatArea_SEAK_Harvest_Map.f(fishery = "Drift", max.col = 3.25e5)  # max(SEAK_StatArea_Harvest_Fishery.mat)
Plot_StatArea_SEAK_Harvest_Map.f(fishery = "Seine", max.col = 3.25e5)  # max(SEAK_StatArea_Harvest_Fishery.mat)
Plot_StatArea_SEAK_Harvest_Map.f(fishery = "Commercial", max.col = 3.25e5)  # max(SEAK_StatArea_Harvest_Fishery.mat)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Add district level harvest data to shapefile@data
StatAreas.shp@data <- cbind(StatAreas.shp@data, SEAK_District_Harvest_Fishery.mat[match(as.character(StatAreas.shp@data$DISTRICT), rownames(SEAK_District_Harvest_Fishery.mat)), ])

# Replace NA with 0
table(is.na(StatAreas.shp@data[, 8:14]))
StatAreas.shp@data[, 8:14][is.na(StatAreas.shp@data[, 8:14])] <- 0
str(StatAreas.shp@data)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Plot_District_SEAK_Harvest_Map.f <- function(fishery, max.col = NULL) {
  if(is.null(max.col)) {max.col <- max(StatAreas.shp@data[, fishery])}
  
  map("worldHires", "usa", xlim = c(-137.5, -130), ylim = c(54.5, 59.75), col = "gray90", fill = TRUE)
  plot(StatAreas.shp, add = TRUE, 
       col = colorRampPalette(c("white", "green", "darkgreen"))(101)[round(StatAreas.shp@data[, fishery] / (max.col/100)) + 1],
       border = TRUE)
  maps::map.scale(x = -137, y = 55, ratio = FALSE, relwidth = 0.2)
  north.arrow(xb = -137, yb = 55.5, len = 0.05, lab = "N")
}

Plot_District_SEAK_Harvest_Map.f(fishery = "Drift", max.col = max(SEAK_District_Harvest_Fishery.mat))
Plot_District_SEAK_Harvest_Map.f(fishery = "Seine", max.col = max(SEAK_District_Harvest_Fishery.mat))
Plot_District_SEAK_Harvest_Map.f(fishery = "Commercial", max.col = max(SEAK_District_Harvest_Fishery.mat))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### PBS Mapping Package ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
while(!require(PBSmapping)) {install.packages("PBSmapping")}

# NOTE: This package requires basemap files to be located in your 'PBSmapping' directory, follow instructions here <https://thedescrambler.wordpress.com/2012/09/15/draw-maps-in-r/>
# Test basemap of SEAK with Rivers and Borders
land <- importGSHHS("C:/Users/krshedd/Documents/R/win-library/3.3/PBSmapping/gshhs_h.b", xlim = c(222.5, 230) , ylim = c(54.5, 59.75) , maxLevel = 4, useWest = TRUE)
plotMap(land, col = "beige")

rivers <- importGSHHS("C:/Users/krshedd/Documents/R/win-library/3.3/PBSmapping/wdb_rivers_f.b", xlim = c(222.5, 230) , ylim = c(54.5, 59.75), useWest = TRUE)
addLines(polys = rivers, col = "lightblue", lwd = 2)

borders <- importGSHHS("C:/Users/krshedd/Documents/R/win-library/3.3/PBSmapping/wdb_borders_f.b", xlim = c(222.5, 230) , ylim = c(54.5, 59.75), useWest = TRUE)
addLines(polys = borders, col = "black", lwd = 2)

# Read in stat area shapefile
# StatAreasPBS.shp <- importShapefile(fn = "GIS Data/Stat Area/pvs_stat.shp")
StatAreasPBS.shp <- importShapefile(fn = "GIS Data/Stat Area/SEAKStatAreas.shp")
str(StatAreasPBS.shp, max.level = 2)
str(attributes(StatAreasPBS.shp))
str(attributes(StatAreasPBS.shp)[["PolyData"]])
attributes(StatAreasPBS.shp)[["PolyData"]] <- cbind(attributes(StatAreasPBS.shp)[["PolyData"]], SEAK_StatArea_Harvest_Fishery.mat[match(as.character(attributes(StatAreasPBS.shp)[["PolyData"]]$STAT_AREA), rownames(SEAK_StatArea_Harvest_Fishery.mat)), ])
# attributes(StatAreasPBS.shp)[["PolyData"]] <- cbind(attributes(StatAreasPBS.shp)[["PolyData"]], SEAK_StatArea_Harvest_Fishery13to17.mat[match(as.character(attributes(StatAreasPBS.shp)[["PolyData"]]$STAT_AREA), rownames(SEAK_StatArea_Harvest_Fishery13to17.mat)), ])
attributes(StatAreasPBS.shp)[["PolyData"]][, 9:15][is.na(attributes(StatAreasPBS.shp)[["PolyData"]][, 9:15])] <- 0

# Read in dsistrict shapefile
DistrictsPBS.shp <- importShapefile(fn = "GIS Data/District/pvs_dist.shp")
str(DistrictsPBS.shp)
addPolys(polys = DistrictsPBS.shp, border = "black")

# Transform longitude from -180:180 to 0:360 if not using "useWest" logical in import GSHHS
# str(StatAreasPBS.shp$X)
# StatAreasPBS.shp$X <- StatAreasPBS.shp$X + 360

# plot(StatAreasPBS.shp, add = TRUE)  # Fail
addPolys(polys = StatAreasPBS.shp, border = "black", col = color.ramp[round(attributes(StatAreasPBS.shp)[["PolyData"]][, "Drift"] / (max.col/100)) + 1])


# Read in waterbody shapefile
WaterbodyPBS.shp <- importShapefile(fn = "GIS Data/Waterbody/Waterbody.shp")
str(WaterbodyPBS.shp)
addPolys(polys = WaterbodyPBS.shp, boder = "black", col = "white", xlim = c(-137.5, -130), ylim = c(54.5, 59.75))

# Read in Canadian waterbody shapefile NOTE: it is massive! and also in mercator projection.
# CanadianWaterbodyPBS.shp <- importShapefile(fn = "GIS Data/Canada Waterbody/waterbody.shp")
# str(CanadianWaterbodyPBS.shp)
# addPolys(polys = StatAreasPBS.shp, col = "white", xlim = c(222.5, 230), ylim = c(54.5, 59.75))

# Read in rivers shapefile
RiversPBS.shp <- importShapefile(fn = "GIS Data/Rivers/mv_hydro_2mil_ln.shp")
str(RiversPBS.shp)
addLines(polys = RiversPBS.shp, col = "gray50", lwd = 1)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Replicate Harvest Map Function
Plot_StatArea_SEAK_Harvest_MapPBS.f <- function(fishery, max.col = NULL, xmin = 222.5, xmax = 230, ymin = 54.5, ymax = 59.75) {
  if(is.null(max.col)) {max.col <- max(attributes(StatAreasPBS.shp)[["PolyData"]][, fishery])}
  
  land <- importGSHHS("C:/Users/krshedd/Documents/R/win-library/3.3/PBSmapping/gshhs_h.b", xlim = c(xmin, xmax), ylim = c(ymin, ymax) , maxLevel = 4, useWest = TRUE)
  plotMap(land, col = "gray90")
  
  # rivers <- importGSHHS("C:/Users/krshedd/Documents/R/win-library/3.3/PBSmapping/wdb_rivers_f.b", xlim = c(xmin, xmax), ylim = c(ymin, ymax), useWest = TRUE)
  # addLines(polys = rivers, col = "gray50", lwd = 2)
  
  borders <- importGSHHS("C:/Users/krshedd/Documents/R/win-library/3.3/PBSmapping/wdb_borders_f.b", xlim = c(xmin, xmax), ylim = c(ymin, ymax), useWest = TRUE)
  addLines(polys = borders, col = "black", lwd = 2)
  
  color.ramp <- colorRampPalette(c("white", "green", "darkgreen", "black"))(101)
  # color.ramp <- colorRampPalette(c("white", brewer.pal(9, "Greys")))(101)
  
  addPolys(polys = StatAreasPBS.shp, border = "black", col = color.ramp[round(attributes(StatAreasPBS.shp)[["PolyData"]][, fishery] / (max.col/100)) + 1])
  
  legend("bottomleft",
         legend = c(max.col, rep("", 99), 0),
         fill = rev(color.ramp),
         border = NA,
         bty = 'n', x.intersp = 0.5, y.intersp = 0.07, lty = NULL, cex = 0.7)
  text(x = -137.3, y = 56.05, labels = "Harvest", cex = 1.3, adj = c(0, 0.5))  #  x = 55.65
  text(x = -137.3, y = 56.2, labels = fishery, cex = 1.3, adj = c(0, 0.5))  # x = 55.8
}

Plot_StatArea_SEAK_Harvest_MapPBS.f(fishery = "Drift", max.col = 2.2e5)  # max(SEAK_StatArea_Harvest_Fishery.mat)
Plot_StatArea_SEAK_Harvest_MapPBS.f(fishery = "Seine", max.col = 2.2e5)  # max(SEAK_StatArea_Harvest_Fishery.mat)
Plot_StatArea_SEAK_Harvest_MapPBS.f(fishery = "SHA", max.col = 2.2e5)  # max(SEAK_StatArea_Harvest_Fishery.mat)
Plot_StatArea_SEAK_Harvest_MapPBS.f(fishery = "Commercial", max.col = 1.5e5)  # max(SEAK_StatArea_Harvest_Fishery.mat)

setwd("V:/Presentations/Regional/1_SEAK/Sockeye/2017 April Port Sampler Meeting/Figures/")

png(filename = "2016 Drift Harvest.png", width = 1100, height = 1400, res = 180)
Plot_StatArea_SEAK_Harvest_MapPBS.f(fishery = "Drift", max.col = 2.2e5)  # max(SEAK_StatArea_Harvest_Fishery.mat)
dev.off()

png(filename = "2016 Seine Harvest.png", width = 1100, height = 1400, res = 180)
Plot_StatArea_SEAK_Harvest_MapPBS.f(fishery = "Seine", max.col = 2.2e5)  # max(SEAK_StatArea_Harvest_Fishery.mat)
dev.off()

png(filename = "2016 Commercial Harvest.png", width = 1100, height = 1400, res = 180)
Plot_StatArea_SEAK_Harvest_MapPBS.f(fishery = "Commercial", max.col = 2.2e5)  # max(SEAK_StatArea_Harvest_Fishery.mat)
dev.off()





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### PBS Mapping D1-4 ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## NOTE: It is very important that the xtent of the map shows ALL stat areas
## If you want to restrict the map extent (zoom in), you'll need to subset the 
## shapefile to show just the stat areas you want, otherwise you will get
## inaccurate results. The shapes are drawn in order and will bomb out once a
## shape is not on the extent, potentially leaving you with white space in a
## subsequent stat area that actually had harvest!!!

StatAreasPBS.shp <- importShapefile(fn = "GIS Data/Stat Area/D1to4StatAreas.shp")
str(StatAreasPBS.shp, max.level = 2)
str(attributes(StatAreasPBS.shp))
str(attributes(StatAreasPBS.shp)[["PolyData"]])
attributes(StatAreasPBS.shp)[["PolyData"]] <- cbind(attributes(StatAreasPBS.shp)[["PolyData"]], SEAK_StatArea_Harvest_Fishery.mat[match(as.character(attributes(StatAreasPBS.shp)[["PolyData"]]$STAT_AREA), rownames(SEAK_StatArea_Harvest_Fishery.mat)), ])
# attributes(StatAreasPBS.shp)[["PolyData"]] <- cbind(attributes(StatAreasPBS.shp)[["PolyData"]], SEAK_StatArea_Harvest_Fishery13to17.mat[match(as.character(attributes(StatAreasPBS.shp)[["PolyData"]]$STAT_AREA), rownames(SEAK_StatArea_Harvest_Fishery13to17.mat)), ])
attributes(StatAreasPBS.shp)[["PolyData"]][, 9:15][is.na(attributes(StatAreasPBS.shp)[["PolyData"]][, 9:15])] <- 0

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Replicate Harvest Map Function
Plot_StatArea_SEAK_Harvest_MapPBS.f <- function(fishery, max.col = NULL, xmin = 222.5, xmax = 230, ymin = 54.5, ymax = 59.75) {
  if(is.null(max.col)) {max.col <- max(attributes(StatAreasPBS.shp)[["PolyData"]][, fishery])}
  
  land <- importGSHHS("C:/Users/krshedd/Documents/R/win-library/3.3/PBSmapping/gshhs_h.b", xlim = c(xmin, xmax), ylim = c(ymin, ymax) , maxLevel = 4, useWest = TRUE)
  plotMap(land, col = "gray90")
  
  # rivers <- importGSHHS("C:/Users/krshedd/Documents/R/win-library/3.3/PBSmapping/wdb_rivers_f.b", xlim = c(xmin, xmax), ylim = c(ymin, ymax), useWest = TRUE)
  # addLines(polys = rivers, col = "gray50", lwd = 2)
  
  borders <- importGSHHS("C:/Users/krshedd/Documents/R/win-library/3.3/PBSmapping/wdb_borders_f.b", xlim = c(xmin, xmax), ylim = c(ymin, ymax), useWest = TRUE)
  addLines(polys = borders, col = "black", lwd = 2)
  
  color.ramp <- colorRampPalette(c("white", "green", "darkgreen", "black"))(101)
  # color.ramp <- colorRampPalette(c("white", brewer.pal(9, "Greys")))(101)
  
  addPolys(polys = StatAreasPBS.shp, border = "black", col = color.ramp[round(attributes(StatAreasPBS.shp)[["PolyData"]][, fishery] / (max.col/100)) + 1])
  
  legend("bottomleft",
         legend = c(max.col, rep("", 99), 0),
         fill = rev(color.ramp),
         border = NA,
         bty = 'n', x.intersp = 0.5, y.intersp = 0.07, lty = NULL, cex = 0.7)
  text(x = -133.95, y = 55.05, labels = "Harvest", cex = 0.8, adj = c(0, 0.5))  #  x = 55.65
  text(x = -133.95, y = 55.12, labels = fishery, cex = 0.8, adj = c(0, 0.5))  # x = 55.8
}

# dir.create("Figures")
png(filename = "Figures/2013-17 Avg Commercial Harvest.png", width = 1100, height = 1400, res = 180)
Plot_StatArea_SEAK_Harvest_MapPBS.f(fishery = "Commercial", max.col = 1.5e5, xmin = 226, ymax = 56.2)  # max(SEAK_StatArea_Harvest_Fishery.mat)
dev.off()



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
png(filename = "Figures/Districts 3 and 4.png", width = 1100, height = 1400, res = 180)

xmin = 226; xmax = 230; ymin = 54.5; ymax = 56.2

land <- importGSHHS("C:/Users/krshedd/Documents/R/win-library/3.3/PBSmapping/gshhs_h.b", xlim = c(xmin, xmax), ylim = c(ymin, ymax) , maxLevel = 4, useWest = TRUE)
plotMap(land, col = "gray90")

# rivers <- importGSHHS("C:/Users/krshedd/Documents/R/win-library/3.3/PBSmapping/wdb_rivers_f.b", xlim = c(xmin, xmax), ylim = c(ymin, ymax), useWest = TRUE)
# addLines(polys = rivers, col = "gray50", lwd = 2)

borders <- importGSHHS("C:/Users/krshedd/Documents/R/win-library/3.3/PBSmapping/wdb_borders_f.b", xlim = c(xmin, xmax), ylim = c(ymin, ymax), useWest = TRUE)
addLines(polys = borders, col = "black", lwd = 2)

addPolys(polys = StatAreasPBS.shp, border = "black", col = c("white", "white", "red", "blue")[as.numeric(attributes(StatAreasPBS.shp)[["PolyData"]][, "DISTRICT"])] )

legend("bottomleft",
       legend = c("D103", "D104"),
       fill = c("red", "blue"),
       border = NA,
       bty = 'n', cex = 1)
dev.off()
