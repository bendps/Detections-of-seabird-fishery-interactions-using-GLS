rm(list = ls())
#Your WD #!!#
setwd("C:/Users/dupui/Desktop/Shared_method_seabirds-fisheries")

#~~~~~~#
library(ggrepel) ; library(ggmap) ; library(viridis)
require(maps); library(cowplot) ; library(maptools)
library(dplyr) ; library(ggplot2)
library(sp) ; library(rgdal) ; library(raster)
library(sf) ; library(ggspatial) ; library(rnaturalearth) ; library(rnaturalearthdata)
#~~~~~~#

#Classic bw theme of ggplot2
theme_set(theme_bw())

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# STEP1: prepare CRS strings 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Load position and shapefile
zones <- readOGR("Data/Zones/My_zones.shp") #Change to the path were the polygones defining your zones are #!!#
world <- ne_countries(scale = "medium", returnclass = "sf")
positions <- readRDS("Data/Fulmar/encounters_final_global.rds") #!!# encounters positions
globalpos <- readRDS("Data/Fulmar/Output_GLS_&_IRMA_locations_Fulmarus_glacialis_2020-04-30.rds") #!!# all birds positions

# Prepare CRS Strings
# CRS for WGS84
proj.latlon <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# Center projection on the centroid of our observations
medlon <- 0
medlat <- 66.5

# CRS string 
proj.aeqd <- paste("+proj=aeqd +lat_0=",round(medlat), " +lon_0=",round(medlon)," +units=km ", sep="")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# STEP 2: create a SpatialPointsDataFrame with positions 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Transforms to a SpatialPointsDataFrame
coordinates(positions) <- ~ lon + lat
coordinates(globalpos) <- ~ lon + lat

# Define CRS
proj4string(positions) <- CRS(proj.latlon)
proj4string(globalpos) <- CRS(proj.latlon)

# Project the data
positions <- spTransform(positions, CRS(proj.aeqd))
globalpos <- spTransform(globalpos, CRS(proj.aeqd))

# Project the World SpatialPolygon
zones <- spTransform(zones, CRS(proj.aeqd))

# Plot points
positions.df <- data.frame(positions) #Important because ggplot doesn't like spatial points
positions.df$optional <- "Light detections"

# Create raster of the studied area, res = size of cell, in km here
r <- raster(xmn=-4500, ymn=-2500, xmx=2000, ymx=3000, res=200)

# Raster of the number of positions per cell
r.nb <- rasterize(globalpos, r, field = globalpos$species , fun='count')  # field can be any column with fun="count"

# plot ggplot, il faut repasser en data.frame
df.nb <- as.data.frame(r.nb,xy=T)
colnames(df.nb)[which(names(df.nb) == "layer")] <- "myvariable"
df.nb$myvariable[which(is.na(df.nb$myvariable))] <- 0

plot2A <- ggplot(data = world) +
  geom_raster(data = df.nb, aes( x = x, y = y, fill = myvariable)) +
  scale_fill_gradientn(colors = c("#FFFFFF00","#E1E1E1",rev(plasma(30))))+
  geom_polygon(data=zones, aes(x=long, y=lat, group=group), colour = "#333333", fill = NA) +
  geom_sf() +
  geom_point(data=positions.df, aes(x=lon, y=lat, shape = optional), colour = "#440154FF", size=1) +
  annotation_scale(location = "bl", width_hint = 0.25, text_cex = 1.3) +
  coord_sf(crs = proj.aeqd, xlim = c(-4500,2000), ylim = c(-2500,3000), expand = F) + 
  labs(shape = "",
       fill = "Sum of\npositions",
       title = "A.") +
  scale_x_discrete(name = "Longitude",
                   breaks = seq(from = -200, to = 200, by = 10),
                   drop = FALSE) +
  scale_y_discrete(name = "Latitude",
                   breaks = seq(from = 30, to = 100, by = 5),
                   drop = FALSE) + 
  theme(text = element_text(size = 30),
        panel.grid = element_line(color = "#C4C4C4"))

plot(plot2A)

ggsave(filename = "fig2A.svg", units = "in", width = 16, height = 9)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# STEP 3: create a raster 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Create raster of the studied area, res = size of cell, in km here
r <- raster(xmn=-4500, ymn=-2500, xmx=2000, ymx=3000, res=200)

# Raster of the number of positions per cell
r.nb <- rasterize(positions, r, field = positions$NEnorm , fun='sum')
saveRDS(r.nb, paste0("Data/Fulmar/raster_global_birds",".rds")) #Change to the desired output path #!!#

# Quick plot
plot(r.nb)
plot(world,add=T)
plot(zones,add=T)

# plot ggplot, go back in data.frame
df.nb <- as.data.frame(r.nb,xy=T)
colnames(df.nb)[which(names(df.nb) == "layer")] <- "myvariable"
df.nb$myvariable[which(is.na(df.nb$myvariable))] <- 0

plot2B <- ggplot(data = world) +
  geom_raster(data = df.nb, aes(x = x, y = y,fill=myvariable)) +    
  geom_polygon(data=zones, aes(x=long, y=lat, group=group), colour = "#333333", fill = NA) +
  geom_sf() +
  annotation_scale(location = "bl", width_hint = 0.25, text_cex = 1.3) +
  coord_sf(crs = proj.aeqd, xlim = c(-4500,2000), ylim = c(-2500,3000), expand = F) + 
  scale_fill_gradientn(colors = c("#FFFFFF00",heat.colors(50, rev = T)),
                       breaks = c(0, 500, 1000, 1500),
                       labels = c(0, 500, 1000, 1500))+
  labs(fill = "Sum of WD",
       title = "B.") +
  scale_x_discrete(name = "Longitude",
                   breaks = seq(from = -200, to = 200, by = 10),
                   drop = FALSE) +
  scale_y_discrete(name = "Latitude",
                   breaks = seq(from = 30, to = 100, by = 5),
                   drop = FALSE) +
  theme(text = element_text(size = 30),
        panel.grid = element_line(color = "#C4C4C4"))

plot(plot2B)

ggsave(filename = "fig2B.svg", units = "in", width = 16, height = 9)
