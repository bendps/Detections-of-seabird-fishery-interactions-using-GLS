rm(list = ls())
#Your WD #!!#
setwd("C:/Users/dupui/Documents/Internship_M1/Shared_method_seabirds-fisheries")

#~~~~~~#
library(ggrepel) ; library(viridis)
library(cowplot) ; library(raster)
library(dplyr) ; library(ggplot2)
library(sp) ; library(rgdal)
library(sf) ; library(ggspatial) ;
library(rnaturalearth) ; library(rnaturalearthdata)
#~~~~~~#

#Classic bw theme of ggplot2
theme_set(theme_bw())

# Prepare world ----
zones <- readOGR("Data/Zones/My_zones.shp") #Change to the path were the polygones defining your zones are #!!#
world <- ne_countries(scale = "medium", returnclass = "sf")
proj.latlon <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# Center projection on the centroid of our observations
medlon <- 0
medlat <- 66.5

# CRS string 
proj.aeqd <- paste("+proj=aeqd +lat_0=",round(medlat), " +lon_0=",round(medlon)," +units=km ", sep="")

# Project the World SpatialPolygon
zones <- spTransform(zones, CRS(proj.aeqd))

# Mapping ----
# Load birds data
global_birds <- readRDS("Data/Fulmar/raster_global_birds.rds") #Change to the output path of script 10.

# Load fisheries data
global_vessel <- readRDS("Data/Fisheries/raster_global_fisheries.rds") #Change to the output path of script 9. (folder with fisheries data)

# Overlap percentage raster
r.nb <- global_birds*global_vessel
r.nb <- r.nb/maxValue(r.nb) * 100

# Back to df for ggplot
df.nb <- as.data.frame(r.nb,xy=T)
colnames(df.nb)[which(names(df.nb) == "layer")] <- "myvariable"
df.nb$myvariable[which(is.na(df.nb$myvariable))] <- 0

plot2D <- ggplot(data = world) +
  geom_raster(data = df.nb, aes(x = x, y = y,fill=myvariable)) +    
  geom_polygon(data=zones, aes(x=long, y=lat, group=group), colour = "#333333", fill = NA) +
  geom_sf() +
  annotation_scale(location = "bl", width_hint = 0.25, text_cex = 1.3) +
  coord_sf(crs = proj.aeqd, xlim = c(-4500,2000), ylim = c(-2500,3000), expand = F) + 
  scale_fill_gradientn(colors = c("#FFFFFF00",rev(terrain.colors(100))))+
  labs(fill = "Overlap\nindex (%)",
       title = "D.") +
  scale_x_discrete(name = "Longitude",
                   breaks = seq(from = -200, to = 200, by = 10),
                   drop = FALSE) +
  scale_y_discrete(name = "Latitude",
                   breaks = seq(from = 30, to = 100, by = 5),
                   drop = FALSE) +
  theme(text = element_text(size = 30),
        panel.grid = element_line(color = "#C4C4C4"))

plot(plot2D)

ggsave(filename = "fig2D.svg", units = "in", width = 16, height = 9)
