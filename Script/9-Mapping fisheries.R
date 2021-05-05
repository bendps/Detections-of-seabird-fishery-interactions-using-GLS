
#~~~~~~#
library(furrr); library(tidyverse); library(lubridate); library(maps); library(maptools)
library(maptools);library(raster); library(rgeos); library(adehabitatHR); library(rgdal)
library(viridis); library(RColorBrewer); library(ggsci); library(cowplot) ; library(svglite)
library(sf) ; library(ggspatial) ; library(rnaturalearth) ; library(rnaturalearthdata)
#~~~~~~#

#Classic bw theme of ggplot2
theme_set(theme_bw())

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# STEP1: prepare CRS strings 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Load position and shapefile
world <- ne_countries(scale = "medium", returnclass = "sf") 
positions <- readRDS("Data/Fisheries/2012-2016_filtered_loc.rds") 
zones <- readOGR("Data/Zones/My_zones.shp") 
mypositions <- positions

#Rename columns
mypositions <- mypositions %>% rename( lon = lon_bin)
mypositions <- mypositions %>% rename( lat = lat_bin)

#Ref grid
ref <- data.frame(c(-70,70),c(48,85))
names(ref) <- c("lon","lat")

# Prepare CRS Strings
# CRS for WGS84
proj.latlon <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# Center projection on the centroid of our observations
medlon <- median(ref$lon)
medlat <- median(ref$lat)

# CRS string 
proj.aeqd <- paste("+proj=aeqd +lat_0=",round(medlat), " +lon_0=",round(medlon)," +units=km ", sep="")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# STEP 2: create a SpatialPointsDataFrame with positions 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Transforms to a SpatialPointsDataFrame
coordinates(mypositions) <- ~ lon + lat

# Define CRS
proj4string(mypositions) <- CRS(proj.latlon)

# Project the data
mypositions <- spTransform(mypositions, CRS(proj.aeqd))


# Project the World SpatialPolygon
zones <- spTransform(zones, CRS(proj.aeqd))

# Plot points
mypositions.df <- data.frame(mypositions) #Important because ggplot doesn't like spatial points


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# STEP 3: create a raster 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Create raster of the studied area, res = size of cell, in km here
#r <- raster(xmn=-4000, ymn=-2000, xmx=2000, ymx=2500, res=200)
r <- raster(xmn=-4500, ymn=-2500, xmx=2000, ymx=3000, res=200)

# Raster of the number of positions per cell
r.nb <- rasterize(mypositions, r, field = mypositions$fishing_hours, fun='sum')
saveRDS(r.nb, paste0("Data/Fisheries/raster_global_fisheries.rds")) 

# plot ggplot of the fisheries, need to go back to data.frame
df.nb <- as.data.frame(r.nb,xy=T)
colnames(df.nb)[which(names(df.nb) == "layer")] <- "myvariable"
df.nb$myvariable[which(is.na(df.nb$myvariable))] <- 0


plot2C <- ggplot(data = world) +
  geom_raster(data = df.nb, aes(x = x, y = y,fill=myvariable)) +  
  geom_polygon(data=zones, aes(x=long, y=lat, group=group), colour = "#333333", fill = NA) +
  geom_sf() +
  annotation_scale(location = "bl", width_hint = 0.25, text_cex = 1.3) +
  coord_sf(crs = proj.aeqd, xlim = c(-4500,2000), ylim = c(-2500,3000), expand = F) +
  scale_fill_gradientn(colors = c("#FFFFFF00",rev(viridis(20))),
                       labels = c(0, "2.5e+05", "5e+05", "7.5e+05", "1e+06")) +
  labs(fill = "Sum of\nfishing hours",
       title = "C.") +
  scale_x_discrete(name = "Longitude",
                   breaks = seq(from = -200, to = 200, by = 10),
                   drop = FALSE) +
  scale_y_discrete(name = "Latitude",
                   breaks = seq(from = 30, to = 100, by = 5),
                   drop = FALSE) +
  theme(text = element_text(size = 30),
        panel.grid = element_line(color = "#C4C4C4"))

plot2C

ggsave(filename = "fig2C.svg", units = "in", width = 16, height = 9)
