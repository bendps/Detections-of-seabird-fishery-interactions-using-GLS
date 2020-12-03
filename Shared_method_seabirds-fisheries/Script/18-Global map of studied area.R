rm(list=ls())
#your WD #!!#
setwd("C:/Users/dupui/Documents/Internship_M1/Shared_method_seabirds-fisheries")

#~~~~~~#
library(sf) ; library(ggplot2) ; library(ggrepel)
library(ggspatial) ; library(rnaturalearth) ; library(rnaturalearthdata)
library(sp) ; library(rgdal) ; library(raster)
library(dplyr) ; library(viridis)
#~~~~~~#

#Classic bw theme of ggplot2
theme_set(theme_bw())

#Loading data
world <- ne_countries(scale = "medium", returnclass = "sf")
zones <- readOGR("Data/Zones/My_zones.shp")
colonies <- readRDS("Data/Fulmar/colony_coordinates.rds")

colonies <- colonies %>% dplyr::filter(colony %in% c("Alkefjellet",
                                                     "Bjørnøya",
                                                     "Breidafjordur",
                                                     "Eynhallow",
                                                     "Faroe Islands",
                                                     "Jan Mayen",
                                                     "Jarsteinen",
                                                     "Langanes and Skjalfandi",
                                                     "Papey"))

colonies$colony[which(colonies$colony == "Breidafjordur")] <- "Breidafjordur & Reykjanes"
colonies$colony[which(colonies$colony == "Langanes and Skjalfandi")] <- "Langanes & S. & Grimsey"
colonies$colony[which(colonies$colony == "Papey")] <- "Papey & Hólmanes"

# Prepare CRS Strings
# CRS for WGS84
proj.latlon <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# Center projection on the centroid of our observations
medlon <- 0
medlat <- 66.5

# CRS string 
proj.aeqd <- paste("+proj=aeqd +lat_0=",round(medlat), " +lon_0=",round(medlon)," +units=km ", sep="")

# Transforms to a SpatialPointsDataFrame
coordinates(colonies) <- ~ col_lon + col_lat

# Define CRS
proj4string(colonies) <- CRS(proj.latlon)

# Project the data
colonies <- spTransform(colonies, CRS(proj.aeqd))

# Project the SpatialPolygon
zones <- spTransform(zones, CRS(proj.aeqd))

#Mapping the world
colonies.df <- data.frame(colonies) #Important because ggplot doesn't like spatial points
colonies.df <- colonies.df[order(colonies.df$colony),]

shapescale <- c(rep(21,3),rep(24,3),rep(23,3))
fillscale <- rep(viridis(3),3)

zones_names <- st_as_sf(zones)
zones_names <- cbind(zones_names, st_coordinates(st_centroid(zones_names)))
zones_names$LME_NAME <- as.character(zones_names$LME_NAME)
zones_names$LME_NAME[2] <- "Iceland"
zones_names$LME_NAME[7] <- "Canadian Eastern Arctic"
zones_names$LME_NAME <- as.factor(zones_names$LME_NAME)
zones_names$nudge_y <- 0
zones_names$nudge_y[zones_names$LME_NAME == "Iceland"] <- 200
zones_names$nudge_y[zones_names$LME_NAME == "Norwegian Sea"] <- -150
zones_names$nudge_y[zones_names$LME_NAME == "Faroe Plateau"] <- -100

ggplot(data = world) +
  geom_polygon(data=zones, aes(x=long, y=lat, group=group), colour = "#333333", fill = "#9CCCD91A") +
  geom_point(data = colonies.df, aes(x = col_lon, y = col_lat, fill = colony, shape = colony),
             size = 7) +
  scale_fill_manual(values = fillscale)+
  scale_shape_manual(values = shapescale)+
  geom_sf() +
  geom_label(data = zones_names, aes(X, Y, label = LME_NAME),
             size = 7,
             nudge_y = zones_names$nudge_y) +
  geom_point(data = colonies.df, aes(x = col_lon, y = col_lat, fill = colony, shape = colony),
             size = 7,
             fill = fillscale,
             shape = shapescale) +
  annotation_scale(location = "bl", width_hint = 0.25, text_cex = 1.3) +
  coord_sf(crs = proj.aeqd, xlim = c(-4500,2000), ylim = c(-2500,3000), expand = F) + 
  scale_x_discrete(name = "Longitude",
                   breaks = seq(from = -200, to = 200, by = 10),
                   drop = FALSE) +
  scale_y_discrete(name = "Latitude",
                   breaks = seq(from = 30, to = 100, by = 5),
                   drop = FALSE) + 
  labs(fill = "Colony", shape = "Colony")  + 
  theme(text = element_text(size = 30),
        panel.grid = element_line(color = "#C4C4C4"))

ggsave(filename = "fig1.svg", units = "in", width = 16, height = 9)

theme(text = element_text(size = 10),
      legend.key = element_rect(size = 7),
      legend.key.size = unit(2, 'lines'))
