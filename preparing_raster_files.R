test <- 
  raster('C:/Users/EvansBr/Dropbox (Smithsonian)/leaflet_biomes/biomeRas')

test_crop <-
  crop(test, extent(biomes_rat))

# Set water to NA

test_crop[test_crop == 4] <- NA

# Set values, arrangubg from 1 to 10

test_crop[test_crop == 8] <- 31         # Rock and ice
test_crop[test_crop == 16] <- 32        # Tundra
test_crop[test_crop == 1] <- 33         # Boreal
test_crop[test_crop == 3] <- 34         # Wetland
test_crop[test_crop %in% c(9,10)] <- 35 # Temperate
test_crop[test_crop %in% c(7,11)] <- 36 # Grassland, savanna and shrub
test_crop[test_crop == 6] <- 37         # Med forest
test_crop[test_crop == 2] <- 38         # Desert and dry shrubland
test_crop[test_crop %in% c(12,13,14,15)] <- 39 # Tropical and subtropical forest
test_crop[test_crop == 5] <- 40         # Mangrove

# Subtract 30 so values are between 1 and 10

test_crop[] <- test_crop[]-30

bioColors <- c("#9E0142", "#D9444E", "#F77E4A", "#FEC171", "#F3EB91", "#C2E6A0",
               "#79C9A5", "#3D8FBA", "#3D64BA", "#5E4FA2")

plot(test_crop, col = rev(bioColors))

test_crop %>%
  KML('biomesBig_proj.kml', col =rev(bioColors), maxpixels = Inf, blur = 10, overwrite = TRUE)

test_crop_proj <-
  test_crop %>%
  projectRaster(crs = '+init=EPSG:3857 +proj=longlat') %>%
  KML('biomesBig_proj.kml', col =rev(bioColors), maxpixels = Inf, blur = 10, overwrite = TRUE)
    
test_crop %>%
  projectRaster(crs = '+init=EPSG:3857 +proj=longlat') %>%
  

# Agricultural intensity map:
  
  bigAg <-
  raster('agIntensity33') %>%
  projectRaster(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

bigAg[] <- floor(bigAg[])

bigAg[bigAg == 0] <- NA

plot(bigAg, col = colorRamps::matlab.like(10))

bigAg_proj <-
  bigAg %>%
  projectRaster(crs = '+init=EPSG:3857 +proj=longlat')



KML(bigAg, 'bigAg.kml', col = colorRamps::matlab.like(10), maxpixels = Inf, blur = 10, overwrite = TRUE)

bigAg

KML(bigAg_proj, 'bigAg_proj.kml', col = colorRamps::matlab.like(10), maxpixels = Inf, blur = 10, overwrite = TRUE)



# potentially junk --------------------------------------------------------

library(raster)
library(tidyverse)

biomeR <-
  raster('biomes.tif')

r <- focal(biomeR, Tps)

r <-
  focal(
    biomeR,
    w = matrix(1/25,nrow=5,ncol=5), fun = mode, pad=FALSE, padValue=NA, NAonly=FALSE)

raster('biomes.tif') %>% plot

# Try epsg 4326 ------------------------------------------------------------

bigAg_proj4326 <-
  bigAg_proj %>%
  projectRaster(crs = '+init=EPSG:4326 +proj=longlat')

KML(bigAg_proj4326, 'bigAg_proj4326.kml', col = colorRamps::matlab.like(10), maxpixels = Inf, blur = 10, overwrite = TRUE)

