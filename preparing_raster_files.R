test <- 
  raster('C:/Users/EvansBr/Dropbox (Smithsonian)/leaflet_biomes/biomeRas')

test <- 
  raster('C:/Users/Guest user/Dropbox (Smithsonian)/leaflet_biomes/biomeRas')

biomes <- raster('biomes.tif')

test_crop <-
  crop(test, extent(biomes))

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

biomes_proj <-
  test_crop %>%
  projectRaster(crs = '+init=EPSG:4326 +proj=longlat')
  
  projectRaster(crs = '+init=EPSG:3857')


biome_crop <-
  biomes_proj %>%
  crop(extent(c(-168, -13.238066, -56.44362, 84))) %>%
  aggregate(2, modal)

biome_crop[] <- floor(biome_crop[])


KML(biomes_proj, 'biomes_proj4326.kml', col =rev(bioColors), maxpixels = 10000000, blur = 5, overwrite = TRUE)

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



# Try epsg 4326 ------------------------------------------------------------

bigAg_proj4326 <-
  bigAg_proj %>%
  projectRaster(crs = '+init=EPSG:4326 +proj=longlat')

KML(bigAg_proj4326, 'bigAg_proj4326.kml', col = colorRamps::matlab.like(10), maxpixels = Inf, blur = 10, overwrite = TRUE)

biomeR <-
  raster('biomes.tif')


biome_proj4326 <-
  biomeR %>%
  projectRaster(crs = '+init=EPSG:4326 +proj=longlat')


KML(biome_proj4326, 'biome_proj4326.kml', col = colorRamps::matlab.like(10), maxpixels = Inf, blur = 10, overwrite = TRUE)




