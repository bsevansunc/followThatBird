library(raster)
library(tidyverse)

ag <- 
  raster(
    'appCurriculum/data/agIntensity'
  )

writeRaster(ag, 'agIntensity.tif', format = 'GTiff')

raster('agIntensity.tif') %>% plot

biomes <-
  raster(
    'appCurriculum/data/biomesR'
  )

biomes_rat <-
  ratify(biomes)

biomeLevels <- read_csv('appCurriculum/data/biomeLevels.csv')

levels(biomes_rat)[[1]]$NAME <- biomeLevels$biome

writeRaster(biomes_rat, 'biomes.tif', format = 'GTiff', overwrite = TRUE)

raster('biomes.tif') %>% plot

agProj <-
  projectRaster(ag, projection(biomes))


# change rasters to kml for getting png -----------------------------------

agProj <- projectRaster(ag, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")


writeRaster(agProj, 'ag.tif', format = 'GTiff', overwrite = TRUE)

KML(agProj, 'agK.kml', maxpixels = Inf, blur = 10, overwrite = TRUE)
