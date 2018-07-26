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
