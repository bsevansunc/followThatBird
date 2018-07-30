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

KML(agProj, 'agK.kml', col = colorRamps::matlab.like(10), maxpixels = Inf, blur = 10, overwrite = TRUE)

# Color set for biomes:

bioColors <- c("#9E0142", "#D9444E", "#F77E4A", "#FEC171", "#F3EB91", "#C2E6A0",
               "#79C9A5", "#3D8FBA", "#3D64BA", "#5E4FA2")

KML(raster('biomes.tif'), 'biomes.kml', col = bioColors, maxpixels = Inf, blur = 10, overwrite = TRUE)


# begin to color ----------------------------------------------------------



# Biome colors:

factPal <- colorFactor(bioColors, domain = 1:10, na.color = 'transparent')

# Agricultural intensity colors:

pal <- colorNumeric(colorRamps::matlab.like(10), c(0, values(agriculture)),
                    na.color = "transparent")

# Set colors for birds:

colorPalette <- colorFactor(
  colorRamps::primary.colors(length(unique(tracks$birdID))),
  tracks$birdID
)
