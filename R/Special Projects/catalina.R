#catalina
library(tidyverse)

catalina_zoning <- sf::st_read('~/Downloads/City of LA/DRP_CATALINA_ZONING/DRP_CATALINA_ZONING.shp')
catalina_zoning <- sf::st_transform(catalina_zoning,sf:: st_crs(rumo::WRI_POWER_DATABASE_sf))
catalina_border <- catalina_zoning %>% sf::st_union()
catalina_border %>% plot()
catalina_grid <- sf::st_make_grid(catalina_border,cellsize = 0.002145775,what = 'centers')

catalina_grid <- sf::st_sf(cbind(data.frame(n = 1:length(catalina_grid)),(catalina_grid))) %>% filter(sf::st_within( catalina_grid, catalina_border,sparse = FALSE))

catalina_grid %>% ggplot() + geom_sf(aes())

catalina_grid %>% nrow()

sf::st_distance(catalina_grid[1,], catalina_grid[2,])
# catalina_filenames <-downloadGoogleImagesFromSpatial(sf::as_Spatial(catalina_grid),path = './output/png/catalina/', filename_only = TRUE)

while (sum(!file.exists(catalina_filenames))>0)
{

  cat('retrying ', sum(!file.exists(catalina_filenames)), ' images')
  tryCatch({
    downloadGoogleImagesFromSpatial(sf::as_Spatial(catalina_grid),path = './output/png/catalina/', start = 171)
  }, error = function(e){
    cat('found an error ')

  })
}

rumo::png2raster(png_path = './output/png/catalina/',raster_path = './output/raster/catalina/',

#catalina osm
catalina_buildings <-  sf::st_read('~/Downloads/City of LA/OSM - california-latest-free.shp/gis_osm_buildings_a_free_1.shp')
sf::st_crs(catalina_buildings)
class(catalina_buildings)
head(catalina_buildings)
catalina_buildings <-NULL catalina_buildings %>% filter  (sf::st_within( catalina_border,sparse = FALSE))

nrow(catalina_buildings)
catalina_buildings %>% plot()
sf::as_Spatial(catalina_grid[1,]) %>% points()
