#la buildings
library(tidyverse)

la_building_footprints <- sf::st_read('~/Downloads/City of LA/lariac_buildings_2008/lariac_buildings_2008.shp')
head(la_building_footprints)

la_permits <- read.csv('~/Downloads/City of LA/Building_and_Safety_Permit_Information.csv',stringsAsFactors = FALSE)

la_permits %>% select(Permit.Type, Permit.Sub.Type) %>% table()
la_permits %>% head()

la_hvac_permits <- la_permits %>% filter(Permit.Type =='HVAC')
la_hvac_permits %>% nrow()
lat_lon <- str_extract_all(la_hvac_permits$Latitude.Longitude, '\\d+\\.\\d+',simplify = TRUE)

la_hvac_permits$Latitude <- as.numeric(lat_lon[,1])
la_hvac_permits$Longitude <- -as.numeric(lat_lon[,2])

la_hvac_permits$Issue.Date <- as.Date(la_hvac_permits$Issue.Date,'%m/%d/%Y')
la_hvac_permits$Status.Date <- as.Date(la_hvac_permits$Status.Date,'%m/%d/%Y')

la_hvac_permits_sf <-  sf::st_as_sf(  la_hvac_permits %>% filter(!is.na(Latitude) &!is.na(Longitude)  ), coords = c('Longitude', 'Latitude'  ), crs = 4326,agr = "constant")
la_hvac_permits_sf %>% transmute( status = Status, issue_year = lubridate::year(Issue.Date) ) %>% sf::st_set_geometry(NULL) %>% table()


sf::st_crs(la_building_footprints) <- la_hvac_permits_sf %>% sf::st_crs()


la_hvac_permits_sf %>%  sample_n(4000) %>%   ggplot()+  geom_sf(aes(color = Permit.Sub.Type)) + facet_grid(lubridate::year(Issue.Date) ~ . )
#  geom_sf(data= la_building_footprints %>% sample_n(100), aes(fill = AREA))

la_building_footprints %>% head(n=1) %>% ggplot()+geom_sf(aes())


sf::st_distance(la_hvac_permits_sf , la_building_footprints %>% head()) %>% pmin()
sf::st_within(la_hvac_permits_sf %>% head(n=100),la_building_footprints,  sparse = FALSE) %>% sum()



  rumo::downloadGoogleImagesFromSpatial(spdf = sf::as_Spatial(la_hvac_permits_sf %>%filter(Permit.Sub.Type=='Commercial') %>% sample_n(10)), zoom_factor = 20)
for(filename in Sys.glob('./output/png/*18.png'))
{
  png <-png::readPNG( filename)
  png[225:226,199:200,1] <- 1
  png::writePNG(png,gsub('\\.png', '_dot\\.png', filename))
}
