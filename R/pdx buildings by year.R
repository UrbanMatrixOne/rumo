library(tidyverse)

pdx_building_footprints <- sf::st_read('~/Downloads/City of LA/PDX_Building_Footprints/Building_Footprints.shp')
pdx_building_footprints %>% head()
pdx_building_footprints$STRUC_TYPE %>% table()

pdx_building_footprints %>% nrow()

pdx_building_footprints$DECADE_BUILT <- 10*floor( pdx_building_footprints$YEAR_BUILT/10)




neighborhood <-pdx_building_footprints[ sf::st_is_within_distance( pdx_building_footprints[1,], pdx_building_footprints[1:10000,] ,dist =  50, sparse = FALSE) ,]
pdx_building_footprints %>% filter(lengths(st_is_within_distance(pdx_building_footprints, pdx_building_footprints, dist = 1000) > 0))

st_distance(neighborhood,neighborhood) %>%
neighborhood[1:2,]%>% ggplot()+geom_sf(aes(fill = DECADE_BUILT))
