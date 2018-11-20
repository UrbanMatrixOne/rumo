#boston
library(tidyverse)
boston_energy_reporting <- read.csv('~/Downloads/berdo-data-for-disclosure-calendar-year-2017.csv',stringsAsFactors = FALSE)
boston_energy_reporting %>% nrow()
boston_energy_reporting %>% head()
boston_energy_reporting$Address

boston_buildings <- sf::st_read('~/Downloads/Live_Street_Address_Management_SAM_Addresses/Live_Street_Address_Management_SAM_Addresses.shp')
boston_buildings %>% nrow()
boston_buildings %>% head()
boston_buildings$Address <-boston_buildings$FULL_ADDRE

#435 / 1914 reporting buildings without address massaging
boston_energy_reporting_sf <- boston_energy_reporting %>% inner_join(boston_buildings)


boston_energy_reporting_sf %>% ggplot() +
  geom_sf(aes(fill = Year.Built))


#things to look at
boston_energy_reporting_sf$Year.Built %>% hist()

boston_energy_reporting_sf$Site.EUI..kBTU.sf. %>% as.numeric() %>% hist()
boston_energy_reporting_sf$Energy.Star.Score %>% hist()

boston_energy_reporting_sf$GHG.Emissions..MTCO2e.%>% as.numeric() %>% hist()
boston_energy_reporting_sf$GHG.Intensity..kgCO2.sf. %>% as.numeric() %>% hist()

boston_energy_reporting_sf$Gross.Area..sq.ft. %>% as.numeric() %>% hist

boston_energy_reporting_sf$Total.Site.Energy..kBTU.
boston_energy_reporting$Onsite.Renewable..kWh.


rumo::WRI_POWER_DATABASE$country %>% table()
