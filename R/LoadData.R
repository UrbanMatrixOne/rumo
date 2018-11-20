library(dplyr)
#Carma :  Carbon Monitoring for Action http://carma.org/


CARMA_DATABASE <- read.csv('./data/Energy/CARMA- All Powerplants.csv',stringsAsFactors = FALSE)
CARMA_DATABASE_sf <- sf::st_as_sf(CARMA_DATABASE , coords = c('longitude', 'latitude'  ), crs = 4326,agr = "constant")

#WRI Global Power Plant Database https://github.com/wri/global-power-plant-database

WRI_POWER_DATABASE <- read.csv('./data/Energy/global_power_plant_database.csv', stringsAsFactors = FALSE)
WRI_POWER_DATABASE$Primary.Fuel <-  gsub('Petcoke|Storage|Cogeneration|Wave and Tidal',replacement = 'Other', WRI_POWER_DATABASE$fuel1)
WRI_POWER_DATABASE$name <- toupper(WRI_POWER_DATABASE$name)
WRI_POWER_DATABASE$iso3 <- toupper(WRI_POWER_DATABASE$country)
WRI_POWER_DATABASE_sf   <- sf::st_as_sf(WRI_POWER_DATABASE , coords = c('longitude', 'latitude'  ), crs = 4326,agr = "constant")

#merge WRI & Carma on name & country
CARMA_WRI_COMBINED_DATABASE <- CARMA_DATABASE %>% select(-c(longitude,latitude))%>% inner_join(WRI_POWER_DATABASE, by= c('name','iso3'))
CARMA_WRI_COMBINED_DATABASE$energy_Future <-  ifelse(is.na(CARMA_WRI_COMBINED_DATABASE$estimated_generation_gwh),
                                                     CARMA_WRI_COMBINED_DATABASE$energy_Future,
                                                     CARMA_WRI_COMBINED_DATABASE$estimated_generation_gwh)
CARMA_WRI_COMBINED_DATABASE_sf   <- st_as_sf(CARMA_WRI_COMBINED_DATABASE , coords = c('longitude', 'latitude'  ), crs = 4326,agr = "constant")

save(WRI_POWER_DATABASE_sf,file = './data/WRI_POWER_DATABASE_sf')
save(CARMA_DATABASE,file = './data/CARMA_DATABASE.RData')
save(CARMA_DATABASE_sf,file = './data/CARMA_DATABASE_sf.RData')
save(CARMA_WRI_COMBINED_DATABASE,file = './data/CARMA_WRI_COMBINED_DATABASE.RData')
save(CARMA_WRI_COMBINED_DATABASE_sf,file = './data/CARMA_WRI_COMBINED_DATABASE_sf.RData')


#IGES CDM Data (combined wih world bank data )
IGES_CDM <- read.csv('./data/CDM/20180311_Combined_IGES_CDM_WBD_dataset.csv',stringsAsFactors = FALSE)

#clean up
IGES_CDM<- IGES_CDM %>% filter(NPVperkw <.5 ,Type_of_Project %in% c('PV', 'Wind power', 'Hydro power')) %>% transmute(start_date = as.Date(Starting_Date_of_a_CDM_Project_Activity, '%Y-%m-%d'),
                  capacity   = Totalelgen,
                  fuel_type  = Type_of_Project,
                  NPVperkw   )
save(IGES_CDM,file = './data/IGES_CDM.RData')
#Boundaries and Population
#TODO: Source ? (Get Dataset)
AFRICA_POPULATION <- read.csv('./data/Population/africa population 2015.csv',stringsAsFactors = FALSE)
WORLD_BOUNDARIES <- sf::st_read('./data/Shapefiles/Africa SHP/ne_110m_admin_0_countries_lakes.shp')
AFRICA_BOUNDARIES <- inner_join( WORLD_BOUNDARIES, AFRICA_POPULATION, by = c('ADMIN' = 'Country' ))
US_STATES_coords <-
save(AFRICA_POPULATION,file = './data/AFRICA_POPULATION.RData')
save(WORLD_BOUNDARIES,file = './data/WORLD_BOUNDARIES.RData')
save(AFRICA_BOUNDARIES,file = './data/AFRICA_BOUNDARIES.RData')

#Electricity Pricing
#TODO: Source ?
EUROPE_ELECTRICITY_PRICING_sf <-  inner_join( WORLD_BOUNDARIES, read.csv('./data/Energy/europe_2017_s1_electricity_prices.csv',stringsAsFactors = FALSE)  , by = c('ADMIN' = 'country' ))
save(EUROPE_ELECTRICITY_PRICING_sf,file = './data/EUROPE_ELECTRICITY_PRICING_sf.RData')

#Nordic Electricity Pricing

norway_bidding <- rbind( st_read('./data/Shapefiles/electricitymap-master/web/third_party_maps/NO-NO1.json'),
                         st_read('./data/Shapefiles/electricitymap-master/web/third_party_maps/NO-NO2.json'),
                         st_read('./data/Shapefiles/electricitymap-master/web/third_party_maps/NO-NO3.json'),
                         st_read('./data/Shapefiles/electricitymap-master/web/third_party_maps/NO-NO4.json'),
                         st_read('./data/Shapefiles/electricitymap-master/web/third_party_maps/NO-NO5.json'))

sweden_bidding <- rbind( st_read('./data/Shapefiles/electricitymap-master/web/third_party_maps/SE-SE1.json'),
                         st_read('./data/Shapefiles/electricitymap-master/web/third_party_maps/SE-SE2.json'),
                         st_read('./data/Shapefiles/electricitymap-master/web/third_party_maps/SE-SE3.json'),
                         st_read('./data/Shapefiles/electricitymap-master/web/third_party_maps/SE-SE4.json'))
denmark_bidding <- rbind(st_read('./data/Shapefiles/electricitymap-master/web/third_party_maps/DK-DK2-without-BHM.json'))
rest_of_nord_bidding <- WORLD_BOUNDARIES %>%
  transmute(id = ADMIN) %>%
  filter(id %in% c('Latvia', 'Lithuania', 'Estonia', 'Finland', 'Germany','United Kingdom')) %>%
  st_as_sf()

NORDIC_ELECTRICITY_BIDDING_ZONES_sf <- rbind(norway_bidding,sweden_bidding,denmark_bidding,rest_of_nord_bidding)
nord_price <- read.csv('./data/Market/nordpool/elspot-prices_2018_daily_eur.csv',stringsAsFactors = FALSE,dec = ',')
nord_price_collapse <- nord_price %>% transmute('SE-SE1' = SE1, 'SE-SE2' = SE2, 'SE-SE3' =SE3, 'SE-SE4' = SE4,
                                                'NO-NO1' = Oslo, 'NO-NO2' = Kr.sand, 'NO-NO3' = Bergen, 'NO-NO4' = Molde, 'NO-NO5' = Tr.heim,
                                                Estonia = EE, Latvia = LV, Lithuania = LT ) %>%summarise_all(mean) %>% gather(id , price.eur.MWh )
nord_price_collapse$price.usd.kWh <- nord_price_collapse$price.eur.MWh *1.2 / 1000
NORD_PRICE_sf <- NORDIC_ELECTRICITY_BIDDING_ZONES_sf %>% inner_join(nord_price_collapse )

save(NORDIC_ELECTRICITY_BIDDING_ZONES_sf,file = './data/NORDIC_ELECTRICITY_BIDDING_ZONES_sf.RData')
save(NORD_PRICE_sf,file = './data/NORD_PRICE_sf.RData')

# nordicElectricityPal <- colorNumeric(palette= 'RdYlGn', domain = nord_price_sf$price.usd.kWh)

#source: bloomberg
GREEN_BONDS <-  read.csv('./data/Market/GreenBond/green_bond_muni_june_15_2018.csv',stringsAsFactors = FALSE)
US_STATE_COORDS <- read.csv('./data/Shapefiles/US/US States Geocode.csv',stringsAsFactors = FALSE)

#june 15 curve
TREASURY_CURVE <- data.frame( type = 'Treasury',
                              maturity = Sys.Date()+ 365* c(1,2,3,5,7,10,20,30),
                              Yield =  c(2.34, 2.55,2.68, 2.81, 2.89, 2.93, 2.98, 3.05 ))
GREEN_BONDS$interp_tsy <-approx(x = TREASURY_CURVE$maturity,y = TREASURY_CURVE$Yield,
                                xout = as.Date(GREEN_BONDS$Maturity,'%m/%d/%Y'))$y
GREEN_BONDS$spread <- as.numeric(GREEN_BONDS$yld_ytm_ask) - GREEN_BONDS$interp_tsy


save(GREEN_BONDS,file = './data/GREEN_BONDS.RData')
save(TREASURY_CURVE,file = './data/TREASURY_CURVE.RData')
save(US_STATE_COORDS,file = './data/US_STATE_COORDS.RData')


#SDG's
formatSDGData <- function(){
wbd_sdg_data <- read.csv('./data/SDG/worldbank_sdg_20180402_all.csv',stringsAsFactors = FALSE)
wbd_sdg_data$most_recent <-
  ifelse(wbd_sdg_data$X2017..YR2017. != '..',wbd_sdg_data$X2017..YR2017.,
         ifelse(wbd_sdg_data$X2016..YR2016. != '..',wbd_sdg_data$X2016..YR2016.,
                ifelse(wbd_sdg_data$X2015..YR2015. != '..',wbd_sdg_data$X2015..YR2015.,
                       ifelse(wbd_sdg_data$X2014..YR2014. != '..',wbd_sdg_data$X2014..YR2014.,
                              ifelse(wbd_sdg_data$X2013..YR2013. != '..',wbd_sdg_data$X2013..YR2013.,
                                     ifelse(wbd_sdg_data$X2012..YR2012. != '..',wbd_sdg_data$X2012..YR2012.,
                                            ifelse(wbd_sdg_data$X2011..YR2011. != '..',wbd_sdg_data$X2011..YR2011.,
                                                   ifelse(wbd_sdg_data$X2010..YR2010. != '..',wbd_sdg_data$X2010..YR2010.,
                                                          ifelse(wbd_sdg_data$X2009..YR2009. != '..',wbd_sdg_data$X2009..YR2009.,
                                                                 wbd_sdg_data$X2008..YR2008.)))))))))

colnames(wbd_sdg_data)[1] <- 'Country.Name'
wbd_sdg_data <- wbd_sdg_data %>% group_by(Country.Name,Country.Code,Series.Name) %>% summarise(most_recent= mean(as.numeric(most_recent)))
write.csv(wbd_sdg_data, file = './data/SDG/processed_sdg_data.csv')
}
wbd_sdg_data <- read.csv('./data/SDG/processed_sdg_data.csv', stringsAsFactors = FALSE )
wbd_electricty_data <- read.csv('./data/SDG/worldbank_access_to_electricity.csv', stringsAsFactors = FALSE)
wbd_electricty_data$rank <- rank(wbd_electricty_data$X2014..YR2014.)
wbd_rural_electricty_data <- read.csv('./data/SDG/worldbank_rural_access_to_electricity.csv', stringsAsFactors = FALSE) %>% filter(X2014..YR2014. != '..' & X2014..YR2014. != '' )
wbd_rural_electricty_data$rank <- rank(as.numeric(wbd_rural_electricty_data$X2014..YR2014.),ties.method = 'max')

INTENSITY_PROJECTION_POINTS <- readRDS('./data/Estimates/intensity_projection_points.rds')
save(INTENSITY_PROJECTION_POINTS,file = './data/INTENSITY_PROJECTION_POINTS.RData')


#http://www.worldpop.org.uk/data/get_data/
#TODO:



#Download Google Images for DHS clusters
# for (i in 1:nrow(cluster_shp@coords))
# {
#   cluster_shp@data$DHSCLUST
#   lon = cluster_shp@coords[i,1]
#   lat = cluster_shp@coords[i,2]
#   downloadGoogleImageWithCoordinates(lat = lat, lon = lon ,zoom_factor = 18,
#                                      filename =gsub('-', 'neg_',paste0('./output/', paste(cluster_shp$CCFIPS[i], cluster_shp$DHSCLUST[i] ,'google_image_lat',lat, 'lon',lon,  sep = '_') ,'tif'))
#   )
# }
# png2raster(png_path = './output/', raster_path = './output/raster/',config_path = './output/raster/config/' ,  dist_between_centroids_lat = 0.002159009, dist_between_centroids_lon = 0.002159009, dimensions = 400, border_pixels =0)

#UMO Spatial predictions
#rwanda_building_predictions <- rgdal::readOGR('./data/Shapefiles/rwanda_predictions_20180904.json',stringsAsFactors = FALSE)
rwanda_building_predictions <- rgdal::readOGR('./data/Shapefiles/rwanda_predictions_20180914.json',stringsAsFactors = FALSE)
rwanda_building_predictions$ADMIN <- 'Rwanda'


UMO_PREDICTIONS <- list(Rwanda = list(building_predictions = rwanda_building_predictions))
save(UMO_PREDICTIONS,file = './data/UMO_PREDICTIONS.RData')

#prediction rasters
#
# UMO_RASTER <- list( Rwanda = list(Median_Wealth = raster::raster("./output/rwanda_med_wealth_prediction20180904.tif"),
#                                   Percent_Household_Electricity = raster::raster("./output/rwanda_household_electicity_prediction20180905.tif"),
#                                   Median_Years_Education = raster::raster('./output/rwanda_median_years_education_tifprediction20180905.tif'))
# )
# save(UMO_RASTER,file = './data/UMO_RASTER.RData')
