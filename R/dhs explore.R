Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

writePredictionMaps <- function(country_name, year, n_grid, result_data)
{
  for( i in 1:length(result_data))
  {
    if ('raster' %in% names(result_70[[1]]))
    {
      raster::writeRaster(result_data[[i]]$raster,
                          paste0('./output/predictions/raster/',paste(country_name, year,names(result_data)[i],n_grid, format(Sys.Date(),'%Y%m%d'),  sep ='_'),'.tif'),
                          overwrite = TRUE
      )
    }
  }
}


#library(tidyverse)
#setwd('~/gitprojects/sandbox/rumo/')
function(){
country_name <- 'Rwanda'
grid_size <- 50

rwanda_DHS_53 <- foreign::read.dta('./data/DHS/RWHR53DT/RWHR53FL.DTA', convert.factors = FALSE)
rwanda_DHS_53_54_shp <- rgdal::readOGR('./data/DHS/RWGE54FL/RWGE54FL.shp')
result_53 <- rumo::spatialSDGpredictions(DHS = rwanda_DHS_53,cluster_shp = rwanda_DHS_53_54_shp,country_name = country_name, n_grid = grid_size)
plot(result_53[[1]])

plot(result_53$education)

writePredictionMaps(country_name , Mode(rwanda_DHS_53$hv007),grid_size, result_53 )



rwanda_DHS_61 <- foreign::read.dta('./data/DHS/RWHR61DT/RWHR61FL.DTA', convert.factors = FALSE)
rwanda_DHS_61_shp <- rgdal::readOGR('./data/DHS/RWGE61FL/RWGE61FL.shp')
result_61 <- rumo::spatialSDGpredictions(DHS = rwanda_DHS_61,cluster_shp = rwanda_DHS_61_shp,country_name = country_name, n_grid = grid_size)
plot(result_61[[1]])

writePredictionMaps(country_name , Mode(rwanda_DHS_61$hv007),grid_size, result_61 )


rwanda_DHS_70 <- foreign::read.dta('./data/DHS/RWHR70DT/RWHR70FL.DTA', convert.factors = FALSE)
rwanda_DHS_72_shp <- rgdal::readOGR('./data/DHS/RWGE72FL/RWGE72FL.shp')
result_70 <- rumo::spatialSDGpredictions(DHS = rwanda_DHS_70,cluster_shp = rwanda_DHS_72_shp,country_name = country_name, n_grid = grid_size )



save( result_70, file = './data/Rwanda_2015_125_grid_Output.RData')

writePredictionMaps(country_name , Mode(rwanda_DHS_70$hv007),grid_size, result_70 )




#crazy stuff:
rwanda_power_plants <- rumo::WRI_POWER_DATABASE_sf %>% filter(country_long== 'Rwanda')

covariates <- data.frame(cluster.name = unique(rwanda_DHS_70$hv001),
           closest_power_plant = sf::st_distance( sf::st_as_sf(rwanda_DHS_72_shp ), rwanda_power_plants) %>% apply(MARGIN = 1,FUN = function(x){min(x)}),
           median_wealth_index = data.frame( rwanda_DHS_70 %>% dplyr::group_by(hv001) %>% dplyr::summarise(computed_value = median(hv270, na.rm= TRUE)) %>% dplyr::select(computed_value))$computed_value,
           percent_household_electricity =    data.frame( rwanda_DHS_70 %>% dplyr::group_by(hv001) %>% dplyr::summarise(computed_value = mean(hv206, na.rm= TRUE)) %>% dplyr::select(computed_value))$computed_value
            )
rwanda_DHS_72_shp@data <- bind_cols(rwanda_DHS_72_shp@data, covariates)
rwanda_DHS_70 <-  bind_cols(rwanda_DHS_70 ,  covariates[rwanda_DHS_70$hv001,])
glm.fit <- glm(data = rwanda_DHS_70 , formula = hv206 ~ closest_power_plant, family = binomial)
summary(glm.fit)
coef(glm.fit)

ggplot(rwanda_power_plants)+ geom_sf(aes(size=capacity_mw)) + geom_sf(data= sf::st_as_sf(rwanda_DHS_72_shp), aes(color  =percent_household_electricity))

regression_colnames <- c()
household_electicity_result <- rumo::buildSpatialDistributionModelAndGrid(predict_colname = 'percent_household_electricity',

                                                                    #regression_colnames =regression_colnames,
                                                                    cluster_data = covariates,
                                                                    cluster_shp = rwanda_DHS_72_shp,
                                                                    country_name = country_name,
                                                                    n_grid = grid_size)#, keep.inbag = TRUE)
household_electicity_result$model
sp::plot(household_electicity_result$grid['pred_percent_household_electricity'])
hist(household_electicity_result$grid$pred_percent_household_electricity)
points(sf::as_Spatial(rwanda_power_plants))
regression_colnames <- c('closest_power_plant')


household_electicity_result$grid$closest_power_plant <-  sf::st_distance( sf::st_as_sf(household_electicity_result$grid ), rwanda_power_plants) %>% apply(MARGIN = 1,FUN = function(x){min(x)})
household_electicity_result$grid$closest_power_plant[household_electicity_result$grid$closest_power_plant >70000] <- 0
household_electicity_result$grid$closest_power_plant<- 0
sp::plot(household_electicity_result$grid['closest_power_plant'])
household_electicity_result3 <- buildSpatialDistributionModelAndGrid(predict_colname = 'percent_household_electricity',
                                                                          grid =  household_electicity_result$grid,
                                                                          regression_colnames =regression_colnames,
                                                                          cluster_data = covariates,
                                                                          cluster_shp = rwanda_DHS_72_shp,
                                                                          country_name = country_name,
                                                                          n_grid = grid_size)

plot(covariates$closest_power_plant,covariates$percent_household_electricity)
lm(data = covariates , formula = percent_household_electricity ~ closest_power_plant) %>% summary()
ranger::ranger(data = covariates , formula = percent_household_electricity ~ closest_power_plant)
glm.fit2 <- glm(data = covariates , formula = percent_household_electricity ~ closest_power_plant, family = binomial)
summary(glm.fit2)
sp::plot(household_electicity_result2$grid['pred_percent_household_electricity'])
points(rwanda_DHS_72_shp)
sp::plot(household_electicity_result3$grid['pred_percent_household_electricity'])






#try
library(sp)
dev.off()
pt <- data.frame(lat = -2, lon = 29.5)
coordinates(pt) <- ~lon+lat
proj4string(pt) <- raster::crs(household_electicity_result$grid['pred_percent_household_electricity'])
plot(rworldmap::countriesCoarse[rworldmap::countriesCoarse$ADMIN == 'Rwanda',])
points(pt)
pt_sf <- st_as_sf(pt)
ggplot(pt_sf)+ geom_sf(data=st_as_sf(rworldmap::countriesCoarse[rworldmap::countriesCoarse$ADMIN == 'Rwanda',]),aes(alpha = .25)) +
  geom_sf(data= st_buffer(pt_sf,1681*1/110575.63),aes(fill = 'red')) +geom_sf(aes())



radius_45 <- raster::rasterize(st_buffer(pt_sf,45000*1/110575.63),  raster::raster(household_electicity_result$grid['pred_percent_household_electricity']))
raster::values(radius_45)[!is.na(raster::values(radius_45))] <- .562-.34
raster::values(radius_45)[is.na(raster::values(radius_45))] <- 0
radius_1.68 <- raster::rasterize(st_buffer(pt_sf,1681*1/110575.63),  raster::raster(household_electicity_result$grid['pred_percent_household_electricity']))
raster::values(radius_1.68)[!is.na(raster::values(radius_1.68))] <- .785 -.562
raster::values(radius_1.68)[is.na(raster::values(radius_1.68))] <- 0

#attempt one : add rasters
plot(raster::raster(household_electicity_result$grid['pred_percent_household_electricity']) + (radius_45+radius_1.68))


#attempt 2 :modify the surveys
covariates_new <-covariates#   dplyr::bind_cols(rwanda_DHS_72_shp@data, cluster_data)
ggplot(st_buffer(pt_sf,45000*1/110575.63)) + geom_sf(aes()) + geom_sf(data=st_as_sf(rwanda_DHS_72_shp) , aes())

intersection <- st_intersects(st_as_sf(rwanda_DHS_72_shp) ,st_buffer(pt_sf,45000*1/110575.63), sparse = FALSE)[,1]
ggplot(st_buffer(pt_sf,45000*1/110575.63)) + geom_sf(aes()) + geom_sf(data=st_as_sf(rwanda_DHS_72_shp) , aes())
covariates_new[intersection,]$percent_household_electricity <- pmin(1, covariates_new[intersection,]$percent_household_electricity +.22)

household_electicity_result_new <- buildSpatialDistributionModelAndGrid(predict_colname = 'percent_household_electricity',
                                                                        grid = household_electicity_result_new$grid,
                                                                          #regression_colnames =regression_colnames,
                                                                          cluster_data = covariates_new,
                                                                          cluster_shp = rwanda_DHS_72_shp,
                                                                          country_name = country_name,
                                                                          n_grid = grid_size)#, keep.inbag = TRUE)
household_electicity_result_new2 <- buildSpatialDistributionModelAndGrid(predict_colname = 'percent_household_electricity',
                                                                        grid = household_electicity_result_new$grid,
                                                                        grid_distances =  household_electicity_result_new$grid_dist,
                                                                        cluster_data = covariates_new,
                                                                        cluster_shp = rwanda_DHS_72_shp,
                                                                        country_name = country_name,
                                                                        n_grid = grid_size)#, keep.inbag = TRUE)

sp::plot(household_electicity_result$grid['pred_percent_household_electricity'])
sp::plot(household_electicity_result_new$grid['pred_percent_household_electricity'])
sp::plot(household_electicity_result_new2$grid['pred_percent_household_electricity'])

household_electicity_result_new$grid$
##tanzania
#2004
country_name <- 'Tanzania'
tanzan_DHS_4 <- foreign::read.dta('./data/DHS/TZHR4ADT/tzhr4afl.dta', convert.factors = FALSE)
tanzan_DHS_4_shp <- rgdal::readOGR('./data/DHS/TZGE4CFL/TZGE4CFL.shp')

result_4 <- spatialSDGpredictions(DHS = tanzan_DHS_4,cluster_shp = tanzan_DHS_4_shp,country_name = country_name, n_grid = 25)


#2015


rwanda_DHS_53 %>% dim() #10272 x 1734
rwanda_DHS_61 %>% dim() #12540 x 3396
rwanda_DHS_70 %>% dim() #12699 x 2712
#clusters:
length(rwanda_DHS_53$hv001 %>% unique()) # 462 clusters
length(rwanda_DHS_61$hv001 %>% unique()) # 492 clusters
length(rwanda_DHS_70$hv001 %>% unique()) # 492 clusters

rwanda_DHS_61$hv007

rwanda_DHS_70$hv007


rwanda_DHS_53$hv040 %>% head
rwanda_DHS_61$hv040 %>% head
rwanda_DHS_70$hv040 %>% head


#todo:  layer in when do power plants come online

#extend to other countries.
}
