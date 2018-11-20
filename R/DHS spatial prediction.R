# library(tidyverse)
# library(sf)
# library(rgdal)

#read shp file containing location of clusters
spatialSDGpredictions <- function( DHS, cluster_shp, cluster_covariates =NA, country_name ='Rwanda', prediction_grid = NA, regression_colnames = c(),  n_grid = 5 ){


  #count how many features exist within 1km of each cluster.
  num_features <- do.call(rbind,lapply( sf::st_is_within_distance(sf::st_as_sf(cluster_shp), sf::st_as_sf( rumo::UMO_PREDICTIONS[[country_name]]$building_predictions),500),function(x){length(x)}))[,1]
  cluster_shp$num_buildings <- num_features

  #calculate median_wealth_index by cluster
  if(!is.na(cluster_covariates)){
    cluster_shp@data <-  dplyr::bind_cols(cluster_shp@data, cluster_data)
  }
  cluster_shp$median_wealth_index <-data.frame( DHS %>% dplyr::group_by(hv001) %>% dplyr::summarise(computed_value = median(hv270,na.rm = TRUE)) %>% dplyr::select(computed_value))$computed_value
  cluster_shp$percent_household_electricity <-    data.frame( DHS %>% dplyr::group_by(hv001) %>% dplyr::summarise(computed_value = mean(hv206,na.rm = TRUE)) %>% dplyr::select(computed_value))$computed_value
  cluster_shp$median_years_education <- (DHS %>% dplyr:: select(c(hv001,paste0('hv108_0',1:8) )) %>%   tidyr::gather(key = 'person', value = 'data', -'hv001') %>% dplyr::group_by(hv001) %>% dplyr::summarise(computed_value = median(data, na.rm=TRUE)) %>% dplyr::select(computed_value))$computed_value



  #electricity
  household_electicity_result <- buildSpatialDistributionModelAndGrid(predict_colname = 'percent_household_electricity',
                                                                      grid = prediction_grid,
                                                                      regression_colnames =regression_colnames,
                                                                      #  cluster_data = cluster_covariates,
                                                                      cluster_shp = cluster_shp,
                                                                      country_name = country_name,
                                                                      n_grid = n_grid)
  household_electicity_model <- household_electicity_result$model
  household_electicity_grid <- household_electicity_result$grid

  household_electicity_grid@data <- data.frame(percent_household_electricity = household_electicity_result$grid@data$pred_percent_household_electricity)
  regression_colnames <- c(regression_colnames,'percent_household_electricity')
  #wealth

  median_wealth_result <- buildSpatialDistributionModelAndGrid(predict_colname = 'median_wealth_index',
                                                               grid = household_electicity_grid,
                                                               grid_distances = household_electicity_result$grid_distances,
                                                               regression_colnames =regression_colnames,
                                                               # cluster_data = cluster_covariates,
                                                               cluster_shp = cluster_shp,
                                                               country_name = country_name,
                                                               n_grid = n_grid)
  median_wealth_model <- median_wealth_result$model
  median_wealth_grid <- median_wealth_result$grid

  #education
  median_years_education_result <- buildSpatialDistributionModelAndGrid(predict_colname = 'median_years_education',
                                                                        grid = household_electicity_grid,
                                                                        grid_distances = household_electicity_result$grid_distances,
                                                                        regression_colnames =regression_colnames,
                                                                        # cluster_data = cluster_covariates,
                                                                        cluster_shp = cluster_shp,
                                                                        country_name = country_name,
                                                                        n_grid = n_grid)

  median_years_education_model <- median_years_education_result$model
  median_years_education_grid <- median_years_education_result$grid

  #plot the prediction
  # sp::plot(median_wealth_grid['pred_median_wealth_index'])
  # sp::plot(household_electicity_grid ['pred_percent_household_electricity'])
  # sp::plot(median_years_education_grid ['pred_median_years_education'])

  #to raster
  median_wealth_result$raster <- raster::raster(median_wealth_result$grid['pred_median_wealth_index'])
  #med_wealth_pred_tif %>% raster::writeRaster(paste0('./output/med_wealth_prediction',format(Sys.Date(),'%Y%m%d'),'.tif'),overwrite=TRUE)

  household_electicity_result$raster <- raster::raster(household_electicity_result$grid ['pred_percent_household_electricity'])
  #household_electicity_pred_tif %>% raster::writeRaster(paste0('./output/rwanda_household_electicity_prediction',format(Sys.Date(),'%Y%m%d'),'.tif'),overwrite=TRUE)

  median_years_education_result$raster <- raster::raster(median_years_education_result$grid ['pred_median_years_education'])
  #median_years_education_tif %>% raster::writeRaster(paste0('./output/rwanda_median_years_education_tifprediction',format(Sys.Date(),'%Y%m%d'),'.tif'),overwrite=TRUE)

return(  list(wealth  = median_wealth_result,
       electricity= household_electicity_result,
       education = median_years_education_result,
       cluster_shp = cluster_shp))

}


buildSpatialDistributionModelAndGrid <- function(predict_colname,cluster_data =NA, cluster_shp, country_name ,  n_grid =50, n_trees=1000, grid=NA,grid_distances= NA , regression_colnames = c(),importance = 'none', quantreg=FALSE, keep.inbag = FALSE){
  #load cluster location

  if (!is.na(cluster_data))
  {
    cluster_shp@data <-  dplyr::bind_cols(cluster_shp@data, cluster_data)
  }
  #load borders
  borders_shp <- rworldmap::countriesLow[rworldmap::countriesLow@data$ADMIN == country_name,]
  # borders_shp <- sf::as_Spatial( sf::st_buffer(sf::st_as_sf(borders_shp),.1) )
  # ggplot()+
  #   geom_sf(data=  sf::st_buffer(sf::st_as_sf(borders_shp),.1))+
  #   geom_sf(data=  sf::st_as_sf(borders_shp))

  #create a grid within the polygon borders
  if(is.na(grid))
  {
    grid <- sp::SpatialPixelsDataFrame(  sp::makegrid(borders_shp, n_grid * n_grid),proj4string = borders_shp@proj4string , data = data.frame(sp::makegrid(borders_shp, n_grid  *n_grid)))[borders_shp,]
  }

  #calculate distance between each cluster and each point
  grid_dist <- grid
  if(is.na(grid_distances))
  {
    grid_distances <- data.frame(sf::st_distance(  st_as_sf(grid), sf::st_as_sf(cluster_shp) ))
  }
  grid_dist@data <- cbind(grid_dist@data,  grid_distances)
    # grid_dist <- GSIF::buffer.dist(cluster_shp[predict_colname],
  #                                       grid,
  #                                       as.factor(1:nrow(cluster_shp)))


  regr_formula <-  as.formula(paste(predict_colname, '~', paste0( paste(names(grid_dist),  collapse = '+'),ifelse(is.null(regression_colnames),'',paste( '+',regression_colnames,  collapse = '+') ))))
  regr_data    <- cbind(cluster_shp@data, sp::over(cluster_shp[predict_colname], grid_dist))
  regr_data[is.na(regr_data)] <- 0

  #fit model
  rf_model <-ranger::ranger(regr_formula,regr_data,num.trees = n_trees, seed = 1 , quantreg = quantreg, importance = importance,keep.inbag = keep.inbag)

  #predict on grid
  grid@data[paste0('pred_',predict_colname)] <- predict(rf_model,  cbind( grid_dist@data, grid@data))$predictions

  list('model' = rf_model , 'grid' = grid, 'grid_distances'= grid_dist@data)

}



