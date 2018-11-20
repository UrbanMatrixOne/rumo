function()
{
  #devtools::install_github("mlr-org/mlr")
  library(tidyverse)
  library(mlr)

  #data
  rwanda_DHS_70 <- foreign::read.dta('./data/DHS/RWHR70DT/RWHR70FL.DTA', convert.factors = FALSE)
  rwanda_DHS_72_shp <- rgdal::readOGR('./data/DHS/RWGE72FL/RWGE72FL.shp')
  data <-rwanda_DHS_72_shp@data
  data$percent_household_electricity <-    data.frame( rwanda_DHS_70 %>% dplyr::group_by(hv001) %>% dplyr::summarise(computed_value = mean(hv206, na.rm = TRUE)) %>% dplyr::select(computed_value))$computed_value

  # number of buildings
  data$num_buildings <- do.call(rbind,lapply( sf::st_is_within_distance(sf::st_as_sf(rwanda_DHS_72_shp), sf::st_as_sf( rumo::UMO_PREDICTIONS[['Rwanda']]$building_predictions),500),function(x){length(x)}))[,1]
  # nearby plants

  dist_matrix <-  sf::st_distance(  rumo::WRI_POWER_DATABASE_sf[rumo::WRI_POWER_DATABASE_sf$country == 'RWA',] , sf::st_as_sf(rwanda_DHS_72_shp) )
  data$closest_plant_1 <-  apply(dist_matrix, MARGIN = 2, function(x){sort(x,FALSE)[1]})
  data$closest_plant_2 <-  apply(dist_matrix, MARGIN = 2, function(x){sort(x,FALSE)[2]})
  data$closest_plant_3 <-  apply(dist_matrix, MARGIN = 2, function(x){sort(x,FALSE)[3]})

  cluster_dist_matrix <- sf::st_distance(  sf::st_as_sf(rwanda_DHS_72_shp) , sf::st_as_sf(rwanda_DHS_72_shp) )
  data$closest_cluster_1 <-  apply(cluster_dist_matrix, MARGIN = 2, function(x){sort(x,FALSE)[1]})
  data$closest_cluster_2 <-  apply(cluster_dist_matrix, MARGIN = 2, function(x){sort(x,FALSE)[2]})
  data$closest_cluster_3 <-  apply(cluster_dist_matrix, MARGIN = 2, function(x){sort(x,FALSE)[3]})

  train_data <-  data %>% select(num_buildings,closest_plant_1, closest_plant_2,closest_plant_3,URBAN_RURA,percent_household_electricity)
  #train_data <-  data %>% select(num_buildings,closest_plant_1, closest_plant_2,closest_plant_3,closest_cluster_1,closest_cluster_2,closest_cluster_3,URBAN_RURA,percent_household_electricity)
  regr.task = makeRegrTask(data = train_data , target = 'percent_household_electricity')



  regr.lrn = makeLearner("regr.ranger", predict.type = "response")

  regr.model <- train(regr.lrn,regr.task)

  regr.model$learner.model

  pred <- predict(regr.model, newdata= train_data)
  plot(pred$data$truth,pred$data$response)
  performance(pred)


  #lime

  explainer <- lime::lime(train_data,model)
  explanation <- lime::explain(train_data %>% sample_n(5), explainer, n_features = 5)

  train_data$percent_household_electricity[1:5]
  explanation %>% lime::plot_features()


  #grid electricity distances
  #need more u/r predictions and building predictions
  borders_shp <- rumo::WORLD_BOUNDARIES %>%  sf::as_Spatial()
  n_grid <- 50
  world_grid <- sp::SpatialPixelsDataFrame(  sp::makegrid(borders_shp, n_grid * n_grid),proj4string = borders_shp@proj4string , data = data.frame(sp::makegrid(borders_shp, n_grid  *n_grid)))[borders_shp,]

  #africa_grid$img_filename <- downloadGoogleImagesFromSpatial(spdf = africa_grid,pixel_dim = 1000,zoom_factor = 17,path = './output/png/africa/', start = 1265 )
  world_grid$img_filename  <- downloadGoogleImagesFromSpatial(spdf = world_grid,pixel_dim = 1000,zoom_factor = 16,path = './output/png/world/', start = 1 , filename_only = TRUE)
  world_grid$img_exists <- lapply(world_grid@data$img_filename,FUN = function(x){ file.exists(file = x)}) %>% unlist()


  predictions <- classifyUrbanRural( world_grid$img_filename)
  world_grid@data <- bind_cols(world_grid@data, predictions %>% select(Rural, Urban, class))
  world_grid@data[world_grid@data$class=='U',]
  world_grid$URBAN_RURA <- world_grid@data$class

  #building prediction:
  world_grid$num_buildings <- 0

  pred_grid <- world_grid# africa_grid#rumo::result_70$electricity$grid
  grid_dist_matrix <- sf::st_distance(rumo::WRI_POWER_DATABASE_sf[rumo::WRI_POWER_DATABASE_sf$country %in% borders_shp$SOV_A3 ,] , sf::st_as_sf(pred_grid))
  #grid_dist_matrix <- sf::st_distance(rumo::WRI_POWER_DATABASE_sf[rumo::WRI_POWER_DATABASE_sf$country == 'RWA',] , sf::st_as_sf(pred_grid))

  #pred_grid$num_buildings <- do.call(rbind,lapply( sf::st_is_within_distance(sf::st_as_sf(pred_grid), sf::st_as_sf( rumo::UMO_PREDICTIONS[['Rwanda']]$building_predictions),500),function(x){length(x)}))[,1]

  pred_grid$closest_plant_1 <-  apply(grid_dist_matrix, MARGIN = 2, function(x){sort(x,FALSE)[1]})
  pred_grid$closest_plant_2 <-  apply(grid_dist_matrix, MARGIN = 2, function(x){sort(x,FALSE)[2]})
  pred_grid$closest_plant_3 <-  apply(grid_dist_matrix, MARGIN = 2, function(x){sort(x,FALSE)[3]})


  #cluster distances
  # grid_cluster_dist_matrix <-  sf::st_distance(  sf::st_as_sf(rwanda_DHS_72_shp) , sf::st_as_sf(pred_grid) )
  # pred_grid$closest_cluster_1 <-  apply(grid_cluster_dist_matrix, MARGIN = 2, function(x){sort(x,FALSE)[1]})
  # pred_grid$closest_cluster_2 <-  apply(grid_cluster_dist_matrix, MARGIN = 2, function(x){sort(x,FALSE)[2]})
  # pred_grid$closest_cluster_3 <-  apply(grid_cluster_dist_matrix, MARGIN = 2, function(x){sort(x,FALSE)[3]})

  #pred_grid$URBAN_RURA <- sp::over(pred_grid, all_predictions_spdf)$class
  #pred_grid$URBAN_RURA[is.na(pred_grid$URBAN_RURA)] <- 'U'
  pred_grid$pred_percent_household_electricity_2 <-predict(regr.model, newdata = pred_grid@data %>% select(num_buildings,closest_plant_1, closest_plant_2,closest_plant_3,URBAN_RURA))$data$response

  plot(pred_grid['pred_percent_household_electricity'])
  plot(pred_grid['pred_percent_household_electricity_2'])



  rumo::AFRICA_BOUNDARIES %>% plot()
}
