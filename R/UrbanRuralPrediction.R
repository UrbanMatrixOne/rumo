library(keras)
library(tidyverse)
mobilenet <-application_mobilenet_v2(include_top=FALSE)
x <- mobilenet$output
x <- layer_global_average_pooling_2d(x)
x =  layer_dense(x,1280, activation='relu')
predictions_layer <- keras::layer_dense(x,2,activation = 'softmax')

model = keras_model(inputs=mobilenet$input, outputs=predictions_layer)
load_model_weights_hdf5(model,'./data/Weights/20180924-mobilenet-best_weights.hdf5')

runImageClassificationTensor <- function(img_filenames=c(1,2,3))
{
  image_size <- c(224,224)
  image_array <-  array(NA, c(length(img_filenames),image_size,3))

  for (i in (1: length(img_filenames)))
  {
    image <-  image_to_array(image_load(img_filenames[i], target_size =c(224,224) ))
    image_array[i,,,] <- image
  }

  image_array <- mobilenet_preprocess_input(image_array)
  predictions <- predict(model,image_array ,verbose = 1)
  colnames(predictions) <-  c('Rural', 'Urban')

  predictions
}


classifyUrbanRural<- function(img_filenames , file_out = NULL, naming_convention = c('lon','lat','zoom'))
{



  now <- Sys.time()
  img_class <- runImageClassificationTensor(img_filenames)
  # img_class <-rbind(img_class, runImageClassificationTensor(img_filenames[10001:20000] ))
  # img_class <-rbind(img_class, runImageClassificationTensor(img_filenames[20001:30000] ))
  # img_class <-rbind(img_class, runImageClassificationTensor(img_filenames[30001:36490] ))
  Sys.time()-now


  all_predictions <- data.frame(cbind(img_filenames, img_class),stringsAsFactors = FALSE)
  all_predictions$Rural <- as.numeric(all_predictions$Rural)

  all_predictions$class <- as.factor(ifelse(all_predictions$Rural>.5,'R','U' ))

  if(!is.null(file_out))
  {
    write.csv(all_predictions,file = file_out)
  }
  all_predictions
  #associate each prediction with a grid point
  #
  #
  # all_predictions[,c('lat', 'long')] <- gsub('_','.',gsub('^.','',str_extract_all( gsub('grid_250', '',gsub("neg_",'-', all_predictions$img_filenames )),'_-*\\d+_*\\d*', simplify = TRUE))) %>% as.numeric()
  # all_predictions$lat %>% summary()
  # all_predictions$long%>% summary()
  #
  #
  # all_predictions_sf <- st_as_sf( all_predictions , coords= c('long','lat'), crs = 4326)
  #
  # all_predictions_spdf <- sp::SpatialPixelsDataFrame(st_coordinates(all_predictions_sf) ,data =all_predictions,proj4string = sp::CRS('+proj=longlat +datum=WGS84 +no_defs') )
  # all_predictions_spdf$Urban_Pred <- all_predictions_spdf$Urban>.5
  #
  # sp::plot(all_predictions_spdf['Urban'])
  #
  #
  #
  # ggplot(all_predictions_sf %>% sample_n(100) )+ geom_sf(  )
  #
  #
  # now <- Sys.time()
  # result_70 <- rumo::spatialSDGpredictions(DHS = rwanda_DHS_70,cluster_shp = rwanda_DHS_72_shp,  country_name = country_name,prediction_grid =  all_predictions_spdf, n_grid = 250)
  # Sys.time()-  now
  #
  #
  # now <- Sys.time()
  # predict(result_70$electricity$model,  cbind( result_70$electricity$grid_distances, result_70$electricity$grid@data))$predictions
  # Sys.time()-  now
  #
  # sp::plot(result_70$electricity$grid['Urban'])
  # urban_raster <- raster::raster(result_70$electricity$grid['Urban'])
  # raster::plot(urban_raster>.5)
  # points(rwanda_DHS_72_shp[rwanda_DHS_72_shp$URBAN_RURA == 'U',])
  # raster::plot(result_70$electricity$raster)
  #
  # result_70$wealth$grid %>% head()
  #
  # result_70$electricity$grid@coords[ result_70$electricity$grid@coords[,1] %>% between(30.037,30.038) &
  #                                      result_70$electricity$grid@coords[,2] %>%  between(-2.137,-2.138)  ,]
  #
  # data.frame(img_class)
}
