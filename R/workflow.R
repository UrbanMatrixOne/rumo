function(){
library(tidyverse)
n_grid <- 250
rwanda_DHS_70 <- foreign::read.dta('./data/DHS/RWHR70DT/RWHR70FL.DTA', convert.factors = FALSE)
rwanda_DHS_72_shp <- rgdal::readOGR('./data/DHS/RWGE72FL/RWGE72FL.shp')
cluster_covariates <- data.frame(cluster.name = unique(rwanda_DHS_70$hv001) )
cluster_covariates$median_wealth_index <-data.frame( rwanda_DHS_70 %>% dplyr::group_by(hv001) %>% dplyr::summarise(computed_value = median(hv270)) %>% dplyr::select(computed_value))$computed_value


grid_predict <- rumo::buildSpatialDistributionModelAndGrid(predict_colname = 'median_wealth_index',
                                     cluster_data =  cluster_covariates,
                                     cluster_shp = rwanda_DHS_72_shp,
                                     country_name = 'Rwanda',
                                     n_grid = n_grid)$grid



print(nrow(grid_predict))

#download a google image for each grid point:
country <- 'Rwanda'
batchname <- 'grid_250'
num_images <- nrow(grid_predict)
rumo::downloadNationalImagery(grid = grid_predict,batchname= batchname,country_name = country )
#rumo::downloadNationalImagery(grid = grid_predict,batchname= batchname,country_name = country ,start_idx = 11469)
#external steps
#1 clean the tifs
paste0('python3 ~/gitprojects/spacenet/rio_rastervision/scripts/clean_geotif_dir.py ',
'./output/raster/',tolower(country),'/',batchname, '/'  ,
 ' ./output/raster/',tolower(country), '/',batchname ,'/')

#2 run the predictions
  #a) copy the configs to /home/dan/gitprojects/spacenet/rio_rastervision/COUNTRY/config/BATCHNAME
  #b) copy the images to /home/dan/gitprojects/spacenet/rio_rastervision/COUNTRY/zoom18/
        # find  ./output/raster/rwanda/grid_250/ -name "*.tif" -exec cp -uf "{}" /home/dan/gitprojects/spacenet/rio_rastervision/rwanda/zoom18/ \;
  #c) generate & run the bash script
    write( paste0('#!/bin/bash\n',paste0( 'python -m rastervision.run predict /opt/data/',tolower(country) ,  '/config/',batchname,'/config', 1:num_images , '.json', collapse = '\n')),  #sample.int(n = num_images,size = num_images)
    paste0(paste('/home/dan/gitprojects/spacenet/rio_rastervision/run',tolower(country),batchname, sep = "_"),'.sh' ))

    print(paste0('bash ', paste('/opt/data/run',tolower( country),batchname, sep = "_"),'.sh' ))

#3 consolidate predictions
    paste0('python3 ~/gitprojects/spacenet/rio_rastervision/scripts/merge_geojson.py ',
           '~/gitprojects/spacenet/rio_rastervision/predictions/',tolower(country),'/',batchname,'/ ',
           './output/predictions/',tolower(country), '_',batchname ,'.json')

#predictions are here:

prediction_path <- paste0('./output/predictions/',tolower(country), '_',batchname ,'.json')  #'./data/Shapefiles/rwanda_grid_predictions.json'
predictions <- rgdal::readOGR(prediction_path, stringsAsFactors = FALSE)



num_features <- do.call(rbind,lapply( sf::st_is_within_distance(sf::st_as_sf(grid_predict), sf::st_as_sf( predictions),500),function(x){length(x)}))[,1]
grid_predict$num_buildings <- num_features

#train a model with this new info:
tmp_result<- buildSpatialDistributionModelAndGrid(predict_colname = 'median_wealth_index',
                                     regression_colnames ='num_buildings',
                                     grid = grid_predict,
                                     cluster_data = rwanda_cluster_covariates,
                                     cluster_shp = rwanda_shp,
                                     country.name = 'Rwanda',
                                     n_grid = n_grid)



      sp::plot(grid_predict['pred_median_wealth_index'])
      sp::plot(tmp_result$grid['pred_median_wealth_index'])
      points(tmp_result$grid[tmp_result$grid@data$num_buildings >10 , ])


}
