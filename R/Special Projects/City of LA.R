#LA
function(){
  LA_County_Limit <-tidycensus::get_acs(geography = 'county',state = 'CA', county = 'Los Angeles', variables = 'B19013_001' ,geometry = TRUE)
  LA_City_Limit <-sf::st_read('~/Downloads/City of LA/City_Boundary/City_Boundary.shp')


  LA_City_Limit %>% ggplot()+geom_sf(aes(fill = CITY_LABEL))

  st_crs(t) <- st_crs(rumo::WRI_POWER_DATABASE_sf)
  LA_intersections <- sf::st_read('~/Downloads/City of LA/GeomIntersections/GeomIntersections.shp')

  traffic_count <- read.csv('~/Downloads/City of LA/LADOT_Traffic_Counts_Summary.csv', stringsAsFactors = FALSE)
  traffic_count$Primary.Street <- toupper(gsub(' th', 'TH',gsub(' nd', 'ND',gsub(' rd', 'RD',  gsub(' st','ST',traffic_count$Primary.Street)))))
  traffic_count$Cross.Street <- toupper(gsub(' th', 'TH',gsub(' nd', 'ND',gsub(' rd', 'RD',  gsub(' st','ST',traffic_count$Cross.Street)))))
  LA_traffic_intersections <- LA_intersections %>% inner_join(traffic_count, by = c('FROM_ST'= 'Primary.Street', 'TO_ST'='Cross.Street'))
  #only matched on 349 out of 5953 in csv (41k isections overall)

  pop_density <- read.csv('~/Downloads/City of LA/TblPopsum.csv')
  commute_by_mode <- read.csv('~/Downloads/City of LA/TblCommutebyMode.csv')
  employment <- read.csv('~/Downloads/City of LA/TblEmployment.csv')
  land_use <- read.csv('~/Downloads/City of LA/TblLu.csv')
  transportation <- read.csv('~/Downloads/City of LA/TblTransportation.csv')
  health <-  read.csv('~/Downloads/City of LA/TblHealth.csv')

  plot(health$calenvscreen,health$healthind)


  commute_by_mode %>% head()
  land_use %>% head()
  transportation %>% head()

  LA_intersection_data <- LA_intersections %>%
    inner_join(pop_density, by = c('boeint_pke'='boeint_fkey')) %>%
    inner_join(commute_by_mode, by = c('boeint_pke'='boeint_fkey')) %>%
    inner_join(employment, by = c('boeint_pke'='boeint_fkey')) %>%
    inner_join(land_use, by = c('boeint_pke'='boeint_fkey')) %>%
    inner_join(transportation, by = c('boeint_pke'='boeint_fkey')) %>%
    inner_join(health, by = c('boeint_pke'='boeint_fkey')) %>%
    select(-starts_with('GlobalId'),-starts_with('OBJECTID'))

  cov(LA_intersection_data,use = 'all.obs')

  LA_intersection_data %>% sample_n(1000)%>% ggplot()+geom_sf(aes(fill=calenvscreen))
  LA_intersection_data %>% transmute(drivealone_ct/ commute_total, driveshare_ct/ commute_total,bus_ct/ commute_total,railferry_ct/ commute_total,bike_ct/ commute_total,walk_ct/ commute_total,commuteother_ct/ commute_total,workathome_ct/ commute_total)%>%  plot()

  LA_intersection_data %>% transmute(drivealone_ct/ commute_total, driveshare_ct/ commute_total,bus_ct/ commute_total,railferry_ct/ commute_total,bike_ct/ commute_total,walk_ct/ commute_total,commuteother_ct/ commute_total,workathome_ct/ commute_total)%>%  plot()

  #traffic
  traffic_data <- sf::st_read('~/Downloads/City of LA/Traffic_Data/Traffic_Data.shp',stringsAsFactors = FALSE)

  #health
  LA_Adult_Obesity <- sf::st_read('~/Downloads/City of LA/Prevalence_of_Adult_Obesity_20132014/Prevalence_of_Adult_Obesity_20132014.shp',)
  LA_Adult_Asthma <- sf::st_read('~/Downloads/City of LA/Prevalence_of_Adult_Asthma_20132014/Prevalence_of_Adult_Asthma_20132014.shp')
  LA_Adult_Heart_Disease <- sf::st_read('~/Downloads/City of LA/Prevalence_of_Adult_Heart_Disease_20132014/Prevalence_of_Adult_Heart_Disease_20132014.shp')
  LA_Diabetes <- sf::st_read('~/Downloads/City of LA/Diabetes_2013to2014_CHISNE/Diabetes_2013to2014_CHISNE.shp')
  #LA_Health <- LA_Adult_Obesity %>% select(ZIPCODE)

  LA_Adult_Asthma %>% select(Percent_) %>% plot()

  #SWITRS (statewide traffic- large) 163,415 traffic incidents
  SWITRS <- sf::st_read('~/Downloads/City of LA/GeomSWITRS_2009to2013/GeomSWITRS_2009to2013.shp',stringsAsFactors = FALSE)

  #contains some points with lon,lat ~ 0,0
  SWITRS <- SWITRS[!is.na(SWITRS$Match_addr),]
  SWITRS <- SWITRS[abs(st_coordinates(SWITRS)[,1]) > .5 & abs(st_coordinates(SWITRS)[,1]) >.5 ,]
  st_bbox(SWITRS)
  nrow(SWITRS)

  SWITRS %>% group_by(coord= paste(round(POINT_X,3),round(POINT_Y,3))) %>% summarise(count = n())%>% sample_n(1000) %>% select(count) %>% ggplot()+ geom_sf(aes(color = count))


  Connect_High_Bike_Walk_Communties_to_Job_Rich_Districts <- sf::st_read('~/Downloads/City of LA/Connect_High_Bike_Walk_Communties_to_Job_Rich_Districts/Connect_High_Bike_Walk_Communties_to_Job_Rich_Districts.shp',stringsAsFactors = FALSE)

  #zoning

  #general plan circulation
  #(types of streets)
  General_Plan_Circulation <- sf::st_read('~/Downloads/City of LA/General_Plan_Circulation/General_Plan_Circulation.shp',stringsAsFactors = FALSE)
  General_Plan_Circulation %>% filter(DESCRIPTIO == 'Freeway') %>% select(TYPE) %>% leaflet() %>% addProviderTiles(providers$e) %>% addPolylines()
  General_Plan_Circulation$DESCRIPTIO %>% table()

  #general_Plan_Land_Use
  General_Plan_Land_Use <-  sf::st_read('~/Downloads/City of LA/General_Plan_Land_Use_GPLU/General_Plan_Land_Use_GPLU.shp',stringsAsFactors = FALSE)
  General_Plan_Land_Use %>% head()
  General_Plan_Land_Use$CATEGORY %>% table()
  table(General_Plan_Land_Use$GPDESC, General_Plan_Land_Use$CATEGORY)
  General_Plan_Land_Use %>% sample_n(10000) %>% ggplot() + geom_sf(aes(fill = CATEGORY))

  coords <- sf::st_centroid(General_Plan_Land_Use) %>% sf::st_coordinates()
  classif_data <- data.frame(CATEGORY= General_Plan_Land_Use$CATEGORY, AREA = General_Plan_Land_Use$Shape__Are, lon = coords[,1], lat = coords[,2])

  train_set <- sample(c(FALSE,TRUE),size = round(.75*nrow(classif_data),0),replace = TRUE)
  test_set <- ! train_set

  classif_task <- mlr::makeClassifTask(data = classif_data[,c('CATEGORY','lon','lat')], target = 'CATEGORY')

    classif_learner <- mlr::makeLearner('classif.linDA', predict.type = 'response' )#

  classif_model <- mlr::train(classif_learner,classif_task,subset =train_set )
  mlr::performance(predict(classif_model, newdata= classif_data[test_set,c('CATEGORY','lon','lat')]), measures = list("mmce" = mlr::mmce, "acc" = mlr::acc))
  mlr::performance(predict(classif_model, newdata= classif_data[train_set,c('CATEGORY','lon','lat')]), measures = list("mmce" = mlr::mmce, "acc" = mlr::acc))
  mlr::calculateConfusionMatrix(predict(classif_model, newdata= classif_data[train_set,c('CATEGORY','lon','lat')]))
  General_Plan_Land_Use$CATEGORY_pred <- predict(classif_model, newdata= classif_data[,c('lon','lat')])$data$response

  General_Plan_Land_Use %>% sample_n(10000) %>% ggplot() + geom_sf(aes(fill = CATEGORY_pred))



  pred_grid <- sf::st_make_grid(LA_City_Limit,n = 250,what='centers')

  grid_coords <- data.frame(lon = sf::st_coordinates(pred_grid)[,1],lat = sf::st_coordinates(pred_grid)[,2])
  pred_grid <- sf::st_sf(cbind(pred_grid,grid_coords)) %>% filter(sf::st_within( pred_grid, LA_City_Limit,sparse = FALSE))

  pred_grid$pred <-  predict(classif_model,newdata = pred_grid)$data$response

  pred_grid %>% ggplot()+geom_sf(aes(color = pred))
  plot(pred_grid %>% select(pred))

  cluster_task <-   mlr::makeClusterTask(data=as.data.frame(coords))
  cluster_learner <- mlr::makeLearner('cluster.kmeans',predict.type= 'response',centers = 4)
  cluster_model <- mlr::train(cluster_learner,cluster_task)
  General_Plan_Land_Use$cluster_pred <- cluster_model$learner.model$cluster


  #an asthma model
  isect <-   sf::st_intersects(General_Plan_Land_Use, LA_Adult_Asthma,sparse = TRUE)
  General_Plan_Land_Use_DF <- General_Plan_Land_Use
  sf::st_geometry( General_Plan_Land_Use_DF) <- NULL

  General_Plan_Land_Use_DF$ZIP <- as.character(LA_Adult_Asthma[  unlist(lapply(isect,function(x){ifelse(length(x)>1,x[[1]],x)}) ) ,]$ZIPCODE)
  General_Plan_Land_Use_DF$CATEGORY <- gsub(' ','_', General_Plan_Land_Use_DF$CATEGORY)
  land_use_summary <- General_Plan_Land_Use_DF %>% group_by(ZIP,CATEGORY) %>% summarise(Area= sum(Shape__Are)) %>% mutate(percentage = Area/sum(Area))

  land_use_summary_spread <- General_Plan_Land_Use_DF %>% spread(key = CATEGORY, value = Shape__Are,fill = 0) %>% group_by(ZIP) %>%
    summarise_at(General_Plan_Land_Use_DF$CATEGORY %>% unique(), sum)

  land_use_summary_spread$total_area <- rowSums(land_use_summary_spread %>% select(-ZIP))
  land_use_summary_spread <- land_use_summary_spread %>% mutate_at(General_Plan_Land_Use_DF$CATEGORY %>% unique(), function(x){x/land_use_summary_spread$total_area})

  #how many power plants in each zip
  LA_City_Power <- rumo::WRI_POWER_DATABASE_sf %>% sf::st_join(LA_Adult_Asthma, left = FALSE)

  #traffic - how many big streets or cars at intersections in the zip...more cars -> worse health

  asthma_dataset <- land_use_summary_spread %>% inner_join(LA_Adult_Asthma %>% transmute(ZIP = ZIPCODE, pct_asthma = Percent_))
  asthma_dataset <- inner_join(asthma_dataset,  (LA_City_Power %>% group_by(ZIP = ZIPCODE) %>%summarise(num_plants = n())), by= 'ZIP')
  asthma_dataset$geometry.x <- asthma_dataset$geometry.y <-  NULL
  #   LA_ZIP_Power_dist_matrix <-  st_distance(LA_County_Power,LA_Adult_Asthma ) %>% as.data.frame()
  #
  # asthma_dataset <- data.frame(pct_asthma = LA_Adult_Asthma$Percent_)
  # asthma_dataset$closest_plant_1 <-  apply(LA_ZIP_Power_dist_matrix, MARGIN = 2, function(x){sort(x,FALSE)[1]})
  # asthma_dataset$closest_plant_1_fuel <-  LA_County_Power[ apply(LA_ZIP_Power_dist_matrix, MARGIN = 2, function(x){which(x == sort(x, FALSE)[1])[1]}),]$Primary.Fuel %>% as.factor()
  # asthma_dataset$closest_plant_1_capacity <-  LA_County_Power[ apply(LA_ZIP_Power_dist_matrix, MARGIN = 2, function(x){which(x == sort(x, FALSE)[1])[1]}),]$capacity_mw
  # asthma_dataset$closest_plant_1_output <-  LA_County_Power[ apply(LA_ZIP_Power_dist_matrix, MARGIN = 2, function(x){which(x == sort(x, FALSE)[1])[1]}),]$estimated_generation_gwh
  #

  asthma_dataset <- asthma_dataset[!is.na(asthma_dataset$pct_asthma),]
  regr_colnames <- colnames(asthma_dataset)[! colnames(asthma_dataset) %in% c( 'pct_asthma', 'geometry', 'total_area','ZIP')]
  regr_formula <- as.formula(paste('pct_asthma', paste(regr_colnames,collapse = '+'), sep = '~'))

  lm.fit <- lm(data = asthma_dataset, regr_formula)
  summary(lm.fit)
  plot(lm.fit)
  dev.off()
  plot(asthma_dataset$pct_asthma, predict(lm.fit))

  plot(asthma_dataset$num_plants, asthma_dataset$pct_asthma)

  asthma_dataset %>% select(-c(ZIP,total_area)) %>% ggplot()+ geom_sf(aes(fill = -Open_Space))
}
