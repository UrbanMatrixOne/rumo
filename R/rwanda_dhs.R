# library(tidyverse)
# library(sf)
# library(rgdal)
#
# #read shp file containing location of clusters
# rwanda_shp <- readOGR(('./Rwanda/RW_2010_DHS_04162018_125_119378/RWGE61FL/RWGE61FL.shp'))
# rwanda_shp@data %>% head()
# plot(rwanda_shp)
#
# #load cluster covariates and attach to rwanda_shp
# geo_covariates <- read.csv('./Rwanda/RW_2010_DHS_04162018_125_119378/RWGC61FL/RWGC61FL.csv')
# rwanda_shp@data <- bind_cols(rwanda_shp@data , geo_covariates)
# rwanda_shp@data %>% head()
#
# # read dhs stata file
# rwanda_DHS <- foreign::read.dta('./Rwanda/RW_2010_DHS_04162018_125_119378/RWHR61DT/RWHR61FL.DTA', convert.factors = FALSE)
# #str(rwanda_dat)
#
#
# #compute median & average wealth_index per cluster
# rwanda_shp$avg_wealth_index <-data.frame( rwanda_DHS %>% group_by(hv001) %>% summarise(avg_wealth_index = mean(hv270)) %>% dplyr::select(avg_wealth_index))$avg_wealth_index
# rwanda_shp$median_wealth_index <-data.frame( rwanda_DHS %>% group_by(hv001) %>% summarise(median_wealth_index = median(hv270)) %>% dplyr::select(median_wealth_index))$median_wealth_index
# plot( rwanda_shp['avg_wealth_index'])
#
# #load rwanda polygon
# rwanda_subnational_borders <- st_read('./Rwanda/RW_2010_DHS_04162018_125_119378/sdr_subnational_boundaries_2018-07-22/shps/sdr_subnational_boundaries.shp')
#
# #create a grid within the polygon borders
# rwanda_grid <- st_make_grid(rwanda_subnational_borders,n = c(50,50), what = 'corners',crs= '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'  )
# rwanda_grid <- st_intersection(rwanda_grid,rwanda_subnational_borders)
# rwanda_grid <- SpatialPixelsDataFrame(proj4string =CRS(st_crs(rwanda_grid)[['proj4string']]),  st_coordinates(rwanda_grid), data = data.frame(n=1:length(rwanda_grid) ))[1]
# plot(rwanda_grid)
#
#
# #calculate buffer distances
# rwanda_grid_dist <- GSIF::buffer.dist(rwanda_shp['avg_wealth_index'],
#                                       rwanda_grid,
#                                       as.factor(1:nrow(rwanda_shp)))
#
# #construct regression formulae
# avg_wealth_formula <- as.formula(paste('avg_wealth_index ~',paste(names(rwanda_grid_dist),collapse =  '+'), '+ Nightlights_Composite'))
# med_wealth_formula <- as.formula(paste('median_wealth_index ~',paste(names(rwanda_grid_dist),collapse =  '+'), '+ Nightlights_Composite'))
# nightlights_formula <- as.formula(paste('Nightlights_Composite ~',paste(names(rwanda_grid_dist),collapse =  '+')))
#
# #construct regression matrices
# rm_wealth <-cbind(rwanda_shp@data, over(rwanda_shp['avg_wealth_index'], rwanda_grid_dist))
# rm_wealth[is.na(rm_wealth)] <- 0
#
# #fit the RF model
# avg_wealth_model <- ranger::ranger(avg_wealth_formula, rm_wealth, quantreg=TRUE,num.trees = 150, seed= 1)
# med_wealth_model <- ranger::ranger(med_wealth_formula,importance = 'impurity', rm_wealth, quantreg=TRUE,num.trees = 1000, keep.inbag = TRUE, seed= 1)
# nightlights_model <- ranger::ranger(nightlights_formula, rm_wealth, quantreg=TRUE,num.trees = 1000, keep.inbag = TRUE, seed= 1)
# xl <- as.list(ranger::importance(med_wealth_model))
# par(mfrow=c(1,1),oma=c(0.7,2,0,1), mar=c(4,3.5,1,0))
# plot(vv <- t(data.frame(xl[order(unlist(xl), decreasing=TRUE)[10:1]])), 1:10, type = "n", ylab = "", yaxt = "n", xlab = "Variable Importance (Node Impurity)")
# abline(h = 1:10, lty = "dotted", col = "grey60")
# points(vv, 1:10)
# axis(2, 1:10, labels = dimnames(vv)[[1]], las = 2)
# dev.off()
#
# #predict on the grid
# rwanda_grid$avg_wealth_pred <-  predict(avg_wealth_model,rwanda_grid_dist@data )$predictions
# rwanda_grid$med_wealth_pred <-  predict(med_wealth_model,rwanda_grid_dist@data )$predictions
# rwanda_grid$night_lights_pred <-  predict(nightlights_model,rwanda_grid_dist@data )$predictions
#
# #plot
# plot((rwanda_grid['avg_wealth_pred']))
# plot(rwanda_grid['med_wealth_pred'])
# plot(rwanda_grid['night_lights_pred'])
#
# #save to raster
# library(raster)
# raster(rwanda_grid['med_wealth_pred']) %>% writeRaster('./rwanda_med_wealth_pred.tif',overwrite=TRUE)
#
#
#
#
# #some column code/ name mapping names
#
# dhs_column_mapping <- c(
#   'hv000'= 'Country Code and Phase',
#   'hv001'= 'Cluster Number',
#   'hv002'= 'Household number',
#   'hv003'= "Respondent's line number",
#   'hv004'= 'Ultimate Area Unit',
#   'hv005'= 'Household sample weight',
#   'hv006'= 'Month of Interview',
#   'hv007'= 'Year of Interview',
#   'hv008'= 'Date of Interview',
#   'hv009'= 'Number of Houshehold Members',
#   'hv010'= 'Number of Eligible Women In Household',
#   'hv011'= 'Number of Eligible Men In Household',
#   'hv012'= 'Number of De Jure Members',
#   'hv013'= 'Number of De Facto Members',
#   'hv014'= 'Number of Children 5 and Under',
#   'hv015'= 'Result of Household Interview',
#   #...
#   'hv024'= 'Region',
#   'hv025'= 'Type of Place of Residence', #1: Urban , 2: Rural
#   'hv026'= 'Place of Residence', # 0 : Capital, 1: Small City, 2: Town, 3: Countryside
#   #...
#   'hv040'= 'Cluster Altitude in Meters',
#   'hv106'= 'Highest Educational Level Attained',
#   'hv107'= 'Highest Year of Education Completed',
#   'hv108'= 'Education Completed in Single Years',
#   'hv109'= 'Educational Attainment', #0 None, 1: Incomplete Primary, 2: Complete Primary 3 Incomplete Secondary 4 Complete Secondary 5 Higher
#   #...
#   'hv201'= 'Source of Drinking Water', #10 PIPED WATER    11 Piped into dwelling  12 Piped to yard/plot  13 Public tap/standpipe  20 TUBE WELL WATER  21 Tube well or borehole  30 DUG WELL (OPEN/PROTECTED)  31 Protected well  32 Unprotected well  40 SURFACE WATER  41 Protected spring  42 Unprotected spring  43 River/dam/lake/ponds/stream/canal/irrigation channel  51 Rainwater  61 Tanker truck  62 Cart with small tank  71 Bottled water  96 Other  (m) 99 Missing
#   'hv202'= 'Source of Non-Drinking Water',
#   'hv205'= 'Type of toilet facility', #10 FLUSH TOILET  11 Flush to piped sewer system  12 Flush to septic tank  13 Flush to pit latrine  14 Flush to somewhere else    15 Flush, don't know where 20 PIT TOILET LATRINE 21 Ventilated Improved Pit latrine (VIP) 22 Pit latrine with slab 23 Pit latrine without slab/open pit 30 NO FACILITY 31 No facility/bush/field 41 Composting toilet 42 Bucket toilet 43 Hanging toilet/latrine 96 Other (m) 99 Missing
#   'hv206'= 'Has Electricity',
#   'hv207'= 'Has Radio',
#   'hv208'= 'Has Televeision',
#   'hv209'= 'Has Refrigerator',
#   'hv210'= 'Has Bicycle',
#   'hv211'= 'Has Motorcycle Or Scooter',
#   'hv212'= 'Has Car Or Truck',
#   'hv213'= 'Main Floor Material',
#   'hv214'= 'Main Wall Material',
#   'hv215'= 'Main Roof Material',
#   #...
#   'hv221'= 'Has Telephone (Land-Line)',
#   'hv225'= 'Share Toilet With Other Households',
#   'hv226'= 'Type of Cooking Fuel',
#   #
#   'hv270'= 'Wealth Index'
# )
