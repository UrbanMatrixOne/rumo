#install.packages('geoR')
function()
{
  result_70$cluster_shp$percent_household_electricity_1_2 <- result_70$cluster_shp$percent_household_electricity_1_2_modified <- result_70$cluster_shp$percent_household_electricity+1

  pt <- data.frame(lat = -2, lon = 29.5)
  coordinates(pt) <- ~lon+lat
  proj4string(pt) <- result_70$cluster_shp@proj4string
  plot(rworldmap::countriesCoarse[rworldmap::countriesCoarse$ADMIN == 'Rwanda',])
  points(pt)
  pt_sf <- st_as_sf(pt)

  intersection <- st_intersects(st_as_sf(result_70$cluster_shp) ,st_buffer(pt_sf,45000*1/110575.63), sparse = FALSE)[,1]
  result_70$cluster_shp$percent_household_electricity_1_2_modified[intersection] <- pmin(2, result_70$cluster_shp[intersection,]@data$percent_household_electricity_1_2_modified +.22)

  result_70$cluster_shp %>% summary



  data_geo <- geoR::as.geodata(result_70$cluster_shp['median_wealth_index'])
  data_geo <- geoR::as.geodata(result_70$cluster_shp['percent_household_electricity_1_2'])
  data_geo_modified <- geoR::as.geodata(result_70$cluster_shp['percent_household_electricity_1_2_modified'])

  ini.v <- c(var(log1p(data_geo$data)),500)
  now <-  Sys.time()
  sdg_variogram <- geoR::likfit(data_geo, lambda=0, ini=ini.v, cov.model="exponential")
  Sys.time()-now  # 12-15 seconds

  locs = result_70$electricity$grid@coords
  now <-  Sys.time()
  sdg_krige <- geoR::krige.conv(data_geo, locations=locs, krige=geoR::krige.control(obj.model=sdg_variogram))
  sdg_krige_modified <- geoR::krige.conv(data_geo_modified, locations=locs, krige=geoR::krige.control(obj.model=sdg_variogram))
  Sys.time()-now # 8-9 seconds

  result_70$electricity$grid$sdg_predict_krige <- sdg_krige$predict
  result_70$electricity$grid$sdg_predict_krige_range <- sdg_krige$krige.var

  result_70$electricity$grid$sdg_predict_krige_modified <- sdg_krige_modified$predict
  result_70$electricity$grid$sdg_predict_krige_range_modified <- sdg_krige_modified$krige.var


  dev.off ()
  plot(raster::raster(result_70$electricity$grid['sdg_predict_krige']))
  plot(raster::raster(result_70$electricity$grid['sdg_predict_krige_modified']))

  plot(result_70$electricity$grid['sdg_predict_krige_range'])


  devtools::install_github('edzer/gstat')

  meuse['zinc']
}
