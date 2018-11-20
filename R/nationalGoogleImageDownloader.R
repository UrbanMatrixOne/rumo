google_api_key <- 'AIzaSyAig-Wkbj1Rw-6ElabUWK_JlWvOpn57YJs'

downloadGoogleImageryFromBBox <- function(bbox, pixel_dim =400, zoom_factor = 18, path ='./output/png/', start =1, filename_only = FALSE, crop =FALSE )
{
  lon_min <- bbox$xmin %>% as.numeric()
  lon_max <- bbox$xmax %>% as.numeric()
  lat_min <- bbox$ymin %>% as.numeric()
  lat_max <- bbox$ymax %>% as.numeric()
  avg_lon <- as.numeric(lon_min + lon_max)/2
  avg_lat <- as.numeric(lat_min + lat_max)/2
  metersPerPx = 156543.03392 * cos(avg_lat * pi / 180) / (2^ zoom_factor)
  coordPerMeter= data.frame(lat =  1/geosphere::distHaversine(c(avg_lon,avg_lat), c(avg_lon,1+avg_lat)),
                    lon =  1/geosphere::distHaversine(c(avg_lon,avg_lat), c(1+avg_lon,avg_lat)))
  coordPerPx = coordPerMeter * metersPerPx
  coordPerScene = coordPerPx * pixel_dim
  border_pixels <- 0
  scene_distance <- metersPerPx*400

  centroids <- expand.grid(lon = seq(lon_min+ coordPerScene$lon/2, lon_max  , by = coordPerScene$lon ), lat =  seq(lat_min+ coordPerScene$lat/2, lat_max , by = coordPerScene$lat ))
  filenames <- c()
  for(i in 1:nrow(centroids))
  {
    filename <- paste0(path, 'lon_', centroids$lon[i], 'lat_', centroids$lat[i],'zoom_',zoom_factor, '.png')
    filenames <- c(filenames, filename)
    if(!filename_only)
    {
      rumo::downloadGoogleImageWithCoordinates(centroids$lat[i],centroids$lon[i], pixel_dim ,zoom_factor,filename = filename)
    }


     if(crop)
     {
      png <- png::readPNG(filename)[26:(25+pixel_dim),1:pixel_dim,]
      png::writePNG(png, filename)
     }
  }
  return(filenames)
}

downloadGoogleImagesFromSpatial <- function(spdf, pixel_dim = 400,zoom_factor = 18 , path ='./output/png/', start =1, filename_only = FALSE)
{

  img_filenames  = paste0(path, 'lon_', spdf@coords[,1], 'lat_', spdf@coords[,2],'zoom_',zoom_factor, '.png')
  if(!filename_only )
  {

    for (i in (start:nrow(spdf)))
    {
      lon = spdf[i,]@coords[,1]
      lat = spdf[i,]@coords[,2]
      filename <- paste0(path, 'lon_', lon, 'lat_', lat,'zoom_',zoom_factor, '.png')
      rumo::downloadGoogleImageWithCoordinates(lat,lon, pixel_dim = pixel_dim, zoom_factor = zoom_factor,filename = filename)
    }
  }#end if
  return (img_filenames)
}

downloadGoogleImageWithCoordinates <- function(lat,lon, pixel_dim = 400,zoom_factor = 18 , filename )
{
  url <-  paste0('https://maps.googleapis.com/maps/api/staticmap?center=' , lat, ',' , lon , '&zoom=',zoom_factor,'&size=',pixel_dim,'x',pixel_dim+50,'&maptype=satellite&key=' ,google_api_key)
  download.file(url, filename)
}

downloadNationalImagery <- function( country_name = 'Rwanda', image_path = './output/',grid =NA,start_idx =1 , end_idx = NA, batchname = 'default' ){

  png_path <- paste0(image_path,'png/',tolower(country_name),'/', batchname,'/')
  raster_path <- paste0(image_path,'raster/',tolower(country_name),'/', batchname,'/')
  config_path <- paste0(image_path,'config/',tolower(country_name),'/', batchname,'/')


  if(!file.exists(png_path))
  {
    dir.create(png_path)
  }
  if(!file.exists(png_path))
  {
    dir.create(png_path)
  }
  if(!file.exists(raster_path))
  {
    dir.create(raster_path)
  }
  if(!file.exists(config_path))
  {
    dir.create(config_path)
  }

  country_shp <-  rworldmap::countriesLow[rworldmap::countriesLow$ADMIN==country_name,]

  country_lat = country_shp$LAT
  zoom_factor <- 18

  metersPerPx = 156543.03392 * cos(country_lat * pi / 180) / (2^ zoom_factor)

  coordPerMeter=  1/geosphere::distHaversine(c(0,country_lat), c(1,country_lat))#1/(110.57 *1000)#rwanda
  coordPerPx = coordPerMeter * metersPerPx
  cellsize = coordPerPx * 400
  border_pixels <- 0
  coordPerPx*400
  if (is.na(grid))
  {
    country_grid <- SpatialPointsDataFrame( makegrid(country_shp, 1000), data =makegrid(country_shp, 1000), proj4string = CRS(proj4string(country_shp)))[country_shp,]
    colnames(country_grid@data) <- c('lon','lat')
  }else{
    country_grid <- grid
    colnames(country_grid@data)[1:2] <- c('lon','lat')
  }
  # dev.off()
  # sp::plot(country_shp)
  # points (country_grid)

  if (is.na(end_idx)){
    end_idx <- nrow(country_grid)
  }

  for (i in start_idx :nrow(country_grid))
  {

    lat <- country_grid$lat[i]
    lon <- country_grid$lon[i]

    pixel_dim <- as.integer(200 / metersPerPx)
    # zoom 16 ~ 2.3 m
    # zoom 17 ~ 1.15 m
    # zoom 18 ~ .6 m
    # zoom 19 ~ .3 m

    #remove:
    #url <-  paste0('https://maps.googleapis.com/maps/api/staticmap?center=' , lat, ',' , lon , '&zoom=',zoom_factor,'&size=',pixel_dim,'x',pixel_dim+50,'&maptype=satellite&key=' ,google_api_key)
    raw_png_location <- paste0(png_path,gsub('-', 'neg_', gsub('\\.','_' ,paste0('image_lat_',lat,'_long_',lon))) ,'.png')
    #download.file(url, raw_png_location)

    rumo::downloadGoogleImageWithCoordinates(lat,lon , pixel_dim , filename = raw_png_location )
    #load the png

    png <-255 * png::readPNG(raw_png_location,native = FALSE)[26:(25+pixel_dim),1:pixel_dim,]
    #break into 3 layers (RGB)
    r1 <- raster::raster(png[,,1], crs = country_grid@proj4string)
    r2 <- raster::raster(png[,,2], crs = country_grid@proj4string)
    r3 <- raster::raster(png[,,3], crs = country_grid@proj4string)

    #stack the layers
    raster_image <- raster::stack(list(red =r1, green = r2,blue = r3))

    #copy the extent
    raster::extent(raster_image) <-  raster::extent(
      lon - cellsize/2 - border_pixels*coordPerPx,
      lon + cellsize/2 + border_pixels*coordPerPx,
      lat - cellsize/2 - border_pixels*coordPerPx,
      lat + cellsize/2 + border_pixels*coordPerPx
    )


    filename <-  paste0(raster_path,gsub('-', 'neg_', gsub('\\.','_' ,paste0('image_lat_',lat,'_long_',lon))) ,'.tif')
    img_name <- strsplit(filename,'/')[[1]][length(strsplit(filename,'/')[[1]])]
    raster::writeRaster(overwrite = TRUE,
                        options="COMPRESS=NONE",
                        raster_image,
                        filename)
    write(   rumo::create_rastervision_config(i,img_name,scoreThresh = .50,batchname = batchname,country_name = country_name),
             file = paste0( config_path,'config',i,'.json'))
  }

  #  set.seed(1)
}

# #continuous imagery:
function()
{
  grid_radius = 15
  country_lat = -2
  zoom_factor <- 18

  metersPerPx = 156543.03392 * cos(country_lat * pi / 180) / (2^ zoom_factor)
  coordPerMeter=  1/(110.57 *1000)
  coordPerPx = coordPerMeter * metersPerPx
  pixel_dim = 400
  cellsize = coordPerPx * pixel_dim
  border_pixels <- 0

  lat_center <- -1.964893
  lon_center <- 30.15019

  lat_seq <- lat_center + cellsize* (-grid_radius:grid_radius)
  lon_seq <- lon_center + cellsize* (-grid_radius:grid_radius)

  coords <- expand.grid(lat_seq, lon_seq)
  for (i in 1:nrow(coords))
  {
    lat <- coords[i,1]
    lon <- coords[i,2]
    downloadGoogleImageWithCoordinates(lat = lat,lon = lon, filename = paste0('./output/png/rwanda/kigali/kigali',i,'.png'))

    png <-255 * png::readPNG(paste0('./output/png/rwanda/kigali/kigali',i,'.png'),native = FALSE)[26:(25+pixel_dim),1:pixel_dim,]
    #break into 3 layers (RGB)
    raster_crs <- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs '
    r1 <- raster::raster(png[,,1], crs = raster_crs)
    r2 <- raster::raster(png[,,2], crs = raster_crs)
    r3 <- raster::raster(png[,,3], crs = raster_crs)

    #stack the layers
    raster_image <- raster::stack(list(red =r1, green = r2,blue = r3))

    #copy the extent
    raster::extent(raster_image) <-  raster::extent(
      lon - cellsize/2 - border_pixels*coordPerPx,
      lon + cellsize/2 + border_pixels*coordPerPx,
      lat - cellsize/2 - border_pixels*coordPerPx,
      lat + cellsize/2 + border_pixels*coordPerPx
    )

    batchname= 'kigali'

    filename <-  paste0('./output/raster/rwanda/kigali/',gsub('-', 'neg_', gsub('\\.','_' ,paste0('image_lat_',lat,'_long_',lon))) ,'.tif')
    img_name <- strsplit(filename,'/')[[1]][length(strsplit(filename,'/')[[1]])]
    raster::writeRaster(overwrite = TRUE,
                        options="COMPRESS=NONE",
                        raster_image,
                        filename)

    config_path <- paste0('./output/config/',tolower(country_name),'/', batchname,'/')

    write(   rumo::create_rastervision_config(i,img_name,scoreThresh = .50,batchname = batchname,country_name = country_name),
             file = paste0( config_path,'config',i,'.json'))

  }
}
