
png2raster <- function(png_path =  '~/Downloads/Rwanda/images/', raster_path ='~/gitprojects/spacenet/rio_rastervision/rwanda/', config_path = NA,
                       dist_between_centroids_lat = 1/120  ,dist_between_centroids_lon = 1/120, dimensions = 400, border_pixels = 6 ,
                       batchname = '')
{
  png_filenames <- Sys.glob(paste0(png_path,'*'))
  png_filenames %>% head()

  coords <-  data.frame(
    lon = as.numeric(gsub('_','.', gsub('neg_','-',gsub('long?_','', str_extract(png_filenames, 'long?_(neg_)?\\d+(_|\\.)\\d+'))))),
    lat = as.numeric(gsub('_','.', gsub('neg_','-',gsub('lat_','', str_extract(png_filenames, 'lat_(neg_)?\\d+(_|\\.)\\d+'))))),
    filename = png_filenames, stringsAsFactors = FALSE
  )
  coords <-coords %>% arrange(lat,lon)

  centroids <- sp::SpatialPointsDataFrame(coords[,c('lon','lat')], data = data.frame(coords$filename),proj4string = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))

  #plot(centroids %>% head)

  for (i in 1:nrow(centroids)){
    if  (i%%100 ==0){
      print(paste('iteration:',i))
    }



    pixels_between_centroids_lat <- dimensions
    size_of_each_pixel_lat <-  dist_between_centroids_lat/pixels_between_centroids_lat


    png <-255 * png::readPNG(coords$filename[i],native = FALSE)[26:(25+dimensions),1:dimensions,]
    r1 <- raster::raster(png[,,1], crs = centroids@proj4string)
    r2 <- raster::raster(png[,,2], crs = centroids@proj4string)
    r3 <- raster::raster(png[,,3], crs = centroids@proj4string)

    #    raster_image <- stack('~/gitprojects/spacenet/rio_rastervision/imagery/3band_AOI_1_RIO_img6006.tif')
    raster_image <- raster::stack(list(red =r1, green = r2,blue = r3))
    raster_image



    raster::extent(raster_image) <-  raster::extent(
      coords$lon[i] - size_of_each_pixel_lat*dimensions /2 - border_pixels*size_of_each_pixel_lat,
      coords$lon[i] + size_of_each_pixel_lat*dimensions/2 + border_pixels*size_of_each_pixel_lat,
      coords$lat[i] - size_of_each_pixel_lat*dimensions/2 - border_pixels*size_of_each_pixel_lat,
      coords$lat[i] + size_of_each_pixel_lat*dimensions/2 + border_pixels*size_of_each_pixel_lat
    )

    #print(paste0('"/opt/data/rwanda/','google2raster_',coords$lat[i],'_', coords$lon[i],'.tif'))

    raster::writeRaster(overwrite = TRUE,
                        options="COMPRESS=NONE",
                        raster_image,
                        paste0(raster_path,'google2raster_',coords$lat[i],'_', coords$lon[i],'.tif')
    )
    if (! is.na(config_path ))
    {
      write( rumo::create_rastervision_config(i, paste0('google2raster_',coords$lat[i],'_', coords$lon[i],'.tif'), batchname = batchname), file = paste0( config_path, 'config',i,'.json'))
    }

  }#end for
}#end png2raster


create_rastervision_config <- function(i,filename,  scoreThresh= .5,country_name, batchname)
{
  paste0('{
  "scenes": [
    {
      "predictionLabelStore": {
        "objectDetectionGeojsonFile": {
          "uri": "/opt/data/predictions/',tolower(country_name) ,'/', batchname,'/',batchname,'_predictions',i,'.json"
        }
      },
      "rasterSource": {
        "rasterTransformer": {
          "statsUri": "/opt/data/stats/stats.json",
          "channelOrder": [
            0,
            1,
            2
          ]
        },
        "geotiffFiles": {
          "uris": [
            "/opt/data/rwanda/zoom18/',filename,'"

          ]
        }
      },
      "id": "R03C2"
    }
  ],
  "machineLearning": {
    "task": "OBJECT_DETECTION",
    "classItems": [
      {
        "id": 1,
        "name": "building"
      }
    ],
    "backend": "TF_OBJECT_DETECTION_API"
  },
  "options": {
    "modelUri": "/opt/data/rio-model",
    "debug": true,
    "objectDetectionOptions": {
      "mergeThresh": 0.10000000149011612,
      "scoreThresh": ',scoreThresh,'
    },
    "debugUri": "/opt/data/debug",
    "chipSize": 300
  }
}
')
}
