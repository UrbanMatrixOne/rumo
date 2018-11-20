#' intensity_projection
#' This function estimates the intensity of power generation for a location
#' @param x is a sf object representing a location
#' @param TW how many terawatts should we assume are relevant to the projection (default 50)
#' @param min_disclosure
#' @param country.in (optional) restrict neighbors to within this country
#' @param source data source: either 'carma' or 'combined' (WRI + carma)
#' @param weight_distance whether to weight the contribution of power plants by their proximity to point x
#' @export
#' @examples
#' intensity_projection()
intensity_projection<- function(x, TW= 50,min_disclosure = 0, country.in = '', source = "carma", weight_distance = FALSE) {
  plant.database <-   switch(source,
         carma = CARMA_DATABASE,

         combined= carma_WRI_combined_database
           )

  plant.database.sf <-   switch(source,
                             carma = CARMA_DATABASE_sf,

                             combined= carma_WRI_combined_database_sf
  )

  if( !country.in %in% unique(plant.database$country))
  {
    country.in <- ''
  }
  plant.database %>% filter(grepl(paste0(country.in,"*"),country),  dis >= min_disclosure,longitude != 0, latitude!= 0) %>% select(name,intensity_Future ,energy_Future) %>%
    bind_cols(data.frame(st_distance( plant.database.sf %>% filter(grepl(paste0(country.in,"*"),country),dis >= min_disclosure,(st_coordinates(.) != c(0,0))[,1]),x ) )) %>%
    gather(Project, distance,-c(name, intensity_Future, energy_Future) ) %>%
    arrange(Project, distance) -> plant.distance.tbl

  plant.distance.list <- split( plant.distance.tbl  ,plant.distance.tbl $Project)

  output <-  do.call(rbind, lapply(plant.distance.list, FUN =
                                     function(x){x %>%mutate(local.capacity = cumsum(energy_Future)) %>%
                                         filter(local.capacity-energy_Future <=TW*1000000 ) %>%
                                         summarise( intensity_projection= weighted.mean(intensity_Future,energy_Future/ifelse(weight_distance,distance,1))) }))
  output$input.name <- gsub('X','', rownames(output)) %>% as.numeric()
  output  <-   output %>% arrange(input.name)
  output
}



#' closest_plants
#' This function returns the list of power plants relevant to an intensity projection.
#' @param x is a sf object representing a location
#' @param TW how many terawatts should we assume are relevant to the projection (default 50)
#' @param min_disclosure
#' @param country.in (optional) restrict neighbors to within this country
#' @param source data source: either 'carma' or 'combined' (WRI + carma)
#' @param weight_distance whether to weight the contribution of power plants by their proximity to point x
#' @export
#' @examples
#' closest_plants()
closest_plants<- function(x, TW= 50, min_disclosure = 0, country.in = '', source = "carma", weight_distance = FALSE) {
  plant.database <-   switch(source,
                             carma = CARMA_DATABASE,

                             combined= carma_WRI_combined_database
  )

  plant.database.sf <-   switch(source,
                                carma = CARMA_DATABASE_sf,

                                combined= carma_WRI_combined_database_sf
  )



    if( !country.in %in% unique(plant.database$country))
  {
    country.in <- ''
  }

  plant.database %>% filter(grepl(paste0(country.in,"*"),country),  dis >= min_disclosure,longitude != 0, latitude!= 0) %>% select(country, name,intensity_Future ,energy_Future,dis, longitude,latitude) %>%
    bind_cols(data.frame(st_distance( plant.database.sf %>% filter(grepl(paste0(country.in,"*"),country),dis >= min_disclosure, (st_coordinates(.) != c(0,0))[,1]),x[1,] ) )) %>%
    gather(Project, distance,-c(name, intensity_Future, energy_Future,dis, longitude, latitude, country) ) %>%
    arrange(Project, distance) %>%  mutate(local.capacity = cumsum(energy_Future)) %>%   filter(local.capacity-energy_Future <=TW*1000000 )#-> plant.distance.tbl

  #plant.distance.list <- split( plant.distance.tbl)

  # do.call(rbind, lapply(plant.distance.list, FUN = function(x){x %>%mutate(local.capacity = cumsum(energy_Future)) %>%   filter(local.capacity-energy_Future <=TW*1000000 )  }))

}

#' getNationalProduction
#' This function estimates how much energy is produced by grid power plants in a Country
#' @param country.in (optional) restrict neighbors to within this country
#' @param source data source: either 'carma' or 'combined' (WRI + carma) (not implemented)
#' @param weight_distance whether to weight the contribution of power plants by their proximity to point x
#' @export
#' @examples
#' getNationalProduction()
getNationalProduction <- function(country.in, source = "carma")
{
  #TODO add WRI as a data source
  CARMA_DATABASE %>% filter(grepl(paste0(country.in,"*"),country)) %>% select(energy_Future) %>% sum()
}

#' getCapacityFactor
#' This function estimates capacity factor for Wind, Hydro , and Solar grid power plants
#' @param fuel_type e.g. "Wind power" , "Hydro power" or "PV"
#' @param latitude relevant for PV capacity
#' @export
#' @examples
#' getCapacityFactor()
getCapacityFactor<- function(fuel_type, latitude=30){
  ifelse(fuel_type == 'Wind power', .25,
         ifelse(fuel_type == 'Hydro power', .4,
                predict(solar_capacity_model, data.frame(Latitude = latitude))#solar
         )
  )
}

#' getCapacityFactorDistribution
#' This function returns a normal distribution for capacity factor for Wind, Hydro , and Solar grid power plants
#' @param fuel_type e.g. "Wind power" , "Hydro power" or "PV"
#' @param latitude relevant for PV capacity
#' @export
#' @examples
#' getCapacityFactorDistribution()
getCapacityFactorDistribution<- function(fuel_type, latitude=30, num_sample=10){
    rnorm( num_sample,
           getCapacityFactor(fuel_type, latitude),
           .15 )

}



