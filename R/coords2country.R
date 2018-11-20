
#' coords2country
#' This function returns the countries in which a set of points resides
#' @param points a data.frame in which:column 1 contains the longitude in degrees and column 2 contains the latitude in degrees
#' @export
#' @examples
#' coods2country()
coords2country = function(points)
{
  countries_sf <- st_as_sf(rworldmap::getMap(resolution='low'))

  #setting CRS directly to that from rworldmap
  st_crs(points)  <- sf::st_crs(countries_sf)

  indices <- countries_sf[sapply(sf::st_intersects(points,countries_sf), function(z) if (length(z)==0) NA_integer_ else z[1]),]
  # return the ADMIN names of each country
  as.character(indices$NAME)
  #indices$ISO3 # returns the ISO3 code
  #indices$continent   # returns the continent (6 continent model)
  #indices$REGION   # returns the continent (7 continent model)
}


#' country2ISO3
#' This function returns the ISO3 abbreviation for a country
#' @param country_name The country's name
#' @export
#' @examples
#' country2ISO3()
country2ISO3 <- function(country_name)
{
  rworldmap::rwmGetISO3(gsub('S//.', 'South' ,country_name))
}

#' countrylist2ISO3
#' This function returns the ISO3 abbreviation for a vector of countries
#' @param country_name The vector containing country names
#' @export
#' @examples
#' countrylist2ISO3()
countrylist2ISO3 <- function(country_list)
{
    mapply(country_list, FUN = function(x){rworldmap::rwmGetISO3(x)})
}
