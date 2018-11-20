#TODO: document this!
getWorldBankData <- function(country,field)
{
  result <- wbd_sdg_data %>% filter (Country.Code== country2ISO3(country), Series.Name %in% field)   %>% spread(Series.Name, most_recent) %>% summarise_all(mean, na.rm = TRUE) %>% select(field)
  result
}
getMeanWorldBankData <- function(field)
{
  result <- (wbd_sdg_data %>%group_by(Series.Name) %>%  filter ( Series.Name %in% field)) %>% select(most_recent) %>% summarise(avg_value = mean(most_recent, na.rm = TRUE))%>% spread(Series.Name, avg_value)
  result
}

sdgFieldSearch <- function(field)
{
  wbd_sdg_data$Series.Name %>% grep((field),., ignore.case = TRUE, value = TRUE) %>% unique()
}



getRuralElectricityPct<- function(country){
  wbd_rural_electricty_data %>% filter(Country.Code== country2ISO3(country)) %>% select(X2014..YR2014.) %>% as.numeric()
}
getRuralElectricityRank<- function(country){
  wbd_rural_electricty_data %>% filter(Country.Code== country2ISO3(country)) %>% select(rank) /max(wbd_rural_electricty_data$rank)
}


getElectricityPct<- function(country){
  wbd_electricty_data %>% filter(Country.Code== country2ISO3(country)) %>% select(X2014..YR2014.) %>% as.numeric()
}
getElectricityRank<- function(country){
  wbd_electricty_data %>% filter(Country.Code== country2ISO3(country)) %>% select(rank) /max(wbd_rural_electricty_data$rank)
}

graphSDGvsEnergy <- function(field_list)
{
  x_field <- "Renewable energy consumption (% of total final energy consumption)"
  sdg_data <- wbd_sdg_data %>% filter(Series.Name %in% field_list) %>% select(Country.Name, Series.Name, most_recent) %>% inner_join(wbd_sdg_data %>% filter(Series.Name == x_field) %>% select(Country.Name,most_recent), by= 'Country.Name') %>% transmute(Series.Name = Series.Name, value = as.numeric(most_recent.x), X = as.numeric(most_recent.y))


  ggplot(sdg_data, aes(x=X, y = value , color = Series.Name) )+geom_point()+theme_bw() + xlab(x_field) +geom_smooth(method = lm)
}

#cor(model_data %>% select(-c(Country.Code,Country.Name))) %>% RTier::linux.clipboard()
# summary(linear_model)
#
#
# test <-  wbd_sdg_data %>% filter( Country.Name == 'India', Series.Name %in% field_list) %>% select(Country.Name, Series.Name, most_recent) %>% spread(Series.Name, most_recent) %>% na.omit()
#
# test[,field_list[1]]
#
# test$`Renewable energy consumption (% of total final energy consumption)` <- test$`Renewable energy consumption (% of total final energy consumption)` +5
# test$`Access to electricity (% of population)` <- test$`Access to electricity (% of population)` +5
# predict(linear_model, newdata = test)


predictSDGChange <- function(model, country ='', gainRenewable=.5, gainAccess=.5){

  if(model=='poverty'){
    prediction_model <- poverty_linear_model
    prediction_field <- 'Poverty headcount ratio at $1.90 a day (2011 PPP) (% of population)'
    multiplier = 100
  }else if (model == 'mortality'){
    prediction_model <- mortality_linear_model
    prediction_field <- 'Mortality rate, under-5 (per 1,000 live births)'
    multiplier = 1
  }else{
    return(NA)
  }
  fields <- c(prediction_field,
              'Access to electricity (% of population)',
              'Renewable energy consumption (% of total final energy consumption)' )

  baseline <-  rumo::getWorldBankData(country,fields )
  if(nrow(baseline)==0){
    baseline <- rumo::getMeanWorldBankData(fields)
  }

  test <- baseline
  test$`Renewable energy consumption (% of total final energy consumption)` <- min(100,baseline$`Renewable energy consumption (% of total final energy consumption)` +gainRenewable)
  test$`Access to electricity (% of population)` <- min(100,baseline$`Access to electricity (% of population)` +gainAccess)
  y0 <- predict(prediction_model, newdata = baseline,type = 'response')[[1]]
  y1 <- predict(prediction_model, newdata = test,type = 'response')[[1]]

  y1-y0
  return( (y1-y0))
}
