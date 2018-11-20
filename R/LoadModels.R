solar_capacity_model <- readRDS('./models/solar_capacity_model.rds')
poverty_linear_model <- readRDS('./models/poverty_linear_model.rds')
mortality_linear_model <- readRDS('./models/mortality_linear_model.rds')

fit_solar_model <- function(){
  solar_capacity <- read.csv('./data/Energy/20180329_Solar_Capacity_by_latitude.csv',stringsAsFactors = FALSE)
  solar_capacity$Installed %>% substr(start = 1,stop = 4)
  plot(x= abs(solar_capacity$Latitude), y= solar_capacity$CF)
  solar_capacity_model <- lm(data= solar_capacity,formula =  CF~Latitude)
  saveRDS(solar_capacity_model,'./models/solar_capacity_model.rds')
  plot(predict(solar_capacity_model, data.frame(Latitude = 30)))
}

fit_SDG_model <- function()
{
#model
field_list <- c("Poverty headcount ratio at national poverty lines (% of population)",
                'Poverty headcount ratio at $1.90 a day (2011 PPP) (% of population)',
                'People using at least basic drinking water services (% of population)',
                'Urban population (% of total)',
                'Urban population growth (annual %)',
                'Access to electricity (% of population)',
                'Access to electricity, rural (% of rural population)',
                'Access to electricity, urban (% of urban population)',
                'Renewable electricity output (% of total electricity output)',
                'Renewable energy consumption (% of total final energy consumption)',
                'People practicing open defecation (% of population)',
                'People using at least basic sanitation services (% of population)',
                'Individuals using the Internet (% of population)',
                'Primary education, duration (years)',
                'Preprimary education, duration (years)',
                'Secondary education, duration (years)',

                'CO2 emissions (metric tons per capita)',
                'Intentional homicides (per 100,000 people)',
                'GDP per capita (current US$)',
                'GDP per capita growth (annual %)',
                'PM2.5 air pollution, mean annual exposure (micrograms per cubic meter)',
                'PM2.5 air pollution, population exposed to levels exceeding WHO guideline value (% of total)',
                'Mortality rate, neonatal (per 1,000 live births)',
                'Mortality rate, under-5 (per 1,000 live births)',
                'Mortality rate, under-5, female (per 1,000 live births)',
                'Mortality rate, under-5, male (per 1,000 live births)'

)

model_data <- wbd_sdg_data %>% filter( Series.Name %in% field_list) %>% select(Country.Name,Country.Code, Series.Name, most_recent) %>% spread(Series.Name, most_recent) %>% na.omit() %>% as.data.frame()

poverty_linear_model <-lm(data = model_data, `Poverty headcount ratio at $1.90 a day (2011 PPP) (% of population)` ~ `Access to electricity (% of population)` + `Renewable energy consumption (% of total final energy consumption)`+ (`Access to electricity (% of population)` * `Renewable energy consumption (% of total final energy consumption)`))
summary(poverty_linear_model)


mortality_linear_model <- lm(data = model_data, `Mortality rate, under-5 (per 1,000 live births)` ~  `Access to electricity (% of population)` )
summary(mortality_linear_model)

saveRDS(poverty_linear_model,'./models/poverty_linear_model.rds')
saveRDS(mortality_linear_model,'./models/mortality_linear_model.rds')
}
