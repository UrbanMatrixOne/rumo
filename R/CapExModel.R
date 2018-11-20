#cap ex projection
fitTreeModel <- function(){
  test_set  <-  clean_IGES_data%>% sample_frac(.1)
  train_set <- clean_IGES_data %>% setdiff(test_set)

  fit<- tree.model <- rpart::rpart(data =  train_set %>% filter(NPVperkw <.5 ),  NPVperkw ~ (start_date) + fuel_type + capacity , method = 'anova')
  printcp(fit) # display the results
  plotcp(fit) # visualize cross-validation results
  summary(fit) # detailed summary of splits
  par(mfrow=c(1,2)) # two plots on one page
  rsq.rpart(fit) # visualize cross-validation results
  plot(fit, uniform=TRUE,
       main="Classification Tree for IGES_CDM")
  text(fit, use.n=TRUE, all=TRUE, cex=.8)
  fit.2 <- prune(tree.model, cp= fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
  plot(fit.2, uniform=TRUE,
       main="Classification Tree for IGES_CDM")
  text(fit.2, use.n=TRUE, all=TRUE, cex=.8)

  ggplot(data.frame(predict  =   predict(fit, clean_data), actual  = clean_data$NPVperkw, type= clean_data$fuel_type), aes(x = actual, y = predict, color = type)) + geom_point( ) + theme_bw()


  #Save Model
  saveRDS(fit, 'CapExModel.rds')
}

#' getCapEx
#' This function estimats the cost of a grid power plant with a given capacity
#' @param fuel_type e.g. "Wind power" , "Hydro power" or "PV"
#' @param capacity the capacity of the project to be estimated
#' @param country the country the project is built in
#' @export
#' @examples
#' getCapEx()
getCapEx <- function(fuel_type,capacity , country = 'generic')
{
  #predict(fit,data.frame(start_date = as.Date('2018-03-21'), capacity = capacity, fuel_type = fuel_type ))
  ifelse(fuel_type == 'Wind power', 1385706,
         ifelse(fuel_type == 'Hydro power', 1240639*2 ,
                1000000   #solar
                )
         )*ifelse(country == 'China', 1.2,1)
}
