#Carbon Volatility

#
# daily_CER <- read.csv('../data/market data/CER_Prices_daily.csv',stringsAsFactors = FALSE)
# daily_CER <- daily_CER %>% mutate(Date=as.Date(Date)) %>% filter(Date %>% between(as.Date('2008-01-01'),as.Date('2010-12-31')))
#
# daily_log_returns <-  log(daily_CER$Settle.USD) - log(lead(daily_CER$Settle.USD,1))
# daily_returns <-  (daily_CER$Settle.USD-(lead(daily_CER$Settle.USD,1)) ) /(lead(daily_CER$Settle.USD,1))
# mean(daily_returns, na.rm = TRUE)
# mu <-  mean(daily_log_returns, na.rm = TRUE)
# sigma <-sd(daily_log_returns, na.rm = TRUE)

simulate_CER_Price <- function(starting_price = 10, t = 12*20, num_paths = 1, start_date = Sys.Date())
{
  #20 years
  output <-data.frame()
  for( i in (1:num_paths)){

    forecast_returns <- rnorm(t, 0.005, .112)#monthly #rnorm(t, 0.00005, .029) daily
    #assume start price = 10
    forecast_prices <-data.frame(iteration = i , date =seq.Date(start_date,length.out = t, by =30), price=  starting_price *cumprod(1+forecast_returns))
    output <- bind_rows(output,forecast_prices)
  }
  output
}
#
# num <- 100
# sim <- simulate_CER_Price( t = 100, num_paths = num)
# sim_matrix <- as.matrix(sim %>%spread('iteration','price') %>% select(2:(num+1)) )
#
# #line graph
# apply(sim_matrix, MARGIN = 1,FUN = function(x){quantile(x,c(.05,.25,.5,.75,.95))}) %>% t() %>% as.data.frame() %>%mutate(t = 1:100) %>%   gather('key', 'value', -t ) %>%
#   ggplot(aes(x = t, y = value, colour = key))+ geom_line()+theme_bw()
# #ribbon graph
#  apply(sim_matrix, MARGIN = 1,FUN = function(x){quantile(x,c(.05,.32,.5,.68,.95))}) %>% t() %>% as.data.frame() %>%mutate(t = 1:100) %>%
#   ggplot(aes(x = t))+theme_bw()+
#    geom_ribbon(aes(ymin = `5%`, ymax = `32%`), fill = 'grey20')+
#    geom_ribbon(aes(ymin = `32%`, ymax = `50%`),fill = 'grey50')+
#    geom_ribbon(aes(ymin = `50%`, ymax = `68%`),fill = 'grey70')+
#    geom_ribbon(aes(ymin = `68%`, ymax = `95%`),fill = 'grey90')+ geom_line(data= sim , aes(x =date, y= price, group = iteration), size =.1, alpha = .5)
#
#
# sim_matrix[1,] %>% quantile(c(.1,.2,.3))


