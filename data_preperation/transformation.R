###########################
#Thesis: Data Transformation
###########################

#source('class_model.R')
#source('deep_learning_model.R')
#source('packages.R')
#source('data_cleaning_h.R')#for hourly
#source('data_cleaning_d.R')#for daily


#https://prac.im.pwr.edu.pl/~hugo/RePEc/wuu/wpaper/HSC_17_01.pdf
#summary(elect_price_d)

#temp_0 = elect_price_d[elect_price_d$marginal_p_eur_mwh <=0,]
#View(temp_0)
#######################################
#Thesis: Log transformation of the data
#######################################

c = min(elect_price_d$marginal_p_eur_mwh)*2

#adding a log value with the constant 
elect_price_d$log_marg_price = log(elect_price_d$marginal_p_eur_mwh - c)

#adding a first difference of the log value
elect_price_d$diff_log_marg_price = c(NA, diff(elect_price_d$log_marg_price))

#Adding a lagged value of the difference price
elect_price_d$diff_lag_log_marg_price = lag(elect_price_d$diff_marg_price)

#adding a seasonal difference of 7 lags 
elect_price_d$diff7log_marg_price = c(rep(NA,7), diff(elect_price_d$log_marg_price,7))

#adding a lagged variable of the log price
elect_price_d$laglog_marg_price = lag(elect_price_d$log_marg_price)

#Lagged seasonal difference data
elect_price_d$diff7laglog_marg_price = lag(elect_price_d$diff7log_marg_price)

#Function to return back from logged values to Price
log_inverse = function(data, c){
  exp(data)+c
}

#testing teh log inverse results in prices
elect_price_d$marginal_p_eur_mwh[1:10] - log_inverse(elect_price_d$log_marg_price[1:10], c)









