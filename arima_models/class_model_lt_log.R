###########################
#Thesis:Classical ARIMA Model With Log
###########################
source('packages.R')
source('data_cleaning_d.R')
source('transformation.R')
source('forcasting.R')

###########################
#ARIMA Model Monthly - LOG
###########################


###########################
#Model Fit
###########################

#sets of the length for looping over data
sets_lt_m = length(split_day_m$test)


#estimating our model through auto.arima
fit_lt_d_arima = c()
f_lt_d = list()

#Loop applying auto.arima algorithm across all of our intervals
for(i in 1:sets_lt_m){
  fit_lt_d_arima[[i]] = auto.arima(split_day_m$train[[i]]$log_marg_price, ic = 'aic')
}

###########################
#Infromation Criterion Analysis
###########################

#Seting time period of a time series of Information Criterion
start_date <- as.Date("2022-01-01")
end_date <- as.Date("2022-12-01")

# Generate a sequence of dates by month
months_seq <- seq.Date(from = start_date, to = end_date, by = "month")

#Create an object for the IC time series
ic_lt = tibble("AIC" = rep(NA, length(sets_lt_m)), "BIC" = rep(NA, length(sets_lt_m)), "Month" = months_seq)

#
for(i in 1:sets_lt_m){
  ic_lt$AIC[i] = fit_lt_d_arima[[i]]$aic
  ic_lt$BIC[i] = fit_lt_d_arima[[i]]$bic
}

#Mean IC across time interval
mean_ic_lt  = tibble("Mean AIC" = round(mean(ic_lt$AIC),3), "Mean BIC" = round(mean(ic_lt$BIC),3))


#Plot of IC 
ic_plot_lt = ggplot(ic_lt)+
  geom_line(aes(x = Month, y = AIC, color = "AIC"),alpha = 0.8, linetype = "dashed")+
  geom_line(aes(x = Month, y = BIC, color = "BIC"), alpha = 0.8)+
  labs(x = "Monthly",
       y = " ",
       title = "Development of IC Across Rolling Interval Estimated Models (Long Term)")+
  scale_colour_manual(values = c("AIC"='blue', "BIC"='firebrick1'), labels = c("AIC", "BIC"))+
  labs(col = "Legend")+
  geom_table_npc(data = mean_ic_lt, label = list(mean_ic_lt), npcx = 0.00, npcy = 1, size = 3)+
  theme_light()  

#ic_plot_st needs to be initialized to the global environment in the class_model_st_log copy file to plot them together
ic_plot_st / ic_plot_lt

###########################
#Model Forecast
###########################

f_lt_d = c()
f_lt_val_d = c()

for(i in 1:sets_lt_m){
  f_lt_d[[i]] = forecast(fit_lt_d_arima[[i]], h = length(split_day_m$test[[i]]$log_marg_price))
  f_lt_val_d[[i]] = cbind(Day = as.POSIXct(split_day_m$test[[i]]$Day),x = as_tibble(f_lt_d[[i]]$mean))
}

###########################
#Transformation Back from Log values
###########################

#transforming back to prices
temp_lt_arima_c = c()
f_lt_val_d2 = c()
for(i in 1:sets_lt_m){
  temp_lt_arima_c[[i]] = log_inverse(f_lt_d[[i]]$mean, c)
  f_lt_val_d2[[i]] = cbind(Day = as.POSIXct(split_day_m$test[[i]]$Day),x = as_tibble(temp_lt_arima_c[[i]]))
}

###########################
#Accuracy Measures
###########################

rmse_lt_arima2 = c()
mae_lt_arima2 = c()
smape_lt_arima2 = c()
for(i in 1:sets_lt_m){
  rmse_lt_arima2[i] = RMSE(f_lt_val_d2[[i]]$x, split_day_m$test[[i]]$marginal_p_eur_mwh)
  mae_lt_arima2[i] = MAE(f_lt_val_d2[[i]]$x, split_day_m$test[[i]]$marginal_p_eur_mwh)
  smape_lt_arima2[i] = smape(f_lt_val_d2[[i]]$x, split_day_m$test[[i]]$marginal_p_eur_mwh)
  mean_lt_arima2 = c(RMSE = mean(rmse_lt_arima2),
                     MAE = mean(mae_lt_arima2),
                     sMAPE = mean(smape_lt_arima2))
}

#Mean accuracy
mean_lt_arima2

#Creating a time series of accuracy metrics
start_date <- as.Date("2022-01-01")
end_date <- as.Date("2022-12-01")

# Generate a sequence of dates by month
months_seq <- seq.Date(from = start_date, to = end_date, by = "month")

acc_m_lt_arima2 = tibble(Day = months_seq, RMSE = rmse_lt_arima2,MAE = mae_lt_arima2,sMAPE = smape_lt_arima2)

#Creating a time series object to plot 
f_val_lt_arima2 = data_frame(NA)
for(i in 1:sets_lt_m){
  f_val_lt_arima2[i,1] = f_lt_val_d2[[i]]$Day
  f_val_lt_arima2[i,2] = f_lt_val_d2[[i]]$x
}
colnames(f_val_lt_arima2) = c("Day", "value")


arima_lt_fplot_md = ggplot()+
  geom_line(data = split_day_m$test[[1]], aes(x = Day, y = marginal_p_eur_mwh, color = "price"), alpha = 0.8)+
  geom_line(data = f_lt_val_d2[[1]], aes(x = Day, y = x, color = "forecasted"), alpha = 0.8)+
  geom_vline(xintercept = tail(split_day_m$test[[1]]$Day, n = 1))+
  geom_line(data = split_day_m$test[[2]], aes(x = Day, y = marginal_p_eur_mwh,  color = "price"), alpha = 0.8)+
  geom_line(data = f_lt_val_d2[[2]], aes(x = Day, y = x, color = "forecasted"), alpha = 0.8)+
  geom_vline(xintercept = tail(split_day_m$test[[2]]$Day, n = 1))+
  geom_line(data = split_day_m$test[[3]], aes(x = Day, y = marginal_p_eur_mwh,  color = "price"),alpha = 0.8)+
  geom_line(data = f_lt_val_d2[[3]], aes(x = Day, y = x, color = "forecasted"), alpha = 0.8)+
  geom_vline(xintercept = tail(split_day_m$test[[3]]$Day, n = 1))+
  geom_line(data = split_day_m$test[[4]], aes(x = Day, y = marginal_p_eur_mwh,  color = "price"),alpha = 0.8)+
  geom_line(data = f_lt_val_d2[[4]], aes(x = Day, y = x, color = "forecasted"),alpha = 0.8)+
  geom_vline(xintercept = tail(split_day_m$test[[4]]$Day, n = 1))+
  geom_line(data = split_day_m$test[[5]], aes(x = Day, y = marginal_p_eur_mwh,  color = "price"),alpha = 0.8)+
  geom_line(data = f_lt_val_d2[[5]], aes(x = Day, y = x, color = "forecasted"),alpha = 0.8)+
  geom_vline(xintercept = tail(split_day_m$test[[5]]$Day, n = 1))+
  geom_line(data = split_day_m$test[[6]], aes(x = Day, y = marginal_p_eur_mwh,  color = "price"),alpha = 0.8)+
  geom_line(data = f_lt_val_d2[[6]], aes(x = Day, y = x, color = "forecasted"), alpha = 0.8)+
  geom_vline(xintercept = tail(split_day_m$test[[6]]$Day, n = 1))+
  geom_line(data = split_day_m$test[[7]], aes(x = Day, y = marginal_p_eur_mwh,  color = "price"),alpha = 0.8)+
  geom_line(data = f_lt_val_d2[[7]], aes(x = Day, y = x, color = "forecasted"),alpha = 0.8)+
  geom_vline(xintercept = tail(split_day_m$test[[7]]$Day, n = 1))+
  geom_line(data = split_day_m$test[[8]], aes(x = Day, y = marginal_p_eur_mwh,  color = "price"), alpha = 0.8)+
  geom_line(data = f_lt_val_d2[[8]], aes(x = Day, y = x, color = "forecasted"),alpha = 0.8)+
  geom_vline(xintercept = tail(split_day_m$test[[8]]$Day, n = 1))+
  geom_line(data = split_day_m$test[[9]], aes(x = Day, y = marginal_p_eur_mwh,  color = "price"),alpha = 0.8)+
  geom_line(data = f_lt_val_d2[[9]], aes(x = Day, y = x, color = "forecasted"),alpha = 0.8)+
  geom_vline(xintercept = tail(split_day_m$test[[9]]$Day, n = 1))+
  geom_line(data = split_day_m$test[[10]], aes(x = Day, y = marginal_p_eur_mwh,  color = "price"),alpha = 0.8)+
  geom_line(data = f_lt_val_d2[[10]], aes(x = Day, y = x, color = "forecasted"),alpha = 0.8)+
  geom_vline(xintercept = tail(split_day_m$test[[10]]$Day, n = 1))+
  geom_line(data = split_day_m$test[[11]], aes(x = Day, y = marginal_p_eur_mwh,  color = "price"), alpha = 0.8)+
  geom_line(data = f_lt_val_d2[[11]], aes(x = Day, y = x, color = "forecasted"),alpha = 0.8)+
  geom_vline(xintercept = tail(split_day_m$test[[11]]$Day, n = 1))+
  geom_line(data = split_day_m$test[[12]], aes(x = Day, y = marginal_p_eur_mwh,  color = "price"), alpha = 0.8)+
  geom_line(data = f_lt_val_d2[[12]], aes(x = Day, y = x, color = "forecasted"),alpha = 0.8)+
  labs(title =  "Monthly Rolling Interval of Log Transformed ARIMA Forecasted Against Log Test Values")+
  scale_colour_manual(values = c("price"='grey15', "forecasted"='firebrick1'), labels = c("Price", "Forecasted"))+
  labs(col = "Legend")+
  theme_bw()  




