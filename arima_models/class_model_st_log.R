###########################
#Thesis:Classical Model with log
###########################
source('packages.R')
source('data_cleaning_d.R')
source('transformation.R')
source('forcasting.R')

###########################
#ARIMA Daily Short term Forecast - LOG
###########################

###########################
#Correlogram
###########################

train_acfd = ggAcf(train_data_shortd$log_marg_price)+
  labs(title = "Autocorrelation Plot of Log Maginal Price")+
  theme_bw()
train_pacfd = ggPacf(train_data_shortd$log_marg_price)+
  labs(title = ' Partial Autocorrelation Plot of Log Maginal Price')+
  theme_bw()

train_acfd/train_pacfd

#We can see that there is a white noise process from the autocorrelation.

#Autocorrelation of first differences ACF and PACF

dtrain_acfd = ggAcf(diff(train_data_shortd$log_marg_price))+
  labs(title = "Autocorrelation Plot of First Diff Log Marginal Price")+
  theme_bw()
dtrain_pacfd = ggPacf(diff(train_data_shortd$log_marg_price))+
  labs(title = ' Partial Autocorrelation Plotof First Diff Log Maginal Price')+
  theme_bw()

dtrain_acfd/dtrain_pacfd

(train_acfd+train_pacfd)/(dtrain_acfd+dtrain_pacfd)

###########################
#KPSS Test
###########################

?ur.kpss

kpss_log_price = ur.kpss(train_data_shortd$log_marg_price)
summary(kpss_log_price)

###########################
#Estimation
###########################

sets_st = length(split_day2$train)

fit_temp_stlog = c()
for(i in 1:365){
  fit_temp_stlog[[i]] = auto.arima(na.omit(split_day2$train[[i]]$log_marg_price))
}

#save(fit_temp_stlog, file = 'fit_temp_stlog.RData')
load("fit_temp_stlog.RData")

###########################
#Information Criteria Analysis
###########################

start_date <- as.Date("2022-01-01")
end_date <- as.Date("2022-12-31")

day_seq <- seq.Date(from = start_date, to = end_date, by = "day")

ic_st = tibble("AIC" = rep(NA, length(fit_temp_stlog)), "BIC" = rep(NA, length(fit_temp_stlog)), "Day" = day_seq)

for(i in 1:sets_st){
  ic_st$AIC[i] = fit_temp_stlog[[i]]$aic
  ic_st$BIC[i] = fit_temp_stlog[[i]]$bic
}

mean_ic_st  = tibble("Mean AIC" = round(mean(ic_st$AIC),3), "Mean BIC" = round(mean(ic_st$BIC),3))

ic_plot_st = ggplot(ic_st)+
  geom_line(aes(x = Day, y = AIC, color = "AIC"),alpha = 0.8)+
  geom_line(aes(x = Day, y = BIC, color = "BIC"), alpha = 0.8)+
  labs(x = "Daily",
       y = "",
       title = "Development of IC Across Rolling Interval Estimated Models (Short Term)")+
  scale_colour_manual(values = c("AIC"='grey15', "BIC"='firebrick1'), labels = c("AIC", "BIC"))+
  labs(col = "Legend")+
  geom_table_npc(data = mean_ic_st, label = list(mean_ic_st), npcx = 0.00, npcy = 1, size = 3)+
  theme_light()  

###########################
#Forecast
###########################

sets_st = length(split_day2$train)
fc_st_d = c()
f_st_val_d = c()

for(i in 1:sets_st){
  fc_st_d[[i]] = forecast(fit_temp_stlog[[i]], h = 1)
  f_st_val_d[[i]] = cbind(Day = as.POSIXct(split_day2$test[[i]]$Day),x = as_tibble(fc_st_d[[i]]$mean))
}

###########################
#Back Transform Log 
###########################

#transforming back to prices
temp_st_arima = c()
f_st_val_d2 = c()
for(i in 1:sets_st){
  temp_st_arima[[i]] = log_inverse(fc_st_d[[i]]$mean, c)
  f_st_val_d2[[i]] = cbind(Day = as.POSIXct(split_day2$test[[i]]$Day),x = as_tibble(temp_st_arima[[i]]))
}

###########################
#Accuracy Metrics
###########################

rmse_st_arima2 = c()
mae_st_arima2 = c()
smape_st_arima2 = c()
for(i in 1:sets_st){
  rmse_st_arima2[i] = RMSE(f_st_val_d2[[i]]$x, split_day2$test[[i]]$marginal_p_eur_mwh)
  mae_st_arima2[i] = MAE(f_st_val_d2[[i]]$x, split_day2$test[[i]]$marginal_p_eur_mwh)
  #mape_st_arimac[i] = MAPE(svr_val_st[[1]]$value, split_day2$test[[i]]$log_marg_price)
  smape_st_arima2[i] = smape(f_st_val_d2[[i]]$x, split_day2$test[[i]]$marginal_p_eur_mwh)
  mean_st_arima2 = c(RMSE = mean(rmse_st_arima2),
                   MAE = mean(mae_st_arima2),
                   sMAPE = mean(smape_st_arima2))
  #MAPE = mean(smape_st_arimac))
}

acc_m_st_arima = tibble(Day = test_data_shortd$Day,RMSE = rmse_st_arima2,MAE = mae_st_arima2,sMAPE = smape_st_arima2)

mean_st_arima2

###########################
#Results 
###########################

f_val_st_arima2 = data_frame(NA)
for(i in 1:sets_st){
  f_val_st_arima2[i,1] = f_st_val_d2[[i]]$Day
  f_val_st_arima2[i,2] = f_st_val_d2[[i]]$x
}
colnames(f_val_st_arima2) = c("Date", "value")


arima_st_plot2 = ggplot()+
  geom_line(data = test_data_shortd, aes(x = Day, y = marginal_p_eur_mwh, color = "marginal_p_eur_mwh"), alpha = 0.8)+
  geom_line(data = f_val_st_arima2, aes(x = Date, y = value, color = "x"), alpha = 0.8)+
  labs(title = "Daily Rolling Interval of Log Transformed ARIMA Forecasted Against Test Values")+
  scale_colour_manual(values = c("marginal_p_eur_mwh"='grey15', "x"='firebrick1'), labels = c("Price", "Forecasted"))+
  labs(col = "Legend")+
  theme_light()  
arima_st_plot2
