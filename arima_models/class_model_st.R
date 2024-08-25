###########################
#Thesis:Classical Model
###########################
source('packages.R')
source('data_cleaning_d.R')
source('transformation.R')
source('forcasting.R')

###########################
#ARIMA Daily Short term Forecast
###########################

###########################
#Correlograms
###########################


train_acfd_c = ggAcf(train_data_shortd$marginal_p_eur_mwh)+
  labs(title = "Autocorrelation Plot of Maginal Price")+
  theme_bw()
train_pacfd_c = ggPacf(train_data_shortd$marginal_p_eur_mwh)+
  labs(title = ' Partial Autocorrelation Plot of Maginal Price')+
  theme_bw()

train_acfd_c/train_pacfd_c

#We can see that there is a white noise process from the autocorrelation.

#Autocorrelation of first differences ACF and PACF

dtrain_acfd_c = ggAcf(diff(train_data_shortd$diff_marg_price))+
  labs(title = "Autocorrelation Plot of First Diff Marginal Price")+
  theme_bw()
dtrain_pacfd_c = ggPacf(diff(train_data_shortd$diff_marg_price))+
  labs(title = ' Partial Autocorrelation Plotof First Diff Maginal Price')+
  theme_bw()

dtrain_acfd_c/dtrain_pacfd_c

(train_acfd_c+train_pacfd_c)/(dtrain_acfd_c+dtrain_pacfd_c)

###########################
#KPSS Test
###########################
?ur.kpss

kpss_log_price = ur.kpss(train_data_shortd$marginal_p_eur_mwh)
summary(kpss_log_price)

###########################
#Estimation
###########################

sets_st = length(split_day2$train)

fit_temp_st_c = c()
for(i in 1:sets_st){
  fit_temp_st_c[[i]] = auto.arima(na.omit(split_day2$train[[i]]$marginal_p_eur_mwh), ic = "aic")
}

#save(fit_temp_st_c, file = 'fit_temp_st_c.RData')
load("RDATA_files/fit_temp_st_c.RData")

###########################
#Information Criteria
###########################

start_date <- as.Date("2022-01-01")
end_date <- as.Date("2022-12-31")

day_seq <- seq.Date(from = start_date, to = end_date, by = "day")

ic_st_c = tibble("AIC" = rep(NA, length(fit_temp_st_c)), "BIC" = rep(NA, length(fit_temp_st_c)), "Day" = day_seq)

for(i in 1:sets_st){
  ic_st_c$AIC[i] = fit_temp_st_c[[i]]$aic
  ic_st_c$BIC[i] = fit_temp_st_c[[i]]$bic
#  ic_st_c$Day[i] = split_day2$train[[i]]$Day
}

mean_ic_st_c  = tibble("Mean AIC" = round(mean(ic_st_c$AIC),3), "Mean BIC" = round(mean(ic_st_c$BIC),3))

ic_plot_st_c = ggplot(ic_st_c)+
  geom_line(aes(x = Day, y = AIC, color = "AIC"),alpha = 0.8)+
  geom_line(aes(x = Day, y = BIC, color = "BIC"), alpha = 0.8)+
  labs(y = "",
       x = "Daily",
       title = "Development of IC Across Rolling Interval Estimated Models (Short Term)")+
  scale_colour_manual(values = c("AIC"='grey15', "BIC"='firebrick1'), labels = c("AIC", "BIC"))+
  labs(col = "Legend")+
  geom_table_npc(data = mean_ic_st_c, label = list(mean_ic_st_c), npcx = 0.00, npcy = 1, size = 3)+
  theme_light()  

ic_plot_st_c /ic_plot_lt_c

###########################
#Forecast
###########################

sets_st = length(split_day2$train)
fc_st_d_c = c()
f_st_val_d_c = c()


for(i in 1:sets_st){
  fc_st_d_c[[i]] = forecast(fit_temp_st_c[[i]], h = 1)
  f_st_val_d_c[[i]] = cbind(Day = as.POSIXct(split_day2$test[[i]]$Day),x = as_tibble(fc_st_d_c[[i]]$mean))
}

###########################
#Accuracy Measures
###########################

#Manual Calculation
rmse_st_arimac_c = c()
mae_st_arimac_c = c()
smape_st_arimac_c = c()

for(i in 1:sets_st){
  rmse_st_arimac_c[i] = RMSE(f_st_val_d_c[[i]]$x, split_day2$test[[i]]$marginal_p_eur_mwh)
  mae_st_arimac_c[i] = MAE(f_st_val_d_c[[i]]$x, split_day2$test[[i]]$marginal_p_eur_mwh)
  smape_st_arimac_c[i] = smape(f_st_val_d_c[[i]]$x, split_day2$test[[i]]$marginal_p_eur_mwh)
  mean_st_arimac_c = c(RMSE = mean(rmse_st_arimac_c),
                     MAE = mean(mae_st_arimac_c),
                     sMAPE = mean(smape_st_arimac_c))
}

acc_m_st_arima_c = tibble(Day = test_data_shortd$Day,RMSE = rmse_st_arimac_c,MAE = mae_st_arimac_c,sMAPE = smape_st_arimac_c)

mean_st_arimac_c

#save(mean_st_arimac_c, file = 'mean_st_arimac_c.RData')#saved
#load("mean_st_arimac_c.RData")

###########################
#Results
###########################

f_val_st_c = data_frame(NA)
for(i in 1:sets_st){
    f_val_st_c[i,1] = f_st_val_d_c[[i]]$Day
    f_val_st_c[i,2] = f_st_val_d_c[[i]]$x
    
}
colnames(f_val_st_c) = c("Date", "x")

plot(f_val_st_c$x)
rbind(list(f_st))

arima_st_plot_c = ggplot()+
  geom_line(data = test_data_shortd, aes(x = Day, y = marginal_p_eur_mwh, color = "marginal_p_eur_mwh"), alpha = 0.8)+
  geom_line(data = f_val_st_c, aes(x = Date, y = x, color = "x"), alpha = 0.8)+
  labs(title = "Daily Rolling Interval ARIMA Forecasted Against Test Values")+
  scale_colour_manual(values = c("marginal_p_eur_mwh"='grey15', "x"='firebrick1'), labels = c("Price", "Forecasted"))+
  labs(col = "Legend")+
  theme_light()  
arima_st_plot_c






