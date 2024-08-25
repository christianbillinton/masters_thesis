###########################
#Thesis:Classical Model With Price
###########################
source('packages.R')
source('data_cleaning_d.R')
source('transformation.R')
source('forcasting.R')

###########################
#ARIMA Model Monthly
###########################

###########################
#Estimation
###########################

sets_lt_m = length(split_day_m$test)

fit_lt_d_arima_c = c()

for(i in 1:length(split_day_m$test)){
  fit_lt_d_arima_c[[i]] = auto.arima(split_day_m$train[[i]]$marginal_p_eur_mwh, ic = 'aic')
}

###########################
#Information Criteria Analysis
###########################

start_date <- as.Date("2022-01-01")
end_date <- as.Date("2022-12-31")

# Generate a sequence of dates by month
months_seq <- seq.Date(from = start_date, to = end_date, by = "month")

ic_lt_c = tibble("AIC" = rep(NA, length(sets_lt_m)), "BIC" = rep(NA, length(sets_lt_m)), "Month" = months_seq)

for(i in 1:sets_lt_m){
  ic_lt_c$AIC[i] = fit_lt_d_arima_c[[i]]$aic
  ic_lt_c$BIC[i] = fit_lt_d_arima_c[[i]]$bic
}

mean_ic_lt_c = tibble("Mean AIC" = round(mean(ic_lt_c$AIC),3), "Mean BIC" = round(mean(ic_lt_c$BIC), 3))

ic_plot_lt_c = ggplot(ic_lt_c)+
  geom_line(aes(x = Month, y = AIC, color = "AIC"),alpha = 0.8, linetype = "dashed")+
  geom_line(aes(x = Month, y = BIC, color = "BIC"), alpha = 0.8)+
  labs(x = "Monthly",
       y = "", 
       title = "Development of IC Across Rolling Interval Estimated Models (Long Term)")+
  scale_colour_manual(values = c("AIC"='blue', "BIC"='firebrick1'), labels = c("AIC", "BIC"))+
  labs(col = "Legend")+
  geom_table_npc(data = mean_ic_lt_c, label = list(mean_ic_lt_c), npcx = 0.00, npcy = 1, size = 3)+
  theme_light()  

#ic_plot_st needs to be initialized to the global environment in the class_model_st copy file to plot them together
ic_plot_lt_c


###########################
#Forecast
###########################

f_lt_d_c = c()
f_lt_val_d_c = c()

for(i in 1:length(split_day_m$test)){
  f_lt_d_c[[i]] = forecast(fit_lt_d_arima_c[[i]], h = length(split_day_m$test[[i]]$marginal_p_eur_mwh))
  f_lt_val_d_c[[i]] = cbind(Day = as.POSIXct(split_day_m$test[[i]]$Day),x = as_tibble(f_lt_d_c[[i]]$mean))
}

###########################
#Accuracy Metrics
###########################

rmse_lt_arimac_c = c()
mae_lt_arimac_c = c()
smape_lt_arimac_c = c()

for(i in 1:length(split_day_m$test)){
  rmse_lt_arimac_c[i] = RMSE(f_lt_val_d_c[[i]]$x, split_day_m$test[[i]]$marginal_p_eur_mwh)
  mae_lt_arimac_c[i] = MAE(f_lt_val_d_c[[i]]$x, split_day_m$test[[i]]$marginal_p_eur_mwh)
  smape_lt_arimac_c[i] = smape(f_lt_val_d_c[[i]]$x, split_day_m$test[[i]]$marginal_p_eur_mwh)
  mean_lt_arimac_c = c(RMSE = mean(rmse_lt_arimac_c),
                     MAE = mean(mae_lt_arimac_c),
                     sMAPE = mean(smape_lt_arimac_c))
}


start_date <- as.Date("2022-01-01")
end_date <- as.Date("2022-12-01")

# Generate a sequence of dates by month
months_seq <- seq.Date(from = start_date, to = end_date, by = "month")

acc_m_lt_arima_c = tibble(Day = months_seq, RMSE = rmse_lt_arimac_c,MAE = mae_lt_arimac_c,sMAPE = smape_lt_arimac_c)

mean_lt_arimac_c

###########################
#Results
###########################


arima_lt_fplot_m_c = ggplot()+
  geom_line(data = split_day_m$test[[1]], aes(x = Day, y = marginal_p_eur_mwh, color = "price"), alpha = 0.8)+
  geom_line(data = f_lt_val_d_c[[1]], aes(x = Day, y = x, color = "forecasted"), alpha = 0.8)+
  geom_vline(xintercept = tail(split_day_m$test[[1]]$Day, n = 1))+
  geom_line(data = split_day_m$test[[2]], aes(x = Day, y = marginal_p_eur_mwh,  color = "price"), alpha = 0.8)+
  geom_line(data = f_lt_val_d_c[[2]], aes(x = Day, y = x, color = "forecasted"), alpha = 0.8)+
  geom_vline(xintercept = tail(split_day_m$test[[2]]$Day, n = 1))+
  geom_line(data = split_day_m$test[[3]], aes(x = Day, y = marginal_p_eur_mwh,  color = "price"),alpha = 0.8)+
  geom_line(data = f_lt_val_d_c[[3]], aes(x = Day, y = x, color = "forecasted"), alpha = 0.8)+
  geom_vline(xintercept = tail(split_day_m$test[[3]]$Day, n = 1))+
  geom_line(data = split_day_m$test[[4]], aes(x = Day, y = marginal_p_eur_mwh,  color = "price"),alpha = 0.8)+
  geom_line(data = f_lt_val_d_c[[4]], aes(x = Day, y = x, color = "forecasted"),alpha = 0.8)+
  geom_vline(xintercept = tail(split_day_m$test[[4]]$Day, n = 1))+
  geom_line(data = split_day_m$test[[5]], aes(x = Day, y = marginal_p_eur_mwh,  color = "price"),alpha = 0.8)+
  geom_line(data = f_lt_val_d_c[[5]], aes(x = Day, y = x, color = "forecasted"),alpha = 0.8)+
  geom_vline(xintercept = tail(split_day_m$test[[5]]$Day, n = 1))+
  geom_line(data = split_day_m$test[[6]], aes(x = Day, y = marginal_p_eur_mwh,  color = "price"),alpha = 0.8)+
  geom_line(data = f_lt_val_d_c[[6]], aes(x = Day, y = x, color = "forecasted"), alpha = 0.8)+
  geom_vline(xintercept = tail(split_day_m$test[[6]]$Day, n = 1))+
  geom_line(data = split_day_m$test[[7]], aes(x = Day, y = marginal_p_eur_mwh,  color = "price"),alpha = 0.8)+
  geom_line(data = f_lt_val_d_c[[7]], aes(x = Day, y = x, color = "forecasted"),alpha = 0.8)+
  geom_vline(xintercept = tail(split_day_m$test[[7]]$Day, n = 1))+
  geom_line(data = split_day_m$test[[8]], aes(x = Day, y = marginal_p_eur_mwh,  color = "price"), alpha = 0.8)+
  geom_line(data = f_lt_val_d_c[[8]], aes(x = Day, y = x, color = "forecasted"),alpha = 0.8)+
  geom_vline(xintercept = tail(split_day_m$test[[8]]$Day, n = 1))+
  geom_line(data = split_day_m$test[[9]], aes(x = Day, y = marginal_p_eur_mwh,  color = "price"),alpha = 0.8)+
  geom_line(data = f_lt_val_d_c[[9]], aes(x = Day, y = x, color = "forecasted"),alpha = 0.8)+
  geom_vline(xintercept = tail(split_day_m$test[[9]]$Day, n = 1))+
  geom_line(data = split_day_m$test[[10]], aes(x = Day, y = marginal_p_eur_mwh,  color = "price"),alpha = 0.8)+
  geom_line(data = f_lt_val_d_c[[10]], aes(x = Day, y = x, color = "forecasted"),alpha = 0.8)+
  geom_vline(xintercept = tail(split_day_m$test[[10]]$Day, n = 1))+
  geom_line(data = split_day_m$test[[11]], aes(x = Day, y = marginal_p_eur_mwh,  color = "price"), alpha = 0.8)+
  geom_line(data = f_lt_val_d_c[[11]], aes(x = Day, y = x, color = "forecasted"),alpha = 0.8)+
  geom_vline(xintercept = tail(split_day_m$test[[11]]$Day, n = 1))+
  geom_line(data = split_day_m$test[[12]], aes(x = Day, y = marginal_p_eur_mwh,  color = "price"), alpha = 0.8)+
  geom_line(data = f_lt_val_d_c[[12]], aes(x = Day, y = x, color = "forecasted"),alpha = 0.8)+
  labs(title =  "Monthly Rolling Interval ARIMA Forecasted Against Test Values")+
  scale_colour_manual(values = c("price"='grey15', "forecasted"='firebrick1'), labels = c("Price", "Forecasted"))+
  labs(col = "Legend")+
  theme_bw()  




