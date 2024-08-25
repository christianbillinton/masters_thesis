###########################
#Thesis: Long-Short Term Memory Model
###########################

source('data_preperation/packages.R')
source('data_preperation/data_cleaning_d.R')
source('data_preperation/transformation.R')
source('data_preperation/forcasting.R')

###########################
#LSTM Model Monthly
###########################

###########################
#Data Preperation
###########################

#Scaling the data
sets_lt_m = length(split_day_m$train)
lstm_train_lt_c = c()
lstm_train_lt_lagc = c()
lstm_train_lt_day = c()
scale_train_lt_c = c()
lstm_test_lt_c = c()
lstm_test_lt_lagc = c()
lstm_test_lt_day = c()

#Centering and scaling our data
for(i in 1:sets_lt_m){
  scale_train_lt_c[[i]] = c(min(split_day_m$train[[i]]$marginal_p_eur_mwh, na.rm = T), 
                          max(split_day_m$train[[i]]$marginal_p_eur_mwh, na.rm = T),
                          min(split_day_m$train[[i]]$lag_log_marg_price, na.rm = T), 
                          max(split_day_m$train[[i]]$lag_log_marg_price, na.rm = T))
  
  
  lstm_train_lt_c[[i]] = (split_day_m$train[[i]]$marginal_p_eur_mwh-scale_train_lt_c[[i]][1])/(scale_train_lt_c[[i]][2] - scale_train_lt_c[[i]][1])
  
  if(i == 1){
    lstm_train_lt_lagc[[i]] = c(0,na.omit(lag(lstm_train_lt_c[[i]])))
  }else{
    lstm_train_lt_lagc[[i]] = c(lstm_train_lt_c[[i-1]][2], na.omit(lag(lstm_train_lt_c[[i]])))
  }                                                                                        
  
}

for(i in 1:sets_lt_m){
  
  lstm_test_lt_c[[i]] = (split_day_m$test[[i]]$marginal_p_eur_mwh-scale_train_lt_c[[i]][1])/ (scale_train_lt_c[[i]][2] - scale_train_lt_c[[i]][1])
  if(i == 1){
    lstm_test_lt_lagc[[i]] = c(last(lstm_train_lt_c[[sets_lt_m]]), na.omit(lag(lstm_test_lt_c[[i]])))
  }else{
    lstm_test_lt_lagc[[i]] = c(last(lstm_test_lt_c[[i-1]]), na.omit(lag(lstm_test_lt_c[[i]])))
  }
  
}

#Back Transform Scaled data

scale_inv = function(scaled_data, scaling_factor1, scaling_factor2){
  temp = (scaled_data * (scaling_factor2-scaling_factor1))+ scaling_factor1
  out = temp %>% as.matrix()
  
  return(out)
}

X_shape2_lt = c()
X_shape3_lt = c()
for(i in 1:sets_lt_m){
  dim(lstm_train_lt_lagc[[i]]) = c(length(lstm_train_lt_lagc[[i]]), 1, 1)
  dim(lstm_test_lt_lagc[[i]]) = c(length(lstm_test_lt_lagc[[i]]), 1, 1)
  
  X_shape2_lt[[i]] = dim(lstm_train_lt_lagc[[i]])[2]
  X_shape3_lt[[i]] = dim(lstm_train_lt_lagc[[i]])[3]
}

batch_size = 1
units = 365

###########################
#Hyperparameter Tuning
###########################

?tuning_run

runs = tuning_run("lstm_price_run.R",
                  flags = list(lstm_units = c(16,36,64),
                               epoch = c(1,5,10)))



###########################
#Estimation and Forecast
###########################

lstm_pred = function(x_train, y_train, x_test, units_l1, epochs, b_size = 1){
  
  #initalize the sequential model
  model = keras_model_sequential()
  
  model %>% layer_lstm(units = units_l1, 
                       batch_input_shape = c(1,1,1),
                       return_sequences = T,
                       stateful= F)%>%
    layer_dense(units = 1)
  
  model %>% compile(loss = 'mean_squared_error',
                    optimizer = "adam")
  
  model %>%  fit(
    x = x_train
    ,y = y_train
    ,batch_size = 1,
    ,epochs = 10
    ,shuffle = FALSE)
  
  out = model %>%  predict(x_test, batch_size = b_size) 
  
  return(out) 
  
  k_clear_session()  
  rm(model)
}

for(i in 1:sets_lt_m){
  print(paste0("Run: ", i))
  
  assign(paste0("pred_lt_lstm_",i), lstm_pred(x_train = lstm_train_lt_lagc[[i]], 
                                              y_train = lstm_train_lt_c[[i]],
                                              x_test = lstm_test_lt_lagc[[i]],
                                              units_l1 = 36,
                                              b_size = 1) 
  )
  k_clear_session()
  
}

lstm_lt_f_c =c()
pred_val_lt_c = c()
for(i in 1:sets_lt_m){
  pred_val_lt_c[[i]] = get(paste0("pred_lt_lstm_", i))
  lstm_lt_f_c[[i]] = scale_inv(pred_val_lt_c[[i]], scale_train_lt_c[[i]][1], scale_train_lt_c[[i]][2])
}


#save(lstm_lt_f, file = 'lstm_f_lt.RData')#Using log_marg_price 
  #units_l1 = 36,
  #b_size = 1) 

load("lstm_f_lt.RData")

###########################
#Accuracy metrics
###########################

rmse_lt_lstm_c = c()
mae_lt_lstm_c = c()
smape_lt_lstm_c = c()

for(i in 1:sets_lt_m){
  rmse_lt_lstm_c[i] = RMSE(lstm_lt_f_c[[i]],split_day_m$test[[i]]$marginal_p_eur_mwh)
  mae_lt_lstm_c[i] = MAE(lstm_lt_f_c[[i]], split_day_m$test[[i]]$marginal_p_eur_mwh)
  #mape_st_arimac[i] = MAPE(svr_val_st[[1]]$value, split_day2$test[[i]]$log_marg_price)
  smape_lt_lstm_c[i] = smape(lstm_lt_f_c[[i]], split_day_m$test[[i]]$marginal_p_eur_mwh)
  mean_lt_lstm_c = c(RMSE = mean(rmse_lt_lstm_c),
                   MAE = mean(mae_lt_lstm_c),
                   sMAPE = mean(smape_lt_lstm_c))
  #MAPE = mean(smape_st_arimac))
}
round(mean_lt_lstm_c,3)

acc_m_lt_lstm_m = c()

start_date <- as.Date("2022-01-01")
end_date <- as.Date("2022-12-01")

# Generate a sequence of dates by month
months_seq <- seq.Date(from = start_date, to = end_date, by = "month")

acc_m_lt_lstm_c = tibble(Day = months_seq, RMSE = rmse_lt_lstm_c,MAE = mae_lt_lstm_c,sMAPE = smape_lt_lstm_c)

mean_lt_lstm
#save(mean_lt_lstm, file = 'mean_lt_lstm1.RData')
load("mean_lt_lstm1.RData")

###########################
#Results
###########################

f_val_lt_lstm_c = c()
for(i in 1:sets_lt_m){
  f_val_lt_lstm_c[[i]] = cbind(Day = as.POSIXct(split_day_m$test[[i]]$Day),x = as_tibble(lstm_lt_f_c[[i]]))
}


lstm_lt_fplot_m_c = ggplot()+
  geom_line(data = split_day_m$test[[1]], aes(x = Day, y = marginal_p_eur_mwh, color = "price"),alpha = 0.8)+
  geom_line(data = f_val_lt_lstm_c[[1]], aes(x = Day, y = V1, color = "forecasted"),alpha = 0.8)+
  geom_vline(xintercept = tail(split_day_m$test[[1]]$Day, n = 1))+
  geom_line(data = split_day_m$test[[2]], aes(x = Day, y = marginal_p_eur_mwh, color = "price"),alpha = 0.8)+
  geom_line(data = f_val_lt_lstm_c[[2]], aes(x = Day, y = V1, color = "forecasted"),alpha = 0.8)+
  geom_vline(xintercept = tail(split_day_m$test[[2]]$Day, n = 1))+
  geom_line(data = split_day_m$test[[3]], aes(x = Day, y = marginal_p_eur_mwh, color = "price"),alpha = 0.8)+
  geom_line(data = f_val_lt_lstm_c[[3]], aes(x = Day, y = V1, color = "forecasted"),alpha = 0.8)+
  geom_vline(xintercept = tail(split_day_m$test[[3]]$Day, n = 1))+
  geom_line(data = split_day_m$test[[4]], aes(x = Day, y = marginal_p_eur_mwh, color = "price"), alpha = 0.8)+
  geom_line(data = f_val_lt_lstm_c[[4]], aes(x = Day, y = V1, color = "forecasted"),alpha = 0.8)+
  geom_vline(xintercept = tail(split_day_m$test[[4]]$Day, n = 1))+
  geom_line(data = split_day_m$test[[5]], aes(x = Day, y = marginal_p_eur_mwh, color = "price"),alpha = 0.8)+
  geom_line(data = f_val_lt_lstm_c[[5]], aes(x = Day, y = V1, color = "forecasted"),alpha = 0.8)+
  geom_vline(xintercept = tail(split_day_m$test[[5]]$Day, n = 1))+
  geom_line(data = split_day_m$test[[6]], aes(x = Day, y = marginal_p_eur_mwh, color = "price"), alpha = 0.8)+
  geom_line(data = f_val_lt_lstm_c[[6]], aes(x = Day, y = V1, color = "forecasted"),alpha = 0.8)+
  geom_vline(xintercept = tail(split_day_m$test[[6]]$Day, n = 1))+
  geom_line(data = split_day_m$test[[7]], aes(x = Day, y = marginal_p_eur_mwh, color = "price"), alpha = 0.8)+
  geom_line(data = f_val_lt_lstm_c[[7]], aes(x = Day, y = V1, color = "forecasted"),alpha = 0.8)+
  geom_vline(xintercept = tail(split_day_m$test[[7]]$Day, n = 1))+
  geom_line(data = split_day_m$test[[8]], aes(x = Day, y = marginal_p_eur_mwh, color = "price"),alpha = 0.8)+
  geom_line(data = f_val_lt_lstm_c[[8]], aes(x = Day, y = V1, color = "forecasted"), alpha = 0.8)+
  geom_vline(xintercept = tail(split_day_m$test[[8]]$Day, n = 1))+
  geom_line(data = split_day_m$test[[9]], aes(x = Day, y = marginal_p_eur_mwh, color = "price"),alpha = 0.8)+
  geom_line(data = f_val_lt_lstm_c[[9]], aes(x = Day, y = V1, color = "forecasted"),alpha = 0.8)+
  geom_vline(xintercept = tail(split_day_m$test[[9]]$Day, n = 1))+
  geom_line(data = split_day_m$test[[10]], aes(x = Day, y = marginal_p_eur_mwh, color = "price"),alpha = 0.8)+
  geom_line(data = f_val_lt_lstm_c[[10]], aes(x = Day, y = V1, color = "forecasted"),alpha = 0.8)+
  geom_vline(xintercept = tail(split_day_m$test[[10]]$Day, n = 1))+
  geom_line(data = split_day_m$test[[11]], aes(x = Day, y = marginal_p_eur_mwh, color = "price"),alpha = 0.8)+
  geom_line(data = f_val_lt_lstm_c[[11]], aes(x = Day, y = V1, color = "forecasted"), alpha = 0.8)+
  geom_vline(xintercept = tail(split_day_m$test[[11]]$Day, n = 1))+
  geom_line(data = split_day_m$test[[12]], aes(x = Day, y = marginal_p_eur_mwh, color = "price"),alpha = 0.8)+
  geom_line(data = f_val_lt_lstm_c[[12]], aes(x = Day, y = V1, color = "forecasted"),alpha = 0.8)+
  labs(title =  "Monthly Rolling Interval LSTM Forecasted Against Test Values")+
  scale_colour_manual(values = c("price"='grey15', "forecasted"='firebrick1'), labels = c("Price", "Forecasted"))+
  labs(col = "Legend")+
  theme_bw()  

lstm_lt_fplot_m_c




