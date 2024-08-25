###########################
#Thesis: Long-Short Term Memory Model - Log
###########################

source('data_preperation/packages.R')
source('data_preperation/data_cleaning_d.R')
source('data_preperation/transformation.R')
source('data_preperation/forcasting.R')

###########################
#LSTM Model Monthly - LOG
###########################

###########################
#Data Preperation
###########################

#Scaling the data
sets_lt_m = length(split_day_m$train)
lstm_train_lt_log = c()
lstm_train_lt_laglog = c()
lstm_train_lt_day = c()
scale_train_lt = c()
lstm_test_lt_log = c()
lstm_test_lt_laglog = c()
lstm_test_lt_day = c()

#Centering an scaling our data
for(i in 1:sets_lt_m){
  scale_train_lt[[i]] = c(min(split_day_m$train[[i]]$log_marg_price, na.rm = T), 
                          max(split_day_m$train[[i]]$log_marg_price, na.rm = T),
                          min(split_day_m$train[[i]]$lag_log_marg_price, na.rm = T), 
                          max(split_day_m$train[[i]]$lag_log_marg_price, na.rm = T))
  
  
  lstm_train_lt_log[[i]] = (split_day_m$train[[i]]$log_marg_price-scale_train_lt[[i]][1])/(scale_train_lt[[i]][2] - scale_train_lt[[i]][1])
  
  if(i == 1){
    lstm_train_lt_laglog[[i]] = c(0.422,na.omit(lag(lstm_train_lt_log[[i]])))
  }else{
    lstm_train_lt_laglog[[i]] = c(lstm_train_lt_log[[i-1]][2], na.omit(lag(lstm_train_lt_log[[i]])))
  }                                                                                        
  
}

for(i in 1:sets_lt_m){
  
  lstm_test_lt_log[[i]] = (split_day_m$test[[i]]$log_marg_price-scale_train_lt[[i]][1])/ (scale_train_lt[[i]][2] - scale_train_lt[[i]][1])
  if(i == 1){
    lstm_test_lt_laglog[[i]] = c(last(lstm_train_lt_log[[sets_lt_m]]), na.omit(lag(lstm_test_lt_log[[i]])))
  }else{
    lstm_test_lt_laglog[[i]] = c(last(lstm_test_lt_log[[i-1]]), na.omit(lag(lstm_test_lt_log[[i]])))
  }
  
}
scale_inv = function(scaled_data, scaling_factor1, scaling_factor2){
  temp = (scaled_data * (scaling_factor2-scaling_factor1))+ scaling_factor1
  out = temp %>% as.matrix()
  
  return(out)
}

X_shape2_lt = c()
X_shape3_lt = c()
for(i in 1:sets_lt_m){
  dim(lstm_train_lt_laglog[[i]]) = c(length(lstm_train_lt_laglog[[i]]), 1, 1)
  dim(lstm_test_lt_laglog[[i]]) = c(length(lstm_test_lt_laglog[[i]]), 1, 1)
  
  X_shape2_lt[[i]] = dim(lstm_train_lt_laglog[[i]])[2]
  X_shape3_lt[[i]] = dim(lstm_train_lt_laglog[[i]])[3]
}

batch_size = 1
units = 365

###########################
#Hyperparameter Tuning
###########################

?tuning_run

runs = tuning_run("lstm_log_run.R",
                  flags = list(lstm_units = c(16,32,64,100),
                               epoch = c(1,5,10)))

runs[which(runs1$metric_loss == min(runs1$metric_loss)),]

###########################
#Estimation And Forecast
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
    ,epochs = epochs,
    ,shuffle = FALSE)
  
  out = model %>%  predict(x_test, batch_size = b_size) 
  
  return(out) 
  
  k_clear_session()  
  rm(model)
}

#fitting and predicting with assignment to stop 'modify in place'

for(i in 1:sets_lt_m){
  print(paste0("Run: ", i))
  
  assign(paste0("pred_lt_lstm_",i), lstm_pred(x_train = lstm_train_lt_laglog[[i]], 
                                              y_train = lstm_train_lt_log[[i]],
                                              x_test = lstm_test_lt_laglog[[i]],
                                              units_l1 = 36,
                                              epochs = 10,
                                              b_size = 1) 
  )
  k_clear_session()
  
  #pred_val_st = rbind(pred_val_st, get(paste0("pred_st_lstm_",i))
}

#rescaling the data

lstm_lt_f =c()
pred_val_lt = c()
for(i in 1:sets_lt_m){
  pred_val_lt[[i]] = get(paste0("pred_lt_lstm_", i))
  lstm_lt_f[[i]] = scale_inv(pred_val_lt[[i]], scale_train_lt[[i]][1], scale_train_lt[[i]][2])
}

#save(lstm_lt_f, file = 'lstm_f_lt.RData')#Using log_marg_price 
  #units_l1 = 36,
  #b_size = 1) 

load("RDATA_files/lstm_f_lt.RData")

###########################
#Back Transforming Log
###########################

#transforming back to prices
temp_lt_lstm = c()
lstm_val_lt2 = c()
for(i in 1:length(split_day_m$train)){
  temp_lt_lstm[[i]] = log_inverse(lstm_lt_f[[i]][,1], c)
  lstm_val_lt2[[i]] = cbind(Day = as.POSIXct(split_day_m$test[[i]]$Day),x = as_tibble(temp_lt_lstm[[i]]))
}

###########################
#Accuracy Metrics
###########################

rmse_lt_lstm2 = c()
mae_lt_lstm2 = c()
smape_lt_lstm2 = c()
for(i in 1:length(split_day_m$train)){
  rmse_lt_lstm2[i] = RMSE(lstm_val_lt2[[i]]$value, split_day_m$test[[i]]$marginal_p_eur_mwh)
  mae_lt_lstm2[i] = MAE(lstm_val_lt2[[i]]$value, split_day_m$test[[i]]$marginal_p_eur_mwh)
  smape_lt_lstm2[i] = smape(lstm_val_lt2[[i]]$value, split_day_m$test[[i]]$marginal_p_eur_mwh)
  mean_lt_lstm2 = c(RMSE = mean(rmse_lt_lstm2),
                   MAE = mean(mae_lt_lstm2),
                   sMAPE = mean(smape_lt_lstm2))
  #MAPE = mean(smape_st_arimac))
}

start_date <- as.Date("2022-01-01")
end_date <- as.Date("2022-12-01")

# Generate a sequence of dates by month
months_seq <- seq.Date(from = start_date, to = end_date, by = "month")


acc_m_lt_lstm2 = tibble(Day = months_seq, RMSE = rmse_lt_lstm2,MAE = mae_lt_lstm2,sMAPE = smape_lt_lstm2)

round(mean_lt_lstm2,3)

###########################
#Results
###########################

lstm_lt_fplot_m2 = ggplot()+
  geom_line(data = split_day_m$test[[1]], aes(x = Day, y = marginal_p_eur_mwh, color = "price"),alpha = 0.8)+
  geom_line(data = lstm_val_lt2[[1]], aes(x = Day, y = value, color = "forecasted"),alpha = 0.8)+
  geom_vline(xintercept = tail(split_day_m$test[[1]]$Day, n = 1))+
  geom_line(data = split_day_m$test[[2]], aes(x = Day, y = marginal_p_eur_mwh, color = "price"),alpha = 0.8)+
  geom_line(data = lstm_val_lt2[[2]], aes(x = Day, y = value, color = "forecasted"),alpha = 0.8)+
  geom_vline(xintercept = tail(split_day_m$test[[2]]$Day, n = 1))+
  geom_line(data = split_day_m$test[[3]], aes(x = Day, y = marginal_p_eur_mwh, color = "price"),alpha = 0.8)+
  geom_line(data = lstm_val_lt2[[3]], aes(x = Day, y = value, color = "forecasted"),alpha = 0.8)+
  geom_vline(xintercept = tail(split_day_m$test[[3]]$Day, n = 1))+
  geom_line(data = split_day_m$test[[4]], aes(x = Day, y = marginal_p_eur_mwh, color = "price"), alpha = 0.8)+
  geom_line(data = lstm_val_lt2[[4]], aes(x = Day, y = value, color = "forecasted"),alpha = 0.8)+
  geom_vline(xintercept = tail(split_day_m$test[[4]]$Day, n = 1))+
  geom_line(data = split_day_m$test[[5]], aes(x = Day, y = marginal_p_eur_mwh, color = "price"),alpha = 0.8)+
  geom_line(data = lstm_val_lt2[[5]], aes(x = Day, y = value, color = "forecasted"),alpha = 0.8)+
  geom_vline(xintercept = tail(split_day_m$test[[5]]$Day, n = 1))+
  geom_line(data = split_day_m$test[[6]], aes(x = Day, y = marginal_p_eur_mwh, color = "price"), alpha = 0.8)+
  geom_line(data = lstm_val_lt2[[6]], aes(x = Day, y = value, color = "forecasted"),alpha = 0.8)+
  geom_vline(xintercept = tail(split_day_m$test[[6]]$Day, n = 1))+
  geom_line(data = split_day_m$test[[7]], aes(x = Day, y = marginal_p_eur_mwh, color = "price"), alpha = 0.8)+
  geom_line(data = lstm_val_lt2[[7]], aes(x = Day, y = value, color = "forecasted"),alpha = 0.8)+
  geom_vline(xintercept = tail(split_day_m$test[[7]]$Day, n = 1))+
  geom_line(data = split_day_m$test[[8]], aes(x = Day, y = marginal_p_eur_mwh, color = "price"),alpha = 0.8)+
  geom_line(data = lstm_val_lt2[[8]], aes(x = Day, y = value, color = "forecasted"), alpha = 0.8)+
  geom_vline(xintercept = tail(split_day_m$test[[8]]$Day, n = 1))+
  geom_line(data = split_day_m$test[[9]], aes(x = Day, y = marginal_p_eur_mwh, color = "price"),alpha = 0.8)+
  geom_line(data = lstm_val_lt2[[9]], aes(x = Day, y = value, color = "forecasted"),alpha = 0.8)+
  geom_vline(xintercept = tail(split_day_m$test[[9]]$Day, n = 1))+
  geom_line(data = split_day_m$test[[10]], aes(x = Day, y = marginal_p_eur_mwh, color = "price"),alpha = 0.8)+
  geom_line(data = lstm_val_lt2[[10]], aes(x = Day, y = value, color = "forecasted"),alpha = 0.8)+
  geom_vline(xintercept = tail(split_day_m$test[[10]]$Day, n = 1))+
  geom_line(data = split_day_m$test[[11]], aes(x = Day, y = marginal_p_eur_mwh, color = "price"),alpha = 0.8)+
  geom_line(data = lstm_val_lt2[[11]], aes(x = Day, y = value, color = "forecasted"), alpha = 0.8)+
  geom_vline(xintercept = tail(split_day_m$test[[11]]$Day, n = 1))+
  geom_line(data = split_day_m$test[[12]], aes(x = Day, y = marginal_p_eur_mwh, color = "price"),alpha = 0.8)+
  geom_line(data = lstm_val_lt2[[12]], aes(x = Day, y = value, color = "forecasted"),alpha = 0.8)+
  labs(title =  "Monthly Rolling Interval of Log Transformed LSTM Forecasted Against Log Test Values")+
  scale_colour_manual(values = c("price"='grey15', "forecasted"='firebrick1'), labels = c("Price", "Forecasted"))+
  labs(col = "Legend")+
  theme_bw()  

lstm_lt_fplot_m2
