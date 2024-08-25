###########################
#Thesis: Long-Short Term Memory Model - Log
###########################

source('data_preperation/packages.R')
source('data_preperation/data_cleaning_d.R')
source('data_preperation/transformation.R')
source('data_preperation/forcasting.R')

###########################
#LSTM Daily Short term Forecast - Log
###########################

###########################
#Data Preperation
###########################

#Scaling the data
sets_st = length(split_day2$train)
lstm_train_st_log = c()
lstm_train_st_laglog = c()
lstm_train_st_day = c()
lstm_train_st = c()
scale_train_st = c()
lstm_test_st = c()
lstm_test_st_log = c()
lstm_test_st_laglog = c()
lstm_test_st_day = c()

#Centering an scaling our data
for(i in 1:sets_st){
  scale_train_st[[i]] = c(min(split_day2$train[[i]]$log_marg_price, na.rm = T), 
                          max(split_day2$train[[i]]$log_marg_price, na.rm = T),
                          min(split_day2$train[[i]]$laglog7_marg_price, na.rm = T), 
                          max(split_day2$train[[i]]$laglog7_marg_price, na.rm = T))
  
  
  lstm_train_st_log[[i]] = (split_day2$train[[i]]$log_marg_price-scale_train_st[[i]][1])/(scale_train_st[[i]][2] - scale_train_st[[i]][1])
  
  if(i == 1){
    lstm_train_st_laglog[[i]] = c(0.422,na.omit(lag(lstm_train_st_log[[i]])))
  }else{
    lstm_train_st_laglog[[i]] = c(last(lstm_train_st_log[[i-1]]), na.omit(lag(lstm_train_st_log[[i]])))
  }                                                                                        
  
}

for(i in 1:sets_st){
  
  lstm_test_st_log[[i]] = (split_day2$test[[i]]$log_marg_price-scale_train_st[[i]][1])/ (scale_train_st[[i]][2] - scale_train_st[[i]][1])
  if(i == 1){
    lstm_test_st_laglog[[i]] = last(lstm_train_st_log[[sets_st]])
  }else{
    lstm_test_st_laglog[[i]] = lstm_test_st_log[[i-1]]
  }

}

scale_inv = function(scaled_data, scaling_factor1, scaling_factor2){
  temp = (scaled_data * (scaling_factor2-scaling_factor1))+ scaling_factor1
  out = temp %>% as.matrix()
  
  return(out)
}


min(lstm_test_st_laglog[[1]])
last(lstm_test_st_laglog[[365]])

X_shape2 = c()
X_shape3 = c()
for(i in 1:sets_st){
  dim(lstm_train_st_laglog[[i]]) = c(length(lstm_train_st_laglog[[i]]), 1, 1)
  dim(lstm_test_st_laglog[[i]]) = c(length(lstm_test_st_laglog[[i]]), 1, 1)
  
  X_shape2[[i]] = dim(lstm_train_st_laglog[[i]])[2]
  X_shape3[[i]] = dim(lstm_train_st_laglog[[i]])[3]
}


###########################
#Hyperparameter Tuning 
###########################

?tuning_run

runs = tuning_run("lstm_log_run.R",
                  flags = list(lstm_units = c(16,32,64,100),
                               epoch = c(1,5,10)))
tail(runs[,2:8])

i = which(runs$metric_loss == min(runs$metric_loss))

View(runs[i,])

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
    ,batch_size = 1
    ,epochs = 10
    ,shuffle = FALSE)
  
  out = model %>%  predict(x_test, batch_size = b_size) 
  
  return(out) 
  
  k_clear_session()  
  rm(model)
}

#Loop to call function using the new generated value that stops 'modify in place" issues from happening
for(i in 1:sets_st){
  print(paste0("Run: ", i))
  
  assign(paste0("pred_st_lstm_",i), lstm_pred(x_train = lstm_train_st_laglog[[i]], 
                                              y_train = lstm_train_st_log[[i]],
                                              x_test = lstm_test_st_laglog[[i]],
                                              units_l1 = 10,
                                              b_size = 1) 
  )
  k_clear_session()
  
}

#unwraps and rescales the data
lstm_st_f =c()
pred_val_st = c()
for(i in 1:sets_st){
  pred_val_st[[i]] = get(paste0("pred_st_lstm_", i))
  lstm_st_f[[i]] = scale_inv(pred_val_st[[i]], scale_train_st[[i]][1], scale_train_st[[i]][2])
}

###########################
#Back Transform Log
###########################


#transforming back to prices
temp_st_lstm = c()
lstm_val_st2 = c()
for(i in 1:sets_st){
  temp_st_lstm[[i]] = log_inverse(lstm_st_f[[i]][1,], c)
  lstm_val_st2[[i]] = cbind(Day = as.POSIXct(split_day2$test[[i]]$Day),x = as_tibble(temp_st_lstm[[i]]))
}

###########################
#Accuracy Metrics
###########################

#accuracy measure in prices
rmse_st_lstm2 = c()
mae_st_lstm2 = c()
smape_st_lstm2 = c()
for(i in 1:sets_st){
  rmse_st_lstm2[i] = RMSE(lstm_val_st2[[i]]$value, split_day2$test[[i]]$marginal_p_eur_mwh)
  mae_st_lstm2[i] = MAE(lstm_val_st2[[i]]$value, split_day2$test[[i]]$marginal_p_eur_mwh)

  smape_st_lstm2[i] = smape(lstm_val_st2[[i]]$value, split_day2$test[[i]]$marginal_p_eur_mwh)
  mean_st_lstm2 = c(RMSE = mean(rmse_st_lstm2),
                   MAE = mean(mae_st_lstm2),
                   sMAPE = mean(smape_st_lstm2))
}

acc_m_st_lstm = tibble(Day = test_data_shortd$Day, RMSE = rmse_st_lstm2,MAE = mae_st_lstm2,sMAPE = smape_st_lstm2)

mean_st_lstm2

###########################
#Results
###########################

#plotting the forecasted price vlaues
f_val_st_lstm2 = data_frame(NA)
for(i in 1:sets_st){
  f_val_st_lstm2[i,1] = lstm_val_st2[[i]]$Day
  f_val_st_lstm2[i,2] = lstm_val_st2[[i]]$value
}
colnames(f_val_st_lstm2) = c("Date", "value")


lstm_st_plot2 = ggplot()+
  geom_line(data = test_data_shortd, aes(x = Day, y = marginal_p_eur_mwh, color = "marginal_p_eur_mwh"), alpha = 0.8)+
  geom_line(data = f_val_st_lstm2, aes(x = Date, y = value, color = "value"), alpha = 0.8)+
  labs(title = "Daily Rolling Interval  of Log Transformed LSTM Forecasted Against Test Values")+
  scale_colour_manual(values = c("marginal_p_eur_mwh"='grey15', "value"='firebrick1'), labels = c("Price", "Forecasted"))+
  labs(col = "Legend")+
  theme_light()  

lstm_st_plot2

###########################
#RDATA Files
###########################


#Saved files of running model

#save(mean_st_lstm, file = "mean_st_lstm_acc.RData")#Saving the accuracy measures 
load("RDATA_files/mean_st_lstm_acc.RData")
#save(fit_val_st_log, file = "fit_val_st_log.RData")#Saving the predicted values in log scale that have been bound
load("RDATA_files/fit_val_st_log.RData")
#save(fit_val_st_orig, file = "fit_val_st_orig.RData")#Saving the predicted values in log scale that have been bound
load("RDATA_files/fit_val_st_orig.RData")
#save(lstm_st_f, file = "lstm_st_f.RData")#Saving the predicted values that have be reverse normalized
load("RDATA_files/lstm_st_f.RData")
#save(pred_val_st, file = "pred_val_lstm_st.RData")##Saving the raw predicted value
load("RDATA_files/pred_val_lstm_st.RData")


