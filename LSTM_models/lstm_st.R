###########################
#Thesis: Long-Short Term Memory Model
###########################

source('data_preperation/packages.R')
source('data_preperation/data_cleaning_d.R')
source('data_preperation/transformation.R')
source('data_preperation/forcasting.R')

###########################
#LSTM Daily Short term Forecast
###########################

###########################
#Data Preperation
###########################

#Scaling the data
sets_st = length(split_day2$train)
lstm_train_st_c= c()
lstm_train_st_lagc = c()
lstm_train_st_c = c()
scale_train_st_c = c()
lstm_test_st_c = c()
lstm_test_st_c = c()
lstm_test_st_lagc = c()

#Centering an scaling our data
for(i in 1:sets_st){
  scale_train_st_c[[i]] = c(min(split_day2$train[[i]]$marginal_p_eur_mwh, na.rm = T), 
                          max(split_day2$train[[i]]$marginal_p_eur_mwh, na.rm = T),
                          min(split_day2$train[[i]]$laglog7_marg_price, na.rm = T), 
                          max(split_day2$train[[i]]$laglog7_marg_price, na.rm = T))
  
  
  lstm_train_st_c[[i]] = (split_day2$train[[i]]$marginal_p_eur_mwh-scale_train_st_c[[i]][1])/(scale_train_st_c[[i]][2] - scale_train_st_c[[i]][1])
  
  if(i == 1){
    lstm_train_st_lagc[[i]] = c(0,na.omit(lag(lstm_train_st_c[[i]])))
  }else{
    lstm_train_st_lagc[[i]] = c(lstm_train_st_c[[i-1]][2], na.omit(lag(lstm_train_st_c[[i]])))
  }                                                                                        
  
}

for(i in 1:sets_st){
  
  lstm_test_st_c[[i]] = (split_day2$test[[i]]$marginal_p_eur_mwh-scale_train_st_c[[i]][1])/ (scale_train_st_c[[i]][2] - scale_train_st_c[[i]][1])
  if(i == 1){
    lstm_test_st_lagc[[i]] = last(lstm_train_st_c[[sets_st]])
  }else{
    lstm_test_st_lagc[[i]] = lstm_test_st_c[[i-1]]
  }

}

scale_inv = function(scaled_data, scaling_factor1, scaling_factor2){
  temp = (scaled_data * (scaling_factor2-scaling_factor1))+ scaling_factor1
  out = temp %>% as.matrix()
  
  return(out)
}

min(lstm_test_st_lagc[[1]])
max(lstm_test_st_lagc[[1]])
last(lstm_test_st_lagc[[365]])

min(lstm_train_st_lagc[[1]])
max(lstm_train_st_lagc[[1]])

X_shape2 = c()
X_shape3 = c()
for(i in 1:sets_st){
  dim(lstm_train_st_lagc[[i]]) = c(length(lstm_train_st_lagc[[i]]), 1, 1)
  dim(lstm_test_st_lagc[[i]]) = c(length(lstm_test_st_lagc[[i]]), 1, 1)
  
  X_shape2[[i]] = dim(lstm_train_st_lagc[[i]])[2]
  X_shape3[[i]] = dim(lstm_train_st_lagc[[i]])[3]
}

batch_size = 1

###########################
#Hyperparameter Tuning
###########################

runs = tuning_run("lstm_price_run.R",
                  flags = list(lstm_units = c(16,36,64),
                               epoch = c(1,5,10)))
tail(runs[,2:8])

i = which(runs$metric_loss == min(runs$metric_loss))

View(runs[i,])

runs[order(runs$metric_loss, decreasing = F), ]

###########################
#Estimation and Forecast
###########################

lstm_pred = function(x_train, y_train, x_test, units_l1, epochs, b_size = 1){
  
  #initialize the sequential model
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
    ,epochs = epochs,
    ,shuffle = FALSE)
  
  out = model %>%  predict(x_test, batch_size = b_size) 
  
  return(out) 
  
  k_clear_session()  
  rm(model)
}

for(i in 1:sets_st){
  print(paste0("Run: ", i))
  
  assign(paste0("pred_st_lstm_",i), lstm_pred(x_train = lstm_train_st_lagc[[i]], 
                                              y_train = lstm_train_st_c[[i]],
                                              x_test = lstm_test_st_lagc[[i]],
                                              units_l1 = 36,
                                              epochs = 10,
                                              b_size = 1) 
  )
  k_clear_session()
  
}

pred_st_lstm_66
lstm_test_st_log[[66]]

pred_st_lstm_15
#pred_lstm_st[[15]]
lstm_test_st_log[[15]]

lstm_st_f_c =c()
pred_val_st_c = c()
for(i in 1:sets_st){
  pred_val_st_c[[i]] = get(paste0("pred_st_lstm_", i))
  lstm_st_f_c[[i]] = scale_inv(pred_val_st_c[[i]], scale_train_st_c[[i]][1], scale_train_st_c[[i]][2])
}

###########################
#Accuracy Metrics
###########################

rmse_st_lstm_c = c()
mae_st_lstm_c = c()
smape_st_lstm_c = c()

for(i in 1:sets_st){
  rmse_st_lstm_c[i] = RMSE(lstm_st_f_c[[i]][1,], split_day2$test[[i]]$marginal_p_eur_mwh)
  mae_st_lstm_c[i] = MAE(lstm_st_f_c[[i]][1,], split_day2$test[[i]]$marginal_p_eur_mwh)
  smape_st_lstm_c[i] = smape(lstm_st_f_c[[i]][1,], split_day2$test[[i]]$marginal_p_eur_mwh)
  mean_st_lstm_c = c(RMSE = mean(rmse_st_lstm_c),
                   MAE = mean(mae_st_lstm_c),
                   sMAPE = mean(smape_st_lstm_c))
}

acc_m_st_lstm_c = tibble(Day = test_data_shortd$Day, RMSE = rmse_st_lstm_c,MAE = mae_st_lstm_c,sMAPE = smape_st_lstm_c)

mean_st_lstm_c

###########################
#Results
###########################

fit_val_st_c = data.frame(NA)
for(i in 1:sets_st){
  fit_val_st_c[i,1] = as.POSIXct(split_day2$test[[i]]$Day)
  fit_val_st_c[i,2] = lstm_st_f_c[[i]][1,1]
}
colnames(fit_val_st_c) = c("Date", "value")
fit_val_st_c$Date = as.POSIXct(fit_val_st_c$Date)

lstm_st_plot_c = ggplot()+
  geom_line(data = test_data_shortd, aes(x = Day, y = marginal_p_eur_mwh, color = "marginal_p_eur_mwh"), alpha = 0.8)+
  geom_line(data = fit_val_st_c, aes(x = Date, y = value, color = "value"), alpha = 0.8)+
  labs(title = "Daily Rolling Interval LSTM Forecasted Against Test Values")+
  scale_colour_manual(values = c("marginal_p_eur_mwh"='grey15', "value"='firebrick1'), labels = c("Price", "Forecasted"))+
  labs(col = "Legend")+
  theme_light()  

lstm_st_plot_c

###########################
#RDATA Files
###########################

#Saved files of running model

#save(mean_st_lstm_c, file = "mean_st_lstm_acc_c.RData")#Saving the accuracy measures 
load("RDATA_files/mean_st_lstm_acc_c.RData")
#save(fit_val_st_log_c, file = "fit_val_st_log_c.RData")#Saving the predicted values in log scale that have been bound
load("RDATA_files/fit_val_st_log_c.RData")
#save(fit_val_st_orig_c, file = "fit_val_st_orig_c.RData")#Saving the predicted values in log scale that have been bound
load("RDATA_files/fit_val_st_orig_c.RData")
#save(lstm_st_f_c, file = "lstm_st_f_c.RData")#Saving the predicted values that have be reverse normalized
load("RDATA_files/lstm_st_f_c.RData")
#save(pred_val_st_c, file = "pred_val_lstm_st_c.RData")##Saving the raw predicted value
load("RDATA_files/pred_val_lstm_st_c.RData")



