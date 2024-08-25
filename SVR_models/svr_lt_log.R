###########################
#Thesis: Support Vector Regression - Log
###########################

source('data_preperation/packages.R')
source('data_preperation/data_cleaning_d.R')
source('data_preperation/transformation.R')
source('data_preperation/forcasting.R')

###########################
#SVR Model Monthly
###########################

###########################
#Hyperparamter Tuning
###########################

#To train the model
SVRGridCoarse <- expand.grid(.sigma=c(0.001, 0.01, 0.1), .C=c(seq(1,10, by = 2), 50, 100))
ctrl <- trainControl(method = "cv", number=5) 

#Cross Validation
svr_lt_m = c()
set.seed(123)
for(i in 1:length(split_day_m$train)){
  svr_lt_m[[i]] = train(log_marg_price ~ laglog_marg_price,
                      data = split_day_m$train[[i]], 
                      method="svmRadial",
                      tuneGrid=SVRGridCoarse, 
                      trControl=ctrl, 
                      type="eps-svr",
  )
}

###########################
#Estimation and Forecast
###########################

fit_svr_lt_m = c()
pred_svr_lt_m = c()
svr_val_lt_m = c()

for(i in 1:length(split_day_m$train)){
  fit_svr_lt_m[[i]] = svm(log_marg_price ~ laglog_marg_price,data = split_day_m$train[[i]], type="eps-regression",kernel="radial",cost=svr_lt_m[[i]]$bestTune[2], gamma = svr_lt_m[[i]]$bestTune[1])
  pred_svr_lt_m[[i]] =  predict(fit_svr_lt_m[[i]], split_day_m$test[[i]]$laglog_marg_price)
  svr_val_lt_m[[i]] = cbind(Day = as.POSIXct(split_day_m$test[[i]]$Day),x = as_tibble(pred_svr_lt_m[[i]]))
}

###########################
#Back Transforming from Log
###########################

#transforming back to prices
temp_lt_svr = c()
svr_val_lt2 = c()
for(i in 1:length(split_day_m$train)){
  temp_lt_svr[[i]] = log_inverse(svr_val_lt_m[[i]]$value, c)
  svr_val_lt2[[i]] = cbind(Day = as.POSIXct(split_day_m$test[[i]]$Day),x = as_tibble(temp_lt_svr[[i]]))
}

###########################
#Accuracy
###########################

rmse_lt_svr2 = c()
mae_lt_svr2 = c()
smape_lt_svr2 = c()
for(i in 1:length(split_day_m$train)){
  rmse_lt_svr2[i] = RMSE(svr_val_lt2[[i]]$value, split_day_m$test[[i]]$marginal_p_eur_mwh)
  mae_lt_svr2[i] = MAE(svr_val_lt2[[i]]$value, split_day_m$test[[i]]$marginal_p_eur_mwh)
  smape_lt_svr2[i] = smape(svr_val_lt2[[i]]$value, split_day_m$test[[i]]$marginal_p_eur_mwh)
  mean_lt_svr2 = c(RMSE = mean(rmse_lt_svr2),
                   MAE = mean(mae_lt_svr2),
                   sMAPE = mean(smape_lt_svr2))
  #MAPE = mean(smape_st_arimac))
}

mean_lt_svr2

acc_m_lt_svr2 = tibble(Day = months_seq, RMSE = rmse_lt_svr2,MAE = mae_lt_svr2,sMAPE = smape_lt_svr2)

###########################
#Results
###########################

svm_lt_fplot_m2 = ggplot()+
  geom_line(data = split_day_m$test[[1]], aes(x = Day, y = marginal_p_eur_mwh, color = "price"),alpha = 0.8)+
  geom_line(data = svr_val_lt2[[1]], aes(x = Day, y = value, color = "forecasted"),alpha = 0.8)+
  geom_vline(xintercept = tail(split_day_m$test[[1]]$Day, n = 1))+
  geom_line(data = split_day_m$test[[2]], aes(x = Day, y = marginal_p_eur_mwh, color = "price"),alpha = 0.8)+
  geom_line(data = svr_val_lt2[[2]], aes(x = Day, y = value, color = "forecasted"),alpha = 0.8)+
  geom_vline(xintercept = tail(split_day_m$test[[2]]$Day, n = 1))+
  geom_line(data = split_day_m$test[[3]], aes(x = Day, y = marginal_p_eur_mwh, color = "price"),alpha = 0.8)+
  geom_line(data = svr_val_lt2[[3]], aes(x = Day, y = value, color = "forecasted"),alpha = 0.8)+
  geom_vline(xintercept = tail(split_day_m$test[[3]]$Day, n = 1))+
  geom_line(data = split_day_m$test[[4]], aes(x = Day, y = marginal_p_eur_mwh, color = "price"),alpha = 0.8)+
  geom_line(data = svr_val_lt2[[4]], aes(x = Day, y = value, color = "forecasted"), alpha = 0.8)+
  geom_vline(xintercept = tail(split_day_m$test[[4]]$Day, n = 1))+
  geom_line(data = split_day_m$test[[5]], aes(x = Day, y = marginal_p_eur_mwh, color = "price"),alpha = 0.8)+
  geom_line(data = svr_val_lt2[[5]], aes(x = Day, y = value, color = "forecasted"),alpha = 0.8)+
  geom_vline(xintercept = tail(split_day_m$test[[5]]$Day, n = 1))+
  geom_line(data = split_day_m$test[[6]], aes(x = Day, y = marginal_p_eur_mwh, color = "price"),alpha = 0.8)+
  geom_line(data = svr_val_lt2[[6]], aes(x = Day, y = value, color = "forecasted"), alpha = 0.8)+
  geom_vline(xintercept = tail(split_day_m$test[[6]]$Day, n = 1))+
  geom_line(data = split_day_m$test[[7]], aes(x = Day, y = marginal_p_eur_mwh, color = "price"), alpha = 0.8)+
  geom_line(data = svr_val_lt2[[7]], aes(x = Day, y = value, color = "forecasted"), alpha = 0.8)+
  geom_vline(xintercept = tail(split_day_m$test[[7]]$Day, n = 1))+
  geom_line(data = split_day_m$test[[8]], aes(x = Day, y = marginal_p_eur_mwh, color = "price"),alpha = 0.8)+
  geom_line(data = svr_val_lt2[[8]], aes(x = Day, y = value, color = "forecasted"),alpha = 0.8)+
  geom_vline(xintercept = tail(split_day_m$test[[8]]$Day, n = 1))+
  geom_line(data = split_day_m$test[[9]], aes(x = Day, y = marginal_p_eur_mwh, color = "price"), alpha = 0.8)+
  geom_line(data = svr_val_lt2[[9]], aes(x = Day, y = value, color = "forecasted"), alpha = 0.8)+
  geom_vline(xintercept = tail(split_day_m$test[[9]]$Day, n = 1))+
  geom_line(data = split_day_m$test[[10]], aes(x = Day, y = marginal_p_eur_mwh, color = "price"), alpha = 0.8)+
  geom_line(data = svr_val_lt2[[10]], aes(x = Day, y = value, color = "forecasted"),  alpha = 0.8)+
  geom_vline(xintercept = tail(split_day_m$test[[10]]$Day, n = 1))+
  geom_line(data = split_day_m$test[[11]], aes(x = Day, y = marginal_p_eur_mwh, color = "price"), alpha = 0.8)+
  geom_line(data = svr_val_lt2[[11]], aes(x = Day, y = value, color = "forecasted"),alpha = 0.8)+
  geom_vline(xintercept = tail(split_day_m$test[[11]]$Day, n = 1))+
  geom_line(data = split_day_m$test[[12]], aes(x = Day, y = marginal_p_eur_mwh, color = "price"), alpha = 0.8)+
  geom_line(data = svr_val_lt2[[12]], aes(x = Day, y = value, color = "forecasted"),  alpha = 0.8)+
  labs(title =  "Monthly Rolling Interval of Log Transformed SVR Forecasted Against Test Values")+
  scale_colour_manual(values = c("price"='grey15', "forecasted"='firebrick1'), labels = c("Price", "Forecasted"))+
  labs(col = "Legend")+
  theme_bw()  

svm_lt_fplot_m2


