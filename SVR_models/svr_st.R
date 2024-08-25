###########################
#Thesis: Support Vector Regression 
###########################

source('data_preperation/packages.R')
source('data_preperation/data_cleaning_d.R')
source('data_preperation/transformation.R')
source('data_preperation/forcasting.R')

###########################
#SVR Daily Short term Forecast
###########################

###########################
#Hyperparmeter Tuning
###########################

sets_st = length(split_day2$train)

SVRGridCoarse <- expand.grid(.sigma=c(0.001, 0.01, 0.1), .C=c(seq(1,10, by = 1), 50, 100))
ctrl <- trainControl(method = "cv", number=5) 

fit_temp_svr_c = c()
set.seed(123)
for(i in 1:sets_st){
  fit_temp_svr_c[[i]] = train(data = split_day2$train[[i]], marginal_p_eur_mwh ~ lag_marg_price_eur_mwh, method="svmRadial", tuneGrid=SVRGridCoarse, trControl=ctrl, type="eps-svr")
}


#save(fit_temp_svr_c, file = 'fit_temp_svr_c.RData')
load("RDATA_files/fit_svr_stlog_train_c.RData")

###########################
#Estimation
###########################

sets_st = 365
fit_svr_st_c = c()

split_sv = split_day2
split_sv$train[[1]] = na.omit(split_sv$train[[1]])


set.seed(123)
for(i in 1:365){
  fit_svr_st_c[[i]] = svm(marginal_p_eur_mwh ~ lag_marg_price_eur_mwh, data = split_sv$train[[i]], type="eps-regression",kernel="radial", cost = fit_temp_svr_c[[i]]$bestTune[1,2], gamma = fit_temp_svr_c[[i]]$bestTune[1,1])          
}

#save(fit_svr_st_c, file = 'fit_svr_st_c.RData')
load("RDATA_files/fit_svr_st_c.RData")

###########################
#Forecast
###########################

pred_svr_st_c = c()
svr_val_st_c = c()
for(i in 1:365){
  pred_svr_st_c[[i]] =  predict(fit_svr_st_c[[i]], split_day2$test[[i]]$lag_marg_price_eur_mwh)
  svr_val_st_c[[i]] = cbind(Day = as.POSIXct(split_day2$test[[i]]$Day),x = as_tibble(pred_svr_st_c[[i]]))
}

###########################
#Accuracy Metrics
###########################

rmse_st_svr_c = c()
mae_st_svr_c = c()
smape_st_svr_c = c()

for(i in 1:sets_st){
  rmse_st_svr_c[i] = RMSE(svr_val_st_c[[i]]$value, split_day2$test[[i]]$marginal_p_eur_mwh)
  mae_st_svr_c[i] = MAE(svr_val_st_c[[i]]$value, split_day2$test[[i]]$marginal_p_eur_mwh)
  smape_st_svr_c[i] = smape(svr_val_st_c[[i]]$value, split_day2$test[[i]]$marginal_p_eur_mwh)
  mean_st_svr_c = c(RMSE = mean(rmse_st_svr_c),
                     MAE = mean(mae_st_svr_c),
                     sMAPE = mean(smape_st_svr_c))
}

acc_m_st_svr_c = tibble(Day = test_data_shortd$Day, RMSE = rmse_st_svr_c,MAE = mae_st_svr_c,sMAPE = smape_st_svr_c)

mean_st_svr_c
#save(mean_st_svr_c, file = 'mean_st_svr_c.RData')
load("RDATA_files/mean_st_svr_c.RData")

###########################
#Results
###########################


f_val_st_svm_c = data_frame(NA)
for(i in 1:sets_st){
  f_val_st_svm_c[i,1] = svr_val_st_c[[i]]$Day
  f_val_st_svm_c[i,2] = svr_val_st_c[[i]]$value
}
colnames(f_val_st_svm_c) = c("Date", "value")

svr_st_plot_c = ggplot()+
  geom_line(data = test_data_shortd, aes(x = Day, y = marginal_p_eur_mwh, color = "marginal_p_eur_mwh"), alpha = 0.8)+
  geom_line(data = f_val_st_svm_c, aes(x = Date, y = value, color = "value"), alpha = 0.8)+
  labs(y = "Marginal Price",
       x = "",
       title = "Daily Rolling Interval SVR Forecasted Against Test Values")+
  scale_colour_manual(values = c("marginal_p_eur_mwh"='grey15', "value"='firebrick1'), labels = c("Price", "Forecasted"))+
  labs(col = "Legend")+
  theme_light()  

svr_st_plot_c


