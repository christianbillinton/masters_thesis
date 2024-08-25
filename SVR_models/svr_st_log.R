###########################
#Thesis: Support Vector Regression - Log
###########################

source('data_preperation/packages.R')
source('data_preperation/data_cleaning_d.R')
source('data_preperation/transformation.R')
source('data_preperation/forcasting.R')

###########################
#SVR Daily Short term Forecast - LOG
###########################

###########################
#Hyperparmeter Tuning
###########################

sets_st = length(split_day2$train)

#Setting the Hyperparamters for the Crossvalidation
SVRGridCoarse <- expand.grid(.sigma=c(0.001, 0.01, 0.1), .C=c(seq(1,10, by = 1), 50, 100))
ctrl <- trainControl(method = "cv", number=5) 


#Reestimation approach to cross validation
fit_temp_svr = c()
set.seed(123)
for(i in 1:sets_st){
  fit_temp_svr[[i]] = train(data = split_day2$train[[i]], log_marg_price ~ laglog_marg_price, method="svmRadial", tuneGrid=SVRGridCoarse, trControl=ctrl, type="eps-svr")
}

#save(fit_temp_svr, file = 'fit_svr_stlog_train.RData')
load("RDATA_files/fit_svr_stlog_train.RData")

###########################
#Estimation and Forecast
###########################

#Training the model
fit_svr_st = c()

split_sv = split_day2
split_sv$train[[1]] = na.omit(split_sv$train[[1]])

set.seed(123)
for(i in 1:sets_st){
  fit_svr_st[[i]] = svm(log_marg_price ~ laglog_marg_price,data = split_sv$train[[i]], type="eps-regression",kernel="radial",cost= fit_temp_svr[[i]]$bestTune[2], gamma = fit_temp_svr[[i]]$bestTune[1])
}

#save(fit_svr_st, file = 'fit_svr_st.RData')
fit_st = load("RDATA_files/fit_svr_st.RData")

#Predicting values
pred_svr_st = c()
svr_val_st = c()
for(i in 1:365){
  pred_svr_st[[i]] =  predict(fit_svr_st[[i]], split_day2$test[[i]]$laglog_marg_price)
  svr_val_st[[i]] = cbind(Day = as.POSIXct(split_day2$test[[i]]$Day),x = as_tibble(pred_svr_st[[i]]))
}

###########################
#Back Transforming Log
###########################


temp_st_svr = c()
svr_val_st2 = c()
for(i in 1:sets_st){
  temp_st_svr[[i]] = log_inverse(svr_val_st[[i]]$value, c)
  svr_val_st2[[i]] = cbind(Day = as.POSIXct(split_day2$test[[i]]$Day),x = as_tibble(temp_st_svr[[i]]))
}

###########################
#Accuracy Metrics
###########################

rmse_st_svr2 = c()
mae_st_svr2 = c()
smape_st_svr2 = c()
for(i in 1:sets_st){
  rmse_st_svr2[i] = RMSE(svr_val_st2[[i]]$value, split_day2$test[[i]]$marginal_p_eur_mwh)
  mae_st_svr2[i] = MAE(svr_val_st2[[i]]$value, split_day2$test[[i]]$marginal_p_eur_mwh)
  smape_st_svr2[i] = smape(svr_val_st2[[i]]$value, split_day2$test[[i]]$marginal_p_eur_mwh)
  mean_st_svr2 = c(RMSE = mean(rmse_st_svr2),
                  MAE = mean(mae_st_svr2),
                  sMAPE = mean(smape_st_svr2))
}

mean_st_svr2

acc_m_st_svr = tibble(Day = test_data_shortd$Day, RMSE = rmse_st_svr2,MAE = mae_st_svr2,sMAPE = smape_st_svr)

###########################
#Results
###########################

#Plotting forecasted values
f_val_st_svm2 = data_frame(NA)
for(i in 1:sets_st){
  f_val_st_svm2[i,1] = svr_val_st2[[i]]$Day
  f_val_st_svm2[i,2] = svr_val_st2[[i]]$value
}
colnames(f_val_st_svm2) = c("Date", "value")


svr_st_plot2 = ggplot()+
  geom_line(data = test_data_shortd, aes(x = Day, y = marginal_p_eur_mwh, color = "marginal_p_eur_mwh"), alpha = 0.8)+
  geom_line(data = f_val_st_svm2, aes(x = Date, y = value, color = "value"), alpha = 0.8)+
  labs(title = "Daily Rolling Interval of Log Transformed SVR Forecasted Against Test Values")+
  scale_colour_manual(values = c("marginal_p_eur_mwh"='grey15', "value"='firebrick1'), labels = c("Price", "Forecasted"))+
  labs(col = "Legend")+
  theme_light()  

svr_st_plot2


