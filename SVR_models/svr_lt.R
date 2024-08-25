###########################
#Thesis: Support Vector Regression
###########################

source('data_preperation/packages.R')
source('data_preperation/data_cleaning_d.R')
source('data_preperation/transformation.R')
source('data_preperation/forcasting.R')

###########################
#SVR Model Monthly
###########################

###########################
#Hyperparmeter Tuning
###########################

#To train the model
SVRGridCoarse <- expand.grid(.sigma=c(0.001, 0.01, 0.1), .C=c(seq(1,10, by = 2), 50, 100))
ctrl <- trainControl(method = "cv", number=5) 

svr_lt_m_c = c()
set.seed(123)
for(i in 1:length(split_day_m$train)){
  svr_lt_m_c[[i]] = train(marginal_p_eur_mwh ~ lag_marg_price_eur_mwh,
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

#Fitting the trained model
fit_svr_lt_m_c = c()
pred_svr_lt_m_c = c()
svr_val_lt_m_c = c()

for(i in 1:length(split_day_m$train)){
  fit_svr_lt_m_c[[i]] = svm(marginal_p_eur_mwh ~ lag_marg_price_eur_mwh,data = split_day_m$train[[i]], type="eps-regression",kernel="radial",cost=svr_lt_m_c[[i]]$bestTune[1,2], gamma = svr_lt_m_c[[i]]$bestTune[1,1])
  pred_svr_lt_m_c[[i]] =  predict(fit_svr_lt_m_c[[i]], split_day_m$test[[i]]$lag_marg_price_eur_mwh)
  svr_val_lt_m_c[[i]] = cbind(Day = as.POSIXct(split_day_m$test[[i]]$Day),x = as_tibble(pred_svr_lt_m_c[[i]]))
}

###########################
#Accuracy Metrics
###########################

mean_lt_svr_m_c = c()
rmse_lt_svr_m_c =c()
mae_lt_svr_m_c = c()
smape_lt_svr_m_c = c()

for(i in 1:length(split_day_m$train)){
  rmse_lt_svr_m_c[i] = RMSE(svr_val_lt_m_c[[i]]$value, split_day_m$test[[i]]$marginal_p_eur_mwh )
  mae_lt_svr_m_c[i] = MAE(svr_val_lt_m_c[[i]]$value, split_day_m$test[[i]]$marginal_p_eur_mwh )
  smape_lt_svr_m_c[i] = smape(svr_val_lt_m_c[[i]]$value, split_day_m$test[[i]]$marginal_p_eur_mwh )
  mean_lt_svr_m_c = c(RMSE = mean(rmse_lt_svr_m_c),
                  MAE = mean(mae_lt_svr_m_c),
                  sMAPE = mean(smape_lt_svr_m_c))
}

start_date <- as.Date("2022-01-01")
end_date <- as.Date("2022-12-01")

# Generate a sequence of dates by month
months_seq <- seq.Date(from = start_date, to = end_date, by = "month")

acc_m_lt_svr_c = tibble(Day = months_seq, RMSE = rmse_lt_svr_m_c,MAE = mae_lt_svr_m_c,sMAPE = smape_lt_svr_m_c)

round(mean_lt_svr_m_c,3)

#Saving the object for later
#save(mean_lt_svr, file = 'mean_lt_svr.RData')
load("mean_lt_svr.RData")

###########################
#Results
###########################

svm_lt_fplot_m_c = ggplot()+
  geom_line(data = split_day_m$test[[1]], aes(x = Day, y = marginal_p_eur_mwh, color = "price"),alpha = 0.8)+
  geom_line(data = svr_val_lt_m_c[[1]], aes(x = Day, y = value, color = "forecasted"),alpha = 0.8)+
    geom_vline(xintercept = tail(split_day_m$test[[1]]$Day, n = 1))+
  geom_line(data = split_day_m$test[[2]], aes(x = Day, y = marginal_p_eur_mwh, color = "price"),alpha = 0.8)+
  geom_line(data = svr_val_lt_m_c[[2]], aes(x = Day, y = value, color = "forecasted"),alpha = 0.8)+
    geom_vline(xintercept = tail(split_day_m$test[[2]]$Day, n = 1))+
  geom_line(data = split_day_m$test[[3]], aes(x = Day, y = marginal_p_eur_mwh, color = "price"),alpha = 0.8)+
  geom_line(data = svr_val_lt_m_c[[3]], aes(x = Day, y = value, color = "forecasted"),alpha = 0.8)+
    geom_vline(xintercept = tail(split_day_m$test[[3]]$Day, n = 1))+
  geom_line(data = split_day_m$test[[4]], aes(x = Day, y = marginal_p_eur_mwh, color = "price"),alpha = 0.8)+
  geom_line(data = svr_val_lt_m_c[[4]], aes(x = Day, y = value, color = "forecasted"), alpha = 0.8)+
    geom_vline(xintercept = tail(split_day_m$test[[4]]$Day, n = 1))+
  geom_line(data = split_day_m$test[[5]], aes(x = Day, y = marginal_p_eur_mwh, color = "price"),alpha = 0.8)+
  geom_line(data = svr_val_lt_m_c[[5]], aes(x = Day, y = value, color = "forecasted"),alpha = 0.8)+
    geom_vline(xintercept = tail(split_day_m$test[[5]]$Day, n = 1))+
  geom_line(data = split_day_m$test[[6]], aes(x = Day, y = marginal_p_eur_mwh, color = "price"),alpha = 0.8)+
  geom_line(data = svr_val_lt_m_c[[6]], aes(x = Day, y = value, color = "forecasted"), alpha = 0.8)+
    geom_vline(xintercept = tail(split_day_m$test[[6]]$Day, n = 1))+
  geom_line(data = split_day_m$test[[7]], aes(x = Day, y = marginal_p_eur_mwh, color = "price"), alpha = 0.8)+
  geom_line(data = svr_val_lt_m_c[[7]], aes(x = Day, y = value, color = "forecasted"), alpha = 0.8)+
    geom_vline(xintercept = tail(split_day_m$test[[7]]$Day, n = 1))+
  geom_line(data = split_day_m$test[[8]], aes(x = Day, y = marginal_p_eur_mwh, color = "price"),alpha = 0.8)+
  geom_line(data = svr_val_lt_m_c[[8]], aes(x = Day, y = value, color = "forecasted"),alpha = 0.8)+
    geom_vline(xintercept = tail(split_day_m$test[[8]]$Day, n = 1))+
  geom_line(data = split_day_m$test[[9]], aes(x = Day, y = marginal_p_eur_mwh, color = "price"), alpha = 0.8)+
  geom_line(data = svr_val_lt_m_c[[9]], aes(x = Day, y = value, color = "forecasted"), alpha = 0.8)+
    geom_vline(xintercept = tail(split_day_m$test[[9]]$Day, n = 1))+
  geom_line(data = split_day_m$test[[10]], aes(x = Day, y = marginal_p_eur_mwh, color = "price"), alpha = 0.8)+
  geom_line(data = svr_val_lt_m_c[[10]], aes(x = Day, y = value, color = "forecasted"),  alpha = 0.8)+
    geom_vline(xintercept = tail(split_day_m$test[[10]]$Day, n = 1))+
  geom_line(data = split_day_m$test[[11]], aes(x = Day, y = marginal_p_eur_mwh, color = "price"), alpha = 0.8)+
  geom_line(data = svr_val_lt_m_c[[11]], aes(x = Day, y = value, color = "forecasted"),alpha = 0.8)+
    geom_vline(xintercept = tail(split_day_m$test[[11]]$Day, n = 1))+
  geom_line(data = split_day_m$test[[12]], aes(x = Day, y = marginal_p_eur_mwh, color = "price"), alpha = 0.8)+
  geom_line(data = svr_val_lt_m_c[[12]], aes(x = Day, y = value, color = "forecasted"),  alpha = 0.8)+
    labs(title =  "Monthly Rolling Interval SVR Forecasted Against Test Values")+
  scale_colour_manual(values = c("price"='grey15', "forecasted"='firebrick1'), labels = c("Price", "Forecasted"))+
  labs(col = "Legend")+
    theme_bw()  

svm_lt_fplot_m_c

