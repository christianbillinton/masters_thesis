###########################
#Thesis: Cross validation prediction
###########################

#source('class_model.R')
#source('deep_learning_model.R')
#source('packages.R')
#source('data_cleaning_d.R')


#n_w = nweeks(elec_p)
#n_d = ndays(el_price)
#n_y = nyears(elec_p)
#n_m = nmonths(elec_p)
#n_h = nhours(elec_p)

###########################
#LongTerm Rolling Windows Daily
###########################

#Longterm Rolling window Daily

#rolling sets 2
sets_lt = 3

traind_lt = c()
testd_lt = c()
#traind_lt_ind = c()
#train_lt_ind$index1 = which(as.Date("2018-01-01") <= elect_price$Day & elect_price$Day <= as.Date("2019-12-31"))


year = c(2018:2022)
for(i in 1:sets_lt){
  #if(i == 1){
  traind_lt[[i]] = elect_price_d[which(as.Date(paste0(year[i],"-01-01")) <= elect_price_d$Day & elect_price_d$Day <= as.Date(paste0(year[i+1],"-12-31"))),]#two year window 
  testd_lt[[i]] = elect_price_d[which(as.Date(paste0(year[i+2],"-01-01")) <= elect_price_d$Day & elect_price_d$Day <= as.Date(paste0(year[i+2],"-12-31"))),]#one year window 
}
traind_lt[[1]] = na.omit(traind_lt[[1]])



range(traind_lt[[1]]$Day)
range(traind_lt[[2]]$Day)
range(traind_lt[[3]]$Day)
range(testd_lt[[1]]$Day)
range(testd_lt[[2]]$Day)
range(testd_lt[[3]]$Day)
#n_lt_test1d = length(testd_lt[[1]]$marginal_p_eur_mwh)
#n_lt_test2d = length(testd_lt[[2]]$marginal_p_eur_mwh)
#n_lt_test1d#difference of 24 values as the test date is a leap year
#n_lt_test2d

#n_lt_train1d = length(traind_lt[[1]])
#n_lt_train2d = length(traind_lt[[2]])
#n_lt_train1d
#n_lt_train2d

traind_lt[[1]] = na.omit(traind_lt[[1]])

lt_plotd = c()
for(i in 1:sets_lt){
  lt_plotd[[i]] = ggplot()+
    geom_line(data = traind_lt[[i]], aes(x = Day, y = marginal_p_eur_mwh), color = 'grey15', alpha = 0.8)+
    geom_line(data = testd_lt[[i]], aes(x = Day, y = marginal_p_eur_mwh), color = 'firebrick1', alpha = 0.8)+
    labs(title = paste0("Slice ",i, " of Train/Test of long Term Horizon"))+
    theme_bw()  
}

lt_plotd[[1]] / lt_plotd[[2]] / lt_plotd[[3]]

lt_plotd_log = c()
for(i in 1:sets_lt){
  lt_plotd_log[[i]] = ggplot()+
    geom_line(data = traind_lt[[i]], aes(x = Day, y = log_marg_price), color = 'grey15', alpha = 0.8)+
    geom_line(data = testd_lt[[i]], aes(x = Day, y = log_marg_price), color = 'firebrick1', alpha = 0.8)+
    labs(title = paste0("Slice ",i, " of Train/Test of long Term Horizon"))+
    theme_bw()  
}

lt_plotd_log[[1]] / lt_plotd_log[[2]] / lt_plotd_log[[3]]


###########################
#Short term Rolling Window Daily
###########################

# Function to create rolling window splits
rolling_window_split <- function(data, train_window, test_window) {
  
  
  # Define window sizes in days
  train_days <- train_window 
  test_days <- test_window
  
  # Initialize empty lists for train and test sets
  train_data <- list()
  test_data <- list()
  train_start = c()
  train_end = c()
  test_start = c()
  test_end = c()
  
  # Loop through the data with rolling window
  for (i in 1:365) {
    # Training data window
    train_start[i] <- i
    train_end[i] <- train_start[i] + train_days
    train_data[[i]] <- data[train_start[i]:train_end[i], ]
    
    # Test data window (1 day after training window)
    test_start[i] <- train_end[i] + 1
    test_end[i] <- test_start[i] + test_days
    test_data[[i]] <- data[test_start[i],]
  }
  
  return(list(train = train_data, test = test_data))
}


train_windowd2 = 1460
test_windowd2 = 1

split_day = c()
split_day2 = rolling_window_split(data = elect_price_d, train_window = train_windowd2, test_window = test_windowd2)

sets_st = length(split_day2$train)
for(i in 1:sets_st){
  split_day2$train[[i]] = na.omit(split_day2$train[[i]])
}

range(split_day2$train[[1]]$Day)
range(split_day2$train[[365]]$Day)
range(split_day2$test[[1]]$Day)
range(split_day2$test[[365]]$Day)

#Day Ahead 
train_data_shortd = elect_price_d[1:1461,]
train_data_shortd = na.omit(train_data_shortd)
test_windowd = 1462
test_data_shortd = elect_price_d[test_windowd:length(elect_price_d$marginal_p_eur_mwh),]

#Training data
plot_marg_price_train = ggplot()+
  geom_line(data = train_data_shortd, aes(x = Day, y = marginal_p_eur_mwh, color = "marginal_p_eur_mwh"), alpha = 0.8)+
  geom_line(data = test_data_shortd, aes(x = Day, y = marginal_p_eur_mwh, color = "marginal_p_eur_mwh2"), alpha = 0.8)+
  labs(y = "Marginal Price (EUR/MWH)", title  = "Initial Train/Test set Marginal Price (EUR/MWH)")+
  scale_colour_manual(values = c("marginal_p_eur_mwh"='grey15', "marginal_p_eur_mwh2"='firebrick1'), labels = c("Price", "Forecasted"))+
  labs(col = "Legend")+
  theme_bw()

plot_marg_price_train



#Training data
plot_marg_price_trainlog = ggplot()+
  geom_line(data = train_data_shortd, aes(x = Day, y = log_marg_price, color = "log_marg_price1"), alpha = 0.8)+
  geom_line(data = test_data_shortd, aes(x = Day, y = log_marg_price, color = "log_marg_price2"), alpha = 0.8)+
  labs(y = "Log Marginal Price (EUR/MWH)", title  = "Initial Train/Test set of Log Transformed Marginal Price (EUR/MWH)")+
  labs(y = "Marginal Price (EUR/MWH)", title  = "Initial Train/Test set Marginal Price (EUR/MWH)")+
  scale_colour_manual(values = c("log_marg_price1"='grey15', "log_marg_price2"='firebrick1'), labels = c("Price", "Forecast"))+
  labs(col = "Legend")+
  theme_bw()

plot_marg_price_trainlog

###########################
#Long term Rolling Window Monthly
###########################

# Function to create rolling window splits
rolling_window_split_m <- function(data, train_window, test_window, increment){ 
  
  
  # Define window sizes in days
  train_days <- train_window
  test_days <- test_window
  
  # Initialize empty lists for train and test sets
  train_data <- list()
  test_data <- list()
  train_start = c()
  train_end = c()
  test_start = c()
  test_end = c()
  
  # Loop through the data with rolling window
  for (i in 1:12) {
    if(i == 1){
      train_start[i] <- increment[i]
    }else{
      train_start[i] <- train_start[i-1] + increment[i]
    }
    train_end[i] <- train_start[i] + train_days
    train_data[[i]] <- data[train_start[i]:train_end[i], ]
    
    # Test data window (1 day after training window)
    test_start[i] <- train_end[i] + 1
    test_end[i] <- test_start[i] + test_days[i]
    test_data[[i]] <- data[test_start[i]:test_end[i],]
  }
  
  return(list(train = train_data, test = test_data))
}

train_windowd3 = 1460
increment = c(1,31,28,31,30,31,30,31,31,30,31,30)
test_windowd3 = c(30,27,30,29,30,29,30,30,29,30,29,30)

split_day_m = c()
split_day_m = rolling_window_split_m(data = elect_price_d, train_window = train_windowd3, test_window = test_windowd3, increment = increment)


length(split_day_m$train)
for(i in 1:length(split_day_m$train)){
  split_day_m$train[[i]] = na.omit(split_day_m$train[[i]])
}

range(split_day_m$train[[1]]$Day)
range(split_day_m$train[[12]]$Day)
range(split_day_m$test[[1]]$Day)
range(split_day_m$test[[12]]$Day)

#Day Ahead 
train_data_shortd = elect_price_d[1:1461,]
train_data_shortd = na.omit(train_data_shortd)
test_windowd = 1462
test_data_shortd = elect_price_d[test_windowd:length(elect_price_d$marginal_p_eur_mwh),]

#Training data
plot_marg_price_train_m = ggplot()+
  geom_line(data = split_day_m$train[[1]], aes(x = Day, y = marginal_p_eur_mwh), color = 'grey15', alpha = 0.8)+
  geom_line(data = split_day_m$test[[1]], aes(x = Day, y = marginal_p_eur_mwh), color = 'firebrick1', alpha = 0.8)+
  labs(y = "Marginal Price (EUR/MWH)", title  = "Initial Train/Test set Marginal Price (EUR/MWH)")+
  theme_bw()

plot_marg_price_train_m



#Training data
plot_marg_price_trainlog = ggplot()+
  geom_line(data = train_data_shortd, aes(x = Day, y = log_marg_price, color = "train"), alpha = 0.8)+
  geom_line(data = test_data_shortd, aes(x = Day, y = log_marg_price, color = "test"),  alpha = 0.8)+
  geom_vline(xintercept = tail(split_day_m$test[[1]]$Day, n = 1))+
  geom_vline(xintercept = tail(split_day_m$test[[2]]$Day, n = 1))+
  geom_vline(xintercept = tail(split_day_m$test[[3]]$Day, n = 1))+
  geom_vline(xintercept = tail(split_day_m$test[[4]]$Day, n = 1))+
  geom_vline(xintercept = tail(split_day_m$test[[5]]$Day, n = 1))+
  geom_vline(xintercept = tail(split_day_m$test[[6]]$Day, n = 1))+
  geom_vline(xintercept = tail(split_day_m$test[[7]]$Day, n = 1))+
  geom_vline(xintercept = tail(split_day_m$test[[8]]$Day, n = 1))+
  geom_vline(xintercept = tail(split_day_m$test[[9]]$Day, n = 1))+
  geom_vline(xintercept = tail(split_day_m$test[[10]]$Day, n = 1))+
  geom_vline(xintercept = tail(split_day_m$test[[11]]$Day, n = 1))+
  geom_vline(xintercept = tail(split_day_m$test[[12]]$Day, n = 1))+
  labs(y = "Log Marginal Price (EUR/MWH)", title  = "Initial Train/Test set of Log Transformed Marginal Price (EUR/MWH)")+
  scale_colour_manual(values = c("train"='grey15', "test"='firebrick1'), labels = c("Train", "Test"))+
  labs(col = "Legend")+
  theme_bw()

plot_marg_price_trainlog




