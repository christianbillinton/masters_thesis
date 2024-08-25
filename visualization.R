###########################
#Thesis: Visuallization
###########################

### Visualization ###
source('packages.R')
source('data_cleaning.R')

date_range = range(elect_price$Hour)

date_range


###########################
#Daily Data
###########################



###########################
#Simple plot of the Marginal Price Daily
###########################

#dimensions 929x500

plot_marg_priced = ggplot(elect_price_d)+
  geom_line(aes(x = Day, y = marginal_p_eur_mwh), color = "grey15", alpha = 0.8)+
  labs(y = "Marginal Price (EUR/MWH)", title  = "Daily Averaged Marginal Price (EUR/MWH)")+
  theme_bw()

plot_marg_priced

#ggsave('marg_price.png', plot = plot_marg_price)

plot_marg_priced_diff = ggplot(elect_price_d, aes(x = Day, y = diff_marg_price), color = "grey15", alpha = 0.8)+
  geom_line()+
  labs(y = "Marginal Price (EUR/MWH)", title  = "Daily Averaged Marginal Price (EUR/MWH)")+
  theme_bw()

plot_marg_priced_log = ggplot(elect_price_d, aes(x = Day, y = log_marg_price), color = "grey15", alpha = 0.8)+
  geom_line()+
  labs(y = "Marginal Price (EUR/MWH)", title  = "Log Daily Averaged Marginal Price (EUR/MWH)")+
  theme_bw()

plot_marg_priced_log

elect_price_d_ma7 = SMA(elect_price_d$marginal_p_eur_mwh, n = 7)
elect_price_d_ma30 = SMA(elect_price_d$marginal_p_eur_mwh, n = 30)
months = seq(as.POSIXct("2018-01-01", tz = "UTC"), as.POSIXct("2022-12-01", tz = "UTC"), by = "month")
elect_price_d$MA7_l = SMA(elect_price_d$log_marg_price, n = 7)

dates = which(elect_price_d$Day %in% as.POSIXct("2022-02-21 00:00:00", tz = "UTC"))

plot_marg_priced_sma = ggplot(elect_price_d, aes(x = Day))+
  geom_line(aes(y = marginal_p_eur_mwh, color = "Marginal Price"), alpha = 0.9)+
  geom_line(aes(y = MA30, color = "30 Day Moving AVG"), alpha = 0.9)+
  scale_colour_manual(name = "Legend",values = c("Marginal Price"='grey15', "30 Day Moving AVG"='firebrick1', labels = c("Marginal Price", "7 Day Moving AVG")))+
  labs(y = "Marginal Price (EUR/MWH)", title  = "Daily Averaged Marginal Price (EUR/MWH)")+
  geom_vline(xintercept = as.numeric(elect_price_d$Day[dates]), linetype = "dashed")+
  theme_bw()

plot_marg_priced_sma



plot_marg_priced_sma_stable = ggplot(elect_price_d[which(elect_price_d$Day < as.POSIXct("2021-01-01 00:00:00", tz = "UTC")),], aes(x = Day))+
  geom_line(aes(y = marginal_p_eur_mwh, color = "Marginal Price"), alpha = 0.9)+
  geom_line(aes(y = MA30, color = "30 Day Moving AVG"), alpha = 0.9)+
  scale_colour_manual(name = "Legend",values = c("Marginal Price"='grey15', "30 Day Moving AVG"='firebrick1', labels = c("Marginal Price", "7 Day Moving AVG")))+
  labs(y = "Marginal Price (EUR/MWH)", title  = "Daily Averaged Marginal Price (EUR/MWH)")+
  geom_vline(xintercept = as.numeric(elect_price_d$Day[dates]), linetype = "dashed")+
  theme_bw()

plot_marg_priced_sma_stable


sumry_marg_price_d  = elect_price_d %>% 
  summarize(min = round(min(marginal_p_eur_mwh, na.rm = T), digits = 3), 
            max = round(max(marginal_p_eur_mwh, na.rm = T), digits = 3),
            Q1 = round(quantile(marginal_p_eur_mwh, prob = 0.25, na.rm = T), digits = 3),
            Median = round(median(marginal_p_eur_mwh, na.rm = T), digits = 3),
            Q3 = round(quantile(marginal_p_eur_mwh, prob = 0.75, na.rm = T), digits = 3),
            Mean = round(mean(marginal_p_eur_mwh, na.rm = T), digits =3),
            SD = round(sd(marginal_p_eur_mwh, na.rm = T), digits = 3),
            Range = round(range(marginal_p_eur_mwh, na.rm = T), digits = 3),
            IQR = round(IQR(marginal_p_eur_mwh, na.rm = T), digits = 3),
            Skewness = round(skewness(marginal_p_eur_mwh, na.rm = T), digits = 3),
            Kurtosis = round(kurtosis(marginal_p_eur_mwh, na.rm = T), digits = 3),
            Jaque_Bera = jarque.bera.test(na.omit(marginal_p_eur_mwh))$p.value,
            Obs = length(marginal_p_eur_mwh))
sumry_marg_price_d = sumry_marg_price_d[1,]

hist_marg_price = ggplot(elect_price_d, aes(x = marginal_p_eur_mwh))+
  geom_histogram(fill = '#30a771', alpha = 0.9, colour = "#e9ecef")+
  xlab("Marginal Price (EUR/MWH)")+
  labs(title = "Histogram of Marginal Price (EUR/MWH)")+
  geom_table_npc(data = sumry_marg_price_d, label = list(sumry_marg_price_d), npcx = 0.00, npcy = 1, size = 3)+
  theme_bw()


###########################
#Simple plot of the Marginal Price Train Set and Test set Daily
###########################

#Training data
plot_marg_price_train = ggplot(train, aes(x = Hour, y = marginal_p_eur_mwh))+
  geom_line()+
  labs(y = "Marginal Price (EUR/MWH)", title  = "Hourly Averaged Train set Marginal Price (EUR/MWH)")+
  theme_bw()

plot_marg_price_train

#ggsave('marg_price.png', plot = plot_marg_price)


sumry_marg_price_train  = train_data %>% 
  summarize(min = round(min(marginal_p_eur_mwh, na.rm = T), digits = 3), 
            max = round(max(marginal_p_eur_mwh, na.rm = T), digits = 3),
            Q1 = round(quantile(marginal_p_eur_mwh, prob = 0.25, na.rm = T), digits = 3),
            Median = round(median(marginal_p_eur_mwh, na.rm = T), digits = 3),
            Q3 = round(quantile(marginal_p_eur_mwh, prob = 0.75, na.rm = T), digits = 3),
            Mean = round(mean(marginal_p_eur_mwh, na.rm = T), digits =3),
            SD = round(sd(marginal_p_eur_mwh, na.rm = T), digits = 3),
            Range = round(range(marginal_p_eur_mwh, na.rm = T), digits = 3),
            IQR = round(IQR(marginal_p_eur_mwh, na.rm = T), digits = 3),
            Skewness = round(skewness(marginal_p_eur_mwh, na.rm = T), digits = 3),
            Kurtosis = round(kurtosis(marginal_p_eur_mwh, na.rm = T), digits = 3),
            Jaque_Bera = jarque.bera.test(na.omit(marginal_p_eur_mwh))$p.value)

sumry_marg_price_train = sumry_marg_price_train[1,]

hist_marg_price_train = ggplot(train_data, aes(x = marginal_p_eur_mwh))+
  geom_histogram(fill = '#30a771', alpha = 0.9, colour = "#e9ecef")+
  xlab("Marginal Price (EUR/MWH)")+
  labs(title = "Histogram of Training data for Marginal Price (EUR/MWH)")+
  geom_table_npc(data = sumry_marg_price, label = list(sumry_marg_price), npcx = 0.00, npcy = 1, size = 3)+
  theme_bw()


#Testing set

plot_marg_price_test = ggplot(test_data, aes(x = Hour, y = marginal_p_eur_mwh))+
  geom_line()+
  labs(y = "Marginal Price (EUR/MWH)", title  = "Hourly Averaged Test set Marginal Price (EUR/MWH)")+
  theme_bw()

plot_marg_price_test

#ggsave('marg_price.png', plot = plot_marg_price)


sumry_marg_price_test  = test_data %>% 
  summarize(min = round(min(marginal_p_eur_mwh, na.rm = T), digits = 3), 
            max = round(max(marginal_p_eur_mwh, na.rm = T), digits = 3),
            Q1 = round(quantile(marginal_p_eur_mwh, prob = 0.25, na.rm = T), digits = 3),
            Median = round(median(marginal_p_eur_mwh, na.rm = T), digits = 3),
            Q3 = round(quantile(marginal_p_eur_mwh, prob = 0.75, na.rm = T), digits = 3),
            Mean = round(mean(marginal_p_eur_mwh, na.rm = T), digits =3),
            SD = round(sd(marginal_p_eur_mwh, na.rm = T), digits = 3),
            Range = round(range(marginal_p_eur_mwh, na.rm = T), digits = 3),
            IQR = round(IQR(marginal_p_eur_mwh, na.rm = T), digits = 3),
            Skewness = round(skewness(marginal_p_eur_mwh, na.rm = T), digits = 3),
            Kurtosis = round(kurtosis(marginal_p_eur_mwh, na.rm = T), digits = 3),
            Jaque_Bera = jarque.bera.test(na.omit(marginal_p_eur_mwh))$p.value)
sumry_marg_price_train = sumry_marg_price_train[1,]

hist_marg_price_test = ggplot(train_data, aes(x = marginal_p_eur_mwh))+
  geom_histogram(fill = '#30a771', alpha = 0.9, colour = "#e9ecef")+
  xlab("Marginal Price (EUR/MWH)")+
  labs(title = "Histogram of Test data for Marginal Price (EUR/MWH)")+
  geom_table_npc(data = sumry_marg_price, label = list(sumry_marg_price), npcx = 0.00, npcy = 1, size = 3)+
  theme_bw()

######Plotting 


plot_marg_price_train + plot_marg_price_test

###########################
#TS decomposition plots
###########################

#STL decomposition of Marginal Price
stl_marg = stl(marg_price2, s.window = 'periodic')
summary(stl_marg)
autoplot(stl_marg)
stl_marg

mstl_marg = mstl(marg_price2)

stl_plot = autoplot(stl_marg)+
  labs(title = 'STL Decomposition of Marginal Price (EUR/MWH)')+
  theme_bw()
stl_plot

mstl_plot = autoplot(mstl_marg)+
  labs(title = 'STL Decomposition of Marginal Price (EUR/MWH)')+
  theme_bw()
mstl_plot

#ggsave('stl_plot.png', plot = stl_plot)

#Seasonal plot  

plot_y22 = el_price %>% 
  filter(Hour >= "2022-01-01 00:00:00" & Hour <= "2022-12-31 00:00:00") %>% 
  gg_season(marginal_p_eur_mwh, period = 'year')+
  xlab("Yearly (2022)")+
  ylab("Marginal Price (eur/MWH)")+
  labs(title = "Yearly Seasonality 2022")+
  theme_bw()

plot_m22 = el_price %>% 
  filter(Hour >= "2022-01-01 00:00:00" & Hour <= "2022-12-31 00:00:00") %>% 
  gg_season(marginal_p_eur_mwh, period = 'month')+
  xlab("Monthly (2022)")+
  ylab("Marginal Price (eur/MWH)")+
  labs(title = "Monthly Seasonality 2022")+
  theme_bw()

plot_w22 = el_price %>% 
  filter(Hour >= "2022-01-01 00:00:00" & Hour <= "2022-12-31 00:00:00") %>% 
  gg_season(marginal_p_eur_mwh, period = 'week')+
  xlab("Weekly (2022)")+
  ylab("Marginal Price (eur/MWH)")+
  labs(title = "Weekly Seasonality 2022")+
  theme_bw()

plot_d22 = el_price %>% 
  filter(Hour >= "2022-01-01 00:00:00" & Hour <= "2022-12-31 00:00:00") %>% 
  gg_season(marginal_p_eur_mwh, period = 'day')+
  xlab("Daily (2022)")+
  ylab("Marginal Price (eur/MWH)")+
  labs(title = "Daily Seasonality 2022")+
  theme_bw()

season_22 = (plot_y22 + plot_m22)/(plot_w22 + plot_d22)

season_22

#ggsave('season_22.png', plot = season_22)

plot_y = el_price %>% 
  fill_gaps() %>% 
  #filter(Hour >= "2018-01-01 00:00:00" & Hour <= "2020-12-31 00:00:00") %>% 
  gg_season(marginal_p_eur_mwh, period = 'year')+
  xlab("Yearly")+
  ylab("Marginal Price (eur/MWH)")+
  labs(title = "Yearly Seasonality")+
  theme_bw()


ggseasonplot(marg_priced2)

gg_season(marg_priced, period = "week")


###########################
#TS Autocorrelation plots
###########################

#autocorrelation of marginal price 

?ggAcf
?acf

marg_acfd = ggAcf(elect_price_d$marginal_p_eur_mwh)+
  labs(title = "Autocorrelation Plot of Maginal Price (EUR/MWH)")+
  theme_bw()
marg_pacfd = ggPacf(elect_price_d$marginal_p_eur_mwh)+
  labs(title = ' Partial Autocorrelation Plot of Maginal Price (EUR/MWH)')+
  theme_bw()

marg_acfd/marg_pacfd 
#We can see that there is a white noise process from the autocorrelation.


#Autocorrelation of first differences ACF and PACF
#elect_price_d$diff_marg_price = c(NA, diff(elect_price_d$marginal_p_eur_mwh))

dmarg_acfd = ggAcf(elect_price_d$diff_marg_price)+
  labs(title = "Autocorrelation PLot of Maginal Price (EUR/MWH)")+
  theme_bw()
dmarg_pacfd = ggPacf(elect_price_d$diff_marg_price)+
  labs(title = ' Partial Autocorrelation PLot of Maginal Price (EUR/MWH)')+
  theme_bw()

dmarg_acfd/dmarg_pacfd
#We can see the cyclicall nature of the daily values of our data from the ACF




###########################
#Simple plot of the Marginal Price HOurly
###########################
plot_marg_price = ggplot(elect_price, aes(x = Hour, y = marginal_p_eur_mwh))+
  geom_line()+
  labs(y = "Marginal Price (EUR/MWH)", title  = "Hourly Averaged Marginal Price (EUR/MWH)")+
  theme_bw()

plot_marg_price

#ggsave('marg_price.png', plot = plot_marg_price)


sumry_marg_price  = elect_price %>% 
  summarize(min = round(min(marginal_p_eur_mwh, na.rm = T), digits = 3), 
            max = round(max(marginal_p_eur_mwh, na.rm = T), digits = 3),
            Q1 = round(quantile(marginal_p_eur_mwh, prob = 0.25, na.rm = T), digits = 3),
            Median = round(median(marginal_p_eur_mwh, na.rm = T), digits = 3),
            Q3 = round(quantile(marginal_p_eur_mwh, prob = 0.75, na.rm = T), digits = 3),
            Mean = round(mean(marginal_p_eur_mwh, na.rm = T), digits =3),
            SD = round(sd(marginal_p_eur_mwh, na.rm = T), digits = 3),
            Range = round(range(marginal_p_eur_mwh, na.rm = T), digits = 3),
            IQR = round(IQR(marginal_p_eur_mwh, na.rm = T), digits = 3),
            Skewness = round(skewness(marginal_p_eur_mwh, na.rm = T), digits = 3),
            Kurtosis = round(kurtosis(marginal_p_eur_mwh, na.rm = T), digits = 3),
            Jaque_Bera = jarque.bera.test(na.omit(marginal_p_eur_mwh))$p.value)
sumry_marg_price = sumry_marg_price[1,]

hist_marg_price = ggplot(el_price, aes(x = marginal_p_eur_mwh))+
  geom_histogram(fill = '#30a771', alpha = 0.9, colour = "#e9ecef")+
  xlab("Marginal Price (EUR/MWH)")+
  labs(title = "Histogram of Marginal Price (EUR/MWH)")+
  geom_table_npc(data = sumry_marg_price, label = list(sumry_marg_price), npcx = 0.00, npcy = 1, size = 3)+
  theme_bw()



###########################
#Simple plot of the Marginal Price Train Set and Test set
###########################

#Training data

plot_marg_price_train = ggplot(train_data, aes(x = Hour, y = marginal_p_eur_mwh))+
  geom_line()+
  labs(y = "Marginal Price (EUR/MWH)", title  = "Hourly Averaged Train set Marginal Price (EUR/MWH)")+
  theme_bw()

plot_marg_price_train

#ggsave('marg_price.png', plot = plot_marg_price)


sumry_marg_price_train  = train_data %>% 
  summarize(min = round(min(marginal_p_eur_mwh, na.rm = T), digits = 3), 
            max = round(max(marginal_p_eur_mwh, na.rm = T), digits = 3),
            Q1 = round(quantile(marginal_p_eur_mwh, prob = 0.25, na.rm = T), digits = 3),
            Median = round(median(marginal_p_eur_mwh, na.rm = T), digits = 3),
            Q3 = round(quantile(marginal_p_eur_mwh, prob = 0.75, na.rm = T), digits = 3),
            Mean = round(mean(marginal_p_eur_mwh, na.rm = T), digits =3),
            SD = round(sd(marginal_p_eur_mwh, na.rm = T), digits = 3),
            Range = round(range(marginal_p_eur_mwh, na.rm = T), digits = 3),
            IQR = round(IQR(marginal_p_eur_mwh, na.rm = T), digits = 3),
            Skewness = round(skewness(marginal_p_eur_mwh, na.rm = T), digits = 3),
            Kurtosis = round(kurtosis(marginal_p_eur_mwh, na.rm = T), digits = 3),
            Jaque_Bera = jarque.bera.test(na.omit(marginal_p_eur_mwh))$p.value)

sumry_marg_price_train = sumry_marg_price_train[1,]

hist_marg_price_train = ggplot(train_data, aes(x = marginal_p_eur_mwh))+
  geom_histogram(fill = '#30a771', alpha = 0.9, colour = "#e9ecef")+
  xlab("Marginal Price (EUR/MWH)")+
  labs(title = "Histogram of Training data for Marginal Price (EUR/MWH)")+
  geom_table_npc(data = sumry_marg_price, label = list(sumry_marg_price), npcx = 0.00, npcy = 1, size = 3)+
  theme_bw()


#Testing set

plot_marg_price_test = ggplot(test_data, aes(x = Hour, y = marginal_p_eur_mwh))+
  geom_line()+
  labs(y = "Marginal Price (EUR/MWH)", title  = "Hourly Averaged Test set Marginal Price (EUR/MWH)")+
  theme_bw()

plot_marg_price_test

#ggsave('marg_price.png', plot = plot_marg_price)


sumry_marg_price_test  = test_data %>% 
  summarize(min = round(min(marginal_p_eur_mwh, na.rm = T), digits = 3), 
            max = round(max(marginal_p_eur_mwh, na.rm = T), digits = 3),
            Q1 = round(quantile(marginal_p_eur_mwh, prob = 0.25, na.rm = T), digits = 3),
            Median = round(median(marginal_p_eur_mwh, na.rm = T), digits = 3),
            Q3 = round(quantile(marginal_p_eur_mwh, prob = 0.75, na.rm = T), digits = 3),
            Mean = round(mean(marginal_p_eur_mwh, na.rm = T), digits =3),
            SD = round(sd(marginal_p_eur_mwh, na.rm = T), digits = 3),
            Range = round(range(marginal_p_eur_mwh, na.rm = T), digits = 3),
            IQR = round(IQR(marginal_p_eur_mwh, na.rm = T), digits = 3),
            Skewness = round(skewness(marginal_p_eur_mwh, na.rm = T), digits = 3),
            Kurtosis = round(kurtosis(marginal_p_eur_mwh, na.rm = T), digits = 3),
            Jaque_Bera = jarque.bera.test(na.omit(marginal_p_eur_mwh))$p.value)
sumry_marg_price_train = sumry_marg_price_train[1,]

hist_marg_price_test = ggplot(train_data, aes(x = marginal_p_eur_mwh))+
  geom_histogram(fill = '#30a771', alpha = 0.9, colour = "#e9ecef")+
  xlab("Marginal Price (EUR/MWH)")+
  labs(title = "Histogram of Test data for Marginal Price (EUR/MWH)")+
  geom_table_npc(data = sumry_marg_price, label = list(sumry_marg_price), npcx = 0.00, npcy = 1, size = 3)+
  theme_bw()

######Plotting 


plot_marg_price_train + plot_marg_price_test

###########################
#TS decomposition plots
###########################

#STL decomposition of Marginal Price
stl_marg = stl(marg_price2, s.window = 'periodic')
summary(stl_marg)
autoplot(stl_marg)
stl_marg

mstl_marg = mstl(marg_price2)

stl_plot = autoplot(stl_marg)+
  labs(title = 'STL Decomposition of Marginal Price (EUR/MWH)')+
  theme_bw()
stl_plot

mstl_plot = autoplot(mstl_marg)+
  labs(title = 'STL Decomposition of Marginal Price (EUR/MWH)')+
  theme_bw()
mstl_plot

#ggsave('stl_plot.png', plot = stl_plot)

#Seasonal plot  

plot_y22 = el_price %>% 
  filter(Hour >= "2022-01-01 00:00:00" & Hour <= "2022-12-31 00:00:00") %>% 
  gg_season(marginal_p_eur_mwh, period = 'year')+
  xlab("Yearly (2022)")+
  ylab("Marginal Price (eur/MWH)")+
  labs(title = "Yearly Seasonality 2022")+
  theme_bw()

plot_m22 = el_price %>% 
  filter(Hour >= "2022-01-01 00:00:00" & Hour <= "2022-12-31 00:00:00") %>% 
  gg_season(marginal_p_eur_mwh, period = 'month')+
  xlab("Monthly (2022)")+
  ylab("Marginal Price (eur/MWH)")+
  labs(title = "Monthly Seasonality 2022")+
  theme_bw()

plot_w22 = el_price %>% 
  filter(Hour >= "2022-01-01 00:00:00" & Hour <= "2022-12-31 00:00:00") %>% 
  gg_season(marginal_p_eur_mwh, period = 'week')+
  xlab("Weekly (2022)")+
  ylab("Marginal Price (eur/MWH)")+
  labs(title = "Weekly Seasonality 2022")+
  theme_bw()

plot_d22 = el_price %>% 
  filter(Hour >= "2022-01-01 00:00:00" & Hour <= "2022-12-31 00:00:00") %>% 
  gg_season(marginal_p_eur_mwh, period = 'day')+
  xlab("Daily (2022)")+
  ylab("Marginal Price (eur/MWH)")+
  labs(title = "Daily Seasonality 2022")+
  theme_bw()

season_22 = (plot_y22 + plot_m22)/(plot_w22 + plot_d22)

season_22

#ggsave('season_22.png', plot = season_22)

plot_y = el_price %>% 
  fill_gaps() %>% 
  #filter(Hour >= "2022-01-01 00:00:00" & Hour <= "2022-12-31 00:00:00") %>% 
  gg_season(marginal_p_eur_mwh, period = 'year')+
  xlab("Yearly")+
  ylab("Marginal Price (eur/MWH)")+
  labs(title = "Yearly Seasonality")+
  theme_bw()




###########################
#TS Autocorrelation plots
###########################

#autocorrelation of marginal price 

?ggAcf
?acf

marg_acf = ggAcf(el_price$marginal_p_eur_mwh, lag.max = 100)+
  labs(title = "Autocorrelation PLot of Maginal Price (EUR/MWH)")+
  theme_bw()
marg_pacf = ggPacf(el_price$marginal_p_eur_mwh, lag.max = 100)+
  labs(title = ' Partial Autocorrelation PLot of Maginal Price (EUR/MWH)')+
  theme_bw()

marg_acf/marg_pacf
#We can see that there is a white noise process from the autocorrelation.

#Autocorrelation of first differences ACF and PACF
diff1_marg_price = diff(el_price$marginal_p_eur_mwh)

dmarg_acf = ggAcf(diff1_marg_price)+
  labs(title = "Autocorrelation PLot of Maginal Price (EUR/MWH)")+
  theme_bw()
dmarg_pacf = ggPacf(diff1_marg_price)+
  labs(title = ' Partial Autocorrelation PLot of Maginal Price (EUR/MWH)')+
  theme_bw()

dmarg_acf/dmarg_pacf
#We can see the cyclicall nature of the daily values of our data from the ACF

max(dmarg_acf$data$Freq)

?ggtsdisplay

ggtsdisplay(diff1_marg_price)

###########################
#Simple moving average
###########################

?SMA

elect_price_d$MA7 = SMA(elect_price_d$marginal_p_eur_mwh, n = 7)
elect_price_d$MA7_l = SMA(elect_price_d$log_marg_price, n = 7)

plot_marg_priced_sma = ggplot(elect_price_d, aes(x = Day))+
  geom_line(aes(y = log_marg_price), color = "grey15", alpha = 0.8)+
  geom_line(aes(y = MA7_l), color = "red", alpha = 0.8)+
  labs(y = "Marginal Price (EUR/MWH)", title  = "Daily Averaged Marginal Price (EUR/MWH)")+
  theme_bw()

plot_marg_priced_sma

###########################
#Short term Results ST
###########################

acc_m_st_lstm 
acc_m_st_arima
acc_m_st_svr

results_rmse_st = ggplot()+
  geom_line(data = acc_m_st_arima, aes(x = Day, y = RMSE, color = "ARIMA"),linetype = "solid", alpha = 0.7)+
  geom_line(data = acc_m_st_svr, aes(x = Day, y = RMSE, color = "SVR"), linetype = "dashed")+
  geom_line(data = acc_m_st_lstm, aes(x = Day, y = RMSE, color = "LSTM"), linetype = "dotted")+
  scale_color_manual(values = c(ARIMA = "blue", SVR = "green", LSTM = "red"),
                     labels = c(ARIMA = "ARIMA", SVR = "SVR", LSTM = "LSTM"))+
                     #limits = c("treat", "control"))
  labs(y = "RMSE", title  = "Root Mean Squared Error for Next Day Forecasts")+
  theme_bw()

results_mae_st = ggplot()+
  geom_line(data = acc_m_st_arima, aes(x = Day, y = MAE, color = "ARIMA"),linetype = "solid", alpha = 0.7)+
  geom_line(data = acc_m_st_svr, aes(x = Day, y = MAE, color = "SVR"), linetype = "dashed")+
  geom_line(data = acc_m_st_lstm, aes(x = Day, y = MAE, color = "LSTM"), linetype = "dotted")+
  scale_color_manual(values = c(ARIMA = "blue", SVR = "green", LSTM = "red"),
                     labels = c(ARIMA = "ARIMA", SVR = "SVR", LSTM = "LSTM"))+
  #limits = c("treat", "control"))
  labs(y = "MAE", title  = "Mean Absolute Error for Next Day Forecasts")+
  theme_bw()

results_smape_st = ggplot()+
  geom_line(data = acc_m_st_arima, aes(x = Day, y = sMAPE, color = "ARIMA"),linetype = "solid", alpha = 0.7)+
  geom_line(data = acc_m_st_svr, aes(x = Day, y = sMAPE, color = "SVR"), linetype = "dashed")+
  geom_line(data = acc_m_st_lstm, aes(x = Day, y = sMAPE, color = "LSTM"), linetype = "dotted")+
  scale_color_manual(values = c(ARIMA = "blue", SVR = "green", LSTM = "red"),
                     labels = c(ARIMA = "ARIMA", SVR = "SVR", LSTM = "LSTM"))+
  #limits = c("treat", "control"))
  labs(y = "sMAPE", title  = "Symetric Mean Absolute Error for Next Day Forecasts")+
  theme_bw()

results_rmse_st

results_mae_st

results_smape_st

results_rmse_st / results_smape_st

###########################
#Short term Results LT Log Trans
###########################

acc_m_st_lstm 
acc_m_st_arima
acc_m_st_svr

results_rmse_st = ggplot()+
  geom_line(data = acc_m_st_arima, aes(x = Day, y = RMSE, color = "ARIMA"),linetype = "solid", alpha = 0.5)+
  geom_line(data = acc_m_st_svr, aes(x = Day, y = RMSE, color = "SVR"), linetype = "dashed")+
  geom_line(data = acc_m_st_lstm, aes(x = Day, y = RMSE, color = "LSTM"), linetype = "dotted")+
  scale_color_manual(values = c(ARIMA = "blue", SVR = "green", LSTM = "red"),
                     labels = c(ARIMA = "ARIMA", SVR = "SVR", LSTM = "LSTM"))+
  #limits = c("treat", "control"))
  labs(y = "RMSE", title  = "Root Mean Squared Error for Next Day Forecasts")+
  theme_bw()

results_mae_st = ggplot()+
  geom_line(data = acc_m_st_arima, aes(x = Day, y = MAE, color = "ARIMA"),linetype = "solid", alpha = 0.5)+
  geom_line(data = acc_m_st_svr, aes(x = Day, y = MAE, color = "SVR"), linetype = "dashed")+
  geom_line(data = acc_m_st_lstm, aes(x = Day, y = MAE, color = "LSTM"), linetype = "dotted")+
  scale_color_manual(values = c(ARIMA = "blue", SVR = "green", LSTM = "red"),
                     labels = c(ARIMA = "ARIMA", SVR = "SVR", LSTM = "LSTM"))+
  #limits = c("treat", "control"))
  labs(y = "MAE", title  = "Mean Absolute Error for Next Day Forecasts")+
  theme_bw()

results_smape_st = ggplot()+
  geom_line(data = acc_m_st_arima, aes(x = Day, y = sMAPE, color = "ARIMA"),linetype = "solid", alpha = 0.5)+
  geom_line(data = acc_m_st_svr, aes(x = Day, y = sMAPE, color = "SVR"), linetype = "dashed")+
  geom_line(data = acc_m_st_lstm, aes(x = Day, y = sMAPE, color = "LSTM"), linetype = "dotted")+
  scale_color_manual(values = c(ARIMA = "blue", SVR = "green", LSTM = "red"),
                     labels = c(ARIMA = "ARIMA", SVR = "SVR", LSTM = "LSTM"))+
  #limits = c("treat", "control"))
  labs(y = "sMAPE", title  = "Symetric Mean Absolute Error for Next Day Forecasts")+
  theme_bw()

results_rmse_st

results_mae_st

results_smape_st

results_rmse_st /  results_smape_st

###########################
#Short term Results LT
###########################

results_rmse_st = ggplot()+
  geom_line(data = acc_m_st_arima_c, aes(x = Day, y = RMSE, color = "ARIMA"),linetype = "solid", alpha = 0.5)+
  geom_line(data = acc_m_st_svr_c, aes(x = Day, y = RMSE, color = "SVR"), linetype = "dashed")+
  geom_line(data = acc_m_st_lstm_c, aes(x = Day, y = RMSE, color = "LSTM"), linetype = "dotted")+
  scale_color_manual(values = c(ARIMA = "blue", SVR = "green", LSTM = "red"),
                     labels = c(ARIMA = "ARIMA", SVR = "SVR", LSTM = "LSTM"))+
  #limits = c("treat", "control"))
  labs(y = "RMSE", title  = "Root Mean Squared Error for Next Day Forecasts")+
  theme_bw()

# results_mae_st = ggplot()+
#   geom_line(data = acc_m_st_arima, aes(x = Day, y = MAE, color = "ARIMA"),linetype = "solid", alpha = 0.5)+
#   geom_line(data = acc_m_st_svr, aes(x = Day, y = MAE, color = "SVR"), linetype = "dashed")+
#   geom_line(data = acc_m_st_lstm, aes(x = Day, y = MAE, color = "LSTM"), linetype = "dotted")+
#   scale_color_manual(values = c(ARIMA = "blue", SVR = "green", LSTM = "red"),
#                      labels = c(ARIMA = "ARIMA", SVR = "SVR", LSTM = "LSTM"))+
#   #limits = c("treat", "control"))
#   labs(y = "MAE", title  = "Mean Absolute Error for Next Day Forecasts")+
#   theme_bw()

results_smape_st = ggplot()+
  geom_line(data = acc_m_st_arima_c, aes(x = Day, y = sMAPE, color = "ARIMA"),linetype = "solid", alpha = 0.5)+
  geom_line(data = acc_m_st_svr_c, aes(x = Day, y = sMAPE, color = "SVR"), linetype = "dashed")+
  geom_line(data = acc_m_st_lstm_c, aes(x = Day, y = sMAPE, color = "LSTM"), linetype = "dotted")+
  scale_color_manual(values = c(ARIMA = "blue", SVR = "green", LSTM = "red"),
                     labels = c(ARIMA = "ARIMA", SVR = "SVR", LSTM = "LSTM"))+
  #limits = c("treat", "control"))
  labs(y = "sMAPE", title  = "Symetric Mean Absolute Error for Next Day Forecasts")+
  theme_bw()

results_rmse_st

results_smape_st

results_rmse_st /  results_smape_st


###########################
#Long term Results LT Log trans
###########################

acc_m_lt_lstm2
acc_m_lt_arima2
acc_m_lt_svr2

acc_m_lt_lstm2_c
acc_m_lt_arima_c
acc_m_lt_svr_c

results_rmse_lt = ggplot()+
  geom_line(data = acc_m_lt_arima2, aes(x = Day, y = RMSE, color = "ARIMA_l"),linetype = "solid", alpha = 0.5)+
  geom_point(data = acc_m_lt_arima2, aes(x = Day, y = RMSE, color = "ARIMA_l"), alpha = 0.5)+
  geom_line(data = acc_m_lt_arima_c, aes(x = Day, y = RMSE, color = "ARIMA"),linetype = "solid", alpha = 0.5)+
  geom_point(data = acc_m_lt_arima_c, aes(x = Day, y = RMSE, color = "ARIMA"), alpha = 0.5)+
  geom_line(data = acc_m_lt_svr2, aes(x = Day, y = RMSE, color = "SVR_l"), linetype = "dashed")+
  geom_point(data = acc_m_lt_svr2, aes(x = Day, y = RMSE, color = "SVR_l"))+
  geom_line(data = acc_m_lt_svr_c, aes(x = Day, y = RMSE, color = "SVR"), linetype = "dashed")+
  geom_point(data = acc_m_lt_svr_c, aes(x = Day, y = RMSE, color = "SVR"))+
  geom_line(data = acc_m_lt_lstm2, aes(x = Day, y = RMSE, color = "LSTM_l"), linetype = "dotdash")+
  geom_point(data = acc_m_lt_lstm2, aes(x = Day, y = RMSE, color = "LSTM_l"))+
  geom_line(data = acc_m_lt_lstm_c, aes(x = Day, y = RMSE, color = "LSTM"), linetype = "dotdash")+
  geom_point(data = acc_m_lt_lstm_c, aes(x = Day, y = RMSE, color = "LSTM"))+
  scale_color_manual(values = c(ARIMA_l = "blue", ARIMA = "cadetblue1", SVR_l = "green", SVR = "darkgreen", LSTM_l = "red", LSTM = "coral"),
                     labels = c(ARIMA_l = "ARIMA (log)",ARIMA = "ARIMA",SVR_l = "SVR (log)", SVR = "SVR",LSTM_l = "LSTM (log)", LSTM = "LSTM"))+
  #limits = c("treat", "control"))
  labs(y = "RMSE", title  = "Root Mean Squared Error for Month Multi Step Forecast")+
  theme_bw()

# results_mae_lt = ggplot()+
#   geom_line(data = acc_m_lt_arima, aes(x = Day, y = MAE, color = "ARIMA"),linetype = "solid", alpha = 0.5)+
#   geom_line(data = acc_m_lt_svr, aes(x = Day, y = MAE, color = "SVR"), linetype = "dashed")+
#   geom_line(data = acc_m_lt_lstm, aes(x = Day, y = MAE, color = "LSTM"), linetype = "dotted")+
#   scale_color_manual(values = c(ARIMA = "blue", SVR = "green", LSTM = "red"),
#                      labels = c(ARIMA = "ARIMA", SVR = "SVR", LSTM = "LSTM"))+
#   #limits = c("treat", "control"))
#   labs(y = "MAE", title  = "Mean Absolute Error for Month Multi Step Forecast")+
#   theme_bw()

results_smape_lt = ggplot()+
  geom_line(data = acc_m_lt_arima2, aes(x = Day, y = sMAPE, color = "ARIMA_l"),linetype = "solid", alpha = 0.5)+
  geom_point(data = acc_m_lt_arima2, aes(x = Day, y = sMAPE, color = "ARIMA_l"), alpha = 0.5)+
  geom_line(data = acc_m_lt_arima_c, aes(x = Day, y = sMAPE, color = "ARIMA"),linetype = "solid", alpha = 0.5)+
  geom_point(data = acc_m_lt_arima_c, aes(x = Day, y = sMAPE, color = "ARIMA"), alpha = 0.5)+
  geom_line(data = acc_m_lt_svr2, aes(x = Day, y = sMAPE, color = "SVR_l"), linetype = "dashed")+
  geom_point(data = acc_m_lt_svr2, aes(x = Day, y = sMAPE, color = "SVR_l"))+
  geom_line(data = acc_m_lt_svr_c, aes(x = Day, y = sMAPE, color = "SVR"), linetype = "dashed")+
  geom_point(data = acc_m_lt_svr_c, aes(x = Day, y = sMAPE, color = "SVR"))+
  geom_line(data = acc_m_lt_lstm2, aes(x = Day, y = sMAPE, color = "LSTM_l"), linetype = "dotdash")+
  geom_point(data = acc_m_lt_lstm2, aes(x = Day, y = sMAPE, color = "LSTM_l"))+
  geom_line(data = acc_m_lt_lstm_c, aes(x = Day, y = sMAPE, color = "LSTM"), linetype = "dotdash")+
  geom_point(data = acc_m_lt_lstm_c, aes(x = Day, y = sMAPE, color = "LSTM"))+
  scale_color_manual(values = c(ARIMA_l = "blue", ARIMA = "cadetblue1", SVR_l = "green", SVR = "darkgreen", LSTM_l = "red", LSTM = "coral"),
                     labels = c(ARIMA_l = "ARIMA (log)",ARIMA = "ARIMA",SVR_l = "SVR (log)", SVR = "SVR",LSTM_l = "LSTM (log)", LSTM = "LSTM"))+
  #limits = c("treat", "control"))
  labs(y = "sMAPE", title  = "scaled Mean Absolute Percentage Error for Month Multi Step Forecast")+
  theme_bw()

results_rmse_lt

results_mae_lt

results_smape_lt

results_rmse_lt / results_smape_lt

###########################
#Long term Results LT
###########################

acc_m_st_arima_c
acc_m_lt_arima
acc_m_lt_svr

results_rmse_lt2 = ggplot()+
  geom_line(data = acc_m_st_arima_c, aes(x = Day, y = RMSE, color = "ARIMA"),linetype = "solid", alpha = 0.5)+
  geom_point(data = acc_m_lt_arima, aes(x = Day, y = RMSE, color = "ARIMA"), alpha = 0.5)+
  geom_line(data = acc_m_lt_svr, aes(x = Day, y = RMSE, color = "SVR"), linetype = "dashed")+
  geom_point(data = acc_m_lt_svr, aes(x = Day, y = RMSE, color = "SVR"))+
  geom_line(data = acc_m_lt_lstm, aes(x = Day, y = RMSE, color = "LSTM"), linetype = "dotted")+
  geom_point(data = acc_m_lt_lstm, aes(x = Day, y = RMSE, color = "LSTM"))+
  scale_color_manual(values = c(ARIMA = "blue", SVR = "green", LSTM = "red"),
                     labels = c(ARIMA = "ARIMA", SVR = "SVR", LSTM = "LSTM"))+
  #limits = c("treat", "control"))
  labs(y = "RMSE", title  = "Root Mean Squared Error for Month Multi Step Forecast")+
  theme_bw()

# results_mae_lt = ggplot()+
#   geom_line(data = acc_m_lt_arima, aes(x = Day, y = MAE, color = "ARIMA"),linetype = "solid", alpha = 0.5)+
#   geom_line(data = acc_m_lt_svr, aes(x = Day, y = MAE, color = "SVR"), linetype = "dashed")+
#   geom_line(data = acc_m_lt_lstm, aes(x = Day, y = MAE, color = "LSTM"), linetype = "dotted")+
#   scale_color_manual(values = c(ARIMA = "blue", SVR = "green", LSTM = "red"),
#                      labels = c(ARIMA = "ARIMA", SVR = "SVR", LSTM = "LSTM"))+
#   #limits = c("treat", "control"))
#   labs(y = "MAE", title  = "Mean Absolute Error for Month Multi Step Forecast")+
#   theme_bw()

results_smape_lt = ggplot()+
  geom_line(data = acc_m_lt_arima, aes(x = Day, y = sMAPE, color = "ARIMA"),linetype = "solid", alpha = 0.5)+
  geom_point(data = acc_m_lt_arima, aes(x = Day, y = sMAPE, color = "ARIMA"), alpha = 0.5)+
  geom_line(data = acc_m_lt_svr, aes(x = Day, y = sMAPE, color = "SVR"), linetype = "dashed")+
  geom_point(data = acc_m_lt_svr, aes(x = Day, y = sMAPE, color = "SVR"))+
  geom_line(data = acc_m_lt_lstm, aes(x = Day, y = sMAPE, color = "LSTM"), linetype = "dotted")+
  geom_point(data = acc_m_lt_lstm, aes(x = Day, y = sMAPE, color = "LSTM"))+
  scale_color_manual(values = c(ARIMA = "blue", SVR = "green", LSTM = "red"),
                     labels = c(ARIMA = "ARIMA", SVR = "SVR", LSTM = "LSTM"))+
  #limits = c("treat", "control"))
  xlab("Months")+
  labs(y = "sMAPE", title  = "Symetric Mean Absolute Error for Month Multi Step Forecast")+
  theme_bw()

results_rmse_lt

results_mae_lt

results_smape_lt

results_rmse_lt / results_smape_lt


