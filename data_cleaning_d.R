###########################
#Thesis: Data Cleaning Daily
###########################


###########################
#Packages
###########################
#source('packages.R')

year = c(2018:2022)

###########################
#Reading in Day Ahead Market Data
###########################


elect_price_d = list()

name_dam = function(x){
  paste0("Annual_market_report_", x, "_V0.xls")
}

#loop over and load data in with if statement for leap year
for(i in 1:length(year)){
  if(year[i] == 2020){
    elect_price_d = rbind(elect_price_d, read_xls(name_dam(year[i]), sheet = "DAM", range= "M6:N372"))
  }
  else{
    elect_price_d = rbind(elect_price_d, read_xls(name_dam(year[i]), sheet = "DAM", range= "M6:N371"))
  }
}

elect_price_d = elect_price_d %>% 
  rename(
    marginal_p_eur_mwh = `Average marginal price (EUR/MWh)`
  )

elect_price_d$lag_marg_price_eur_mwh = lag(elect_price_d$marginal_p_eur_mwh)
elect_price_d$diff_marg_price = c(NA,diff(elect_price_d$marginal_p_eur_mwh))
elect_price_d$diff7_marg_price = c(rep(NA, 7),diff(elect_price_d$marginal_p_eur_mwh, lag = 7))

#elect_price_d$bc_price = BoxCox(elect_price_d$marginal_p_eur_mwh)

range(elect_price_d$Day)
sum(is.na(elect_price_d))
x = which(is.na(elect_price_d))
elect_price_d[x,]


#POSIXct time index for the data 

time_seq_d = seq(as.POSIXct("2018-01-01 00:00:00"), as.POSIXct("2022-12-31 23:00:00"), by="day")

#xts object for the data
marg_priced = xts(elect_price_d$marginal_p_eur_mwh, order.by = time_seq_d, frequency = 7)

#ts object for the marginal price per mwh
marg_priced2 = ts(elect_price_d$marginal_p_eur_mwh,start = c(2018,1,1), frequency = 7)
#marg_price2 = ts(elect_price$marginal_p_eur_mwh,start = as.POSIXlt("2018-01-01 00:00:00"), frequency = 24)

ndays(marg_priced)
nyears(marg_priced)
nmonths(marg_priced)

#turn elect_price object from tibble to xts time series table
#elec_pd = as.xts(elect_price_d, order.by = elect_price_d$Day)
?as_tsibble
#el_priced = as_tsibble(elect_price_d, index = index(elect_price_d$Day)) 
#fill_gaps()


#ndays(elec_p)
#nyears(elec_p)
#nmonths(elec_p)

