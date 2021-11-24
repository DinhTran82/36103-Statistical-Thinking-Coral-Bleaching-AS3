library(tidyverse)
library(gridExtra)
library(dplyr)
library(MASS)
library(ggmap)
library(viridis)
library(plotly)
library(lubridate)
library(spdep)
library(sf)
library(sp)
library(spatialreg)







coral <- read.csv("ReefCheck.csv")
head(coral)
names(coral)
#coral$Average_bleaching[coral$Average_bleaching < 1] <-0
#View(coral)

coral <-  coral %>% mutate(Bleaching_level= ifelse(Average_bleaching< 5,0,
                                                   ifelse(Average_bleaching<10,1,
                                                          ifelse(Average_bleaching<50,2, 3))))


coral$Latitude_coarse <- round(coral$Latitude.Degrees,digits=1)
coral$Longitude_coarse <- round(coral$Longitude.Degrees,digits=1)

length(unique(coral$Reef.ID))
coral$Date <- as.Date(coral$Date,"%d-%b-%y")
coral$Month <- floor_date(coral$Date, "month")

head(coral$Month)
class(coral$Month)


#coral$datenew <- as.Date(format(as.Date(coral$Date,"%d-%b-%y"),"01-%m-%Y"),"%d-%m-%Y")
#head(coral$datenew)                     
#class(coral$datenew)

names(coral)
coral$ClimSST_Celcius <- coral$ClimSST-273.15
coral$SST <- coral$Temperature_Kelvin-273.15
coral$SST_Max <- coral$Temperature_Maximum-273.15
coral$SST_Min <- coral$Temperature_Minimum-273.15
coral$SST_Min <- coral$Temperature_Minimum-273.15
coral$SST_Mean <- coral$Temperature_Mean -273.15
coral$SST_stde  <- coral$Temperature_Kelvin_Standard_Deviation
coral <-coral %>%filter(ClimSST_Celcius >0)

nrow(coral)


# Atlantic_data <- coral %>% filter(Ocean=='Atlantic') %>% filter(Latitude.Degrees >= 10 & Latitude.Degrees <= 20)%>% filter(Longitude.Degrees < 0) %>% filter(Year>2002)
# nrow(Atlantic_data)


Atlantic_data <- coral %>% filter(Ocean=='Pacific')  %>% filter(Year>2002)

Atlantic_data$month<- format(Atlantic_data$Date, "%m")

ggplot(data=Atlantic_data)+aes(x=month, y=Average_bleaching)+geom_point()+facet_wrap(~Year, ncol=3)

#View(Atlantic_data)
max(Atlantic_data$Longitude.Degrees)
min(Atlantic_data$Longitude.Degrees)

max(Atlantic_data$Latitude.Degrees)
min(Atlantic_data$Latitude.Degrees)

Atlantic_sf <- st_as_sf(Atlantic_data,coords=c("Longitude.Degrees", "Latitude.Degrees"), crs="+proj=longlat +datum=WGS84 +no_defs")


library(mapview)
mapview(Atlantic_sf)
mapview(Atlantic_sf, zcol="Average_bleaching", cex="Average_bleaching", legend=TRUE, layer.name="bleaching")

#mapview(my_AUS_sf, color = "cyan", col.regions = "white", lwd = 3)




#Atlantic_data$Date <- as.Date(Atlantic_data$Date, "%d-%b-%y")

#Atlantic_data$date <- format(Atlantic_data$Date, "%m-%Y")
#head(Atlantic_data$date)
#Atlantic_data$date <- lubridate::parse_date_time(Atlantic_data$date, orders = c("m/Y"))

#month
#Atlantic_data$date
#class(Atlantic_data$date)

#Atlantic_data$date <-as.Date(Atlantic_data$date,"%b-%Y")
#head(Atlantic_data$date)

# Atlantic_dt <- Atlantic_data %>% group_by(Latitude_coarse, Longitude_coarse, Date_format)%>% summarise_at(vars(Average_bleaching,SST,ClimSST_Celcius, SSTA,TSA, Year), list(new=mean))


 Atlantic_dt <- Atlantic_data %>% group_by(quarter, Year )%>% summarise_at(vars(Average_bleaching,SST,SST_Max, ClimSST_Celcius, SSTA,TSA, SSTA_DHW,TSA_Maximum,SSTA_Maximum,SSTA_Mean,TSA_Mean),mean)


#View(Atlantic_dt)


max(Atlantic_data$quarter)


library(forecast)
library(urca)
library(xts)
library(zoo)
library(stats)
 library(tseries)

#View(Atlantic_dt)


bleaching_plot <- ggplot(Atlantic_dt, aes(x = quarter, y = Average_bleaching)) +
    geom_line() +
    scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
    theme_classic()
#bleaching_plot

ClimSST_plot <- ggplot(Atlantic_dt, aes(x = quarter, y = ClimSST_Celcius)) +
  geom_line() +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  theme_classic()
#ClimSST_plot

bleaching_ts <-ts(Atlantic_dt$Average_bleaching, start = 2003, end = 2017, freq = 12)
stl_bleaching <-stl(bleaching_ts,s.window = "period" )

plot(stl_bleaching)


bleaching_ts %>% ur.kpss() %>% summary()
bleaching_ts %>% diff() %>% ur.kpss() %>% summary()

bleaching_ts %>%diff() %>% ggtsdisplay(main="")

bleaching_model <- window(x = bleaching_ts, start = c(2003), end = c(2013))
bleaching_test <- window(x = bleaching_ts, start = c(2014))

adf.test(diff(bleaching_ts))

ndiffs(bleaching_ts)

diffTS <-diff(bleaching_ts, differences=1)
plot(diffTS)

diffTS %>% ur.kpss() %>% summary()

Acf(diffTS)

adf.test(diffTS)


pacf(diffTS)

ggAcf(bleaching_ts)

ggPacf(bleaching_ts)


arima210 <-Arima(bleaching_ts, order=c(2,1,0))
arima210


auto.arima(bleaching_ts)


fit2 <- auto.arima(bleaching_ts, seasonal=FALSE,
                    stepwise=FALSE, approximation=FALSE)
fit2


bleaching_ts %>% ggtsdisplay(main="")
#decaying or sinusoidal in ACF but significant spike at lag 1 and 2 and non beyond lag 2 so we choose ARIMA(2,0,0)
#which is matching with fit 2

fit2
arima311 <-Arima(bleaching_ts, order=c(3,1,1))
arima311
fit2

bleaching_ts %>%diff() %>% ggtsdisplay(main="")
#decaying or sinusoidal in ACF but significant spike at lag 1 and 2,3, 4 and non beyond lag 4 so we choose ARIMA(2,0,0)
#which is matching with fit 2

#suggest to use arima(4,1,0)
arima410 <-Arima(bleaching_ts, order=c(4,1,0))
arima410
bleaching_ts %>%diff()%>% ggtsdisplay(main="")
fit2

ar2forecast = forecast(fit2)
summary(ar2forecast)

plot(ar2forecast)


training = window(bleaching_ts, end = c(2015,12))
test = window(bleaching_ts, start = c(2016,1))
#fit = auto.arima(training)

fit=arima(training, order=c(2,0,0))
test.arima = forecast(fit, h = 24) 
accuracy(test.arima, test)
test.arima

test.average = meanf(training, h = 24)   # Average method ("forecast" package)
test.naive = rwf(training, h = 24)   # Naive method ("forecast" package)
test.drift = rwf(training, drift = TRUE, h = 24)   # Drift method ("forecast" package)
result = rbind(accuracy(test.arima, test)[2, c(2, 3, 5, 6)],
               accuracy(test.average, test)[2, c(2, 3, 5, 6)],
               accuracy(test.naive, test)[2, c(2, 3, 5, 6)],
               accuracy(test.drift, test)[2, c(2, 3, 5, 6)])
rownames(result) = c("ARIMA", "Average", "Naive", "Drift")
result


bleaching1<-window(bleaching_ts, start = c(2011,1))
autoplot(bleaching1) + 
  autolayer(test.arima, PI = FALSE, series = "ARIMA") + 
  autolayer(test.average, PI = FALSE, series = "Average") + 
  autolayer(test.naive, PI = FALSE, series = "Naive") + 
  autolayer(test.drift, PI = FALSE, series = "Drift") + 
  ggtitle("Forecasts for an AR(2) series") +
  guides(colour = guide_legend(title = "Forecast")) +
  theme_bw()
fit

# auto_ets <-ets(training)
# #ets_mmm <- ets(training, model = "MMM")
# ets_zzz<- ets(training, model = "ZZZ")
# #ets_mmm_damped <- ets(training, model = "MMM", damped = TRUE)
# 
# ets_fc <- forecast(auto_ets, h = 12)  # `h = 60` means that the forecast will be 60 time periods long, in our case a time period is one month
# 
# zzz_fc <- forecast(ets_zzz, h = 12)
# 
# 
# autoplot(bleaching1) + autolayer(ets_fc)+autolayer(zzz_fc)
# ets_fc
