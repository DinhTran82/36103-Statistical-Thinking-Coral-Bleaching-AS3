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


library(forecast)
library(urca)
library(xts)
library(zoo)
library(stats)
library(tseries)
library(astsa)






coral <- read.csv("ReefCheck.csv")


coral$Latitude_coarse <- round(coral$Latitude.Degrees,digits=2)
coral$Longitude_coarse <- round(coral$Longitude.Degrees,digits=2)


coral$Date <- as.Date(coral$Date,"%d-%b-%y")
coral$month <- floor_date(coral$Date, "month")
head(coral$month)



names(coral)
coral$ClimSST_Celcius <- coral$ClimSST-273.15
coral$SST <- coral$Temperature_Kelvin-273.15
coral$SST_Max <- coral$Temperature_Maximum-273.15
coral$SST_Min <- coral$Temperature_Minimum-273.15
coral$SST_Min <- coral$Temperature_Minimum-273.15
coral$SST_Mean <- coral$Temperature_Mean -273.15
coral$SST_stde  <- coral$Temperature_Kelvin_Standard_Deviation
coral <-coral %>%filter(ClimSST_Celcius >0)




Atlantic_data <- coral %>% filter(Ocean=='Atlantic') %>% filter(Latitude.Degrees >0)%>% 
  filter(Longitude.Degrees < 0) %>% filter(Year>2002)

nrow(Atlantic_data)

length(unique(Atlantic_data$Reef.ID))

Atlantic_data <-  Atlantic_data %>% mutate(Bleaching= ifelse(Average_bleaching< 1,0,1))

Atlantic_data <-  Atlantic_data %>% mutate(Bleaching_level= ifelse(Average_bleaching< 1,0,
                                                               ifelse(Average_bleaching<10,1,
                                                                      ifelse(Average_bleaching<50,2, 3))))


ggplot(data=Atlantic_data, aes(x=as.factor(Bleaching_level), y=SSTA, colour=as.factor(Bleaching_level))) + geom_boxplot()+
  geom_jitter(alpha = .5)+xlab("Bleaching level")+ylab("Sea Surface Temprature Anomally")+theme(legend.position = "top")+
  scale_colour_manual(values=c("turquoise3", "blue", "sienna3", "red"), name="Bleaching level", breaks=c(0,1,2,3), labels=c("None", "Low","Moderate", "High"))



bleaching <- Atlantic_data %>% filter(Average_bleaching >=1 ) 
Bleaching_threshold_SST <- mean(bleaching$SST)

Bleaching_threshold_SST

bleaching_rate <- sum(Atlantic_data$Bleaching==1)/length(Atlantic_data$Bleaching)
cat("Fraction of observations that had bleching = ", format(bleaching_rate, digits=3))


data_above_T <- Atlantic_data %>% filter(SST >= Bleaching_threshold_SST) 

new_rate <- sum(data_above_T$Bleaching==1)/length(data_above_T$Bleaching)
cat("Among sites with  degrees higher than the threshold Fraction of observations that had bleching = ", format(new_rate, digits=3))






train <- sample_frac(Atlantic_data,0.75)
test <- anti_join(Atlantic_data, train)
tab1 <- table(Atlantic_data$Bleaching)
tab2  <- table(train$Bleaching)

#######checking if the train_set is not too bias
tab1[2]/(tab1[1]+tab1[2])
tab2[2]/(tab2[1]+tab2[2])



regmodel1 <- glm(Bleaching~ClimSST_Celcius+SST+SSTA+SSTA_DHW+Depth+Latitude_coarse, family=binomial, data=train)
summary(regmodel1)
confint(regmodel1)
anova(regmodel1)


regmodel1 <- glm(Bleaching~ClimSST_Celcius+SST+SSTA+SSTA_DHW, family=binomial, data=train)
summary(regmodel1)
confint(regmodel1)
anova(regmodel1)

y <- train$Bleaching -trunc(2*regmodel1$fitted)
hits <- sum(y==0)
hitratio <- hits/length(y)
hitratio



probability <-predict(regmodel1, newdata=test, type="response")
head(probability)

prediction <- ifelse(probability > 0.5, 1, 0) 

confusion  <- table(test$Bleaching, prediction) 
confusion 


 Atlantic_dt <- Atlantic_data %>% group_by(month, Year)%>% 
   summarise_at(vars(Average_bleaching,SST,SST_Max, ClimSST_Celcius,
                     SSTA,TSA, SSTA_DHW,TSA_Maximum,SSTA_Maximum,SSTA_Mean,TSA_Mean),mean)

 nrow(Atlantic_dt)
 
head(Atlantic_dt$month)
 
 temp <- data.frame(seq.Date(min(Atlantic_dt$month), max(Atlantic_dt$month), "month"))
 colnames(temp) <- c("month")

 #Adding missing months with NA values
 Atlantic_dt1 <- left_join(temp,Atlantic_dt,all=TRUE)
nrow(Atlantic_dt1)

SST_plot <- ggplot(Atlantic_dt, aes(x =month, y = SST)) +
    geom_line() +
    scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
    theme_classic()
SST_plot


# one way of define the SST time series
#SST_ts <-xts(Atlantic_dt1$SST, Atlantic$month)
#

# Define a STT time series (the data has been sorted by dates already)
SST_ts <-ts(Atlantic_dt1$SST, start=c(2003,2),end=c(2017,11),frequency=12)



SST_ts <-na.interp(SST_ts)
autoplot(SST_ts)
stl_SST <-stl(SST_ts,s.window = "period" )

autoplot(stl_SST)



SST_ts %>% ur.kpss() %>% summary()



SST_ts  %>% ggtsdisplay(main="")


diff(SST_ts,12) %>% ggtsdisplay(main="")


season_SST <- diff(SST_ts,12)


season_SST %>% ur.kpss() %>% summary()

diff(season_SST) %>%ggtsdisplay(main="")

fit = auto.arima(SST_ts)
fit


checkresiduals(fit)
diff(SST_ts,12) %>% ggtsdisplay(main="")
# 
guess <-arima(SST_ts, order=c(3,0,1), seasonal=list(order=c(1,1,1),period=12))

guess2 <-arima(SST_ts, order=c(3,0,1), seasonal=list(order=c(2,1,1),period=12))
guess3 <-arima(SST_ts, order=c(3,0,1), seasonal=list(order=c(3,1,1),period=12))

checkresiduals(guess)
checkresiduals(guess2)
checkresiduals(guess3)
summary(guess)
summary(guess2)
summary(guess3)


fitforecast = forecast(fit)
guessforecast =forecast(guess)


plot(fitforecast)
plot(guessforecast)


training = window(SST_ts, end = c(2014,12))
test = window(SST_ts, start = c(2015,1))

auto_fit = auto.arima(training)
test.auto = forecast(auto_fit, h =36)



guess_old <-arima(training, order=c(3,0,1), seasonal=list(order=c(1,1,1),period=12))
test.old =forecast(guess_old, h=36)



test.average = meanf(training, h = 36)   # Average method ("forecast" package)
test.naive = rwf(training, h = 36)   # Naive method ("forecast" package)
test.drift = rwf(training, drift = TRUE, h = 36)   # Drift method ("forecast" package)
result = rbind(accuracy(test.auto, test)[2, c(2, 3, 5, 6)],
               accuracy(test.old, test)[2, c(2, 3, 5, 6)],
               accuracy(test.average, test)[2, c(2, 3, 5, 6)],
               accuracy(test.naive, test)[2, c(2, 3, 5, 6)],
               accuracy(test.drift, test)[2, c(2, 3, 5, 6)])
rownames(result) = c("Auto fit", "Old_model", "Average", "Naive", "Drift")
result

autoplot(SST_ts) + 
  autolayer(test.auto, PI = FALSE, series = "Auto fit") + 
  autolayer(test.old, PI = FALSE, series = "Old model") +
  autolayer(test.average, PI = FALSE, series = "Average") + 
  autolayer(test.naive, PI = FALSE, series = "Naive") + 
  autolayer(test.drift, PI = FALSE, series = "Drift") +
  ggtitle("Forecasts for SST series with arima(1,0,0)(0,1,1)[12]") +
  guides(colour = guide_legend(title = "Forecast")) +
  theme_bw()


autoplot(SST_ts) + 
  autolayer(test.auto, PI = FALSE, series = "Auto fit") + 
  autolayer(test.old, PI = FALSE, series = "Old model") +
  ggtitle("Forecasts for two SARIMA models") +
  guides(colour = guide_legend(title = "Forecast")) +
  theme_bw()



newts <- window(SST_ts, start = c(2013,1))

autoplot(newts) + 
  autolayer(test.auto, PI = FALSE, series = "Auto fit") + 
  autolayer(test.old, PI = FALSE, series = "Old model") +
  ggtitle("Forecasts for two SARIMA models") +
  guides(colour = guide_legend(title = "Forecast")) 

checkresiduals(guess_old)

checkresiduals(auto_fit)

summary(auto_fit)



# 
