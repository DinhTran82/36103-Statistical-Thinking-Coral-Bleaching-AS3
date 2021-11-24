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
library(spatialreg)


oz_states <- ozmaps::ozmap_states
ggplot(oz_states) + 
  geom_sf() + 
  coord_sf()






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
head(coral$Date)
class(coral$Date)


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


# Pacific_data <- coral %>% filter(Ocean=='Pacific') %>% filter(Latitude.Degrees >= 10 & Latitude.Degrees <= 20)%>% filter(Longitude.Degrees < 0) %>% filter(Year>2002)
# nrow(Pacific_data)


Pacific_data <- coral %>% filter(Ocean=='Pacific') %>%filter(Year>2002)

#View(Pacific_data)
max(Pacific_data$Longitude.Degrees)
min(Pacific_data$Longitude.Degrees)

max(Pacific_data$Latitude.Degrees)
min(Pacific_data$Latitude.Degrees)

Pacific_sf <- st_as_sf(Pacific_data,coords=c("Longitude.Degrees", "Latitude.Degrees"), crs="+proj=longlat +datum=WGS84 +no_defs")


library(mapview)
mapview(Pacific_sf)
mapview(Pacific_sf, zcol="Average_bleaching", cex="Average_bleaching", legend=TRUE, layer.name="bleaching")

#mapview(my_AUS_sf, color = "cyan", col.regions = "white", lwd = 3)




Pacific_data$Date <-as.Date(Pacific_data$Date, "%d-%b-%y")
Pacific_data$Date_format <- format(Pacific_data$Date, "%b-%y")

Pacific_data$Date_format
# Pacific_dt <- Pacific_data %>% group_by(Latitude_coarse, Longitude_coarse, Date_format)%>% summarise_at(vars(Average_bleaching,SST,ClimSST_Celcius, SSTA,TSA, Year), list(new=mean))


 Pacific_dt <- Pacific_data %>% group_by(Latitude_coarse, Longitude_coarse, Date_format)%>% summarise_at(vars(Average_bleaching,SST,ClimSST_Celcius, SSTA,TSA, SSTA_DHW, Year),mean)


# Pacific_dt <- Pacific_data %>% group_by(Latitude_coarse, Longitude_coarse, Year)%>% summarise_at(vars(Average_bleaching,SST,ClimSST_Celcius, SSTA,TSA, SSTA_DHW),mean)

PA<- st_as_sf(Pacific_dt,coords=c("Longitude_coarse", "Latitude_coarse"), crs="+proj=longlat +datum=WGS84 +no_defs")

mapview(PA, zcol="Average_bleaching",cex="Average_bleaching", legend=TRUE, layer.name="bleaching")
nrow(Pacific_dt)


#Pacific_data$Average_bleaching[Pacific_data$Average_bleaching < 1] <-0



ggplot(data=Pacific_dt)+aes(x=ClimSST_Celcius, y=Average_bleaching)+geom_point()+facet_wrap(~Year)

ggplot(data=Pacific_dt)+aes(x=SST, y=Average_bleaching)+geom_point()

PA_2016 <-Pacific_dt %>%filter(Year==2016)

#View(PA_2016)


PA_2015 <-Pacific_dt %>%filter(Year==2015)
PA_1516 <-Pacific_dt %>%filter(Year==2015|Year==2016)

PA_16<- st_as_sf(PA_2016,coords=c("Longitude_coarse", "Latitude_coarse"), crs="+proj=longlat +datum=WGS84 +no_defs")


PA_15<- st_as_sf(PA_2015,coords=c("Longitude_coarse", "Latitude_coarse"), crs="+proj=longlat +datum=WGS84 +no_defs")

PA1516<- st_as_sf(PA_1516,coords=c("Longitude_coarse", "Latitude_coarse"), crs="+proj=longlat +datum=WGS84 +no_defs")

mapview(PA_16, zcol="Average_bleaching",cex="Average_bleaching", legend=TRUE, layer.name="bleaching")
mapview(PA_15, zcol="Average_bleaching",cex="Average_bleaching", legend=TRUE, layer.name="bleaching")


eq <- Average_bleaching~ClimSST_Celcius+SST+SSTA+SSTA_DHW+Latitude_coarse+TSA


linearmodel1 <- lm(eq, data=Pacific_dt)
summary(linearmodel1)

linearmode16 <- lm(eq, data=PA_2016)
summary(linearmode16)

linearmode15 <- lm(eq, data=PA_2015)
summary(linearmode15)


eq2 <- Average_bleaching~SST

eq3 <- Average_bleaching~TSA+SSTA+ClimSST_Celcius

linearmodel1 <- lm(eq2, data=Pacific_dt)
summary(linearmodel1)
lm15 <- lm(eq2, data=PA_15)
summary(lm15)
lm16 <- lm(eq2, data=PA_16)
summary(lm16)

lm1516 <- lm(eq2, data=PA1516)
summary(lm1516)
#View(PA_1516)
#plot(lm16)
# summary(lm15)
# lm15 <- lm(eq3, data=PA_15)

plot(PA_15$geometry)

PA16_coords <- st_coordinates(PA_16)

library(tmap)
PA16.nb <- dnearneigh(PA16_coords, 0, 50, longlat = TRUE)
PA16.W <-nb2listw(PA16.nb, style="W", zero.policy=T)

summary(PA16.W,zero.policy=T)

moran <-lm.morantest(model=lm16, listw=PA16.W, zero.policy=T)

print(moran)

LMtest1 <-lm.LMtests(lm16,PA16.W, zero.policy=T, test=c('LMlag', 'LMerr'))
print(LMtest1)

lag16 <-lagsarlm(Average_bleaching~SST+TSA, data=PA_16, PA16.W, zero.policy=T)
summary(lag16)

lag.impacts <- impacts(lag16, listw=PA16.W)
print(lag.impacts)

err16 <- errorsarlm(Average_bleaching~SST+TSA, data=PA_16, PA16.W, zero.policy=T)

summary(err16)

hausman <- Hausman.test(err16)

print(hausman)

PA15_coords <- st_coordinates(PA_15)

library(tmap)
PA15.nb <- dnearneigh(PA15_coords, 0, 50, longlat = TRUE)
PA15.W <-nb2listw(PA15.nb, style="W", zero.policy=T)

summary(PA15.W,zero.policy=T)
summary(lm15)
moran <-lm.morantest(model=lm15, listw=PA15.W, zero.policy=T)

print(moran)

LMtest1 <-lm.LMtests(lm15,PA15.W, zero.policy=T, test=c('LMlag', 'LMerr'))
print(LMtest1)

lag15 <-lagsarlm(Average_bleaching~TSA+ClimSST_Celcius+SSTA, data=PA_15, PA15.W, zero.policy=T)
summary(lag15)

lag.impacts <- impacts(lag15, listw=PA15.W)
print(lag.impacts)

err15 <- errorsarlm(Average_bleaching~SST+TSA, data=PA_15, PA15.W, zero.policy=T)

summary(err15)

hausman <- Hausman.test(err15)

print(hausman)



summary(lm1516)


PA1516_coords <- st_coordinates(PA1516)

library(tmap)
PA1516.nb <- dnearneigh(PA1516_coords, 0, 50, longlat = TRUE)
PA1516.W <-nb2listw(PA1516.nb, style="W", zero.policy=T)

summary(PA1516.W,zero.policy=T)

moran <-lm.morantest(model=lm1516, listw=PA1516.W, zero.policy=T)

print(moran)

LMtest1 <-lm.LMtests(lm1516,PA1516.W, zero.policy=T, test=c('LMlag', 'LMerr'))
print(LMtest1)

lag1516 <-lagsarlm(Average_bleaching~SST+TSA, data=PA_1516, PA1516.W, zero.policy=T)
summary(lag1516)

lag.impacts <- impacts(lag1516, listw=PA1516.W)
print(lag.impacts)

err1516 <- errorsarlm(Average_bleaching~SST+TSA, data=PA_1516, PA1516.W, zero.policy=T)

summary(err1516)

hausman <- Hausman.test(err1516)

print(hausman)



summary(lm1516)















































ggplot(Pacific_dt, aes(x = SSTA_DHW, y = Average_bleaching, col = as.factor(Year))) + geom_line()

singleLinear <- ggplot(Pacific_dt, aes(x  = SSTA_DHW, y = Average_bleaching)) + geom_point(color = "blue", alpha = 0.7) + geom_smooth(method = "lm", color = "black")
singleLinear
singleLinear + facet_wrap(~Year)


library(lme4)
fm1 <-lmer(Average_bleaching~SST+TSA+Latitude_coarse+(1+TSA|Year), Pacific_dt, REML=0)

fm1
ranef(fm1)[["Year"]]

coef(fm1)

fm2 <-lmer(Average_bleaching~SST+Latitude_coarse+(1+TSA|Year), Pacific_dt, REML=0)

coef(fm2)

anova(fm1, fm2)
Pacific_dt <-  Pacific_dt %>% mutate(Bleaching= ifelse(Average_bleaching <=1,0,1))

Monthly_mean <- Pacific_dt %>% group_by(Date_format) %>% summarize_at(vars("SST_new", "ClimSST_Celcius_new", "SSTA_new", "TSA_new", "Average_bleaching_new"), mean) 
Monthly_mean


nrow(PA_15)
nrow(PA_16)
View(PA_2015)


library(forecast)
library(urca)

min(Monthly_mean$Date_format)
max(Monthly_mean$Date_format)

plot(Monthly_mean$Date_format,Monthly_mean$Average_bleaching_new)
plot(Monthly_mean$SSTA_new,Monthly_mean$Average_bleaching_new)

library(zoo)



# library(mapview)
# mapview(my_AUS_15)
# mapview(my_AUS_15, zcol="Average_bleaching",cex=3.5, legend=TRUE, layer.name="bleaching",label="Reef.Name")
# 
# plot(my_AUS_15["Average_bleaching"])
# 
# mapview(my_AUS_sf, zcol="Average_bleaching",cex=3.5, legend=TRUE, layer.name="bleaching",label="Reef.Name")



ggplot(data=AUS_dt)+aes(x=Date_format, y=Average_bleaching_new)+geom_point()
#ggplot(data=AUS_data)+aes(x=Date, y=Average_bleaching)+geom_ponti()

ggplot(data=AUS_15)+aes(x=ClimSST_Celcius, y=Average_bleaching)+geom_point()

ggplot(data=AUS_data)+aes(x=SSTA, y=Average_bleaching)+geom_point()

library(cowplot)
p1 <- ggplot(data=AUS_data)+aes(x=as.factor(Year), fill=Bleaching_Level)+geom_bar()+theme(legend.position='top')+ xlab("Year") + ylab("Average bleaching in percentage") + labs(caption="Plot of years and bleaching levels") + 
  theme(plot.caption = element_text(hjust=0.5, size=rel(1.2)), legend.position="top")+scale_fill_brewer(palette="Dark2")



ggplot(data=coral)+aes(x=Latitude_coarse, y=Longitude_coarse,colour=Average_bleaching)+geom_point()

newdata1 <- coral %>% filter(Latitude_coarse >= 10 & Longitude_coarse >= -100  & Longitude_coarse <= -50 )

ggplot(data=newdata1)+aes(x=Latitude_coarse, y=Longitude_coarse,colour=Average_bleaching)+geom_point()


new_data <- AUS_data %>% group_by(Latitude_coarse, Longitude_coarse, Date)%>% summarise_at(vars(Average_bleaching,SST,ClimSST_Celcius, SSTA, Year), list(new=mean))

nrow(newdata1)
unique(newdata1$Region)


ggplot(data=newdata1)+aes(x=Latitude_coarse, y=Average_bleaching)+geom_point()







unique(new_data[c("Longitude_coarse","Latitude_coarse")])

ggplot(data=newdata1)+aes(x=Latitude_coarse, y=Average_bleaching)+geom_point()+facet_wrap(~Region)

region138 <-coral %>%filter(Region=='ERG138')
nrow(region138)

View(region138)
ggplot(data=region138)+aes(x=TSA, y=Average_bleaching)+geom_point()




new_data <- coral %>% group_by(Latitude_coarse, Longitude_coarse)

unique(new_data[c("Longitude_coarse","Latitude_coarse")])

ggplot(data=coral)+aes(x=ClimSST_Celcius, y=Average_bleaching)+geom_point()+facet_wrap(c("Longitude_coarse","Latitude_coarse"))


new_data <- AUS_data %>% group_by(Latitude_coarse, Longitude_coarse, Date)%>% summarise_at(vars(Average_bleaching,SST,ClimSST_Celcius, SSTA, Year), list(new=mean))


warm_data <-coral %>% filter(lat_height=="Mid-latitude tropics")
hot_data <-coral %>% filter(lat_height=="Equatorial region")
subtropic_data <- coral %>% filter(lat_height=="Subtropics")

tab_hot <- table(hot_data$Bleaching)
tab_hot

bleaching_rate_hot <- tab_hot[2]/(tab_hot[1]+tab_hot[2])
cat("Fraction of observations that had bleching = ", format(bleaching_rate_hot, digits=4))

tab_warm <- table(warm_data$Bleaching)
tab_warm

bleaching_rate_warm <- tab_warm[2]/(tab_warm[1]+tab_warm[2])
cat("Fraction of observations that had bleching = ", format(bleaching_rate_warm, digits=4))

tab_cold <- table(subtropic_data$Bleaching)
tab_cold

bleaching_rate_cold <- tab_cold[2]/(tab_cold[1]+tab_cold[2])
cat("Fraction of observations that had bleching = ", format(bleaching_rate_cold, digits=4))



tab <- table(coral$Bleaching)
tab
bleaching_rate <- tab[2]/(tab[1]+tab[2])
cat("Fraction of observations that had bleching = ", format(bleaching_rate, digits=4))



coral$SSTA_DHW
After2001 <- coral %>%filter(Year>2002)

subdata <-subset(After2001,select=c('Average_bleaching', 'ClimSST_Celcius','SST', 'SST_Max', 'SSTA','SSTA_DHWMax','TSA','TSA_DHW','Latitude.Degrees', 'Longitude.Degrees', 'Depth', 'Year')) %>% na.omit()

cor(subdata)

#bleaching <-After2001 %>%filter(Average_bleaching>0) %>%filter(Year>2002)
bleaching <- coral %>% filter(Average_bleaching >0)

#unique(bleaching$lat_height)


ggplot(data=bleaching)+aes(x=Latitude.Degrees, y=Average_bleaching)+geom_point()


ggplot(data=bleaching)+aes(x=TSA_DHW, y=Average_bleaching)+geom_point()

ggplot(data=bleaching)+aes(x=SSTA_DHW, y=Average_bleaching)+geom_point()

ggplot(data=coral)+aes(x=SSTA_DHW, y=Average_bleaching)+geom_point()

ggplot(data=coral)+aes(x=TSA_DHW, y=Average_bleaching)+geom_point()

ggplot(data=coral)+aes(x=TSA, y=Average_bleaching)+geom_point()
ggplot(data=coral)+aes(x=SSTA_Standard_Deviation, y=Average_bleaching)+geom_point()
ggplot(data=coral)+aes(x=SST_stdev, y=Average_bleaching)+geom_point()

mean(coral$Average_bleaching)

summary(coral$Average_bleaching)

summary(coral$ClimSST_Celcius)
summary(coral$SSTA)

ggplot(data=coral)+aes(x=ClimSST_Celcius, y=Average_bleaching, colour=lat_height)+geom_point()+labs(colour="Regions")+   
  xlab("Climatological SST") + ylab("Average bleaching in percentage") + labs(caption="Plot of climatological SST and average bleaching") + 
  theme(plot.caption = element_text(hjust=0.5, size=rel(1.2)), legend.position="top")+




ggplot(data=bleaching)+aes(x=SSTA, y=Average_bleaching, colour=lat_height)+geom_point()

ggplot(data=coral)+aes(x=ClimSST_Celcius, y=Average_bleaching, colour=lat_height)+geom_point()+labs(colour="Regions")+   
  xlab("Climatological SST") + ylab("Average bleaching in percentage") + labs(caption="Plot of climatological SST and average bleaching") + 
  theme(plot.caption = element_text(hjust=0.5, size=rel(1.2)), legend.position="top")+facet_wrap(~lat_height, ncol=1)


ggplot(data=coral)+aes(x=SSTA_Standard_Deviation, y=Average_bleaching, colour=lat_height)+geom_point()+labs(colour="Regions")+   
  xlab("SSTA Standard Deviation") + ylab("Average bleaching in percentage") + labs(caption="Plot of SSTA standard deviation and average bleaching") + 
  theme(plot.caption = element_text(hjust=0.5, size=rel(1.2)), legend.position="top")


ggplot(data=bleaching)+aes(x=SSTA_Standard_Deviation, y=Average_bleaching, colour=lat_height)+geom_point()

names(coral)
 min(coral$SSTA_Standard_Deviation)

ggplot(data=bleaching)+aes(x=SSTA, y=Average_bleaching, colour=lat_height)+geom_point()

ggplot(data=coral)+aes(x=as.factor(Year), fill=as.factor(Bleaching_level))+geom_bar()
bleaching1 <- bleaching %>% filter(Year>2002)

library(cowplot)
p1 <- ggplot(data=bleaching1)+aes(x=as.factor(Year), fill=Bleaching_Level)+geom_bar()+theme(legend.position='top')+ xlab("Year") + ylab("Average bleaching in percentage") + labs(caption="Plot of years and bleaching levels") + 
  theme(plot.caption = element_text(hjust=0.5, size=rel(1.2)), legend.position="top")+scale_fill_brewer(palette="Dark2")


pp1 <- ggplot(data = bleaching) + 
  geom_bar(mapping = aes(x = Year, y = ..prop.., fill=Bleaching_level), stat = "count")

pp1

Yearly_mean_ClimSST <-bleaching1 %>% group_by(Year) %>% summarize(mean_ClimSST=mean(ClimSST_Celcius)) 

Yearly_mean <-bleaching1 %>% group_by(Year) %>% summarize_at(vars("SST", "ClimSST_Celcius", "SSTA","SSTA_DHW", "TSA", "TSA_DHW"), mean) 
Yearly_mean

bleaching$TSA_DHW
#p2<- ggplot(data=Yearly_mean)+aes(x=Year, y=TSA_DHW)+geom_point()

# p1
# 
# plot_grid(p1,p2)
# p2<- ggplot(data=Yearly_mean)+aes(x=Year, y=SSTA)+geom_point()
# plot_grid(p1,p2)

p2<- ggplot(data=Yearly_mean)+aes(x=Year, y=ClimSST_Celcius)+geom_point() + ylab("Yearly mean of Climatological SST") + labs(caption="Plot of yearly mean of Climatological SST for bleaching data") + 
  theme(plot.caption = element_text(hjust=0.5, size=rel(1.2)))

plot_grid(p1,p2)
#p2<- ggplot(data=Yearly_mean)+aes(x=Year, y=SST)+geom_point()


names(bleaching)

ggplot(data=bleaching)+aes(x=as.factor(Year), y=Average_bleaching)+geom_boxplot(outlier.shape=NA) +stat_summary(fun="mean", shape="diamond", colour="red")+labs(x="Year",y="bleaching in percentage" )


sub_bleaching <-subset(bleaching,select=c('Average_bleaching', 'ClimSST_Celcius','SST', 'SST_Max', 'SST_Min', 'SSTA','SSTA_DHWMax','Latitude.Degrees', 'Longitude.Degrees', 'Depth', 'Year')) %>% na.omit()


cor(sub_bleaching)


bc<-boxcox(After2001$Average_bleaching+1~After2001$ClimSST_Celcius)

lambda <-bc$x[which.max(bc$y)]

lambda

BC_model <- lm(((Average_bleaching+1)^(lambda)-1)/lambda~ClimSST_Celcius, data=After2001)
summary(BC_model)



bc1<-boxcox(sub_bleaching$Average_bleaching~sub_bleaching$ClimSST_Celcius)

lambda1 <-bc1$x[which.max(bc1$y)]

lambda1

BC_model <- lm(((Average_bleaching)^(lambda1)-1)/lambda1~ClimSST_Celcius, data=sub_bleaching)
summary(BC_model)


bleaching2 <-bleaching  %>% filter(Average_bleaching <= 90) %>%filter(Average_bleaching>=10)


# log(p/1-p) model

log_model <- lm(log(Average_bleaching/100/(1-Average_bleaching/100))~ClimSST_Celcius+Depth+SSTA, data=bleaching2)
summary(log_model)



log_model <- lm(log((1-Average_bleaching/100)/(Average_bleaching/100))~SST+Depth+SSTA, data=bleaching2)
summary(log_model)


log_model2 <- lm(log(Average_bleaching+1)~SST+Depth+SSTA, data=After2001)
summary(log_model2)

log_model2 <- lm(log(Average_bleaching+1)~., data=subdata)
summary(log_model2)

log_model2 <- lm(log(Average_bleaching+1)~Depth+ClimSST_Celcius+Latitude.Degrees+SST, data=subdata)
summary(log_model2)

linearmodel <- lm(Average_bleaching~., data=subdata)

summary(linearmodel)

linearmodel1 <- lm(Average_bleaching~SST+Depth+ClimSST_Celcius, data=subdata)

summary(linearmodel1)
#plot(linearmodel1)
linearmodel1 <- lm(Average_bleaching~ClimSST_Celcius+Depth+SST, data=bleaching)

summary(linearmodel1)

#plot(linearmodel1)

linearmodel1 <- lm(Average_bleaching~TSA_DHW+Depth+SST, data=bleaching)
summary(linearmodel1)


linearmodel1 <- lm(Average_bleaching~TSA_DHW, data=bleaching)
summary(linearmodel1)
#polynomial

poly_model<- lm(Average_bleaching~poly(ClimSST_Celcius,3,raw=T), data=After2001)

summary(poly_model)
plot(poly_model)


poly_model<- lm(Average_bleaching~poly(SSTA,3,raw=T), data=bleaching)

summary(poly_model)
plot(poly_model)




#Linear regression

coral$Bleaching

bleaching_rate <- sum(coral$Bleaching==1)/length(coral$Bleaching)
cat("Fraction of observations that had bleching = ", format(bleaching_rate, digits=3))

regmodel <- glm(Bleaching~SSTA+Depth, family=binomial(link='logit'), data=coral)
summary(regmodel)
confint(regmodel)



regmodel1 <- glm(Bleaching~SSTA+Depth+ClimSST_Celcius+SST+SST_Max+SST_Min+Latitude.Degrees, family=binomial(link='logit'), data=coral)
summary(regmodel1)
confint(regmodel1)




#######Linear regression train set and test set

coral_train<- coral %>%sample_frac(0.75)
coral_test <- anti_join(coral, coral_train, by='X')
tab1 <- table(coral$Bleaching)
tab2  <- table(coral_train$Bleaching)

#######checking if the train_set is not too bias
tab1[2]/(tab1[1]+tab1[2])
tab2[2]/(tab2[1]+tab2[2])





# regmodel2 <- glm(Bleaching~Depth+ClimSST_Celcius+SST+SST_Min+Latitude.Degrees, family=binomial(link='logit'), data=coral)

regmodel2 <- glm(Bleaching~Depth+ClimSST_Celcius+SST+Latitude.Degrees, family=binomial, data=coral_train)
summary(regmodel2)
confint(regmodel2)
anova(regmodel2)

y <- coral_train$Bleaching -trunc(2*regmodel2$fitted)
hits <- sum(y==0)
hitratio <- hits/length(y)
hitratio



probability <-predict(regmodel2, newdata=coral_test, type="response")
head(probability)

prediction <- ifelse(probability > 0.5, 1, 0) 

confusion  <- table(coral_test$Bleaching, prediction) 
confusion 

table(coral_test$Bleaching)


prob_train <-predict(regmodel2, type="response")

pred_train <- ifelse(prob_train > 0.5, 1, 0) 
validate_train  <- table(coral_train$Bleaching, pred_train) 
validate_train
max(prob_train)



pregmodel2 <- predict(regmodel2, coral)
table(coral$Bleaching)

prob.table(table(coral$Bleaching))

#confusion matrix with threshold 0.5
tab_matrix <- table(pregmodel2>0.5, coral$Bleaching)
tab_matrix


pregmodel2

coralnew <- coral %>% mutate(fitted=regmodel2$fitted)

coralnew$fitted



#bleching level
bleaching_rate0 <- sum(coral$Bleaching_level==0)/length(coral$Bleaching_level)
bleaching_rate0

bleaching_rate1 <- sum(coral$Bleaching_level==1)/length(coral$Bleaching_level)
bleaching_rate1
bleaching_rate2 <- sum(coral$Bleaching_level==2)/length(coral$Bleaching_level)
bleaching_rate2
bleaching_rate3 <- sum(coral$Bleaching_level==3)/length(coral$Bleaching_level)
bleaching_rate3


#fit order logit model

model_logit <-polr(as.factor(Bleaching_level)~ClimSST_Celcius+SSTA+SST+Depth+Latitude.Degrees+Year, data=coral, Hess=TRUE)
summary(model_logit)

model_logit <-polr(as.factor(Bleaching_level)~ClimSST_Celcius+SST+Depth+Latitude.Degrees, data=coral, Hess=TRUE)
summary(model_logit)

ctable <-coef(summary(model_logit))

ctable

## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

## combined table
(ctable <- cbind(ctable, "p value" = p))







ggplot(data=coral)+aes(x=SSTA, fill=as.factor(Bleaching))+geom_histogram(bins=20)

newcoral <- subset(coral, select= c('SSTA', 'Bleaching'))

ggplot(data=newcoral)+aes(x=SSTA, colour=Bleaching)+ geom_density(colour='red')

ggplot(data=coral)+aes(x=Average_bleaching, fill=as.factor(lat_height))+geom_histogram(bins=10)
ggplot(data=bleaching)+aes(x=Average_bleaching, fill=as.factor(lat_height))+geom_histogram(bins=10)+facet_wrap(~lat_height)

ggplot(data=coral)+aes(x=Average_bleaching)+geom_histogram(bins=10)+facet_wrap(~lat_height)






#############################################################################################
##---------Bleaching mean for each small interval ---------------------------------------

##---------------bleaching data vs SST--------------------------------------------------------------
max(bleaching$SST)

min(bleaching$SST)

max(bleaching$SSTA)

min(bleaching$SSTA)

min(bleaching$SSTA_DHW)
max(bleaching$SSTA_DHW)


n_intervals <- 40

bleaching_mean <- array(data=NA, dim=c(n_intervals,2))

for (i in 1:n_intervals){
  bleaching_mean[i,1] <- 23+(i-1)*0.25
  bleaching_mean[i,2] <- bleaching %>% filter(SST>= 23 + (i-1)*0.25 & SST < 23 +i *0.25) %>% summarise(mean=mean(Average_bleaching)) %>% pull()
}
bleaching_mean

model1 <- lm(bleaching_mean[,2]~bleaching_mean[,1])
plot(bleaching_mean)
summary(model1)



n_intervals <- 60

bleaching_mean <- array(data=NA, dim=c(n_intervals,2))

for (i in 1:n_intervals){
  bleaching_mean[i,1] <- -2+(i-1)*0.1
  bleaching_mean[i,2] <- bleaching %>% filter(SSTA>= -2 + (i-1)*0.1 & SSTA < -2 +i *0.1) %>% summarise(mean=mean(Average_bleaching)) %>% pull()
}
plot(bleaching_mean)


#####-------- bleaching mean vs climate SST------------------ bleching data
max(coral$ClimSST_Celcius)

#######------------------- Good model------------------------------------------



n_intervals <- 50
bleaching_mean <- array(data=NA, dim=c(n_intervals,2))

#divide the reange [20,32.5] of ClimSST into 50 small intervasl with length 0.25 each
#calculate mean of bleaching over a small inverval of ClimSST
for (i in 1:n_intervals){
  bleaching_mean[i,1] <- 20+(i-1)*0.25
  bleaching_mean[i,2] <- bleaching %>% filter(ClimSST_Celcius>= 20 + (i-1)*0.25 & ClimSST_Celcius < 20 +i *0.25) %>% summarise(mean=mean(Average_bleaching)) %>% pull()
}

#plot data
plot(bleaching_mean)

#create a dataframe with ClimSST and mean_bleaching
df_SST <- data.frame(bleaching_mean)
colnames(df_SST) <- c("ClimSST_Celcius", "mean_bleaching")
df_SST <- df_SST  %>%na.omit()
dim(df_SST)

#------------------------------------------------------------------
#  ##--------- quadratic model--------------------------------
mean_model <- lm(mean_bleaching~poly(ClimSST_Celcius,2), data=df_SST)

summary(mean_model)
plot(mean_model$residuals,pch = 16, col = "red")

#calculate mean square error
MSE <- mean(mean_model$residuals^2)
MSE

#create a dataframe with predictions 
preds <-data.frame(ClimSST_Celcius=df_SST$ClimSST_Celcius, preds=predict(mean_model))

#plot fitted model vs data
plot(mean_bleaching ~ ClimSST_Celcius, data=df_SST)
lines(preds$preds~preds$ClimSST_Celcius, col='red')

#----------------------------------------------------------------
## -----------------------piecewise linear model----------------------
library(segmented)
mean_model2 <-lm(mean_bleaching~ClimSST_Celcius, data=df_SST)
seg.out<-segmented(mean_model2,seg.Z = ~ClimSST_Celcius)
summary(seg.out)

#mean square
MSE <- mean(seg.out$residuals^2)
MSE

#slopes and intercepts of the model
slope(seg.out)
intercept(seg.out)

#make predictions
preds <-data.frame(ClimSST_CelciusT=df_SST$ClimSST_Celcius, preds=predict(seg.out))

##plot fitted model vs actual data
plot(seg.out,res=TRUE, ylab="mean bleaching")

#plot residulas
plot(seg.out$residuals, pch = 16, col = "blue")

#plot Q-Q plot of residuals
qqnorm(seg.out$residuals, pch = 1, frame = FALSE)
qqline(seg.out$residuals, col = "steelblue", lwd = 2)





# new.data <- data.frame(ClimSST_Celcius = seq(from = min(df_SST$ClimSST_Celcius),
#                                   to = max(df_SST$ClimSST_Celcius), length.out = 200))
# ###new.data
# pred_lm2 <- predict(mean_model, newdata=new.data)

# plot(mean_model)
#  plot(mean_bleaching ~ ClimSST_Celcius, data=df_SST)
# # lines(pred_lm2~new.data$ClimSST, col="blue")
# # lines(preds$preds~preds$ClimSST_Celcius, col='blue')

n_intervals <- 90

bleaching_mean <- array(data=NA, dim=c(n_intervals,3))

for (i in 1:n_intervals){
  bleaching_mean[i,1] <- 20+(i)*0.125
  bleaching_mean[i,2] <- bleaching %>% filter(ClimSST_Celcius>= 20 + (i-1)*0.125 & ClimSST_Celcius < 20 +(i+1) *0.125) %>% summarise(mean=mean(Average_bleaching)) %>% pull()
  bleaching_mean[i,3] <- bleaching %>% filter(ClimSST_Celcius>= 20 + (i-1)*0.125 & ClimSST_Celcius < 20 +(i+1) *0.125) %>% summarise(mean=mean(SSTA)) %>% pull()
}
bleaching_mean

plot(bleaching_mean)
cor(bleaching_mean)


df_SST <- data.frame(bleaching_mean)



library(segmented)

colnames(df_SST) <- c("ClimSST", "mean_bleaching", "mean_SSTA")
df_SST <- df_SST  %>%na.omit()
cor(df_SST)

df_SST

model <-lm(df_SST$mean_bleaching~df_SST$ClimSST+df_SST$mean_SSTA, data=df_SST)
summary(model)

mean_model <- lm(mean_bleaching~poly(ClimSST,2), data=df_SST)

summary(mean_model)
plot(mean_model$residuals,pch = 16, col = "red")

new.data <- data.frame(ClimSST = seq(from = min(df_SST$ClimSST),
                                     to = max(df_SST$ClimSST), length.out = 200))
###new.data
pred_lm2 <- predict(mean_model, newdata=new.data)

# plot(mean_model)
plot(mean_bleaching ~ ClimSST, data=df_SST)
# lines(pred_lm2~new.data$ClimSST, col="blue")
# 


preds <-data.frame(ClimSST=df_SST$ClimSST, preds=predict(mean_model))
preds

#plot predictions
lines(preds$preds~preds$ClimSST, col='red')


mean_model2 <-lm(mean_bleaching~ClimSST, data=df_SST)
seg.out<-segmented(mean_model2,seg.Z = ~ClimSST)
summary(seg.out)
plot(seg.out$residuals, pch = 16, col = "red")

# SSQ2 <- sum(seg.out$residuals^2)
# SSQ2
# 
# summary(mean_model2)
# SSQ2 <- sum(mean_model2$residuals^2)
# SSQ2

slope(seg.out)

#make predictions
preds <-data.frame(ClimSST=df_SST$ClimSST, preds=predict(seg.out))
#View(preds)
plot(seg.out,res=TRUE)

#plot predictions
lines(preds$preds~preds$ClimSST, col='red')




#ggplot(data=df_SST)+aes(x=ClimSST, y=mean_bleaching)+geom_point()+geom_smooth(method="lm", se=FALSE, colour="red")

#######******************************
#######*
#######*

min(bleaching$SSTA_DHW)
max(bleaching$SSTA_DHW)

n_intervals <- 80

bleaching_mean <- array(data=NA, dim=c(n_intervals,2))

for (i in 1:n_intervals){
  bleaching_mean[i,1] <- 0+(i-1)*0.5
  bleaching_mean[i,2] <- bleaching %>% filter(SSTA_DHW >= 0 + (i-1)*0.5 & SSTA_DHW < 0 +i *0.5) %>% summarise(mean=mean(Average_bleaching)) %>% pull()
}
bleaching_mean

plot(bleaching_mean)
cor(bleaching_mean)

coral$SSTA_DHW

####-------------------------bleaching_mean_ vs Temprature with full data--------------------------
n_intervals <- 40

bleaching_mean <- array(data=NA, dim=c(n_intervals,2))

for (i in 1:n_intervals){
  bleaching_mean[i,1] <- 23+(i-2)*0.25
  bleaching_mean[i,2] <- coral %>% filter(SST>= 23 + (i-2)*0.25 & SST < 23 +(i-1) *0.25) %>% summarise(mean=mean(Average_bleaching)) %>% pull()
}
bleaching_mean

plot(bleaching_mean)

bleaching_mean_model2 <-lm(bleaching_mean[,2]~poly(bleaching_mean[,1],2))
summary(bleaching_mean_model2)


 ##################---- bleaching_mean _vs _climSST---  Full data -----------------------
n_intervals <- 50

bleaching_mean <- array(data=NA, dim=c(n_intervals,2))

for (i in 1:n_intervals){
  bleaching_mean[i,1] <- 20+(i-1)*0.25
  bleaching_mean[i,2] <- coral %>% filter(ClimSST_Celcius>= 20 + (i-1)*0.25 & ClimSST_Celcius < 20 +i *0.25) %>% summarise(mean=mean(Average_bleaching)) %>% pull()
}
bleaching_mean

plot(bleaching_mean)
cor(bleaching_mean)
bleaching_mean_model <-lm(bleaching_mean[,2]~bleaching_mean[,1])
summary(bleaching_mean_model)

max(coral$ClimSST_Celcius)
df_SST <- data.frame(bleaching_mean)


library(segmented)

colnames(df_SST) <- c("ClimSST", "mean_bleaching")
df_SST <- df_SST  %>%na.omit()

mean_model <- lm(mean_bleaching~poly(ClimSST,2), data=df_SST)

summary(mean_model)
plot(mean_model$residuals,pch = 16, col = "red")

new.data <- data.frame(ClimSST = seq(from = min(df_SST$ClimSST),
                                     to = max(df_SST$ClimSST), length.out = 200))
###new.data
pred_lm2 <- predict(mean_model, newdata=new.data)

# plot(mean_model)
plot(mean_bleaching ~ ClimSST, data=df_SST)
# lines(pred_lm2~new.data$ClimSST, col="blue")
# 


preds <-data.frame(ClimSST=df_SST$ClimSST, preds=predict(mean_model))
preds

#plot predictions
lines(preds$preds~preds$ClimSST, col='red')


mean_model2 <-lm(mean_bleaching~ClimSST, data=df_SST)
seg.out<-segmented(mean_model2,seg.Z = ~ClimSST)
summary(seg.out)
plot(seg.out$residuals, pch = 16, col = "red")

# SSQ2 <- sum(seg.out$residuals^2)
# SSQ2
# 
# summary(mean_model2)
# SSQ2 <- sum(mean_model2$residuals^2)
# SSQ2

slope(seg.out)

#make predictions
preds <-data.frame(ClimSST=df_SST$ClimSST, preds=predict(seg.out))
#View(preds)
plot(seg.out,res=TRUE)

#plot predictions
lines(preds$preds~preds$ClimSST, col='red')





#####################################################################
###    -----------analyzing with respect to latitudes




cor(warm_data$ClimSST_Celcius, warm_data$SSTA)
cor(hot_data$ClimSST_Celcius, hot_data$SSTA)
cor(subtropic_data$ClimSST_Celcius, subtropic_data$SSTA)




####hot water----------------------
n_intervals <- 40

bleaching_mean <- array(data=NA, dim=c(n_intervals,3))

for (i in 1:n_intervals){
  bleaching_mean[i,1] <- 24+(i-1)*0.2
  bleaching_mean[i,2] <- hot_data %>% filter(ClimSST_Celcius>= 24 + (i-1)*0.2 & ClimSST_Celcius < 24 +i *0.2) %>% summarise(mean=mean(Average_bleaching)) %>% pull()
  bleaching_mean[i,3] <- hot_data %>% filter(ClimSST_Celcius>= 24 + (i-1)*0.2 & ClimSST_Celcius < 24 +i *0.2) %>% summarise(mean=mean(SSTA)) %>% pull()
}
bleaching_mean


plot(bleaching_mean[,3]~bleaching_mean[,2])

model1 <- lm(bleaching_mean[,2]~bleaching_mean[,1]+bleaching_mean[,3])
summary(model1)


n_intervals <- 60

bleaching_mean <- array(data=NA, dim=c(n_intervals,2))

for (i in 1:n_intervals){
  bleaching_mean[i,1] <- -2+(i-1)*0.1
  bleaching_mean[i,2] <- hot_data %>% filter(SSTA>= -2 + (i-1)*0.1 & SSTA < -2 +i *0.1) %>% summarise(mean=mean(Average_bleaching)) %>% pull()
  bleaching_mean[i,3] <- hot_data %>% filter(ClimSST_Celcius>= 24 + (i-1)*0.2 & ClimSST_Celcius < 24 +i *0.2) %>% summarise(mean=mean(Average_bleaching)) %>% pull()
}
#plot(bleaching_mean)


############## warm data###########################

n_intervals <- 50

bleaching_mean <- array(data=NA, dim=c(n_intervals,2))

for (i in 1:n_intervals){
  bleaching_mean[i,1] <- 20+(i-1)*0.25
  bleaching_mean[i,2] <- warm_data %>% filter(ClimSST_Celcius>= 20 + (i-1)*0.25 & ClimSST_Celcius < 20 +i *0.25) %>% summarise(mean=mean(Average_bleaching)) %>% pull()
}
bleaching_mean

plot(bleaching_mean)


n_intervals <- 60

bleaching_mean <- array(data=NA, dim=c(n_intervals,2))

for (i in 1:n_intervals){
  bleaching_mean[i,1] <- -2+(i-1)*0.1
  bleaching_mean[i,2] <- warm_data %>% filter(SSTA>= -2 + (i-1)*0.1 & SSTA < -2 +i *0.1) %>% summarise(mean=mean(Average_bleaching)) %>% pull()
}
plot(bleaching_mean)

##################sub tropical water #########################


n_intervals <- 50

bleaching_mean <- array(data=NA, dim=c(n_intervals,2))

for (i in 1:n_intervals){
  bleaching_mean[i,1] <- 20+(i-1)*0.25
  bleaching_mean[i,2] <- subtropic_data %>% filter(ClimSST_Celcius>= 20 + (i-1)*0.25 & ClimSST_Celcius < 20 +i *0.25) %>% summarise(mean=mean(Average_bleaching)) %>% pull()
}
bleaching_mean

plot(bleaching_mean)


n_intervals <- 60

bleaching_mean <- array(data=NA, dim=c(n_intervals,2))

for (i in 1:n_intervals){
  bleaching_mean[i,1] <- -2+(i-1)*0.1
  bleaching_mean[i,2] <- subtropic_data %>% filter(SSTA>= -2 + (i-1)*0.1 & SSTA < -2 +i *0.1) %>% summarise(mean=mean(Average_bleaching)) %>% pull()
}
plot(bleaching_mean)

#####bleaching data with respect to latitudes
 hot_bleaching <- hot_data %>% filter(Average_bleaching > 0)
 warm_bleaching <- warm_data %>% filter(Average_bleaching >0)
 subtropic_bleaching <- subtropic_data %>% filter(Average_bleaching>0)
 
 n_intervals <- 50
 
 bleaching_mean <- array(data=NA, dim=c(n_intervals,2))
 
 for (i in 1:n_intervals){
   bleaching_mean[i,1] <- 20+(i-1)*0.25
   bleaching_mean[i,2] <- hot_bleaching %>% filter(ClimSST_Celcius>= 20 + (i-1)*0.25 & ClimSST_Celcius < 20 +i *0.25) %>% summarise(mean=mean(Average_bleaching)) %>% pull()
 }
 bleaching_mean
 
 plot(bleaching_mean)
 
 
 
 
 n_intervals <- 50
 
 bleaching_mean <- array(data=NA, dim=c(n_intervals,2))
 
 for (i in 1:n_intervals){
   bleaching_mean[i,1] <- 20+(i-1)*0.25
   bleaching_mean[i,2] <- warm_bleaching %>% filter(ClimSST_Celcius>= 20 + (i-1)*0.25 & ClimSST_Celcius < 20 +i *0.25) %>% summarise(mean=mean(Average_bleaching)) %>% pull()
 }
 bleaching_mean
 
 plot(bleaching_mean)
 

 
 n_intervals <- 50
 
 bleaching_mean <- array(data=NA, dim=c(n_intervals,2))
 
 for (i in 1:n_intervals){
   bleaching_mean[i,1] <- 20+(i-1)*0.25
   bleaching_mean[i,2] <- subtropic_bleaching %>% filter(ClimSST_Celcius>= 20 + (i-1)*0.25 & ClimSST_Celcius < 20 +i *0.25) %>% summarise(mean=mean(Average_bleaching)) %>% pull()
 }
 bleaching_mean
 
 plot(bleaching_mean)
 

 
 n_intervals <- 60
 
 bleaching_mean <- array(data=NA, dim=c(n_intervals,2))
 
 for (i in 1:n_intervals){
   bleaching_mean[i,1] <- -2+(i-1)*0.1
   bleaching_mean[i,2] <- hot_bleaching %>% filter(SSTA>= -2 + (i-1)*0.1 & SSTA < -2 +i *0.1) %>% summarise(mean=mean(Average_bleaching)) %>% pull()
 }
 plot(bleaching_mean)
 
 
 n_intervals <- 60
 
 bleaching_mean <- array(data=NA, dim=c(n_intervals,2))
 
 for (i in 1:n_intervals){
   bleaching_mean[i,1] <- -2+(i-1)*0.1
   bleaching_mean[i,2] <- warm_bleaching %>% filter(SSTA>= -2 + (i-1)*0.1 & SSTA < -2 +i *0.1) %>% summarise(mean=mean(Average_bleaching)) %>% pull()
 }
 plot(bleaching_mean)
 
 
 n_intervals <- 60
 
 bleaching_mean <- array(data=NA, dim=c(n_intervals,2))
 
 for (i in 1:n_intervals){
   bleaching_mean[i,1] <- -2+(i-1)*0.1
   bleaching_mean[i,2] <- subtropic_bleaching %>% filter(SSTA>= -2 + (i-1)*0.1 & SSTA < -2 +i *0.1) %>% summarise(mean=mean(Average_bleaching)) %>% pull()
 }
 plot(bleaching_mean)
 

##########################################################################################################################

# for (i in 1:n_intervals){
#   bleaching_mean[i,1] <- 23+(i-1)*0.25
#   bleaching_mean[i,2] <- coral %>% filter(SST>= 23 + (i-1)*0.25 & SST < 23 +i *0.25) %>% summarise(mean=mean(Average_bleaching)) %>% pull()
# }

bleaching_mean


plot(abs(bleaching_mean[,1]), bleaching_mean[,2])





ggplot(data=bleaching)+aes(x=ClimSST_Celcius, y=Average_bleaching, colour=lat_height)+geom_point()+ facet_wrap(~Year, ncol=3)

ggplot(data=bleaching)+aes(x=SST, y=Average_bleaching, colour=lat_height)+geom_point()+ facet_wrap(~Year, ncol=3)

ggplot(data=bleaching)+aes(x=SSTA, y=Average_bleaching, colour=lat_height)+geom_point()+ facet_wrap(~Year, ncol=3)


n_intervals <- 15

bleaching_mean <- array(data=NA, dim=c(n_intervals,2))

for (i in 1:n_intervals){
  bleaching_mean[i,1] <- bleaching %>% filter(Year==2002+i) %>% summarise(mean=mean(ClimSST_Celcius)) %>% pull()
  bleaching_mean[i,2] <- bleaching %>% filter(Year== 2002+i) %>% summarise(mean=mean(Average_bleaching)) %>% pull()
}
bleaching_mean


plot(bleaching_mean)


# n_intervals <- 15
# 
# bleaching_mean <- array(data=NA, dim=c(n_intervals,6))
# 
# for (i in 1:n_intervals){
#   bleaching_mean[i,1] <- 2002+i
#   bleaching_mean[i,2] <- bleaching %>% filter(Year==2002+i) %>% summarise(mean=mean(ClimSST_Celcius)) %>% pull()
#   bleaching_mean[i,3] <- bleaching %>% filter(Year==2002+i) %>% summarise(mean=mean(SST)) %>% pull()
#   bleaching_mean[i,4] <- bleaching %>% filter(Year==2002+i) %>% summarise(mean=mean(SSTA)) %>% pull()
#   bleaching_mean[i,5] <- bleaching %>% filter(Year==2002+i) %>% summarise(mean=mean(SST_Max)) %>% pull()
#   bleaching_mean[i,6] <- bleaching %>% filter(Year== 2002+i) %>% summarise(mean=mean(Average_bleaching)) %>% pull()
# }
# bleaching_mean
# 
# cor(bleaching_mean)
# 



# n_intervals <- 15
# 
# bleaching_mean <- array(data=NA, dim=c(n_intervals,6))
# 
# for (i in 1:n_intervals){
#   bleaching_mean[i,1] <- 2002+i
#   bleaching_mean[i,2] <- coral %>% filter(Year==2002+i) %>% summarise(mean=mean(ClimSST_Celcius)) %>% pull()
#   bleaching_mean[i,3] <- coral %>% filter(Year==2002+i) %>% summarise(mean=mean(SST)) %>% pull()
#   bleaching_mean[i,4] <- coral %>% filter(Year==2002+i) %>% summarise(mean=mean(SSTA)) %>% pull()
#   bleaching_mean[i,5] <- coral %>% filter(Year==2002+i) %>% summarise(mean=mean(SST_Max)) %>% pull()
#   bleaching_mean[i,6] <- coral%>% filter(Year== 2002+i) %>% summarise(mean=mean(Average_bleaching)) %>% pull()
# }
# bleaching_mean
# 
# cor(bleaching_mean)
# 


# 
# plot(bleaching_mean[,4],bleaching_mean[,6] )


n_intervals <- 16

bleaching_mean <- array(data=NA, dim=c(n_intervals,2))

for (i in 1:n_intervals){
  bleaching_mean[i,1] <- bleaching %>% filter(Year==2002+i-1) %>% summarise(mean=mean(SSTA)) %>% pull()
  bleaching_mean[i,2] <- bleaching %>% filter(Year== 2002+i-1) %>% summarise(mean=mean(Average_bleaching)) %>% pull()
}
bleaching_mean


n_intervals <- 60

bleaching_mean <- array(data=NA, dim=c(n_intervals,2))

for (i in 1:n_intervals){
  bleaching_mean[i,1] <- 0+(i-1)*0.15
  bleaching_mean[i,2] <- bleaching %>% filter(SSTA>=  (i-1)*0.15& SSTA < i *0.15) %>% summarise(mean=mean(Average_bleaching)) %>% pull()
}

bleaching_mean
plot(bleaching_mean)




n_intervals <- 100

bleaching_mean <- array(data=NA, dim=c(n_intervals,2))

for (i in 1:n_intervals){
  bleaching_mean[i,1] <- -2+(i-1)*0.15
  bleaching_mean[i,2] <- After2001 %>% filter(SSTA>= -2 +(i-1)*0.15& SSTA < -2+ i *0.05) %>% summarise(mean=mean(Average_bleaching)) %>% pull()
}

bleaching_mean
plot(bleaching_mean)
names(coral)


AUS_data <- coral %>% filter(Country=="Australia")
dim(AUS_data)
  

ggplot(data=AUS_data)+aes(x=ClimSST_Celcius, y=Average_bleaching, colour=lat_height)+geom_point()

ggplot(data=AUS_data)+aes(x=SSTA, y=Average_bleaching, colour=lat_height)+geom_point()
View(AUS_data)



n_intervals =14
bleaching_mean <- array(data=NA, dim=c(n_intervals,6))

for (i in 1:n_intervals){
  bleaching_mean[i,1] <- 2002+i
  bleaching_mean[i,2] <- AUS_data %>% filter(Year==2002+i) %>% summarise(mean=mean(ClimSST_Celcius)) %>% pull()
  bleaching_mean[i,3] <- AUS_data %>% filter(Year==2002+i) %>% summarise(mean=mean(SST)) %>% pull()
  bleaching_mean[i,4] <- AUS_data %>% filter(Year==2002+i) %>% summarise(mean=mean(SSTA)) %>% pull()
  bleaching_mean[i,5] <- AUS_data %>% filter(Year==2002+i) %>% summarise(mean=mean(SST_Max)) %>% pull()
  bleaching_mean[i,6] <- AUS_data%>% filter(Year== 2002+i) %>% summarise(mean=mean(Average_bleaching)) %>% pull()
}
bleaching_mean

cor(bleaching_mean)
plot(bleaching_mean[,4], bleaching_mean[,6])





n_intervals <- 100

bleaching_mean <- array(data=NA, dim=c(n_intervals,2))

for (i in 1:n_intervals){
  bleaching_mean[i,1] <- -2+(i-1)*0.05
  bleaching_mean[i,2] <- AUS_data %>% filter(SSTA>= -2 +(i-1)*0.05& SSTA < -2+ i *0.05) %>% summarise(mean=mean(Average_bleaching)) %>% pull()
}

bleaching_mean

plot(bleaching_mean)
AUS_bleaching <- AUS_data %>% filter(Average_bleaching >=1)
dim(AUS_bleaching)



n_intervals <- 40

bleaching_mean <- array(data=NA, dim=c(n_intervals,2))

for (i in 1:n_intervals){
  bleaching_mean[i,1] <- 21+(i-1)*0.25
  bleaching_mean[i,2] <- AUS_bleaching %>% filter(SST>= 21 +(i-1)*0.25& SST < 21+ (i) *0.25) %>% summarise(mean=mean(Average_bleaching)) %>% pull()
}

bleaching_mean

plot(bleaching_mean)

names(coral)

ggplot(data=AUS_data)+aes(x=SSTA, y=Average_bleaching)+geom_point()

ggplot(data=AUS_data)+aes(x=SSTA_Standard_Deviation, y=Average_bleaching)+geom_point()


ggplot(data=coral)+aes(x=SSTA_Standard_Deviation, y=Average_bleaching)+geom_point()
AUS_data$TSA



ggplot(data=AUS_bleaching)+aes(x=TSA, y=Average_bleaching)+geom_point()



# #yearly data analysis
# A2001_bleaching <- After2001 %>% filter(Average_bleaching >0)
# 
# yearly_data <- A2001_bleaching %>% group_by(Year) %>% summarise_at(vars(ClimSST_Celcius, SST,SST_Max, SSTA, SSTA_Standard_Deviation, Average_bleaching), mean)
# 
# cor(yearly_data)
# 
# ggplot(data=yearly_data)+aes(x=SST, y=Average_bleaching)+geom_point()

names(coral)

After2001$Average_bleaching[After2001$Average_bleaching < 1] <-0

yearly_data <- After2001 %>% group_by(Year) %>% summarise_at(vars(ClimSST_Celcius, SST,SST_Max,SST_Min, SSTA, SSTA_Standard_Deviation, SSTA_Maximum, SSTA_Minimum, SSTA_DHWMax, Average_bleaching), mean)

cor(yearly_data)
ggplot(data=yearly_data)+aes(x=SSTA, y=Average_bleaching)+geom_point()
ggplot(data=yearly_data)+aes(x=SST, y=Average_bleaching)+geom_point()



yearly_table <- table(After2001$Year, After2001$Bleaching)
class(yearly_table)
yearly_table
nyear =15


year_bleaching_rate <- array(data=NA, dim=c(nyear,2))
for (i in 1: nyear){
  year_bleaching_rate[i,1] <- 2003+i-1
  year_bleaching_rate[i,2] <- yearly_table[i,2]/(yearly_table[i,1]+yearly_table[i,2])
}

year_bleaching_rate
plot(year_bleaching_rate)
yearly_data


class(yearly_data)
plot(yearly_data$SSTA, yearly_data$Average_bleaching)
 model1 <- lm(Average_bleaching~SST+SSTA, data=yearly_data)
summary(model1)



model1 <- lm(Average_bleaching~., data=yearly_data)
summary(model1)


yearly_data$bleaching_propotion <- year_bleaching_rate[,2]
cor(yearly_data)

plot(abs(yearly_data$SSTA), yearly_data$bleaching_propotion)


plot(yearly_data$SSTA^2, yearly_data$bleaching_propotion)
model2 <- lm(bleaching_propotion~SSTA, data=yearly_data)

####################good model to include##################################################

model3 <- lm(bleaching_propotion~abs(SSTA), data=yearly_data)
summary(model3)


##########################################################################################################

###### fiting data
library(fitdistrplus)

After2001_bleaching <- After2001 %>% filter(Average_bleaching>0)

hist(After2001_bleaching$SST, xlim=c(20,34), freq=FALSE)
d <- density(After2001_bleaching$SST)
plot(d)



hist(After2001_bleaching$SST, xlim=c(20,34), freq=FALSE)
lines(density(After2001_bleaching$SST), col='red')


plotdist(After2001_bleaching$SST, histo = TRUE, demp = TRUE)
descdist(After2001_bleaching$SST)

descdist(After2001_bleaching$SST, boot=1000)



library(fitdistrplus)

fit_w  <- fitdist(After2001_bleaching$SST, "weibull")
fit_g  <- fitdist(After2001_bleaching$SST, "gamma")
fit_ln <- fitdist(After2001_bleaching$SST, "lnorm")
summary(fit_w)






plot.legend <- c("Weibull", "lognormal", "gamma")
denscomp(list(fit_w, fit_ln, fit_g), legendtext = plot.legend)

qqcomp(list(fit_w, fit_ln, fit_g), legendtext = plot.legend)

cdfcomp(list(fit_w, fit_ln, fit_g), legendtext = plot.legend)

gofstat(list(fit_w, fit_ln, fit_g),
          + fitnames = c("Weibull", "lognormal", "gamma"))



library(fitdistrplus)
#draw histogram and  density plot of bleaching data with respect t0 SST
hist(bleaching$SST, xlim=c(20,34), freq=FALSE)
lines(density(bleaching$SST), col='red')

#using desdist to get suggestions for distribution fitting
descdist(bleaching$SST)

descdist(bleaching$SST, boot=1000)

## fitting Weibull, gamma, log normal distributions to the data
fit_w  <- fitdist(bleaching$SST, "weibull")
fit_g  <- fitdist(bleaching$SST, "gamma")
fit_ln <- fitdist(bleaching$SST, "lnorm")

# checking if these distributions provide good fits
plot.legend <- c("Weibull", "lognormal", "gamma")

#density plots
denscomp(list(fit_w, fit_ln, fit_g), legendtext = plot.legend)

##qq plots
qqcomp(list(fit_w, fit_ln, fit_g), legendtext = plot.legend)

##cdf plots
cdfcomp(list(fit_w, fit_ln, fit_g), legendtext = plot.legend)

##sumarry of fitting a Weilbull distribution
summary(fit_w)




plotdist(bleaching$ClimSST_Celcius, histo = TRUE, demp = TRUE)
descdist(bleaching$ClimSST_Celcius)

descdist(bleaching$ClimSST_Celcius, boot=1000)




fit_w  <- fitdist(bleaching$ClimSST_Celcius, "weibull")
fit_g  <- fitdist(bleaching$ClimSST_Celcius, "gamma")
fit_ln <- fitdist(bleaching$ClimSST_Celcius, "lnorm")
summary(fit_w)


plot.legend <- c("Weibull", "lognormal", "gamma")
denscomp(list(fit_w, fit_ln, fit_g), legendtext = plot.legend)

qqcomp(list(fit_w, fit_ln, fit_g), legendtext = plot.legend)

cdfcomp(list(fit_w, fit_ln, fit_g), legendtext = plot.legend)

##After2001_bleaching <- After2001 %>% filter(Average_bleaching>0)

# hist(bleaching$SST, xlim=c(20,34), freq=FALSE)
# d <- density(bleaching$SST)
# plot(d)


