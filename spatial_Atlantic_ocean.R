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
library("spgwr")
library(raster)
library(mapview)




coral <- read.csv("ReefCheck.csv")



coral$Latitude_coarse <- round(coral$Latitude.Degrees,digits=2)
coral$Longitude_coarse <- round(coral$Longitude.Degrees,digits=2)

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


Atlantic_sf <- st_as_sf(Atlantic_data,coords=c("Longitude.Degrees", "Latitude.Degrees"),
                        crs="+proj=longlat +datum=WGS84 +towg84=0,0,0")




mapview(Atlantic_sf)
mapview(Atlantic_sf, zcol="Average_bleaching", cex="Average_bleaching", legend=TRUE, layer.name="bleaching")


Atlantic_data$Date <-as.Date(Atlantic_data$Date, "%d-%b-%y")
Atlantic_data$month<- format(Atlantic_data$Date, "%m")
Atlantic_data$Date_format <- format(Atlantic_data$Date, "%b-%y")


Atlantic_data1 <- Atlantic_data  %>% filter(Year<=2016)
ggplot(data=Atlantic_data)+aes(x=month, y=Average_bleaching, colour=SST)+
        geom_point()+facet_wrap(~Year, ncol=3) +ylab("Bleaching average")+scale_colour_viridis(option='H')


 Atlantic_dt <- Atlantic_data %>% group_by(Latitude_coarse,
                Longitude_coarse, Date_format,Reef.Name, Year)%>% 
         summarise_at(vars(Average_bleaching,SST,SST_Max, 
ClimSST_Celcius, SSTA,TSA, SSTA_DHW,TSA_Maximum,SSTA_Maximum,SSTA_Mean,TSA_Mean),mean)

 Atlantic_dt <-  Atlantic_dt %>% mutate(Bleaching_level= ifelse(Average_bleaching< 1,0,
                                                    ifelse(Average_bleaching<10,1,
                                                           ifelse(Average_bleaching<50,2, 3))))
 
 new_data<- Atlantic_dt %>% group_by(Bleaching_level)%>%
         summarise_at(vars(Average_bleaching,SST,SST_Max, ClimSST_Celcius,
                           SSTA,TSA, SSTA_DHW,TSA_Maximum,SSTA_Maximum,SSTA_Mean,TSA_Mean),mean)
 

AT<- st_as_sf(Atlantic_dt,coords=c("Longitude_coarse", "Latitude_coarse"),
              crs="+proj=longlat +datum=WGS84 +no_defs")

mapview(AT, zcol="Average_bleaching",cex="Average_bleaching", legend=TRUE, layer.name="bleaching")
nrow(Atlantic_dt)


Atlantic_0506 <-Atlantic_data %>% filter(Year==2005 |Year==2006) 
nrow(Atlantic_0506)



ggplot(data=Atlantic_dt)+aes(x=SSTA, y=SSTA_DHW)+geom_point()+facet_wrap(~as.factor(Bleaching_level))


AT_0506_dt <-Atlantic_dt %>%filter(Year==2005 | Year==2006)

AT_0506 <- AT_0506_dt %>% group_by(Latitude_coarse, Longitude_coarse)%>% 
        summarise_at(vars(Average_bleaching,SST,SST_Max,
                          ClimSST_Celcius, SSTA,TSA, SSTA_DHW,
                          TSA_Maximum,SSTA_Maximum,SSTA_Mean,TSA_Mean),mean)



AT0506<- st_as_sf(AT_0506,coords=c("Longitude_coarse", "Latitude_coarse"),
                  crs="+proj=longlat +datum=WGS84 +no_defs")


mapview(AT0506, zcol="Average_bleaching",cex="Average_bleaching", legend=TRUE, layer.name="bleaching")

eq <- Average_bleaching~ClimSST_Celcius+SST+SSTA+SSTA_DHW+Latitude_coarse+
        TSA+SST_Max+TSA+TSA_Maximum+SSTA_Maximum+SSTA_Mean+TSA_Mean

nrow(AT0506)


linearmodel1 <- lm(eq, data=Atlantic_dt)
summary(linearmodel1)


linearmode0506 <- lm(eq, data=AT_0506)
summary(linearmode0506)


lm0506 <- lm( Average_bleaching~SSTA+ClimSST_Celcius+TSA, data=AT0506)
summary(lm0506)

plot(lm0506)


AT0506$reslm <- residuals(lm0506)

qqnorm(AT0506$reslm)
qqline(AT0506$reslm, col = "steelblue", lwd = 2)

p2<- ggplot(data=AT0506)+aes(col=cut(reslm, quantile(reslm)))+geom_sf()+
        labs("residuals of the linear model")+
        scale_colour_manual(values=rainbow(5),name=" ")+theme(legend.position="bottom")
p2


AT0506_coords <- st_coordinates(AT0506)

library(tmap)
AT0506.nb <- dnearneigh(AT0506_coords, 0, 50, longlat = TRUE)
AT0506.W <-nb2listw(AT0506.nb, style="W", zero.policy=T)

summary(AT0506.W,zero.policy=T)

moran <-lm.morantest(model=lm0506, listw=AT0506.W, zero.policy=T)

print(moran)

moran_new <- moran.test(AT0506$Average_bleaching, listw=AT0506.W, zero.policy=T)
print(moran_new)
# p value is significant, our I statistic is positive (i.e. somwhat correlated)



MC<-moran.mc(AT0506$Average_bleaching, AT0506.W, zero.policy=T, nsim=599)
MC

plot(MC, main="", las=1)
moranplot <-moran.plot(AT0506$Average_bleaching, listw=AT0506.W, zero.policy=T )


local <-localmoran(x=AT0506$Average_bleaching, AT0506.W, zero.policy=T)
head(local)

#View(local)

moranmap <- cbind(AT0506,local)
class(moranmap)

plot(moranmap['Ii'], col=cut(moranmap$Ii, quantile(moranmap$Ii)))


LMtest1 <-lm.LMtests(lm0506,AT0506.W, zero.policy=T, test=c('LMlag', 'LMerr'))
print(LMtest1)



lag0506 <-lagsarlm(Average_bleaching~SSTA + ClimSST_Celcius + TSA, data=AT_0506, AT0506.W, zero.policy=T)
summary(lag0506)


AT0506$reslag <- residuals(lag0506)

qqnorm(AT0506$reslag)
qqline(AT0506$reslag, col = "steelblue", lwd = 2)


p2<- ggplot(data=AT0506)+aes(size=1,col=cut(reslm, quantile(reslm)))+geom_sf()+
        labs("residuals of the linear model")+scale_colour_manual(values=rainbow(5),name="Quartiles")
p2





p3<- ggplot(data=AT0506)+aes(size=1,col=cut(reslag, quantile(reslag)))+geom_sf()+
        labs("residuals of the linear model")+scale_colour_manual(values=rainbow(5), name="Quartiles ")
p3
library(gridExtra)
grid.arrange(p2,p3,ncol=1)



MClag<-moran.mc(AT0506$reslag, AT0506.W, zero.policy=T, nsim=599)
MClag

AT0506$reslm <- residuals(lm0506)
MClm<-moran.mc(AT0506$reslm, AT0506.W, zero.policy=T, nsim=599)
MClm
#the lag model adress the issue of spartial autocorrelation in the model residuals.


mapview(AT0506, zcol="reslm",legend=TRUE, layer.name="residual")
mapview(AT0506, zcol="reslag",legend=TRUE, layer.name="residual")



##qq plot of the residuals
ggplot(data=AT0506)+aes(x=SSTA, y=Average_bleaching)+geom_point()
qqnorm(AT0506$reslag)
qqline(AT0506$reslag, col = "steelblue", lwd = 2)



lag.impacts <- impacts(lag0506, listw=AT0506.W)
print(lag.impacts)

err0506 <- errorsarlm(Average_bleaching~SSTA + ClimSST_Celcius + TSA, data=AT0506, AT0506.W, zero.policy=T)

summary(err0506)

hausman <- Hausman.test(err0506)

print(hausman)


anova(err0506,lag0506)


AT0506$predictlm <- predict(lm0506)
head(AT0506$predictlm)
plot(st_geometry(AT0506), axes=TRUE)

AT0506$predictlag <- predict(lag0506)


p1 <- ggplot(data=AT0506)+aes(alpha=0.4,colour=Average_bleaching)+geom_sf()+
        scale_color_viridis(option="H", limits = range(0, 100))
p1

p2<- ggplot(data=AT0506)+aes( alpha=0.4, colour=predictlm)+geom_sf()+
        scale_color_viridis(option="H", limits = range(0, 100))

p3<- ggplot(data=AT0506)+aes(alpha=0.4, colour=predictlag)+geom_sf()+
        scale_color_viridis(option="H", limits = range(0, 100)) 
p3

library(gridExtra)
grid.arrange(p1,p2,p3,ncol=1)


coordinates(AT_0506)<- ~Latitude_coarse +Longitude_coarse

proj4string(AT_0506) = CRS("+proj=longlat +datum=WGS84 +no_defs")
gridded(AT_0506)<-TRUE


fbw1<- gwr.sel(AT_0506$Average_bleaching ~ AT_0506$SSTA + AT_0506$ClimSST_Celcius +
                       AT_0506$TSA, data = AT_0506,longlat=TRUE,adapt=FALSE,
               gweight = gwr.Gauss, verbose = FALSE)
fbw1



gwr.model1 <- gwr(AT_0506$Average_bleaching ~ AT_0506$SSTA + AT_0506$ClimSST_Celcius +
                          AT_0506$TSA, data = AT_0506, bandwidth = fbw1, longlat=TRUE, 
                  gweight = gwr.Gauss, hatmatrix = TRUE, se.fit = TRUE) 


gwr.model1

results1 <-as.data.frame(gwr.model1$SDF)
results1


AT0506$fmb_localR21 <- results1$localR2

mapview(AT0506, zcol="fmb_localR21", legend=TRUE, layer.name="Local R_square")


GWRbandwidth <- gwr.sel(AT_0506$Average_bleaching ~ AT_0506$SSTA +
                        AT_0506$ClimSST_Celcius + AT_0506$TSA, data = AT_0506,
                        adapt = T, longlat=TRUE)
GWRbandwidth

gwr.model2 <- gwr(AT_0506$Average_bleaching ~ AT_0506$SSTA + 
                          AT_0506$ClimSST_Celcius + AT_0506$TSA, data = AT_0506,
                  adapt = GWRbandwidth,longlat=TRUE, hatmatrix = TRUE, se.fit = TRUE) 



gwr.model2


results2 <-as.data.frame(gwr.model2$SDF)
results2


AT0506$fmb_localR22 <- results2$localR2

mapview(AT0506, zcol="fmb_localR22", legend=TRUE, layer.name="Local R_square")



gwr.model3 <- gwr(AT_0506$Average_bleaching ~ AT_0506$SSTA +
                          AT_0506$ClimSST_Celcius + AT_0506$TSA,
                  data = AT_0506, bandwidth = 50, longlat=TRUE,
                  gweight = gwr.Gauss, hatmatrix = TRUE, se.fit = TRUE) 


gwr.model3

results3<-as.data.frame(gwr.model3$SDF)
results3


AT0506$fmb_localR23 <- results3$localR2

mapview(AT0506, zcol="fmb_localR23", legend=TRUE, layer.name="Local R_square")

