##set work dictionary
rm(list = ls())
library(dplyr)
library(raster)
library("rgdal")
library("rgeos")
library(tmap)
library(geosphere)
distm(c(lon1, lat1), c(lon2, lat2), fun = distHaversine)
#setwd("F:/OneDrive - Monash University/co-authored paper/Childcare facility")
setwd("C:/Users/rxuu0017/OneDrive - Monash University/co-authored paper/Childcare facility")

ad1<-read.csv("Education-services-au-export.csv")##read the address data
ad1$id<-1:nrow(ad1)## creat a unique id variable
ad<-ad1[,6:9]%>%
  mutate(Home.Address=paste(ServiceAddress,",",Suburb,",",State,sep = ""))
##get the geocode
library(ggmap)
ad$Home.Address<-as.character(ad$Home.Address)
register_google(key = "***")## register a API key of google account
location<-mutate_geocode(ad,Home.Address)

## AIzaSyDG6QbAP6rjeQJHJeA0khH1f3NmbXGG3dk, the new API
##simple plot for Google lon and lat
plot(location$lon,location$lat)

###### plot the dots on the map to check the whether there are any outliers######
AUS<-readOGR(".", "POA_2016_AUST")##read Australian map

## get the centroid of postcode area
postcode_centre<-gCentroid(AUS,byid = T)
postcode_centre<-as.data.frame(postcode_centre)%>%
  mutate(Postcode=AUS$POA_CODE16)%>%
  mutate(Postcode=as.integer(as.character(Postcode)))
plot(postcode_centre$x,postcode_centre$y)

## link postcode adress to main one
location<-location%>%
  left_join(postcode_centre[,c(1,2,4)])
location$ServiceName<-ad1$ServiceName
location$id<-ad1$id

## things need to do mannually
View(filter(location,is.na(Postcode)))
mannul1<-filter(location,is.na(Postcode))
write.csv(mannul1,"01_Total_missing.csv")
mannul1<-read.csv("01_Total_missing_mannual_inputed.csv")[,-1]%>%
  mutate(Home.Address=as.character(Home.Address))
mannul1_i<-mutate_geocode(mannul1,Home.Address)
summary(mannul1_i$lat-mannul1_i$lat)##identical
location1<-location%>%
  filter(!is.na(Postcode))
location1<-rbind(location1,mannul1)

## iuput the wrong post code
View(filter(location,is.na(x)))
mannul2<-filter(location,is.na(x)&!is.na(Postcode))
write.csv(mannul2,"02_Postcode_wrong.csv")
mannul2<-read.csv("02_Postcode_wrong_corrected.csv")[,-1]%>%
  mutate(Home.Address=as.character(Home.Address))
location1<-location1%>%
  filter(!(id%in%mannul2$id))
location1<-rbind(location1,mannul2)%>%
  arrange(id)
identical(location1$id,location$id)

## update the postcode
summary(location1)
names(location1)
location1<-location1[,c(-8,-9)]%>%
  left_join(postcode_centre[,c(1,2,4)])

## calculate the distance between location and centroid

dist<-distGeo(as.matrix(location1[,c(6,7)]),
              as.matrix(location1[,c(10,11)]))
location1<-location1%>%
  mutate(dist_km=dist/1000)
summary(location1$dist_km)
plot(density(na.omit(location1$dist_km)))
nrow(filter(location1,dist_km>50))

## find the farthest point of each postcode area
#https://stackoverflow.com/questions/30660938/how-to-compute-greatest-distance-between-polygon-centroid-and-edge
find_furthest_dist<-function(polygon){
  coords=fortify(polygon)[,c(1:2)]  
  centre<-as.data.frame(gCentroid(polygon))
  dist<-distGeo(as.matrix(coords),
                as.matrix(centre))
  return(dist/1000)##in km
}
postcode_centre$furthest_dist<-NA
for (i in 1:nrow(AUS)) {
  postcode_centre$furthest_dist[i]<-find_furthest_dist(AUS[AUS$POA_CODE16==postcode_centre$postcode[i],])
  print(i)
}  
summary(postcode_centre$furthest_dist)

location1<-location1%>%
  left_join(postcode_centre[,c(1,2,5)])
summary(location1$furthest_dist)
summary(location1$furthest_dist-location1$dist_km)
nrow(filter(location1,dist_km>furthest_dist+20))
summary(filter(location1,dist_km>furthest_dist)$furthest_dist)
View(filter(location1,dist_km>furthest_dist)$furthest_dist)

## for thoes dist_km>furthest_dist&furthest_dist>20km, 
## do the geocoding again, using services address
loc2<-filter(location1,((dist_km>(furthest_dist+20))|is.na(lon)))%>%
  mutate(Home.Address=paste(ServiceName,"Australia",sep=","))

loc2<-mutate_geocode(loc2,Home.Address)
loc2$dist_km2<-distGeo(as.matrix(loc2[,c(14,15)]),
                       as.matrix(loc2[,c(10,11)]))/1000
loc2$diff2<-loc2$dist_km2-loc2$furthest_dist
summary(loc2$dist_km2)

nrow(filter(loc2,dist_km2<(furthest_dist)))
save1<-filter(loc2,dist_km2<=(furthest_dist+20))

## further filter the remaining 123 rows
loc3<-filter(loc2,!(id%in%save1$id))
save2<-filter(loc3,furthest_dist<=20)##replace with post code

## further filter the left 74
loc4<-filter(loc3,!(id%in%save2$id))%>%
  arrange(dist_km2)
write.csv(loc4,"03_Uncertain_beyond_20km.csv")
save3<-read.csv("03_Uncertain_beyond_20km_checked.csv")


## get the final locations
final_loc<-location1%>%
  mutate(lon_final=lon,
         lat_final=lat)%>%
  select(id,lon_final,lat_final)%>%
  filter(!(id%in%loc2$id))

final_loc1<-save1%>%
  mutate(lon_final=lon1,
         lat_final=lat1)%>%
  select(id,lon_final,lat_final)

final_loc2<-save2%>%
  mutate(lon_final=x,
         lat_final=y)%>%
  select(id,lon_final,lat_final)

final_loc3<-save3%>%
  select(id,lon_final,lat_final)

final_location<-rbind(final_loc,
                      final_loc1,
                      final_loc2,
                      final_loc3)
summary(final_location)
plot(final_location$lon_final,final_location$lat_final)




## plot the points on the AUstralia map
Address.Points <-SpatialPointsDataFrame(
  final_location[,c("lon_final","lat_final")], final_location)
proj4string(Address.Points) <- proj4string(AUS)

tm_shape(AUS) + tm_borders(alpha=.4) +
  tm_shape(Address.Points) + tm_dots(col = "red")



## If there is no problem
##save the results
child_care<-left_join(ad1,final_location)
summary(child_care$lat_final)

save(child_care,file = "Childcare.Rdata")
write.csv(child_care,"Childcare.csv")



