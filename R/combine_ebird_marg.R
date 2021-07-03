# Combine eBird bird diversity data with CANUE marginalization indices
# written by Emma J. Hudgins April 16, 2021
# for questions email emma.hudgins@carleton.ca

# require(devtools)
require(sf)
require(dplyr)
require(here)
require(rgdal)
require(rgeos)
require(ggplot2)
require(sp)

setwd(here())

marg<-read.csv('../raw/cmg_a_2021-05-11_18-54-04_annual/cmg_a_06.csv')#marginalization indices from CANUE downloaded May 11th,2021
colnames(marg)[8]<-"DAUID" #this column is given as the dissemination area ID by https://canue.ca/wp-content/uploads/2018/11/CANUE-Metadata-Canadian-Marginalization-Index.pdf
marg$DAUID<-as.numeric(marg$DAUID)
poly <- readOGR("../raw/lcma000b16a_e/lcma000b16a_e.shp") #2016 CMA and CA boundaries from Canadian Census (Statistics Canada, Downloaded April 14)
poly<-subset(poly, CMATYPE=="B") # only CMAs


files<-list.files('../data/')
files<-files[grepl("CMA_DA_summary_",files)]
all_birddiv<-tibble(DAUID=numeric(), year=numeric(), n_checklists=numeric(), species_richness=numeric())
for (file in files)
{
  dat<-read.csv(paste0('../data/',file))
  all_birddiv<-add_row(dat, all_birddiv)
}

marg_bird<-all_birddiv%>%left_join(marg, "DAUID")
saveRDS(marg_bird, file="../output/marg_bird.RDS")


df<-subset(marg_bird, n_checklists>=17)#threshold for sufficient effort (only ~10%)
df$cmg06_10[which(df$cmg06_10==-9999)]<-NA
df$cmg06_11[which(df$cmg06_11==-9999)]<-NA
df$cmg06_12[which(df$cmg06_12==-9999)]<-NA
df$cmg06_13[which(df$cmg06_13==-9999)]<-NA
df<-df[complete.cases(df),]
df<-st_as_sf(df, coords = c('longitude','latitude'))
poly_sf<-st_as_sf(poly, crs=4326)
poly_sf<-st_transform(poly_sf,'+proj=longlat +datum=WGS84 +no_defs' )
st_crs(df)<-'+proj=longlat +datum=WGS84 +no_defs' 
poly_sf<-st_make_valid(poly_sf)
df<-df%>%st_join(poly_sf)
library(mgcv)
m<-gam(log(df$species_richness+1)~cmg06_10+cmg06_11+cmg06_12+cmg06_13+s(year)+df$CMANAME, data=df)#instability, deprivation, dependency, ethnic concentration
summary(m)
library(viridis)
pal<-viridis
poly_all<-poly
poly<-poly[,c("CMAUID")]
library(raster)
provinces <- raster::getData(country="Canada", level=1)
provinces<-subset(provinces, ENGTYPE_1!="Territory")
crs(provinces)<-"+proj=longlat +datum=WGS84"
# e <- as(extent(poly), "SpatialPolygons")
# crs(e)<- "+proj=lcc +lat_0=63.390675 +lon_0=-91.8666666666667 +lat_1=49 +lat_2=77 +x_0=6200000 +y_0=3000000 +datum=NAD83 +units=m +no_defs"
poly<-spTransform(poly,CRS("+proj=longlat +datum=WGS84"))
#provinces<-crop(provinces, e)
df_postal<-df%>%group_by(DAUID)%>%summarize_if(is.numeric, mean, na.rm=T)
df_postal<-subset(df_postal, is.nan(latitude)==F)
coordinates(df_postal)<-~longitude+latitude
crs(df_postal)<-"+proj=longlat +datum=WGS84"
for (i in 1:nrow(poly))
{
  
  within<-which(over(df_postal,poly[i,])!='<NA>')
  crop_provinces<-crop(provinces, df_postal[within,])
  bins<-seq(0,max((df$species_richness+1)),length.out=50)
  colours<-pal(50)[findInterval(df$species_richness, bins)+1]
  
pdf(paste0('../plots/bird_richness_',gsub("\\/.*", "",poly_all$CMANAME[i]),'.pdf'))
par(mar=c(0.5,0.5,0.5,0.5))
par(oma=c(0,0,0,4))
layout(t(1:2), widths=c(6,1))
plot(crop_provinces, main=gsub("\\/.*", "",poly_all$CMANAME[i]))
plot(poly[i,], add=T)
points(cbind(df_postal$longitude, df_postal$latitude)[within,], pch=15, cex=2,col=colours)
image(1,1:50,t(1:50), col=pal(50), axes=FALSE)
par(las=1)
axis(4,labels=c("lowest", "highest"),at=c(1,50), cex.axis=1)
par(las=0)
mtext(side=4, "Bird Richness", line=3)

dev.off()


bins<-seq(min(df$cmg06_11, na.rm=T),max(df$cmg06_11, na.rm=T),length.out=50)
colours<-pal(50)[findInterval(df$cmg06_11, bins)+1]

pdf(paste0('../plots/Deprivation_',gsub("\\/.*", "",poly_all$CMANAME[i]),'.pdf'))
par(mar=c(0.5,0.5,0.5,0.5))
par(oma=c(0,0,0,4))
layout(t(1:2), widths=c(6,1))
plot(crop_provinces)
plot(poly, main=gsub("\\/.*", "",poly_all$CMANAME[i]), add=T)
points(cbind(df_postal$longitude, df_postal$latitude)[within,], pch=15, cex=2,col=colours)
image(1,1:50,t(1:50), col=pal(50), axes=FALSE)
par(las=1)
axis(4,labels=c("lowest", "highest"),at=c(1,50), cex.axis=1)
par(las=0)
mtext(side=4, "Deprivation", line=3)
dev.off()

bins<-seq(min(df$cmg06_13, na.rm=T),max(df$cmg06_13, na.rm=T),length.out=50)
colours<-pal(50)[findInterval(df$cmg06_13, bins)+1]

pdf(paste0('../plots/Ethnic_concentration_',gsub("\\/.*", "",poly_all$CMANAME[i]),'.pdf'))
par(mar=c(0.5,0.5,0.5,0.5))
par(oma=c(0,0,0,4))
layout(t(1:2), widths=c(6,1))
plot(crop_provinces,main=gsub("\\/.*", "",poly_all$CMANAME[i]))
plot(poly, add=T)
points(cbind(df_postal$longitude, df_postal$latitude)[within,], pch=15, cex=2,col=colours)
image(1,1:50,t(1:50), col=pal(50), axes=FALSE)
par(las=1)
axis(4,labels=c("lowest", "highest"),at=c(1,50), cex.axis=1)
par(las=0)
mtext(side=4, "Ethnic concentration", line=3)
dev.off()


bins<-seq(min(df$cmg06_10, na.rm=T),max(df$cmg06_10, na.rm=T),length.out=50)
colours<-pal(50)[findInterval(df$cmg06_10, bins)+1]

pdf(paste0('../plots/Instability',gsub("\\/.*", "",poly_all$CMANAME[i]),'.pdf'))
par(mar=c(0.5,0.5,0.5,0.5))
par(oma=c(0,0,0,4))
layout(t(1:2), widths=c(6,1))
plot(crop_provinces, main=gsub("\\/.*", "",poly_all$CMANAME[i]))
plot(poly, add=T)
points(cbind(df_postal$longitude, df_postal$latitude)[within,], pch=15, cex=2,col=colours)
image(1,1:50,t(1:50), col=pal(50), axes=FALSE)
par(las=1)
axis(4,labels=c("lowest", "highest"),at=c(1,50), cex.axis=1)
par(las=0)
mtext(side=4, "Instability", line=3)
dev.off()


bins<-seq(min(df$cmg06_12, na.rm=T),max(df$cmg06_12, na.rm=T),length.out=50)
colours<-pal(50)[findInterval(df$cmg06_12,bins)+1]

pdf(paste0('../plots/Dependency_',gsub("\\/.*", "",poly_all$CMANAME[i]),'.pdf'))
par(mar=c(0.5,0.5,0.5,0.5))
par(oma=c(0,0,0,4))
layout(t(1:2), widths=c(6,1))
plot(crop_provinces, main=gsub("\\/.*", "",poly_all$CMANAME[i]))
plot(poly, add=T)
points(cbind(df_postal$longitude, df_postal$latitude)[within,], pch=15, cex=2,col=colours)
image(1,1:50,t(1:50), col=pal(50), axes=FALSE)
par(las=1)
axis(4,labels=c("lowest", "highest"),at=c(1,50), cex.axis=1)
par(las=0)
mtext(side=4, "Dependency", line=3)
dev.off()
}
