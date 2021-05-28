# determine % of DBUIDs covered by >=17 eBird checklists
# written by Emma J. Hudgins April 16, 2021
# for questions email emma.hudgins@carleton.ca

# require(devtools)
require(sf)
require(dplyr)
require(here)
require(rgdal)
require(rgeos)
require(ggplot2)

setwd(here())

poly <- readOGR("../raw/lcma000b16a_e/lcma000b16a_e.shp") #2016 CMA and CA boundaries from Canadian Census (Statistics Canada, Downloaded April 14)
poly<-subset(poly, CMATYPE=="B") # only CMAs

postal<-readOGR("../raw/DMTI_2006_CanMapPS_LDU_ALL_PROV/MB/MBldu.shp")

pdf('../plots/postal.pdf')
ggplot(data=postal)+
  geom_polygon(data = postal, aes(x=long, y=lat, group=group),size = 0.5, colour="black", fill=NA)+
  geom_polygon(data=poly, aes(x=long, y=lat, group=group),size = 0.5, colour="black", fill=alpha('pink',0.5))
dev.off()

cdb<- read_sf('../raw/ldb_000b16a_e/ldb_000b16a_e.shp') #census dissemination block (smallest unit)
cdb<-subset(cdb, CMATYPE=="B") #census dissemination block (smallest 

pccf<-read.csv('../raw/pccf_feb2021_national.csv') # large file (postal code conversion file from StatCan)
pccf$DBUID<-as.character(paste0(pccf$DAuid, sprintf("%03d",pccf$DisBlock)))
cdb_pccf<-cdb%>%left_join(pccf, 'DBUID')
cdb_pccf<-cdb_pccf %>%
  mutate(N = 1) # qualifying factor 
cdb_pccf<-cdb_pccf%>%
  group_by(DBUID) %>%
  mutate(N = N / n()) # number of species qualified per origin/destination

saveRDS(cdb_pccf,'../output/cdb_pccf.RDS')

marg<-read.csv('../raw/cmg_a_2021-05-11_18-54-04_annual/cmg_a_06.csv')#marginalization indices from CANUE downloaded May 11th,2021
colnames(marg)[1]<-"PC"
files<-list.files()
files<-files[grepl("CMA_summary_",files)]
all_birddiv<-tibble(DBUID=numeric(), year=numeric(), n_checklists=numeric(), species_richness=numeric())
for (file in files)
{
  dat<-read.csv(file)
  all_birddiv<-add_row(dat, all_birddiv)
}

cdb_pccf$DBUID<-as.numeric(cdb_pccf$DBUID)

cdb_pccf_bird<-cdb_pccf%>%left_join(all_birddiv, "DBUID")
cdb_pccf_bird_marg<-cdb_pccf_bird%>%left_join(marg, "PC")
saveRDS(cdb_pccf_bird_marg, file="../output/cdb_pccf_bird.RDS")
#free up some memory
rm(cdb_pccf_bird)
rm(cdb_pccf)
rm(pccf)
df<-subset(cdb_pccf_bird_marg, n_checklists>=17)#threshold for sufficient effort (only ~10%)
df$cmg06_10[which(df$cmg06_10==-9999)]<-NA
df$cmg06_11[which(df$cmg06_11==-9999)]<-NA
df$cmg06_12[which(df$cmg06_12==-9999)]<-NA
df$cmg06_13[which(df$cmg06_13==-9999)]<-NA

library(mgcv)
m<-gam(log(df$species_richness+1)~cmg06_10+cmg06_11+cmg06_12+cmg06_13+s(year), data=df)#instability, deprivation, dependency, ethnic concentration
summary(m)
library(viridis)
pal<-viridis
poly<-poly[,c("CMAUID")]
library(raster)
provinces <- getData(country="Canada", level=1)
provinces<-subset(provinces, ENGTYPE_1!="Territory")
crs(provinces)<-"+proj=longlat +datum=WGS84"
e <- as(extent(poly), "SpatialPolygons")
crs(e)<- "+proj=lcc +lat_0=63.390675 +lon_0=-91.8666666666667 +lat_1=49 +lat_2=77 +x_0=6200000 +y_0=3000000 +datum=NAD83 +units=m +no_defs"
# provinces<-spTransform(provinces,CRS("+proj=lcc +lat_0=63.390675 +lon_0=-91.8666666666667 +lat_1=49 +lat_2=77 +x_0=6200000 +y_0=3000000 +datum=NAD83 +units=m +no_defs"))
# provinces<-crop(provinces, e)
bins<-seq(0,max((df$species_richness+1)),length.out=50)
colours<-pal(50)[findInterval(df$species_richness, bins)+1]

pdf('../plots/bird_richness.pdf')
plot(provinces)
plot(poly, add=T)
points(cbind(df$LONG, df$LAT), pch=15, cex=0.05, col=colours)
dev.off()


bins<-seq(min(df$cmg06_11, na.rm=T),max(df$cmg06_11, na.rm=T),length.out=50)
colours<-pal(50)[findInterval(df$cmg06_11, bins)+1]

pdf('../plots/Deprivation.pdf')
plot(provinces)
plot(poly, main='Deprivation', add=T)
points(cbind(df$LONG, df$LAT), pch=15, cex=0.05, col=colours)
dev.off()

bins<-seq(min(df$cmg06_13, na.rm=T),max(df$cmg06_13, na.rm=T),length.out=50)
colours<-pal(50)[findInterval(df$cmg06_13, bins)+1]

pdf('../plots/Ethnic Concentration.pdf')
plot(provinces)
plot(poly, main='Ethnic Concentration', add=T)
points(cbind(df$LONG, df$LAT), pch=15, cex=0.05, col=colours)
dev.off()


bins<-seq(min(df$cmg06_10, na.rm=T),max(df$cmg06_10, na.rm=T),length.out=50)
colours<-pal(50)[findInterval(df$cmg06_10, bins)+1]

pdf('../plots/instability.pdf')
plot(provinces)
plot(poly, main='Instability', add=T)
points(cbind(df$LONG, df$LAT), pch=15, cex=0.05, col=colours)
dev.off()


bins<-seq(min(df$cmg06_12, na.rm=T),max(df$cmg06_12, na.rm=T),length.out=50)
colours<-pal(50)[findInterval(df$cmg06_12,bins)+1]

pdf('../plots/Dependency.pdf')
plot(provinces)
plot(poly, main='Dependency', add=T)
points(cbind(df$LONG, df$LAT), pch=15, cex=0.05, col=colours)
dev.off()