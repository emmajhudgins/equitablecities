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
require(mgcv)
require(viridis)
require(raster)

setwd(here())

marg<-read.csv('../raw/cmg_a_2021-05-11_18-54-04_annual/cmg_a_06.csv')#marginalization indices from CANUE downloaded May 11th,2021
#colnames(marg)[8]<-"DAUID" #this column is given as the dissemination area ID by https://canue.ca/wp-content/uploads/2018/11/CANUE-Metadata-Canadian-Marginalization-Index.pdf
#marg$DAUID<-as.numeric(marg$DAUID)
poly <- readOGR("../raw/lcma000b16a_e/lcma000b16a_e.shp") #2016 CMA and CA boundaries from Canadian Census (Statistics Canada, Downloaded April 14)
poly<-subset(poly, CMATYPE=="B") # only CMAs

da<-read_sf('../raw/lda_000b16a_e/lda_000b16a_e.shp')
da<-st_transform(da, '+proj=longlat +datum=WGS84 +no_defs ')
files<-list.files('../data/')
files<-files[grepl("CMA_summary_",files)]
all_birddiv<-tibble(POSTALCODE=character(), year=numeric(), n_checklists=numeric(), species_richness=numeric())
for (file in files)
{
  dat<-read.csv(paste0('../data/',file))
  all_birddiv<-add_row(dat, all_birddiv)
}

pccf<-read.csv('../raw/pccf_feb2021_national.csv')
all_birddiv$DAUID<-pccf$DAuid[match(all_birddiv$POSTALCODE,pccf$PC)]
colnames(marg)[1]<-'POSTALCODE'
marg_bird<-all_birddiv%>%left_join(marg, "POSTALCODE")
saveRDS(marg_bird, file="../output/marg_bird.RDS")
marg_bird$fiveyr<-(marg_bird$year%/%5)*5
marg_bird<-marg_bird%>%group_by(POSTALCODE,fiveyr)%>%mutate(threeyr_rich=mean(species_richness))
marg_bird<-marg_bird%>%group_by(POSTALCODE,fiveyr)%>%mutate(threeyr_chk=sum(n_checklists))
df<-subset(marg_bird, threeyr_chk>=17)#threshold for sufficient effort (only ~10%)
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
df<-as.data.frame(df)
m<-gam(log(df$threeyr_rich+1)~cmg06_10+cmg06_11+cmg06_12+cmg06_13+s(year)+df$CMANAME, data=df)#instability, deprivation, dependency, ethnic concentration

summary(m)
poly_all<-poly
poly<-poly[,c("CMAUID")]
poly<-spTransform(poly,CRS("+proj=longlat +datum=WGS84"))
df_postal<-df%>%group_by(POSTALCODE)%>%summarize_if(is.numeric, mean, na.rm=T)
df_postal$longitude<-pccf$LONG[match(df_postal$POSTALCODE, pccf$PC)]
df_postal$latitude<-pccf$LAT[match(df_postal$POSTALCODE, pccf$PC)]
df_postal<-subset(df_postal, is.nan(latitude)==F)
coordinates(df_postal)<-~longitude+latitude
crs(df_postal)<-"+proj=longlat +datum=WGS84"
da<-st_make_valid(da)


for (i in 1:nrow(poly))
{
  within<-which(over(df_postal,poly[i,])!='<NA>')
  da_crop<-st_join(da,st_as_sf(df_postal[within,],crs='+proj=longlat +datum=WGS84 +no_defs'), left=F)
 
pdf(paste0('../plots/bird_richness_',gsub("\\/.*", "",poly_all$CMANAME[i]),'.pdf'))
par(mar=c(0.5,0.5,0.5,0.5))
par(oma=c(0,0,1,4))
layout(t(1:2), widths=c(6,1))
plot(da_crop['threeyr_rich'],pal=viridis, main="Bird Richness", key.pos=4, nbreaks=10)
plot(poly[i,], add=T)
dev.off()

pdf(paste0('../plots/Deprivation_',gsub("\\/.*", "",poly_all$CMANAME[i]),'.pdf'))
par(mar=c(0.5,0.5,0.5,0.5))
par(oma=c(0,0,1,4))
plot(da_crop['cmg06_11'],main=gsub("\\/.*","",poly_all$CMANAME[i]),pal=viridis,key.pos=4, nbreaks=10)
plot(poly[i,],add=T)
par(las=0)
mtext(side=4, "Deprivation", line=3)
dev.off()


pdf(paste0('../plots/Ethnic_concentration_',gsub("\\/.*", "",poly_all$CMANAME[i]),'.pdf'))
par(mar=c(0.5,0.5,0.5,0.5))
par(oma=c(0,0,1,4))
plot(da_crop['cmg06_13'],main=gsub("\\/.*","",poly_all$CMANAME[i]),pal=viridis,key.pos=4, nbreaks=10)
plot(poly[i,],add=T)
par(las=0)
mtext(side=4, "Ethnic_concentration_", line=3)
dev.off()


pdf(paste0('../plots/Instability',gsub("\\/.*", "",poly_all$CMANAME[i]),'.pdf'))
par(mar=c(0.5,0.5,0.5,0.5))
par(oma=c(0,0,1,4))
plot(da_crop['cmg06_10'],main=gsub("\\/.*","",poly_all$CMANAME[i]),key.pos=4, nbreaks=10, pal=viridis)
plot(poly[i,],add=T)
par(las=0)
mtext(side=4, "Instability", line=3)
dev.off()

pdf(paste0('../plots/Dependency_',gsub("\\/.*", "",poly_all$CMANAME[i]),'.pdf'))
par(mar=c(0.5,0.5,0.5,0.5))
par(oma=c(0,0,1,4))
plot(da_crop['cmg06_12'],main=gsub("\\/.*","",poly_all$CMANAME[i]),pal=viridis,key.pos=4, nbreaks=10)
plot(poly[i,],add=T)
par(las=0)
mtext(side=4, "Dependency", line=3)
dev.off()
}
