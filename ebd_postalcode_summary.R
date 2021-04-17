# crop eBird EBD for Canada to CMAs defined by 2016 Census
# written by Emma J. Hudgins April 14, 2021
# adapted from https://strimas.com/post/extracting-ebird-data-polygon/
# for questions email emma.hudgins@carleton.ca

# require(devtools)
# install_github("CornellLabofOrnithology/auk")
require(sf)
require(auk)
require(dplyr)
require(here)
setwd(here())

## eBird data extracted April 13, 2021,

#Filters: all species, region==Canada, timespan== Jan 2007-April 2021, excluding unvetted data https://ebird.org/data/download/ebd

poly <- read_sf("lcma000b16a_e/lcma000b16a_e.shp") #2016 CMA and CA boundaries from Canadian Census (Statistics Canada, Downloaded April 14)
poly<-subset(poly, CMATYPE=="B") # only CMAs

cdb<- read_sf('ldb_000b16a_e/ldb_000b16a_e.shp') #census dissemination block (smallest unit)
cdb<-subset(cdb, CMATYPE=="B") #census dissemination block (smallest unit)
cdb<- st_transform(cdb, crs = 4326)
cdb<-cdb[,c("CMANAME","DBUID", "DBRPLAMX", "DBRPLAMY", "geometry")]
for (i in 1:nrow(poly))
{
  cdb_sub<-subset(cdb, CMANAME==poly$CMANAME[i])
  data<-readRDS(paste0('ebd_in_poly_',i,'.RDS'))
  data<- data %>% 
    st_as_sf( coords = c("longitude", "latitude"), crs = 4326)
  data<-data%>%st_join(cdb_sub[,])
  saveRDS(data, file=paste0('cdb_CMA_', i, '.RDS'))

data$year<-gsub("-.*", "",data$observation_date)
data<-subset(data, year>=2007)

data_sum<-data%>%group_by(DBUID, year)%>%summarise_at(c('checklist_id', 'scientific_name'),n_distinct)
colnames(data_sum)[3:4]<-c("n_checklists", "species_richness")
write.csv(as.data.frame(data_sum)[,1:4], paste0("CMA_summary_", gsub("\\/.*", "",poly$CMANAME[i]),".csv"), row.names=F)
}



