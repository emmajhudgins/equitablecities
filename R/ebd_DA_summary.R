# Aggregate eBird checklist data by Canadian Census Block Divisions
# written by Emma J. Hudgins April 16, 2021
# for questions email emma.hudgins@carleton.ca

# require(devtools)
# install_github("CornellLabofOrnithology/auk")
require(sf)
require(auk)
require(dplyr)
require(here)
setwd(here())


poly <- read_sf("../raw/lcma000b16a_e/lcma000b16a_e.shp") #2016 CMA and CA boundaries from Canadian Census (Statistics Canada, Downloaded April 14)
poly<-subset(poly, CMATYPE=="B") # only CMAs


dba<- read_sf('../raw/lda_000b16a_e/lda_000b16a_e.shp') #dissemination area polygons for 2016 Census downloaded July 2 2021


for (i in 1:nrow(poly))
{
  dba_CMA<-subset(dba, dba$CMANAME==poly$CMANAME[i])
  data<-readRDS(paste0('../data/ebd_in_poly_',i,'.RDS'))
  data<- data %>% 
    st_as_sf( coords = c("longitude", "latitude"), crs = 4326)
  data<- st_transform(data, st_crs(poly))
  data<-data%>%st_join(dba_CMA)
  saveRDS(data, file=paste0('../data/db_CMA_', gsub("\\/.*", "",poly$CMANAME[i]), '.RDS'))
  
  data$year<-gsub("-.*", "",data$observation_date)
  data<-subset(data, year>=2007)
  
  data_sum<-data%>%group_by(DAUID, year)%>%summarise_at(c('checklist_id', 'scientific_name'),n_distinct)
  colnames(data_sum)[3:4]<-c("n_checklists", "species_richness")
  write.csv(as.data.frame(data_sum)[,1:4], paste0("../data/CMA_DA_summary_", gsub("\\/.*", "",poly$CMANAME[i]),".csv"), row.names=F)
}

