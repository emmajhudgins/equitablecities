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


postal<- read_sf('../data/LDU_fixed.shp') #postal code polygons clipped in QGIS 3 to CMA type B extent in line 14, and ran 'fix geometries' by Emma on June 25

postal<- st_transform(postal, crs(poly))
postal<-st_make_valid(postal)
for (i in 2:nrow(poly))
{
  postal_CMA<-st_filter(postal, poly[i,])
  data<-readRDS(paste0('../data/ebd_in_poly_',i,'.RDS'))
  data<- data %>% 
    st_as_sf( coords = c("longitude", "latitude"), crs = 4326)
  data<- st_transform(data, crs(poly))
  data<-data%>%st_join(postal_CMA)
  saveRDS(data, file=paste0('../data/postal_CMA_', gsub("\\/.*", "",poly$CMANAME[i]), '.RDS'))
  
  data$year<-gsub("-.*", "",data$observation_date)
  data<-subset(data, year>=2007)
  
  data_sum<-data%>%group_by(POSTALCODE, year)%>%summarise_at(c('checklist_id', 'scientific_name'),n_distinct)
  colnames(data_sum)[3:4]<-c("n_checklists", "species_richness")
  write.csv(as.data.frame(data_sum)[,1:4], paste0("../data/CMA_summary_", gsub("\\/.*", "",poly$CMANAME[i]),".csv"), row.names=F)
}

