# determine % of DBUIDs covered by >=17 eBird checklists
# written by Emma J. Hudgins April 16, 2021
# for questions email emma.hudgins@carleton.ca

# require(devtools)
# install_github("CornellLabofOrnithology/auk")
require(sf)
require(auk)
require(dplyr)
require(here)
setwd(here())

poly <- read_sf("lcma000b16a_e/lcma000b16a_e.shp") #2016 CMA and CA boundaries from Canadian Census (Statistics Canada, Downloaded April 14)
poly<-subset(poly, CMATYPE=="B") # only CMAs

cdb<- read_sf('ldb_000b16a_e/ldb_000b16a_e.shp') #census dissemination block (smallest unit)
cdb<-subset(cdb, CMATYPE=="B") #census dissemination block (smallest 

data_db<-list()
completeness<-list()
for (i in 1:nrow(poly))
{
  cdb_sub<-subset(cdb, CMANAME==poly$CMANAME[i])
  data_db<-read.csv(paste0("CMA_summary_", gsub("\\/.*", "",poly$CMANAME[i]),".csv"))
  data_db<-subset(data_db, n_checklists>=17)
  completeness[[i]]<-data_db%>%group_by(year)%>%summarise_at('DBUID', n_distinct)
  completeness[[i]]$percent<-completeness[[i]]$DBUID/length(unique(cdb_sub$DBUID))
}

saveRDS(completeness, file="completeness_by_CMA.RDS")

