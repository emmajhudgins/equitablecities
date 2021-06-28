# determine % and number of Postal codes in a given CMA covered by >=17 eBird checklists
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
write_sf(poly, '../data/CMA_shp.shp')

data_db<-list()
completeness<-list()
for (i in 1:nrow(poly))
{
  postal_CMA<-readRDS(paste0('../data/postal_CMA_',gsub("\\/.*", "",poly$CMANAME[i]), ".RDS"))
  data_db<-read.csv(paste0("../data/CMA_summary_", gsub("\\/.*", "",poly$CMANAME[i]),".csv"))
  data_db<-subset(data_db, n_checklists>=17)
  completeness[[i]]<-data_db%>%group_by(year)%>%summarise_at('POSTALCODE', n_distinct)
  completeness[[i]]$percent<-completeness[[i]]$POSTALCODE/length(unique(postal_CMA$POSTALCODE))
}

saveRDS(completeness, file="../output/completeness_by_CMA.RDS")

