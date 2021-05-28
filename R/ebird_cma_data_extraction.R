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

#Filters: all species, region==Canada, timespan== Jan 2007-Feb 2021, excluding unvetted data and sensitive species https://ebird.org/data/download/ebd

poly <- read_sf("../raw/lcma000b16a_e/lcma000b16a_e.shp") #2016 CMA and CA boundaries from Canadian Census (Statistics Canada, Downloaded April 14)
poly<-subset(poly, CMATYPE=="B") # only CMAs

for (i in 1:nrow(poly)){
  
poly_sub<-poly[i,]
f_out <- paste0("../data/ebd_CMA_", i,".txt")
auk_ebd("../raw/ebd_CA_relFeb-2021.txt") %>%  # very large file current to Feb 15 2021

  # define filters
  auk_bbox(poly_sub) %>%
  auk_complete() %>%
  # compile and run filters
  auk_filter(f_out, overwrite=T)

ebd <- read_ebd(paste0("../data/ebd_CMA_", i,".txt")) 

# convert to sf object
ebd_sf <- ebd %>% 
  select(longitude, latitude) %>% 
  st_as_sf( coords = c("longitude", "latitude"), crs = 4326)

# put polygons in same crs
poly_ll <- st_transform(poly_sub, crs = st_crs(ebd_sf))

# identify points in polygon
in_poly <- st_within(ebd_sf, poly_ll, sparse = FALSE)

# subset data frame
ebd_in_poly <- ebd[in_poly[, 1], ]

saveRDS(ebd_in_poly, paste0("ebd_in_poly_",i,".RDS"))
}