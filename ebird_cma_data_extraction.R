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

f_out <- "ebd_CMA.txt"
auk_ebd("~/ebd_CA_relFeb-2021.txt") %>%
  # define filters
  auk_bbox(poly) %>%
  auk_complete() %>%
  # compile and run filters
  auk_filter(f_out, overwrite=T)

ebd <- read_ebd("ebd_CMA.txt")


# convert to sf object
ebd_sf <- ebd %>% 
  select(longitude, latitude) %>% 
  st_as_sf( coords = c("longitude", "latitude"), crs = 4326)

# put polygons in same crs
poly_ll <- st_transform(poly, crs = st_crs(ebd_sf))

# identify points in polygon
in_poly <- st_within(ebd_sf, poly_ll, sparse = FALSE)

# subset data frame
ebd_in_poly <- ebd[in_poly[, 1], ]

par(mar = c(0, 0, 0, 0))
plot(poly %>% st_geometry(), col = "grey40", border = NA)
plot(ebd_sf, col = "black", pch = 19, cex = 0.5, add = TRUE)
plot(ebd_sf[in_poly[, 1], ], 
     col = "forestgreen", pch = 19, cex = 0.5, 
     add = TRUE)
legend("top", 
       legend = c("All observations", "After spatial subsetting"), 
       col = c("grey40", "forestgreen"), 
       pch = 19,
       bty = "n",
       ncol = 2)

saveRDS(ebd_in_poly, "ebd_in_poly.RDS")
saveRDS(ebd_in_poly@data,'ebd_CMA_crop.csv')