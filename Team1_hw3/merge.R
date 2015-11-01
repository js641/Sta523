library(data.table)
library(rgdal)
library(magrittr)
library(dplyr)

nyc = fread("/home/vis/cr173/Sta523/data/nyc/nyc_311.csv") %>% tbl_df()
nyc = nyc[, c(10, 11, 12, 13, 14, 15, 25)]
load("/home/vis/cr173/Sta523/data/nyc/pluto/pluto.Rdata")
load("/home/vis/cr173/Sta523/data/nyc/intersections/intersections.Rdata")

nyc$Address = nyc$Incident.Address
nyc$Stree1 = nyc$Intersection.Street.1
nyc$Street2 = nyc$Intersection.Street.2

# combine = merge(nyc, pluto, by = "Address", all.x=T)
# combine = merge(combine, data, by = c("Stree1", "Street2"), all.x=T)

combine = left_join(nyc, pluto, by="Address")
combine$longitude = combine$x
combine$latitude = combine$y

combine = left_join(combine, data, by=c("Stree1", "Street2"))
combine$longitude = combine$longitude.x
combine$longitude[is.na(combine$longitude)==T] = 
  combine$longitude.y[is.na(combine$longitude)==T]
combine$latitude = combine$latitude.x
combine$latitude[is.na(combine$latitude)==T] = 
  combine$latitude.y[is.na(combine$latitude)==T]


combine = combine[is.na(combine$latitude)==F &
                    is.na(combine$longitude)==F, 
                  c(-15, -16, -18, -19)]

save(combine, file="~/jpr26/Sta523/Team1_hw3/combine.Rdata")