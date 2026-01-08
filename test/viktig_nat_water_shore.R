library(arrow)
library(dplyr)
library(sf)
path<-"P:/15220700_gis_samordning_2022_(marea_spare_ecogaps)/Sylvie/recereation_encroachment_accounts/data/viktig_natur"

lakes <- open_dataset(paste0(path, "/lakes_orig.parquet")) |>
  collect()

lakes$geometry <- st_as_sfc(lakes$geometry)
buffer <- open_dataset(paste0(path, "/lakes_buff_100m.parquet")) |>
  collect()
buffer$geometry <- st_as_sfc(buffer$geometry, EWKB = TRUE)


lakes <- st_as_sf(lakes)
st_crs(lakes) <- 25833
buffer <- st_as_sf(buffer)
st_crs(buffer) <- 25833
buffer_only <- st_difference(buffer, lakes)
buffer_only <- buffer_only[, 0]
buffer_only$feat<-"lake"


#rivers
rivers <- open_dataset(paste0(path, "/rivers_main_100m_buff_diss.parquet")) |>
  collect()
rivers$geometry <- st_as_sfc(rivers$geometry, EWKB = TRUE)
rivers <- st_as_sf(rivers)
st_crs(rivers) <- 25833


## remove lake lines from rivers
rivers <- st_difference(rivers, buffer)
rivers <- rivers[, 0]
rivers$feat<-"river"

all<-rbind(rivers,buffer_only)

## lulc
corine <- open_dataset("test_corine.parquet") |>
  collect()
corine$geometry <- st_as_sfc(corine$geometry, EWKB = TRUE)
corine <- st_as_sf(corine)
st_crs(corine)<-32632


corine <- st_transform(corine, st_crs(all))

corine <- st_crop(corine, st_bbox(buffer_only))
buffer_clc <- st_intersection(
  all,
  corine["clc18_kode"]
)

#clc <- factor(buffer_clc$clc18_kode)

# plot(
#   st_geometry(buffer_clc),
#   col = clc,
#   border = NA
# )
# 
# legend(
#   "topright",
#   legend = levels(clc),
#   fill = seq_along(levels(clc)),
#   title = "CLC 2018"
# )
# 
st_write(buffer_clc,"water_100m_lulc.gpkg")