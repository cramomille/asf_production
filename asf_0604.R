
#                                               ASF - SCRIPT DE LA PLANCHE 0604
#                                                        EMPLOYEES ET OUVRIERES
#
#                                                                antoine beroud
#                                                           antonine ribardiere

library(sf)
library(asf)
library(mapsf)


# CASD ------------------------------------------------------------------------
# Fond geographique
mar <- asf_mar(
  md = "iris_xxxx", 
  ma = "iris_r5", 
  geom = TRUE, 
  dir = "input/mar/"
)

tabl <- mar$tabl
geom <- mar$geom

fond <- asf_fond(geom, tabl, by = "IRISF_CODE", maille = "IRISr5_CODE", keep = "IRISr5_LIB")

fond_01 <- asf_simplify(fond, keep = 0.1)

x <- st_read("input/asf_0604/gs5_gs6_CS3.gpkg")

x <- st_drop_geometry(x)
c <- asf_fondata(f = fond_01, d = x, by = "IRISrD_CODE")

c <- st_transform(c, crs = 4326)
sum(nchar(c$tx_menpauvre), na.rm = TRUE) - sum(nchar(x$tx_menpauvre), na.rm = TRUE)
st_write(c, "planche_0702_01.geojson")



