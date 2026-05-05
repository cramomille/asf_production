
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
  md = "com_xxxx", 
  ma = "com_r5", 
  geom = TRUE, 
  dir = "input/mar/"
)

tabl <- mar$tabl
geom <- mar$geom

fond <- asf_fond(geom, tabl, by = "COMF_CODE", maille = "COMr5_CODE", keep = "COMr5_LIB")

fond_s <- asf_simplify(fond, keep = 0.15)

x <- st_read("input/asf_0604/gs5_gs6_CS3.gpkg")
x <- st_drop_geometry(x)

c <- asf_fondata(f = fond_s, d = x, by = "COMr5_CODE")
c <- st_transform(c, crs = 4326)

sum(nchar(c$RESI_CS52), na.rm = TRUE) - sum(nchar(x$RESI_CS52), na.rm = TRUE)

st_write(c, "planche_0604_015.geojson")

# Palette de couleurs
# "#00a183"
# "#8ccaae"
# "#cce5d8"
# "#fbceb4"
# "#f28d65"
# "#dc0d15"