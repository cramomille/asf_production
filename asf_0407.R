
#                                               ASF - SCRIPT DE LA PLANCHE 0407                                     
#                                                    GEOGRAPHIE DES DROMIEN.NES
#
#                                                                antoine beroud
#                                                                     mars 2026

library(sf)
library(asf)
library(mapsf)


data <- read.csv("input/asf_0407/irisres_dromnai_secret.csv")
data <- data[, -1]

mar <- asf_mar(
  md = "iris_xxxx", 
  ma = "iris_r2", 
  geom = TRUE, 
  dir = "input/mar/"
)

tabl <- mar$tabl
geom <- mar$geom

fond <- asf_fond(geom, tabl, by = "IRISF_CODE", maille = "IRISrD_CODE")
fond_01 <- asf_simplify(fond, keep = 0.1)

x <- asf_fondata(f = fond_01, d = data, by = "IRISrD_CODE")
y <- st_transform(x, crs = 4326)

st_write(y, "planche_0407_01.geojson")