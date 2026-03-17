
#                                               ASF - SCRIPT DE LA PLANCHE 0407                                     
#                                   REPARTITION DES PERSONNES NEES DANS UN DROM
#
#                                                                antoine beroud
#                                                                     mars 2026

library(sf)
library(asf)
library(mapsf)


data <- read.csv("input/asf_0407/irisres_dromnai_secret.csv")
data <- data[, -1]
data$POP_TOT <- rowSums(data[, c(2:109)], na.rm = TRUE)

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

x <- x[, c(1, 110, 98:101, 103)]

x$pct_drom <- round((x$DEPT_NAIS_971 + x$DEPT_NAIS_972 + x$DEPT_NAIS_973 + 
                     x$DEPT_NAIS_974 + x$DEPT_NAIS_976) / x$POP_TOT * 100, 3)

x$pct_971 <- round(x$DEPT_NAIS_971 / x$POP_TOT * 100, 3)
x$pct_972 <- round(x$DEPT_NAIS_972 / x$POP_TOT * 100, 3)
x$pct_973 <- round(x$DEPT_NAIS_973 / x$POP_TOT * 100, 3)
x$pct_974 <- round(x$DEPT_NAIS_974 / x$POP_TOT * 100, 3)
x$pct_976 <- round(x$DEPT_NAIS_976 / x$POP_TOT * 100, 3)

pal <- asf_palette(type = "div", nb = 6)

y <- st_transform(x, crs = 4326)

st_write(y, "planche_0407_01.geojson")
