
#                                               ASF - SCRIPT DE LA PLANCHE 0702
#                                                       PAUVRETES ET INEGALITES
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
  ma = "iris_r2", 
  geom = TRUE, 
  dir = "input/mar/"
)

tabl <- mar$tabl
geom <- mar$geom

fond <- asf_fond(geom, tabl, by = "IRISF_CODE", maille = "IRISrD_CODE")
fond <- asf_drom(fond)

fond_05 <- asf_simplify(fond, keep = 0.5)
fond_09 <- asf_simplify(fond, keep = 0.9)

z <- asf_zoom(f = fond_09,
              places = c("5", "4", "Dijon", "Reims", "Rouen"),
              nb_cols = 7)


x <- st_read("input/asf_0702/pauvre.gpkg")
x <- st_drop_geometry(x)
c <- asf_fondata(f = fond_01, d = x, by = "IRISrD_CODE")


# c <- st_transform(c, crs = 4326)
# 
# sum(nchar(c$tx_menpauvre), na.rm = TRUE) - sum(nchar(x$tx_menpauvre), na.rm = TRUE)
# st_write(c, "planche_0702_01.geojson")
# 
# mf_map(c)



# CARTO ----
pal <- c("#ea5289", 
         "#f087b0", 
         "#f8c8d8", 
         "#dae0e3", 
         "#b7c6cf", 
         "#95a6b1")

mf_map(c, var = "tx_menpauvre", type = "choro", 
       breaks = quantile(c$tx_menpauvre, probs = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1), na.rm = TRUE),
       pal = rev(pal), border = NA)
mf_label(z[[2]], var = "label")



pal <- c("#f59c00", 
         "#ffce44", 
         "#ffe596", 
         "#dae0e3", 
         "#b7c6cf", 
         "#95a6b1")

mf_map(c, var = "interd", type = "choro", 
       breaks = quantile(c$interd, probs = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1), na.rm = TRUE),
       pal = rev(pal), border = NA)
mf_label(z[[2]], var = "label")


pal <- c(
  "low_low" =       "#feebdc", 
  "middle_low" =    "#ffe596", 
  "high_low" =      "#ffce44", 
  
  "low_middle" =    "#f8c8d8", 
  "middle_middle" = "#f8b999", 
  "high_middle" =   "#f07f3c", 
  
  "low_high" =      "#f087b0", 
  "middle_high" =   "#e84250", 
  "high_high" =     "#a6393a"
)

mf_map(c, "class", type = "typo", pal = pal, border = NA, 
       val_order = c("low_low", "middle_low", "high_low",
                     "low_middle", "middle_middle", "high_middle",
                     "low_high", "middle_high", "high_high"))
