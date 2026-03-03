
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
              # coords = c(2.584, 49.207), labels = "Senlis", 
              nb_cols = 7, r = 20000)


x <- st_read("input/asf_0702/pauvre.gpkg")
x <- st_drop_geometry(x)

c <- asf_fondata(f = fond_05, z = z[[1]], d = x, by = "IRISrD_CODE")

sum(nchar(c$tx_menpauvre), na.rm = TRUE) - sum(nchar(x$tx_menpauvre), na.rm = TRUE)


# CARTO ----
pal <- c("#e94256", 
         "#f08590", 
         "#f8c7c8", 
         "#dadada", 
         "#9d9d9c", 
         "#575756")

mf_map(c, var = "tx_menpauvre", type = "choro", 
       breaks = quantile(c$tx_menpauvre, probs = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1), na.rm = TRUE),
       pal = rev(pal), border = NA)
mf_label(z[[2]], var = "label")



pal <- c("#009c79", 
         "#7dc4a3", 
         "#c0e0cd", 
         "#dadada", 
         "#9d9d9c", 
         "#575756")

mf_map(c, var = "interd", type = "choro", 
       breaks = quantile(c$interd, probs = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1), na.rm = TRUE),
       pal = rev(pal), border = NA)
mf_label(z[[2]], var = "label")


pal <- c(
  "low_low" =       "#e0d6c4", 
  "middle_low" =    "#7dc4a3", 
  "high_low" =      "#009c79", 
  
  "low_middle" =    "#f08590", 
  "middle_middle" = "#aa7d77", 
  "high_middle" =   "#006757", 
  
  "low_high" =      "#e94256", 
  "middle_high" =   "#95254c", 
  "high_high" =     "#3d3b42"
)

mf_map(c, "class", type = "typo", pal = pal, border = NA, 
       val_order = c("low_low", "middle_low", "high_low",
                     "low_middle", "middle_middle", "high_middle",
                     "low_high", "middle_high", "high_high"))


na <- c[is.na(c$tx_menpauvre) | c$tx_menpauvre == 0.0, ]
mf_map(na)

na <- c[is.na(c$interd), ]
mf_map(na)
