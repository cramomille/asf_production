
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

fond <- asf_fond(geom, tabl, by = "IRISF_CODE", maille = "IRISrD_CODE", keep = "IRISrD_LIB")
fond <- asf_drom(fond)

fond_01 <- asf_simplify(fond, keep = 0.1)
fond_05 <- asf_simplify(fond, keep = 0.5)
fond_09 <- asf_simplify(fond, keep = 0.9)

z <- asf_zoom(f = fond_09,
              places = c("5", "4", "Dijon", "Reims", "Rouen"),
              nb_cols = 7)


x <- st_read("input/asf_0702/pauvre.gpkg")
x <- st_drop_geometry(x)
c <- asf_fondata(f = fond_01, d = x, z = z[[1]], by = "IRISrD_CODE")

# c <- st_transform(c, crs = 4326)
# sum(nchar(c$tx_menpauvre), na.rm = TRUE) - sum(nchar(x$tx_menpauvre), na.rm = TRUE)
# st_write(c, "planche_0702_01.geojson")


# CARTO ----
pal <- c("#ea5289", 
         "#f087b0", 
         "#f8c8d8", 
         "#dae0e3", 
         "#b7c6cf", 
         "#95a6b1")

mf_map(c, var = "tx_menpauvre", type = "choro", 
       breaks = quantile(c$tx_menpauvre, probs = c(0, 1/6, 2/6, 3/6, 4/6, 5/6, 1), na.rm = TRUE),
       pal = rev(pal), border = NA)
mf_label(z[[2]], var = "label")



pal <- c("#f59c00", 
         "#ffce44", 
         "#ffe596", 
         "#dae0e3", 
         "#b7c6cf", 
         "#95a6b1")

mf_map(c, var = "interd", type = "choro", 
       breaks = quantile(c$interd, probs = c(0, 1/6, 2/6, 3/6, 4/6, 5/6, 1), na.rm = TRUE),
       pal = rev(pal), border = NA)
mf_label(z[[2]], var = "label")


# pal <- c(
#   "low_low" =       "#feebdc", 
#   "middle_low" =    "#ffe596", 
#   "high_low" =      "#ffce44", 
#   
#   "low_middle" =    "#f8c8d8", 
#   "middle_middle" = "#f8b999", 
#   "high_middle" =   "#f07f3c", 
#   
#   "low_high" =      "#f087b0", 
#   "middle_high" =   "#e84250", 
#   "high_high" =     "#a6393a"
# )
# 
# mf_map(c, "class", type = "typo", pal = pal, border = NA, 
#        val_order = c("low_low", "middle_low", "high_low", 
#                      "low_middle", "middle_middle", "high_middle", 
#                      "low_high", "middle_high", "high_high"))



d <- c[c$tx_menpauvre <= median(c$tx_menpauvre, na.rm = TRUE), ]
e <- c[c$tx_menpauvre > median(c$tx_menpauvre, na.rm = TRUE), ]

class_1 <- function(x) {
  cut(x,
      breaks = c(min(x, na.rm = TRUE), 16.8, 22.6, max(x, na.rm = TRUE)),
      labels = c("l", "m", "h"),
      include.lowest = TRUE)
}

class_2 <- function(x) {
  cut(x,
      breaks = c(min(x, na.rm = TRUE), 2.967, 3.225, 3.786, max(x, na.rm = TRUE)),
      labels = c("s", "l", "m", "h"),
      include.lowest = TRUE)
}


e$typo_pauvr <- class_1(e$tx_menpauvre)
e$typo_inter <- class_2(e$interd)

e$typo_class <- paste0(e$typo_inter, e$typo_pauvr) 

pal <- c(
  "sl" = "#feebdc",
  "sm" = "#f8c8d8",
  "sh" = "#f087b0",
  
  "ll" = "#feebdc", 
  "lm" = "#f8c8d8", 
  "lh" = "#f087b0", 
  
  "ml" = "#ffe596", 
  "mm" = "#f8b999", 
  "mh" = "#e84250", 
  
  "hl" = "#ffce44", 
  "hm" = "#f07f3c", 
  "hh" = "#a6393a"
)

mf_map(d, "tx_menpauvre", 
       type = "choro",
       breaks = quantile(d$tx_menpauvre, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
       pal = c("#95a6b1", "#b7c6cf", "#dae0e3"),
       border = NA,
       leg_pos = "topleft")

mf_map(e, "typo_class", 
       type = "typo", 
       pal = pal, 
       border = NA, 
       leg_pos = "topright",
       val_order = c("sl", "sm", "sh", "ll", "lm", "lh", "ml", "mm", "mh", "hl", "hm", "hh"),
       add = TRUE)
