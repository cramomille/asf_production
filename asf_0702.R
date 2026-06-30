
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
              places = c("Paris", "Lyon", "Marseille", "Lille", "Toulouse"),
              nb_cols = 5)


x <- st_read("input/asf_0702/pauvre.gpkg")
x <- st_drop_geometry(x)
x <- x[, c(1, 2, 4)]
c <- asf_fondata(f = fond_01, d = x, 
                 z = z[[1]],
                 by = "IRISrD_CODE")

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

quantile(c$tx_menpauvre, probs = c(0, 1/6, 2/6, 3/6, 4/6, 5/6, 1), na.rm = TRUE)
mf_map(c, var = "tx_menpauvre", type = "choro", 
       breaks = c(0, 7.8, 10.4, 13.3, 16.8, 22.6, 72.3),
       pal = rev(pal), border = NA)


pal <- c("#f59c00", 
         "#ffce44", 
         "#ffe596", 
         "#dae0e3", 
         "#b7c6cf", 
         "#95a6b1")

quantile(c$interd, probs = c(0, 1/6, 2/6, 3/6, 4/6, 5/6, 1), na.rm = TRUE)
mf_map(c, var = "interd", type = "choro", 
       breaks = c(1, 2.6, 2.8, 3, 3.2, 3.8, 1500),
       pal = rev(pal), border = NA)



# separation sur la mediane
e <- c[c$tx_menpauvre <= 13.3, ]
p <- c[c$tx_menpauvre > 13.3, ]

# classif des taux de menages pauvres
class_1 <- function(x) {
  cut(x,
      breaks = c(min(x, na.rm = TRUE), 16.8, 22.6, max(x, na.rm = TRUE)), 
      labels = c("l", "m", "h"),
      include.lowest = TRUE)
}

# classif des rapports interdecile
class_2 <- function(x) {
  cut(x,
      breaks = c(min(x, na.rm = TRUE), 3.2, 3.8, max(x, na.rm = TRUE)), # mediane, 75 % = 3.430
      labels = c("l", "m", "h"),
      include.lowest = TRUE)
}

p$typo_pauvr <- class_1(p$tx_menpauvre)
p$typo_inter <- class_2(p$interd)

p$typo_class <- paste0(p$typo_inter, p$typo_pauvr) 

pal <- c(
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

mf_map(e, "tx_menpauvre", 
       type = "choro", 
       breaks = c(0, 7.8, 10.4, 13.3), 
       pal = c("#95a6b1", "#b7c6cf", "#dae0e3"), 
       border = NA, 
       leg_pos = "topleft")

mf_map(p, "typo_class", 
       type = "typo", 
       pal = pal, 
       border = NA, 
       leg_pos = "topright", 
       val_order = c("ll", "lm", "lh", "ml", "mm", "mh", "hl", "hm", "hh"), 
       add = TRUE)




# Jointure entre les deux cartes
e$typo_class <- cut(
  e$tx_menpauvre,
  breaks = c(0, 7.8, 10.4, 13.3),
  include.lowest = TRUE,
  labels = c(1, 2, 3)
)

e$typo_class <- as.character(e$typo_class)

e <- e[, c(1, 3:4, 6)]
p <- p[, c(1, 3:4, 8)]


# Objet final
k <- rbind(e, p)
k <- st_drop_geometry(k)
k <- k[!is.na(k$IRISrD_CODE), ]

k$typo_class[k$typo_class == "NANA"] <- NA
k$typo_class[k$tx_menpauvre == 0] <- NA
k$tx_menpauvre[k$tx_menpauvre == 0] <- NA


m <- asf_fond(geom, tabl, by = "IRISF_CODE", maille = "IRISrD_CODE", keep = "IRISrD_LIB")
m <- asf_simplify(m, keep = 0.1)

m <- asf_fondata(f = m, d = k,
                 by = "IRISrD_CODE")

m <- st_transform(m, crs = 4326)
sum(nchar(m$tx_menpauvre), na.rm = TRUE) - sum(nchar(x$tx_menpauvre), na.rm = TRUE)
st_write(m, "planche_0702.geojson")
