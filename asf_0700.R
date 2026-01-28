
#                                               ASF - SCRIPT DE LA PLANCHE 0700
#                                          TYPOLOGIE SUR LES DECILES DE REVENUS
#
#                                                                antoine beroud
#                                                           antonine ribardiere

library(sf)
library(asf)
library(mapsf)


# IMPORT DU FOND D'ALIETTE ROUX -----------------------------------------------
# Lecture des fichiers
mar <- asf_mar(md = "com_xxxx", ma = "com_r2", geom = TRUE, dir = "input/mar/")

tabl <- mar$tabl
geom <- mar$geom

# Agregation des iris en comr
fond <- asf_fond(f = geom, 
                 t = tabl, 
                 by = "COMF_CODE", 
                 maille = "COMr2_CODE", 
                 keep = "DEP")

fond_carte <- asf_drom(fond)


# IMPORT ET NETTOYAGE DU TABLEAU DE DONNEES -----------------------------------
data <- read.csv("C:/Users/Antoine/Desktop/casd/export/TREVPOP_export_04/donnees/cah_decile.csv")
summary(nchar(data$COM))

data$id_tmp <- substr(data$COM, 1, 5)
data <- data[, c(2,3,7)]

fond$id_tmp <- substr(fond$COMr2_CODE, 1, 5)
fond_carte$id_tmp <- substr(fond_carte$COMr2_CODE, 1, 5)

z <- asf_zoom(f = fond, 
              places = c("Paris", "Marseille", "Lyon", "Toulouse", 
                         "Montpellier", "Lille", "Rennes", "Reims", 
                         "Dijon", "Angers", "Grenoble", "Tours", 
                         "Perpignan", "La Rochelle"  , "Troyes"), 
              coords = c(0.545, 46.815,
                         2.068, 47.221,
                         3.797, 47.815,
                         6.236, 46.187,
                         -61.068, 14.607), 
              labels = c("Chatellerault", "Vierzon", "Chablis", "Annemasse", "Fort-de-France"), 
              epsg = c("20" = "5490"),
              r = 20000, 
              f_ref = fond_carte)

zoom <- z$zooms
label <- z$labels

# Repositionnement des geometries des DROM et simplification des geometries
fond_carte_simply <- asf_simplify(fond_carte, keep = 0.1)

fondata <- asf_fondata(fond_carte_simply, zoom, data[, -1], by = "id_tmp")


# CREATION DE CARTES ----------------------------------------------------------
dep <- asf_borders(fond_carte, by = "DEP", keep = 0.025)

# Definition d'une palette de couleurs
pal <- c(
  "1" = "#fddaac",
  "2" = "#f8c8d0",
  "3" = "#fff7b2",
  "4" = "#f08590",
  "5" = "#d9e6b1",
  "6" = "#00a75d",
  "7" = "#007b3d",
  "8" = "#8ec89a",
  "9" = "#e50040"
)

mf_map(fondata,
       var = "CLASS9",
       type = "typo",
       pal = pal,
       border = NA)

mf_map(dep, col = "#fff", add = TRUE)

mf_label(label,
         var = "label",
         col = "#000000",
         cex = 0.3)





tvma <- read.csv("C:/Users/Antoine/Desktop/casd/export/TREVPOP_export_06/donnees/cah_tvma.csv")
tvma <- tvma[, c(2,3)]

fondtvma <- merge(fondata, tvma, by = "id_tmp", all = TRUE)

pal_r <- c("1" = "#fadceb","2" = "#f5b5d2","3" = "#f088b6","4" = "#ea5297")
pal_m <- c("1" = "#fffbdc","2" = "#fff7b2","3" = "#fff482","4" = "#fff042")
pal_p <- c("1" = "#d4edfc","2" = "#a1daf8","3" = "#5bc5f2","4" = "#00b1eb")

zoom_r <- fondtvma[grepl("6|7|8", fondtvma$CLASS9), ]
zoom_m <- fondtvma[grepl("1|3|5", fondtvma$CLASS9), ]
zoom_p <- fondtvma[grepl("9|4|2", fondtvma$CLASS9), ]

mf_map(zoom_r, 
       var = "CLASS4", 
       type = "typo", 
       pal = pal_r, 
       border = NA)

mf_map(zoom_m, 
       var = "CLASS4", 
       type = "typo", 
       pal = pal_m, 
       border = NA, 
       add = TRUE)

mf_map(zoom_p, 
       var = "CLASS4", 
       type = "typo", 
       pal = pal_p, 
       border = NA, 
       add = TRUE)

mf_label(label, 
         var = "label", 
         col = "#000000", 
         cex = 0.3)
