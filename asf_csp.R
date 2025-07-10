
#                                        ASF - SCRIPT DE LA PLANCHE SUR LES CSP
#
#                                                                antoine beroud
#                                                                  jean riviere

library(sf)
library(asf)
library(mapsf)

###############################################################################
########################################################## FONDS D'ALIETTE ROUX

# Lecture des fichiers
mar <- asf_mar(maille = "irisrd")

tabl <- mar$tabl
geom <- mar$geom

# Agregation des iris en irisrd
fond <- asf_fond(f = geom, 
                 t = tabl, 
                 by = "IRISF_CODE", 
                 maille = "IRISrD_CODE", 
                 keep = "DEP")

# Repositionnement des geometries des DROM
fond <- asf_drom(fond, id = "IRISrD_CODE")


###############################################################################
######################################################### NETTOYAGE DES DONNEES

# Telechargement des donnees 
data <- read.csv2("input/asf_csp/TableTypo15.csv")
data <- data[, c(1, ncol(data))]

# Ajout des zeros manquants dans les identifiants
data$IRISr <- ifelse(nchar(data$IRISr) == 8,
                     paste0("0", data$IRISr),
                     data$IRISr)


###############################################################################
######################################## UTILISATION DES AUTRES FONCTIONS D'ASF

# Creation des zooms
z <- asf_zoom(fond,
              places = c("Paris", "Marseille", "Lyon", "Toulouse", "Nantes", "Montpellier",
                         "Bordeaux", "Lille", "Rennes", "Reims", "Dijon","Strasbourg",
                         "Angers", "Grenoble", "Clermont-Ferrand", "Tours", "Perpignan",
                         "Besancon", "Rouen", "La Rochelle", "Le Havre", "Nice", "Mulhouse"),
              r = 10000)

zoom <- z$zooms
label <- z$labels
point <- z$points

# Simplification des geometries du fond de carte principal
fond <- asf_simplify(fond, keep = 0.1)

# Jointure entre le fond et les donnees
fondata <- asf_fondata(f = fond,
                       z = zoom,
                       d = data,
                       by.x = "IRISrD_CODE",
                       by.y = "IRISr")

# Recuperation des limites departementales
dep <- asf_borders(fond,
                   by = "DEP", 
                   keep = 0.05)

palette <- c("01" = "#94282f",
             "02" = "#e40521",
             "03" = "#f07f3c",
             "04" = "#f7a941",
             "05" = "#ffd744",
             "06" = "#ffeea4",
             "07" = "#bbd043",
             "08" = "#6cbe99",
             "09" = "#bee2e9",
             "10" = "#86c2eb",
             "11" = "#04a64b",
             "12" = "#2581c4",
             "13" = "#aad29a",
             "14" = "#8779b7",
             "15" = "#554596"
             )

mf_map(fondata,
       var = "clust15", 
       type = "typo",
       pal = palette,
       border = NA)

mf_map(dep, 
       col = "white", 
       lwd = 1, 
       add = TRUE)

mf_map(point,
       add = TRUE)

mf_label(label, 
         var = "label", 
         col = "#000000", 
         font = 1)

mf_label(point, 
         var = "label", 
         col = "#000000", 
         font = 1)

# fondata$COM_CODE <- substr(fondata$IRIS_CODE, 1, 5)
# 
# com <- aggregate(fondata, 
#                  by = list(fondata$COM_CODE),
#                  FUN = function(x) x[1])
# 
# mf_map(com, 
#        col = NA,
#        border = "red",
#        lwd = 1, 
#        add = TRUE)

###############################################################################
#################################################################### GRAPHIQUES

tabl <- tabl[!duplicated(tabl$IRISrD_CODE), ]

tmp <- merge(data, tabl[, c(3, 16)], by.x = "IRISr", by.y = "IRISrD_CODE")
tmp$clust15 <- as.character(tmp$clust15)

paramx <- c("1", "2", "3", "4", "5", "6",
            "7", "8", "11", "13",
            "9", "10",  "12",
            "14", "15")

paramy <- c("0", "1", "2", "3", "4", "5")

palette <- c("#554596","#8779b7",
             "#2581c4","#86c2eb","#bee2e9",
             "#aad29a","#04a64b","#6cbe99","#bbd043",
             "#ffeea4","#ffd744","#f7a941","#f07f3c","#e40521","#94282f")

asf_plot_typo(d = tmp,
              vars = "clust15",
              typo = "TAAV2017", 
              order.v = paramx, 
              order.t = paramy, 
              pal = palette
              )

asf_plot_vars(d = tmp, 
              vars = "clust15", 
              typo = "TAAV2017",
              order.v = paramx, 
              order.t = paramy
              )
