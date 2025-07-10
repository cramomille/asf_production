
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
mar <- asf_mar()

# Selection des iris
iris <- mar$ar02$sf.irisr.d
iris <- iris[, c(1,2,7)]
colnames(iris) <- c("IRIS_CODE", "IRIS_LIB", "P21_POP", "geometry")
st_geometry(iris) <- "geometry"

# Repositionnement des geometries des DROM
fond <- asf_drom(iris, id = "IRIS_CODE")


###############################################################################
######################################################### NETTOYAGE DES DONNEES

# Telechargement des donnees 
data <- read.csv2("input/TableTypo15.csv")
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
                       by.x = "IRIS_CODE",
                       by.y = "IRISr")

# Creation des limites departementales
dep <- fond
dep$DEP_CODE <- substr(dep$IRIS_CODE, 1, 2)
dep <- asf_borders(dep,
                   by = "DEP_CODE", 
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
mar <- asf_mar(sf = FALSE)

# Telechargement des donnees 
data <- read.csv2("input/TableTypo15.csv")
data <- data[, c(1, ncol(data))]

# Ajout des zeros manquants dans les identifiants
data$IRISr <- ifelse(nchar(data$IRISr) == 8,
                     paste0("0", data$IRISr),
                     data$IRISr)
tmp <- data

tabl <- mar$ar02$d.irisr.app
tmp <- merge(tmp, tabl, by.x = "IRISr", by.y = "IRISrD_CODE", all.x = TRUE)
tmp <- tmp[, c(1, 2, 24:27)]

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



asf_plotypo(data = tmp,
            vars = "clust15",
            typo = "TAAV2017", 
            order_vars = paramx, 
            order_typo = paramy, 
            pal = palette
            )

asf_plotvar(data = tmp, 
            vars = "clust15", 
            typo = "TAAV2017",
            order_vars = paramx, 
            order_typo = paramy
            )
