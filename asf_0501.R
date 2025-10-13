
#                                               ASF - SCRIPT DE LA PLANCHE 0501 
#                                                             TYPOLOGIE DES CSP
#
#                                                                antoine beroud
#                                                                  jean riviere

library(sf)
library(asf)
library(mapsf)


# IMPORT DU FOND D'ALIETTE ROUX -----------------------------------------------
# Lecture des fichiers
mar <- asf_mar(md = "iris_xxxx", ma = "iris_r2", geom = TRUE)

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


# IMPORT ET NETTOYAGE DU TABLEAU DE DONNEES -----------------------------------
data <- read.csv2("input/asf_0501/TableTypo15.csv")
data <- data[, c(1, ncol(data))]

# Ajout des zeros manquants dans les identifiants
data$IRISr <- ifelse(nchar(data$IRISr) == 8,
                     paste0("0", data$IRISr),
                     data$IRISr)


# TRAITEMENTS A PARTIR DES AUTRES FONCTIONS D'ASF -----------------------------
# Creation des zooms
z <- asf_zoom(fond,
              places = c("5", "4"),
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


# CREATION DE CARTES ----------------------------------------------------------
# Recuperation des limites departementales
dep <- asf_borders(fond,
                   by = "DEP", 
                   keep = 0.05)

# Definition d'une palette de couleurs
pal <- c("01" = "#94282f",
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
       pal = pal,
       border = NA)

mf_map(dep, 
       col = "#ffffff", 
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

# Ajout des limites communales dans les zooms
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

pal <- c(
  "#006757",
  "#009c79",
  "#8dc9a4",
  "#cde4c4",
  "#ecf4e3", 
  "#fce4e0", 
  "#f8c7c8", 
  "#f08590", 
  "#e73458", 
  "#95254c"
)

# Ouverture d'un fichier PDF
pdf("output/asf_0501/explo_decile.pdf", width = 8, height = 8)

# Boucle pour realiser toutes les cartes et les exporter en PDF
for (i in 2:(length(names(fondata)) - 2)) {
  
  # Nom de la variable
  varname <- names(fondata)[i]
  
  # Seuils
  d <- quantile(fondata[[i]], probs = seq(0, 1, 0.1), na.rm = TRUE)
  
  # Carte choroplethe
  mf_map(fondata,
         var = varname,
         type = "choro",
         breaks = d, 
         pal = pal,
         border = NA)
  
  # Contours departementaux
  mf_map(dep,
         col = "#000000",
         lwd = 1,
         add = TRUE)
  
  # Labels
  mf_label(label,
           var = "label",
           col = "#000000",
           font = 1)
}

# Fermeture du fichier PDF
dev.off()


# CREATION DE GRAPHIQUES ------------------------------------------------------
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
