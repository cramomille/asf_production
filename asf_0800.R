
#                                               ASF - SCRIPT DE LA PLANCHE 0800
#                                         INDICE D'ABORDABILITE DE L'IMMOBILIER
#
#                                                                antoine beroud
#                                                                renaud le goix

library(sf)
library(asf)
library(mapsf)
library(ggplot2)
library(reshape2)


# IMPORT DU FOND D'ALIETTE ROUX -----------------------------------------------
# Lecture des fichiers
mar <- asf_mar(md = "com_xxxx", ma = "com_r2", geom = TRUE)

geom <- mar$geom
tabl <- mar$tabl

# Creation du fond geographique des communes regroupees
comr <- asf_fond(geom, 
                 tabl, 
                 by = "COMF_CODE", 
                 maille = "COMr2_CODE", 
                 keep = "DEP")


# IMPORT ET NETTOYAGE DES TABLEAUX DE DONNEES ---------------------------------
# Chargement du fichier FILOCOM -----------------------------------------------
comr_revenu <- read.csv("input/asf_0800/decile_revucm_comar.csv")
comr_revenu <- comr_revenu[, c("comar", "d5_2022")]
names(comr_revenu)[1] <- "COMr2_CODE"

# Chargement des fichiers DVF -------------------------------------------------
pre <- "https://sharedocs.huma-num.fr/wl/?id="
suf <- "&mode=grid&download=1"

dvf <- list(dvf_2014 = paste0(pre, "HzUYLc0qcchwzbQhbl1hrTs0KNhtMCCO", suf),
            dvf_2015 = paste0(pre, "PydNgajOSCBUTCtqA4WQaerFvNaVNl3F", suf),
            dvf_2016 = paste0(pre, "8xC1lMuHHtrb3PKiyV89xDdGrDAsMBv7", suf),
            dvf_2017 = paste0(pre, "HAYU3oU5sdNh7gRnW8Bn8UklS6lZ4BIg", suf),
            dvf_2018 = paste0(pre, "Qjuf32V502nNMykXAL2o6miwYuwvM4RZ", suf),
            dvf_2019 = paste0(pre, "KZXvpPRUEKEJeZGx3n2o7Oafx4y2tU7B", suf),
            dvf_2020 = paste0(pre, "Ur29h89masp31M2c9osIHTMV7JxutWs5", suf),
            dvf_2021 = paste0(pre, "lgqmAEg4kv4P48c50jaKqlNpzlE0dgZq", suf),
            dvf_2022 = paste0(pre, "3yDxA8KiuyFDwSvgz9BxnJDG6zVFX2dd", suf),
            dvf_2023 = paste0(pre, "I2Zvl3aprq9ue3l27hleoiTWIMZyjhII", suf),
            dvf_2024 = paste0(pre, "NJHBVnyL7jCyJorSwk6VjHvIlTljt4vP", suf)
)

a <- read.csv(dvf[[9]])
b <- read.csv(dvf[[10]])

dvf <- rbind(a, b)

dvf <- dvf[, -c(1, 3, 4, 10, 11)]

# # Explo pour connaitre les surfaces medianes des biens pour les loyers
# ma <- dvf[dvf$type == "Maison", ]
# quantile(ma$surface, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
# ap <- dvf[dvf$type == "Appartement", ]
# quantile(ap$surface, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)

# Jointure avec le maillage composite d'Aliette Roux
comr_dvf <- merge(tabl, dvf, by.x = "COM_CODE", by.y = "codecommune")

# Fonction pour traiter un type de bien (Maison/Appartement)
filter_dvf <- function(dvf, id, type_bien) {
  
  # Filtrer les données pour le type de bien
  dvf_filtered <- dvf[dvf$type == type_bien, ]
  
  # Calcul des agregations par regroupement de communes
  com_prix <- tapply(dvf_filtered$prix, dvf_filtered[[id]], median, na.rm = TRUE)
  com_nomb <- tapply(dvf_filtered$prix, dvf_filtered[[id]], length)
  
  # Convertir en data.frame
  result <- data.frame(COMr2_CODE = names(com_prix),
                       prix = as.vector(com_prix),
                       nb = as.vector(com_nomb))
  
  # Renommer les colonnes selon le type de bien
  colnames(result)[which(names(result) == "prix")] <- paste0("median_prix_", tolower(type_bien))
  colnames(result)[which(names(result) == "nb")] <- paste0("nb_", tolower(type_bien))
  
  return(result)
}

maison <- filter_dvf(comr_dvf, "COMr2_CODE", "Maison")
appart <- filter_dvf(comr_dvf, "COMr2_CODE", "Appartement")

comr_dvf <- merge(maison, appart, by = "COMr2_CODE", all = TRUE)

rm(a, b, pre, suf, dvf, maison, appart)

# Chargement du fichier de l'Observatoire des Territoires ---------------------
loyer <- read.csv("input/asf_0800/base_OT_2024.csv", skip = 2, header = TRUE, sep = ";")

loyer[, 3] <- as.numeric(loyer[, 3])
loyer[, 4] <- as.numeric(loyer[, 4])

colnames(loyer)[3:4] <- c("loyer_mai", "loyer_app")

# Comme il manque les arrondissements on dedouble les communes concernees
arr_com <- tabl[grepl("^751|^132|^6938", tabl$COM_CODE), ]
arr_com$id <- substr(arr_com$COM_CODE, 1, 2)

arr_loyer <- loyer[grepl("75056|13055|69123", loyer$Code), ]
arr_loyer$id <- substr(arr_loyer$Code, 1, 2)

arr <- merge(arr_com, arr_loyer, by = "id")
arr <- arr[, c("COM_CODE", "Libellé", "loyer_mai", "loyer_app")]
colnames(arr)[1] <- "Code" 

loyer <- loyer[!grepl("75056|13055|69123", loyer$Code), ]

loyer <- rbind(arr, loyer)

# Utilisation de l'Indice de reference des loyers (INSEE) pour estimer les loyers de 2022
# T1 2022 : 133.93 => T1 2024 : 143.46 
# 143.46 * 100 / 133.93 = 107.12
loyer$loyer_mai <- round(loyer$loyer_mai / 1.0712, 2)
loyer$loyer_app <- round(loyer$loyer_app / 1.0712, 2)

# Chargement du nombre de menages par commune pour ponderer les loyers entre communes lors du regroupement
pop <- read.csv("C:/Users/Antoine/Desktop/casd/export/TREVPOP_export_02/donnees/fra/filocom_2022_decile.csv")
pop <- pop[, c(2,13)]

loyer <- merge(loyer, pop, by.x = "Code", by.y = "COM", all.x = TRUE)
loyer$TOT[is.na(loyer$TOT)] <- 5
loyer$TOTB <- loyer$TOT

# Agregation des communes en calculant une moyenne ponderee des loyers
comr_loyer <- asf_data(d = loyer,
                       t = tabl, 
                       by.x = "Code",
                       by.y = "COM_CODE",
                       maille = "COMr2_CODE",
                       vars = c(3:6),
                       funs = c("prod1", "prod2", "coef1", "coef2"))

rm(arr, arr_com, arr_loyer, pop, loyer)


# TRAITEMENTS A PARTIR DES AUTRES FONCTIONS D'ASF -----------------------------
# Traitement sur les donnees
data <- merge(comr_revenu, comr_loyer, by = "COMr2_CODE", all = TRUE)
data <- merge(data, comr_dvf, by = "COMr2_CODE", all = TRUE)

# Creation du fond et des zooms
fond <- asf_drom(comr)

z <- asf_zoom(fond,
              places = c("Marseille", "Lyon", "Toulouse", "Nantes", "Montpellier",
                         "Bordeaux", "Lille", "Rennes", "Reims", "Dijon",
                         "Angers", "Grenoble", "Clermont-Ferrand", "Tours", "Perpignan",
                         "Besancon", "Rouen", "La Rochelle", "Le Havre", "Nice"
              ),
              r = 15000)

zoom <- z$zooms
label <- z$labels
point <- z$points

fond <- asf_simplify(fond, keep = 0.1)

fondata <- asf_fondata(f = fond,
                       z = zoom,
                       d = data,
                       by = "COMr2_CODE")


# CALCUL DE L'INDICE D'ABORDABILITE -------------------------------------------
# Nombre d'annee de revenu pour acheter un bien
fondata$abord_mai <- (fondata$median_prix_maison * 0.9) / fondata$d5_2022 
fondata$abord_app <- (fondata$median_prix_appart * 0.9) / fondata$d5_2022

# Abordabilite du loyer (pourcentage du salaire)
fondata$abord_mai_loc <- (fondata$loyer_mai * 98) / (fondata$d5_2022 / 12) * 100
fondata$abord_app_loc <- (fondata$loyer_app * 49) / (fondata$d5_2022 / 12) * 100

# Indice final
# Calcul des Q6
q_m <- quantile(fondata$abord_mai, probs = c(0.05, 0.25, 0.5, 0.75, 0.95, 1), na.rm = TRUE)
q_a <- quantile(fondata$abord_app, probs = c(0.05, 0.25, 0.5, 0.75, 0.95, 1), na.rm = TRUE)

# Arrondis des Q6
q_m <- c(4, 7, 9, 12, 18)
q_a <- c(2, 4, 6, 8, 10)

# Creation de la colonne 'typo'
fondata$typo_m <- with(fondata, 
                       ifelse(abord_mai < q_m[1] & abord_app_loc < 30, 1,
                       ifelse(abord_mai < q_m[1] & abord_app_loc > 30, 2,
                       ifelse(abord_mai >= q_m[1] & abord_mai < q_m[2] & abord_app_loc < 30, 3,
                       ifelse(abord_mai >= q_m[1] & abord_mai < q_m[2] & abord_app_loc > 30, 4,
                       ifelse(abord_mai >= q_m[2] & abord_mai < q_m[3] & abord_app_loc < 30, 5,
                       ifelse(abord_mai >= q_m[2] & abord_mai < q_m[3] & abord_app_loc > 30, 6,
                       ifelse(abord_mai >= q_m[3] & abord_mai < q_m[4] & abord_app_loc < 30, 7,
                       ifelse(abord_mai >= q_m[3] & abord_mai < q_m[4] & abord_app_loc > 30, 8,
                       ifelse(abord_mai >= q_m[4] & abord_mai < q_m[5] & abord_app_loc < 30, 9,
                       ifelse(abord_mai >= q_m[4] & abord_mai < q_m[5] & abord_app_loc > 30, 10,
                       ifelse(abord_mai >= q_m[5] & abord_app_loc < 30, 11,
                       ifelse(abord_mai >= q_m[5] & abord_app_loc > 30, 12, 
                       NA)))))))))))))

fondata$typo_a <- with(fondata,
                       ifelse(abord_app < q_a[1] & abord_app_loc < 30, 1,
                       ifelse(abord_app < q_a[1] & abord_app_loc > 30, 2,
                       ifelse(abord_app >= q_a[1] & abord_app < q_a[2] & abord_app_loc < 30, 3,
                       ifelse(abord_app >= q_a[1] & abord_app < q_a[2] & abord_app_loc > 30, 4,
                       ifelse(abord_app >= q_a[2] & abord_app < q_a[3] & abord_app_loc < 30, 5,
                       ifelse(abord_app >= q_a[2] & abord_app < q_a[3] & abord_app_loc > 30, 6,
                       ifelse(abord_app >= q_a[3] & abord_app < q_a[4] & abord_app_loc < 30, 7,
                       ifelse(abord_app >= q_a[3] & abord_app < q_a[4] & abord_app_loc > 30, 8,
                       ifelse(abord_app >= q_a[4] & abord_app < q_a[5] & abord_app_loc < 30, 9,
                       ifelse(abord_app >= q_a[4] & abord_app < q_a[5] & abord_app_loc > 30, 10,
                       ifelse(abord_app >= q_a[5] & abord_app_loc < 30, 11,
                       ifelse(abord_app >= q_a[5] & abord_app_loc > 30, 12,
                       NA)))))))))))))

# # Comptage des effectifs et conversion en pourcentage
# tmp <- table(fondata$typo_m, useNA = "ifany")
# prop_m <- round(100 * tmp / sum(tmp), 2)
# prop_m
# 
# # Comptage des effectifs et conversion en pourcentage
# tmp <- table(fondata$typo_a, useNA = "ifany")
# prop_a <- round(100 * tmp / sum(tmp), 2)
# prop_a


# CREATION DE CARTES ----------------------------------------------------------
palette <- c(
  "#00a183","#6561a9",
  "#8ccaae","#9b99cc",
  "#cce5d8","#d3d5ed",
  "#ffe8b6","#f9c4a7",
  "#fdc75f","#f08159",
  "#f59c00","#dc0d15"
)

dep <- asf_borders(fond, 
                   by = "DEP",
                   keep = 0.1)

mf_map(fondata,
       var = "typo_m",
       type = "typo",
       pal = palette,
       border = NA)

mf_map(dep, 
       col = "white", 
       lwd = 1, 
       add = TRUE)

# mf_map(point,
#        add = TRUE)

mf_label(label, 
         var = "label")



# EXPORT POUR LA CARTO INTERACTIVE --------------------------------------------
export <- fondata[, c(1, 11:16)]
export[, c(2:3)] <- round(sf::st_drop_geometry(export)[, 2:3], 1)
export[, c(4:5)] <- round(sf::st_drop_geometry(export)[, 4:5], 0)

t <- tabl <- tabl[!duplicated(tabl$COMr2_CODE), ]

export <- merge(t[, c(4, 5)], export, by = "COMr2_CODE")



# CREATION DE GRAPHIQUES SUR LES AAV ------------------------------------------
# asf_mar()
# tmp <- fondata[, c(1, 2, 11, 12)]
# com <- mar$ar01$sf.comf
# com <- com[, c(1)]
# tabl <- mar$ar01$d.comf.app
# tabl <- tabl[, c(2, 16)]
# 
# aav <- merge(tabl, com, by.x = "COMF_CODE", by.y = "COMFA_CODE")
# 
# 
# # Fonction pour recuperer les typologies associees aux id d'une cellule
# x <- function(id_multi, df) {
#   # Separation des id
#   id <- trimws(unlist(strsplit(id_multi, "\\|")))
#   # Recuperation des typologies associees a ces id
#   typo <- df$TAAV2017[match(id, df$COMF_CODE)]
#   # Retour des typologies distinctes
#   paste(sort(unique(typo)), collapse = " | ")
# }
# 
# # Application
# tmp$aav <- sapply(tmp$COMF_CODE_MULTI, x, df = aav)
# 
# 
# # Fonction pour recuperer la typologie majoritaire
# y <- function(id_multi, df) {
#   # Separation des id
#   id <- trimws(unlist(strsplit(id_multi, "\\|")))
#   # Recuperation des typologies associees a ces id
#   typo <- df$TAAV2017[match(id, df$COMF_CODE)]
#   typo <- typo[!is.na(typo)]
#   
#   # Si vide (aucun code trouve), retourner NA
#   if (length(typo) == 0) return(NA)
#   
#   # Tableau de fréquence
#   freq <- table(typo)
#   freq_max <- max(freq)
#   maj <- as.numeric(names(freq)[freq == freq_max])
#   maj <- max(maj)
#   
#   return(maj)
# }
# 
# # Application
# tmp$aav_maj <- sapply(tmp$COMF_CODE_MULTI, y, df = aav)


###############################################################################

tabl <-  tabl[, c("COMr2_CODE", "TAAV2017")]
tabl <- tabl[!duplicated(tabl$COMr2_CODE), ]

tmp <- merge(fondata, tabl, by = "COMr2_CODE")

# Calcul des q6
tmp <- tmp[!is.na(tmp$abord_mai), ]
nb <- round(quantile(tmp$abord_mai, probs = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1)))
nb <- c(0, 4, 7, 9, 12, 18, 63)

# Definition pour chaque entite de son q6 d'appartenance
tmp$class <- cut(tmp$abord_mai,
                 breaks = nb,
                 include.lowest = TRUE,
                 labels = paste0("q", 1:6))

tmp$TAAV2017 <- as.character(tmp$TAAV2017)
tmp$class <- as.character(tmp$class)

asf_plot_typo(d = tmp, 
              vars = c("class"), 
              typo = "TAAV2017",
              order.v = c("q1", "q2", "q3", "q4", "q5", "q6"), 
              order.t = c("0", "1", "2", "3", "4", "5"))


asf_plot_typo(d = tmp, 
              vars = c("TAAV2017"), 
              typo = "class",
              order.v = c("0", "1", "2", "3", "4", "5"), 
              order.t = c("q1", "q2", "q3", "q4", "q5", "q6"))


# CREATION DE GRAPHIQUES SOUS FORME DE MATRICE --------------------------------
# Definition des fichiers et des valeurs de division
pre <- "https://sharedocs.huma-num.fr/wl/?id="
suf <- "&mode=grid&download=1"

data <- list(dvf_2014 = paste0(pre, "HzUYLc0qcchwzbQhbl1hrTs0KNhtMCCO", suf),
             dvf_2015 = paste0(pre, "PydNgajOSCBUTCtqA4WQaerFvNaVNl3F", suf),
             dvf_2016 = paste0(pre, "8xC1lMuHHtrb3PKiyV89xDdGrDAsMBv7", suf),
             dvf_2017 = paste0(pre, "HAYU3oU5sdNh7gRnW8Bn8UklS6lZ4BIg", suf),
             dvf_2018 = paste0(pre, "Qjuf32V502nNMykXAL2o6miwYuwvM4RZ", suf),
             dvf_2019 = paste0(pre, "KZXvpPRUEKEJeZGx3n2o7Oafx4y2tU7B", suf),
             dvf_2020 = paste0(pre, "Ur29h89masp31M2c9osIHTMV7JxutWs5", suf),
             dvf_2021 = paste0(pre, "lgqmAEg4kv4P48c50jaKqlNpzlE0dgZq", suf),
             dvf_2022 = paste0(pre, "3yDxA8KiuyFDwSvgz9BxnJDG6zVFX2dd", suf),
             dvf_2023 = paste0(pre, "I2Zvl3aprq9ue3l27hleoiTWIMZyjhII", suf),
             dvf_2024 = paste0(pre, "NJHBVnyL7jCyJorSwk6VjHvIlTljt4vP", suf)
)

rd1 <- c(5720, #2013
         5560, #2014
         5401, #2015
         5416, #2016
         5430, #2017
         5518, #2018
         5605, #2019
         6285, #2020
         6966, #2021
         7285, #2022
         7604  #2023
)

rd5 <- c(16979, #2013
         17214, #2014
         17448, #2015
         17609, #2016
         17770, #2017
         18070, #2018
         18370, #2019
         18811, #2020
         19252, #2021
         19893, #2022
         20534  #2023
)

rd9 <- c(36068, #2013
         36506, #2014
         36943, #2015
         37286, #2016
         37628, #2017
         38323, #2018
         39018, #2019
         39787, #2020
         40555, #2021
         42029, #2022
         43503  #2023
)

tabl <- asf_mar(md = "com_xxxx", ma = "com_r2")
tabl <- tabl[, c(1, 4, 5, 18)]

# Initialisation d'une liste pour stocker les resultats
maison <- list()
appart <- list()

# Boucle sur les fichiers et les valeurs de division
for (i in seq_along(data)) {
  
  rev <- rd9

  # Chargement du fichier
  dvf <- read.csv(data[[i]])
  dvf <- dvf[, c(9, 5, 6)]
  
  mar <- merge(tabl, dvf, by.x = "COM_CODE", by.y = "codecommune")
  mar <- mar[, c("COMr2_CODE", "type", "prix")]
  
  # Filtrage du type de bien (Maison ou Appartement)
  mai <- mar[mar$type == "Maison", ]
  app <- mar[mar$type == "Appartement", ]

  # Agregation par groupe de communes
  mai <- aggregate(prix ~ COMr2_CODE, mai, FUN = median, na.rm = TRUE)
  app <- aggregate(prix ~ COMr2_CODE, app, FUN = median, na.rm = TRUE)
  
  # Calcul des deciles
  brks_mai <- quantile(mai$prix, probs = seq(0, 1, 0.1), na.rm = TRUE)
  brks_app <- quantile(app$prix, probs = seq(0, 1, 0.1), na.rm = TRUE)
  
  mai$decile <- cut(
    mai$prix,
    breaks = brks_mai,
    include.lowest = TRUE,
    labels = paste0("D", 1:10)
  )
  
  app$decile <- cut(
    app$prix,
    breaks = brks_app,
    include.lowest = TRUE,
    labels = paste0("D", 1:10)
  )
  
  mai_dec <- aggregate(
    prix ~ decile,
    data = mai,
    FUN = median,
    na.rm = TRUE
  )

  app_dec <- aggregate(
    prix ~ decile,
    data = app,
    FUN = median,
    na.rm = TRUE
  )
  
  # Stockage des resultats
  maison[[i]] <- data.frame(
    abord = round(mai_dec$prix / rev[i], 1)
  )
  appart[[i]] <- data.frame(
    abord = round(app_dec$prix / rev[i], 1)
  )
  
  # Renommer les colonnes pour chaque annee
  colnames(maison[[i]]) <- paste0(colnames(maison[[i]]), "_", substr(names(data)[i], 5, 8))
  colnames(appart[[i]]) <- paste0(colnames(appart[[i]]), "_", substr(names(data)[i], 5, 8))
  
  print(i)
}

# Fusionner tous les tableaux par la colonne des deciles
maison_d9 <- do.call(cbind, maison)
appart_d9 <- do.call(cbind, appart)

# Creation des graphiques
breaks <- c(0, 2, 4, 6, 8, 10, Inf)
tableau <- appart_d5

breaks <- c(0, 4, 7, 9, 12, 18, Inf)
tableau <- maison_d5


tableau$decile = c("D01","D02","D03","D04","D05","D06","D07","D08","D09","D10")

# Transformation des donnees en format long
tableau_long <- melt(tableau, id.vars = "decile")

# Renommer les colonnes pour ggplot
colnames(tableau_long) <- c("decile", "annee", "valeur")

# Creation d'une colonne categorielle pour les classes
tableau_long$classe <- cut(tableau_long$valeur,
                           breaks = breaks,
                           labels = c("1", "2", "3", "4", "5", "6"),
                           right = FALSE)

palette <- c("1" = "#00a183",
             "2" = "#8ccaae",
             "3" = "#cce5d8",
             "4" = "#fbceb4",
             "5" = "#f28d65",
             "6" = "#dc0d15"
             )

# Creation du heatmap avec ggplot
ggplot(tableau_long, aes(x = annee, y = decile, fill = classe)) +
  geom_tile(color = "white") +  # creation des carres
  scale_fill_manual(values = palette) +  # couleurs
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # rotation des labels
  labs(title = "Evolution des valeurs par decile et annee", 
       fill = "Classe de Valeur")
