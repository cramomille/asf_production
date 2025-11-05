
#                                               ASF - SCRIPT DE LA PLANCHE 0801
#                                  L'EVOLUTION DES PRIX DE VENTE D'UN MEME BIEN
#
#                                                                antoine beroud
#                                                                renaud le goix

library(sf)
library(asf)
library(mapsf)
library(data.table)

# ASF -------------------------------------------------------------------------
mar <- asf_mar(md = "com_xxxx", ma = "com_r2", geom = TRUE)

geom <- mar$geom
tabl <- mar$tabl

com_r2 <- asf_fond(geom, 
                   tabl, 
                   by = "COMF_CODE", 
                   maille = "COMR2_CODE", 
                   keep = "DEP")

com_r2 <- asf_drom(com_r2, id = "COMR2_CODE")


# NETTOYAGE DONNEES -----------------------------------------------------------
dvf_dir <- list(dvf_2014 = "input/asf_0800/dvf_prep2/dvf_2014.csv",
                dvf_2015 = "input/asf_0800/dvf_prep2/dvf_2015.csv",
                dvf_2016 = "input/asf_0800/dvf_prep2/dvf_2016.csv",
                dvf_2017 = "input/asf_0800/dvf_prep2/dvf_2017.csv",
                dvf_2018 = "input/asf_0800/dvf_prep2/dvf_2018.csv",
                dvf_2019 = "input/asf_0800/dvf_prep2/dvf_2019.csv",
                dvf_2020 = "input/asf_0800/dvf_prep2/dvf_2020.csv",
                dvf_2021 = "input/asf_0800/dvf_prep2/dvf_2021.csv",
                dvf_2022 = "input/asf_0800/dvf_prep2/dvf_2022.csv",
                dvf_2023 = "input/asf_0800/dvf_prep2/dvf_2023.csv",
                dvf_2024 = "input/asf_0800/dvf_prep2/dvf_2024.csv"
)

dvf <- read.csv(dvf_dir[[1]])

head(dvf)

# Telechargement et selection des colonnes
dvf_list <- lapply(dvf_dir, function(dir) {
  tmp <- read.csv(dir)
  tmp <- tmp[, c(11, 13, 12, 2:5, 7:10)]
  tmp
})

dvf <- do.call(rbind, dvf_list)
row.names(dvf) <- NULL

dvf$type[dvf$type == "Maison"] <- "M"
dvf$type[dvf$type == "Appartement"] <- "A"

colnames(dvf)[1] <- "COM_CODE"
colnames(dvf)[2] <- "lon"
colnames(dvf)[3] <- "lat"

colnames(dvf)[4] <- "n_disposition"
colnames(dvf)[5] <- "n_adresse"
colnames(dvf)[6] <- "n_lot"

head(dvf)

dt <- as.data.table(dvf)


# MULTI VENTES ----------------------------------------------------------------
# Arrondi de la surface pour definir un "groupe de surface tolere"
# Exemple : 80 et 81 seront dans le meme groupe si tol = 1
tol <- 1

# Creation d'un identifiant flou selon (lon, lat, surface, n_disposition, n_adresse, n_lot)
dt[, id_xy := .GRP, by = .(lon, lat, round(surface / (tol + 1)), n_disposition, n_adresse, n_lot)]

# Comptage du nombre d’occurrences uniques par date
# => on compte uniquement si plusieurs dates differentes existent
dt[, n_id := uniqueN(date), by = id_xy]

# Filtrage des "memes biens" vendus a plusieurs dates
dt_multi <- dt[n_id > 1]


# AUGMENTATION PRIX M² --------------------------------------------------------

data <- dt_multi

# Tri par bien et par date croissante
setorder(data, id_xy, date)

# Calcul de la variation absolue du prix/m²
data[, delta_prixm2 := prixm2 - shift(prixm2), by = id_xy]

# # Pour obtenir aussi le delta total en euros (pas par m²)
# data[, delta_prix := prix - shift(prix), by = id_xy]

# On garde uniquement les lignes ou une variation existe
data <- data[!is.na(delta_prixm2)]

# Voir le nombre de multi ventes dans les com_r2
tmp <- merge(data, tabl, by = "COM_CODE", all.x = TRUE)
tmp <- tapply(1:nrow(data), data$COMR2_CODE, length)
tmp <- data.frame(
  dep = names(tmp),
  nb = as.vector(tmp)
)



# Cartographie avec asf et mapsf
z <- asf_zoom(com_r2, places = c("5", "4"))

data_r2 <- asf_data(data, 
                    tabl, 
                    by = "COM_CODE", 
                    maille = "COMR2_CODE", 
                    vars = c("prix", "prixm2", "delta_prixm2"),
                    funs = "median")

fondata <- asf_fondata(f = com_r2, z = z[[1]], d = data_r2, by = "COMR2_CODE")

palette <- rev(asf_palette(type = "div", nb = 6))

q6 <- quantile(fondata$delta_prixm2, 
               probs = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1), 
               na.rm = TRUE)

mf_map(fondata,
       var = "delta_prixm2", 
       type = "choro", 
       breaks = q6,
       pal = palette,
       border = NA)

fondata$pct <- round(fondata$delta_prixm2 / fondata$prixm2 * 100, 0)

q6 <- quantile(fondata$pct, 
               probs = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1), 
               na.rm = TRUE)

mf_map(fondata,
       var = "pct", 
       type = "choro", 
       breaks = q6,
       pal = palette,
       border = NA)

























# TVAM ------------------------------------------------------------------------
dt_explo <- dt_multi

# Tri par bien et par date croissante
setorder(dt_explo, id_xy, date)  

# Calcul du TVAM pour chaque bien
dt_explo[, TVAM := {
  # Decalage des prix et dates dans chaque groupe
  prix_prev <- shift(prix)
  date_prev <- shift(date)
  
  # Calcul du nombre d'annees entre deux ventes
  years <- as.numeric(as.Date(date) - as.Date(date_prev)) / 365.25
  
  # Calcul du TVAM (NA pour la premiere vente)
  ((prix / prix_prev)^(1 / years) - 1)
}, by = id_xy]


result <- dt_explo[
  !is.na(TVAM) & is.finite(TVAM) & TVAM > -1
]

result$TVAM <- round(result$TVAM *100, 1)

data <- asf_data(result, 
                 tabl, 
                 by = "COM_CODE", 
                 maille = "COMR2_CODE", 
                 vars = "TVAM", 
                 funs = "median")

fondata <- asf_fondata(f = comr, d = data, by = "COMR2_CODE")

q6 <- quantile(fondata$TVAM, 
               probs = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1), 
               na.rm = TRUE)

mf_map(fondata,
       var = "TVAM", 
       type = "choro", 
       breaks = q6,
       border = NA)