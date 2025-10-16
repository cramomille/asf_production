
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

comr <- asf_fond(geom, 
                 tabl, 
                 by = "COMF_CODE", 
                 maille = "COMR2_CODE", 
                 keep = "DEP")

comr <- asf_drom(comr, id = "COMR2_CODE")
mf_map(comr)

# IMPORT ET NETTOYAGE DES TABLEAUX DE DONNEES ---------------------------------
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


# NETTOYAGE -------------------------------------------------------------------
# Arrondi de la surface pour definir un "groupe de surface tolere"
# Exemple : 80 et 81 seront dans le meme groupe si tol = 1
tol <- 1

# Creation d'un identifiant flou selon (lon, lat, surface, n_disposition, n_adresse, n_lot)
dt[, id_xy := .GRP, by = .(lon, lat, round(surface / (tol + 1)), n_disposition, n_adresse, n_lot)]

# Comptage du nombre d’occurrences uniques par date
# => on compte uniquement si plusieurs dates differentes existent
dt[, n_xy := uniqueN(date), by = id_xy]


# Filtre "meme bien" vendu a plusieurs dates
dt_multi <- dt[n_xy > 1]



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



# PRIX DU M2 ------------------------------------------------------------------
dt_explo <- dt_multi

# Tri par bien et par date croissante
setorder(dt_explo, id_xy, date)

# Calcul de la variation absolue du prix/m²
dt_explo[, delta_prixm2 := prixm2 - shift(prixm2), by = id_xy]

# Pour obtenir aussi le delta total en euros (pas par m²)
dt_explo[, delta_prix := prix - shift(prix), by = id_xy]

# On garde uniquement les lignes ou une variation existe
result <- dt_explo[!is.na(delta_prixm2)]


tmp <- merge(result, tabl, by = "COM_CODE", all.x = TRUE)

nb_by_comr <- tapply(1:nrow(tmp), tmp$COMR2_CODE, length)

nb_by_comr <- data.frame(
  dep = names(nb_by_comr),
  nb = as.vector(nb_by_comr)
)






data <- asf_data(result, 
                 tabl, 
                 by = "COM_CODE", 
                 maille = "COMR2_CODE", 
                 vars = c("delta_prixm2", "delta_prix"),
                 funs = "median")

fondata <- asf_fondata(f = comr, d = data, by = "COMR2_CODE")


q6 <- quantile(fondata$delta_prixm2, 
               probs = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1), 
               na.rm = TRUE)

b_median <- c(-310.5, 0, 200, 300, 400, 800, 3750)
b_mean <- c(-430, 0, 200, 300, 600, 800, 3700)

mf_map(fondata,
       var = "delta_prixm2", 
       type = "choro", 
       breaks = q6,
       border = NA)

