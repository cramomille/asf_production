
#                                               ASF - SCRIPT DE LA PLANCHE 0801
#                                  L'EVOLUTION DES PRIX DE VENTE D'UN MEME BIEN
#
#                                                                antoine beroud
#                                                                renaud le goix

library(sf)
library(asf)
library(mapsf)
library(data.table)

# IMPORT ET NETTOYAGE DES TABLEAUX DE DONNEES ---------------------------------
# Chargement des fichiers DVF
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

head(dvf)


dt <- as.data.table(dvf)


# NETTOYAGE -------------------------------------------------------------------
# On arrondit la surface pour definir un "groupe de surface tolere"
# Exemple : 80 et 81 seront dans le meme groupe si tol = 1
tol <- 1

# Creation d'un identifiant flou selon (lon, lat, surface ± tol)
dt[, id_xy := .GRP, by = .(lon, lat, round(surface / (tol + 1)), numero_disposition, adresse_numero, lot1_numero)]

# Comptage du nombre d’occurrences uniques par date
# => on compte uniquement si plusieurs dates differentes existent
dt[, n_xy := uniqueN(date), by = id_xy]

# Filtrer : meme bien (selon lon/lat/surface), vendu a plusieurs dates
dt_multi <- dt[n_xy > 1]



# TVAM ------------------------------------------------------------------------
# Tri par bien et par date croissante
setorder(dt_multi, id_xy, date)  

# Calcul du TVAM pour chaque bien
dt_multi[, TVAM := {
  # Decalage des prix et dates dans chaque groupe
  prix_prev <- shift(prix)
  date_prev <- shift(date)
  
  # Calcul du nombre d'annees entre deux ventes
  years <- as.numeric(as.Date(date) - as.Date(date_prev)) / 365.25
  
  # Calcul du TVAM (NA pour la premiere vente)
  ((prix / prix_prev)^(1 / years) - 1)
}, by = id_xy]


dt_net <- dt_multi[
  !is.na(TVAM) & 
    is.finite(TVAM) & 
    TVAM > -1
]



# PRIX DU M2 ------------------------------------------------------------------
# Tri par bien et par date croissante
setorder(dt_multi, id_xy, date)

# Calcul de la variation absolue du prix/m²
dt_net[, delta_prixm2 := prixm2 - shift(prixm2), by = id_xy]

# Pour obtenir aussi le delta total en euros (pas par m²)
dt_net[, delta_prix := prix - shift(prix), by = id_xy]

# On garde uniquement les lignes où une variation existe
dt_net <- dt_net[!is.na(delta_prixm2)]



# ASF -------------------------------------------------------------------------
# Lecture des fichiers
mar <- asf_mar(md = "com_xxxx", ma = "com_r2", geom = TRUE)

geom <- mar$geom
tabl <- mar$tabl

# Creation du fond geographique des communes regroupees
comr <- asf_fond(geom, 
                 tabl, 
                 by = "COMF_CODE", 
                 maille = "COMR2_CODE", 
                 keep = "DEP")


data <- asf_data(dt_net, 
                 tabl, 
                 by = "COM_CODE", 
                 maille = "COMR2_CODE", 
                 # vars = "TVAM", 
                 vars = c("delta_prixm2", "delta_prix"),
                 funs = "median")

fondata <- asf_fondata(f = comr, d = data, by = "COMR2_CODE")

# fondata$TVAMM <- round(fondata$TVAM *100, 1)



# head(dt_multi)
# 
# d1 <- as.Date("2019-08-08")
# d2 <- as.Date("2022-05-30")
# 
# prix1 <- 228572
# prix2 <- 293580
# 
# 
# year <- as.numeric(d2 - d1) / 365.25
# tvam <- ((prix2 / prix1)^(1 / year) - 1)










"
J'ai un dataframe de 4 millions de lignes avec des ventes de biens immobiliers.

Normlament il est déjà filtré pour ne garder que des biens qui apparaissent plusieurs fois (ce qui signifie qu'ils ont la même localisation xy et la même surface).

Maintenant je veux calculer le TVAM des ventes de ces biens grâce au prix et à la date de la vente (donc bien1 vendu en 2014 et en 2016 (deux lignes) : je veux ajouter une colonne TVAM avec NA pour la ligne de la vente 2014 et la valeur du TVAM dans la ligne de 2016).

Mais je me dis qu'il y a forcement des biens qui ne sont pas les même mais qui ont quand même la même localisation (différents appartements dans un même immeuble).

Donc pour les cas où il y a beaucoup de ventes (donc beaucoup de lignes avec le même identifiant et que je pense correspondent à ces différents appartements dans un même immeuble), je veux que le tvam soit calculer entre la date la plus petite et toutes les autres et uniquement si la période entre deux vente est supérieure à 6 mois.

exemple :

  com        date    prix surface     id
93064  2014-01-31  128000      46   4393
93064  2014-02-26  138000      46   4393   
93064  2014-09-05  110000      46   4393
93064  2017-01-06  143000      46   4393
93064  2017-12-20  150000      46   4393
93064  2018-02-14  165000      46   4393


  com       date1       date2    id  TVAM
93064  2014-01-31  2014-02-26  4393   NA car moins de 6 mois
93064  2014-01-31  2014-09-05  4393   tvam
93064  2014-01-31  2017-01-06  4393   tvam
93064  2014-01-31  2017-12-20  4393   tvam
93064  2014-01-31  2018-02-14  4393   

93064  2014-02-26  2014-09-05  4393 
93064  2014-02-26  2017-01-06  4393 
93064  2014-02-26  2017-12-20  4393 
93064  2014-02-26  2018-02-14  4393 

93064  2014-09-05  2017-01-06  4393
93064  2014-09-05  2017-12-20  4393
93064  2014-09-05  2018-02-14  4393

93064  2017-01-06  2017-12-20  4393
93064  2017-01-06  2018-02-14  4393

93064  2017-12-20  2018-02-14  4393
"








