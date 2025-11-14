
#                                               ASF - SCRIPT DE LA PLANCHE 0801
#                                  L'EVOLUTION DES PRIX DE VENTE D'UN MEME BIEN
#
#                                                                antoine beroud
#                                                                renaud le goix

library(sf)
library(asf)
library(mapsf)
library(data.table)
library(gridExtra)

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

com_r2_simply <- asf_simplify(com_r2)


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

write.csv(dt_multi, "output/asf_0801/dt_multi.csv")


# AUGMENTATION PRIX M² (TVAM et INFLATION) ------------------------------------
data <- read.csv("output/asf_0801/dt_multi.csv")[, -1]
data$annee <- as.numeric(substr(data$date, 1, 4))
data <- data[, c(12, 1, 7, 14, 9, 11)]

# https://www.insee.fr/fr/statistiques/4268033#tableau-figure1
infl <- data.frame(
  annee = 2000:2024,
  inflation = c(1.7, 1.6, 1.9, 2.1, 2.1, 
                1.7, 1.7, 1.5, 2.8, 0.1,
                1.5, 2.1, 2.0, 0.9, 0.5,
                0.0, 0.2, 1.0, 1.9, 1.1, 
                0.5, 1.6, 5.2, 4.9, 2.0)
)

# Inflation cumulee depuis 2000
infl$indice <- cumprod(1 + infl$inflation/100)

# Indice normalise base 2024 = 1
infl$facteur_2024 <- infl$indice[length(infl$indice)] / infl$indice

# Correction des prix en fonction de l'inflation
data <- merge(data, infl[, c("annee","facteur_2024")], by = "annee", all.x = TRUE)
data$prixm2_2024 <- round(data$prixm2 * data$facteur_2024, 0)

head(data)



dt <- as.data.table(data)
setorder(dt, id_xy, date)

# calcul de la duree en annees entre ventes consecutives
dt[, delta_annees := c(NA, as.numeric(difftime(date[-1], date[-.N], units = "days"))/365.25), by = id_xy]

# Calcul du TVAM
dt[, tvam := c(NA, (prixm2_2024[-1]/prixm2_2024[-.N])^(1/delta_annees[-1]) - 1), by = id_xy]

# Delta prix annualise en €/m²/an
dt[, delta_1an := tvam * shift(prixm2_2024), by = id_xy]

# Taux de variation annuel en %
dt[, taux_var := tvam * 100]

dt_clean <- dt[!(is.na(tvam) | !is.finite(tvam) | tvam < -0.9 | tvam > 10 ) ]

data <- as.data.frame(dt_clean)




# CARTOGRAPHIE ----------------------------------------------------------------
v <- c("Marseille", "Lyon", "Toulouse", "Nantes", "Montpellier",
       "Bordeaux", "Lille", "Rennes", "Reims", "Dijon",
       "Angers", "Grenoble", "Clermont-Ferrand", "Tours", "Perpignan",
       "Besancon", "Rouen", "La Rochelle", "Le Havre", "Nice")

z <- asf_zoom(com_r2, 
              places = v, 
              r = 15000)

data_r2 <- asf_data(data, 
                    tabl, 
                    by = "COM_CODE", 
                    maille = "COMR2_CODE", 
                    vars = c(10:12),
                    funs = "median")

fondata <- asf_fondata(f = com_r2_simply, z = z[[1]], d = data_r2, by = "COMR2_CODE")

# Limites des com dans les zooms
com_r2_line <- asf_borders(com_r2, by = "COMR2_CODE")

z <- asf_zoom(com_r2_line, 
              places = v, 
              r = 15000, 
              f_ref = com_r2)


palette <- rev(asf_palette(pal = "rhubarbe", nb = 6))

q6 <- quantile(fondata$delta_1an, 
               probs = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1), 
               na.rm = TRUE)

mf_map(fondata, 
       var = "delta_1an", 
       type = "choro", 
       breaks = q6, 
       pal = palette, 
       border = NA)

mf_map(z[[1]],
       col = "#fff",
       add = TRUE)

mf_label(z[[2]], 
         var = "label")






























# AUGMENTATION PRIX M² --------------------------------------------------------
data <- read.csv("output/asf_0801/dt_multi.csv")[, -1]
data <- as.data.table(data)

# Tri par bien et par date croissante
setorder(data, id_xy, date)

# Calcul de la variation absolue du prix/m²
data[, delta_prixm2 := prixm2 - shift(prixm2), by = id_xy]

# On garde uniquement les lignes ou une variation existe
data <- data[!is.na(delta_prixm2)]

# CARTOGRAPHIE ----------------------------------------------------------------
v <- c("Marseille", "Lyon", "Toulouse", "Nantes", "Montpellier",
       "Bordeaux", "Lille", "Rennes", "Reims", "Dijon",
       "Angers", "Grenoble", "Clermont-Ferrand", "Tours", "Perpignan",
       "Besancon", "Rouen", "La Rochelle", "Le Havre", "Nice")

z <- asf_zoom(com_r2, 
              places = v, 
              r = 15000)

data_r2 <- asf_data(data, 
                    tabl, 
                    by = "COM_CODE", 
                    maille = "COMR2_CODE", 
                    vars = c("prix", "prixm2", "delta_prixm2"),
                    funs = "mean")

fondata <- asf_fondata(f = com_r2_simply, z = z[[1]], d = data_r2, by = "COMR2_CODE")

# Limites des com dans les zooms
com_r2_line <- asf_borders(com_r2, by = "COMR2_CODE")

z <- asf_zoom(com_r2_line, 
              places = v, 
              r = 15000, 
              f_ref = com_r2)


palette <- rev(asf_palette(pal = "rhubarbe", nb = 6))

q6 <- quantile(fondata$delta_prixm2, 
               probs = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1), 
               na.rm = TRUE)

mf_map(fondata, 
       var = "delta_prixm2", 
       type = "choro", 
       breaks = q6, 
       pal = palette, 
       border = NA)

mf_map(z[[1]],
       col = "#fff",
       add = TRUE)

mf_label(z[[2]], 
         var = "label")


# GRAPHIQUES ------------------------------------------------------------------
# Clarification des noms
cateaav_labels <- c(
  "11" = "Commune-Centre",
  "12" = "Autre commune du pôle principal",
  "13" = "Commune d'un pôle secondaire",
  "20" = "Commune de la couronne",
  "30" = "Commune hors attraction des villes"
)

taav_labels <- c(
  "0" = "Hors attraction des villes",
  "1" = "Aire < 50 000 hab.",
  "2" = "Aire 50 000–200 000 hab.",
  "3" = "Aire 200 000–700 000 hab.",
  "4" = "Aire ≥ 700 000 hab. (hors Paris)",
  "5" = "Aire de Paris"
)

# Nombre de multiventes par type d'AAV
x <- merge(data, tabl, by = "COM_CODE", all.x = TRUE)
x <- aggregate(
  list(nb_multivente = x$COM_CODE),
  by = list(COMR2_CODE = x$COMR2_CODE),
  FUN = length
)

x <- merge(x, tabl, by = "COMR2_CODE", all.x = TRUE)

x$TAAV2017 <- taav_labels[as.character(x$TAAV2017)]
x$CATEAAV2020 <- cateaav_labels[as.character(x$CATEAAV2020)]

palette <- asf_palette(type = "qua", nb = 6)

asf_plot_vars(x, vars = "nb_multivente", typo = "TAAV2017", eff = TRUE, pal = palette,
              order.t = c("Hors attraction des villes",
                          "Aire < 50 000 hab.",
                          "Aire 50 000–200 000 hab.",
                          "Aire 200 000–700 000 hab.",
                          "Aire ≥ 700 000 hab. (hors Paris)",
                          "Aire de Paris"))

asf_plot_vars(x, vars = "nb_multivente", typo = "CATEAAV2020", eff = TRUE, pal = palette,
              order.t = c("Commune hors attraction des villes",
                          "Commune de la couronne",
                          "Commune d'un pôle secondaire",
                          "Autre commune du pôle principal",
                          "Commune-Centre"))

asf_plot_vars(x, vars = "nb_multivente", typo = c("TAAV2017", "CATEAAV2020"), eff = TRUE, pal = palette,
              order.t = c("Commune hors attraction des villes",
                          "Commune de la couronne",
                          "Commune d'un pôle secondaire",
                          "Autre commune du pôle principal",
                          "Commune-Centre"))

# Delta_prixm² par type d'AAV
x <- merge(data, tabl, by = "COM_CODE", all.x = TRUE)
x <- aggregate(
  x = list(delta_prixm2 = x$delta_prixm2),
  by = list(
    TAAV2017 = x$TAAV2017,
    CATEAAV2020 = x$CATEAAV2020),
  FUN = mean,
  na.rm = TRUE
)

x <- x[order(x$TAAV2017, x$CATEAAV2020), ]

x$TAAV2017 <- taav_labels[as.character(x$TAAV2017)]
x$CATEAAV2020 <- cateaav_labels[as.character(x$CATEAAV2020)]

grid.table(x)





head(data)






