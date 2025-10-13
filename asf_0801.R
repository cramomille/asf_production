
#                                               ASF - SCRIPT DE LA PLANCHE 0801
#                                  L'EVOLUTION DES PRIX DE VENTE D'UN MEME BIEN
#
#                                                                antoine beroud
#                                                                renaud le goix

library(sf)
library(asf)
library(mapsf)


# IMPORT DU FOND D'ALIETTE ROUX -----------------------------------------------
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


# IMPORT ET NETTOYAGE DES TABLEAUX DE DONNEES ---------------------------------
# Chargement des fichiers DVF -------------------------------------------------
pre <- "https://sharedocs.huma-num.fr/wl/?id="
suf <- "&mode=grid&download=1"

dvf <- list(dvf_2014 = paste0(pre, "vp4DTsuh5ctsBwGTzCSzgdKvZ3HnreAf", suf),
            dvf_2015 = paste0(pre, "QJ3AiWOCYVCkYN6Z0FzqI2yMM7Fu0Jhp", suf),
            dvf_2016 = paste0(pre, "OshgupIqPkg70hEMB7DdSpbsFDuTeAMN", suf),
            dvf_2017 = paste0(pre, "E0Xc2Ahyb0UUGoHFL6JR704dpQnUE7wK", suf),
            dvf_2018 = paste0(pre, "I22yu03q8W53XEFSb0voebmdi0ORUzUl", suf),
            dvf_2019 = paste0(pre, "6muOpXEStHm1Y56YUNv93n14zx2QSQ9i", suf),
            dvf_2020 = paste0(pre, "iq9S63LevHYxoDMCkL01bNQBeE4YlWYx", suf),
            dvf_2021 = paste0(pre, "XYI1SDuWfYRXfCtuz0jvcz7C4LuLi5Qg", suf),
            dvf_2022 = paste0(pre, "5sYwnTlHiFAiTtgD9ZqUeNuEAUTo5T7F", suf),
            dvf_2023 = paste0(pre, "4l09Pfh8OGPICchf9PQEw4X4kdvjOR5P", suf),
            dvf_2024 = paste0(pre, "oSMYbBxT6OWaePSLPydnXOJYoQE3tOID", suf)
)

dvf <- read.csv(dvf[[9]])

dvf <- dvf[, -c(1, 3, 4, 10, 11)]

