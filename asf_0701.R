
#                                               ASF - SCRIPT DE LA PLANCHE 0701                                          
#                               AUTOCORRELATION SPATIALE DES PLUS HAUTS REVENUS
#
#                                                                antoine beroud
#                                                                  juillet 2025

library(sf)
library(asf)
library(mapsf)
library(MTA)


# CHARGEMENT DES DONNEES ------------------------------------------------------
data <- read.csv("input/asf_0701/decile_revucm_comar.csv")
data <- data[, c(2, 123:132)]

mar <- asf_mar(md = "com_xxxx", ma = "com_r2", geom = TRUE)
iris <- mar$geom
tabl <- mar$tabl

iris <- iris[!grepl("^96|^97|^98|^N|^P", iris$IRISF_CODE), ]

fond <- asf_fond(iris, 
                 tabl, 
                 by = "COMF_CODE", 
                 maille = "COMR2_CODE",
                 keep = "DEP")

z <- asf_zoom(fond,
              places = c("Paris", "Marseille", "Lyon", "Nantes"), 
              r = 15000)

zoom <- z$zooms
label <- z$labels

fond <- asf_simplify(fond)

dep <- asf_borders(fond, by = "DEP", keep = 0.1)

fondata <- asf_fondata(f = fond, 
                       z = zoom,
                       d = data, 
                       by.x = "COMR2_CODE", by.y = "comar")


# TRAITEMENTS -----------------------------------------------------------------
x <- fondata

d <- quantile(fondata$d9_2022, probs = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1), na.rm = TRUE)

pal <- asf_palette(type = "div", nb = 6)

mf_map(x, 
       var = "d9_2022",
       type = "choro", 
       breaks = d,
       pal = pal,
       border = NA)

mf_map(dep,
       col = "#ffffff",
       lwd = 1,
       add = TRUE)


# -----------------------------------------------------------------------------
x <- fondata

# a. Deviation generale ----
x$gdevrel <- gdev(x,  var1 = "d9_2022", var2 = "d1_2022",  type = "rel")

mf_map(x, var = "gdevrel", type = "choro", 
       pal = pal, 
       breaks = c(min(x$gdevrel), 75, 90, 100, 110, 125, max(x$gdevrel)),
       border = NA, leg_pos = "left",  leg_val_rnd = 0, 
       leg_title = "Deviation au contexte general\n(100 = moyenne de la France)")
mf_map(dep,
       col = "#000",
       add = TRUE)


# b. Deviation territoriale ----
x$tdevrel <- tdev(x, var1 = "d9_2022", var2 = "d1_2022",  type = "rel", key = "DEP")

mf_map(x, var = "tdevrel", type = "choro", 
       pal = pal, 
       breaks = c(min(x$tdevrel), 75, 90, 100, 110, 125, max(x$tdevrel)),
       border = NA, leg_pos = "left",  leg_val_rnd = 0, 
       leg_title = "Deviation au contexte territorial\n(100 = moyenne du departement)")
mf_map(dep,
       col = "#000",
       add = TRUE)


# c. Deviation spatiale ----
x$sdevrel <- sdev(x, var1 = "d9_2022", var2 = "d1_2022", type = "rel", order = 1)

mf_map(x, var = "sdevrel", type = "choro", 
       pal = pal, 
       breaks = c(min(x$sdevrel), 75, 90, 100, 110, 125, max(x$sdevrel)),
       border = NA, leg_pos = "left",  leg_val_rnd = 0, 
       leg_title = "Deviation au contexte spatial\n(100 = moyenne des communes contigues)")
mf_map(dep,
       col = "#000",
       add = TRUE)




library(spdep)

x$Y <- x$d5_2022
x$Y <- x$d9_2022 / x$d5_2022

Moran <- x[, c(1,17)]
Moran$COMR2_CODE <- substr(Moran$COMR2_CODE, 1, 5)
names(Moran) <- c("code", "Y", "geometry")
row.names(Moran) <- Moran$code

# Normalisation log
Moran$Y <- log(Moran$Y)
Moran$Y_std <- scale(Moran$Y)

# Table de contiguïté
contig_nb <- poly2nb(Moran, row.names = Moran$code)
contig_nb_w <- nb2listw(contig_nb, zero.policy = TRUE)

# # Fond général
# mf_map(Moran, col = "lightgrey", border = NA)
# 
# # Entités sans voisins
# no_neighbors <- which(card(contig_nb) == 0)
# mf_map(Moran[no_neighbors, ], col = "red", border = NA, add = TRUE)

# Moyenne locale
Moran$Y_lag <- lag.listw(contig_nb_w, Moran$Y)
Moran$Y_std_lag <- lag.listw(contig_nb_w, Moran$Y_std)

# Calcul de l'indice de Moran
cor.test(Moran$Y, Moran$Y_lag)





# Local Moran : significativité des valeurs (Z.Ii) 
L.Moran <- localmoran(Moran$Y, contig_nb_w, alternative = "two.sided")
L.Moran <- as.data.frame(L.Moran)
L.Moran$code <- row.names(L.Moran)

# Jointure avec les indices de Moran calculés en amont
Moran <- merge(Moran, L.Moran, by = "code", all.x = TRUE)

# Interprétation des valeurs en quadrants
Moran$q1 <- as.factor(Moran$Y_std > 0)
levels(Moran$q1) <- c("Bas","Haut")
Moran$q2 <- as.factor(Moran$Y_std_lag > 0)
levels(Moran$q2) <- c("Bas","Haut")

# Synthèse des quadrants et définition d'un seuil de valeurs non significatives
signThr <- 0.75
Moran$q <- paste(as.character(Moran$q1), as.character(Moran$q2), sep = "-")
Moran$q <- ifelse(abs(Moran$Z.Ii) < signThr, "Non Sign.", Moran$q)

# Réorganiser les valeurs des quadrants par ordre alphabétique (gestion des couleurs)
Moran <- Moran[order(as.factor(Moran$q)),]
Moran$q <- as.factor(Moran$q)
cols <- c("blue", "skyblue2", "lightpink", "red", "#f5f5f5")[as.factor(Moran$q)]

# Graphique
par(mfrow = c(1,2), mar = c(2,4,2,2), pty = "s")
plot(x = Moran$Y_std, y = Moran$Y_std_lag,  bg = cols, asp = 1,pch = 21,
     cex = 0.8, cex.main = 1, cex.lab = 0.6, cex.axis = 0.6,
     main = "Diagramme de Moran", xlab = "Valeur observée",
     ylab = "Moyenne des communes voisines")

abline(h = 0, v = 0)
lm.Moran <- lm(Moran$Y_std_lag ~ Moran$Y_std)
abline(lm.Moran, lty = 2, lwd = 1, col = "red")

legend(-1, -2, "Bas-Bas", xjust = 0.5,  yjust = 0.5, cex = 0.9,
       x.intersp = -0.5, y.intersp = 0.1, bg = "#ffffff90")
legend(-1, 2, "Bas-Haut", xjust = 0.5,  yjust = 0.5, cex = 0.9,
       x.intersp = -0.5, y.intersp = 0.1, bg = "#ffffff90")
legend(2, -2, "Haut-Bas", xjust = 0.5,  yjust = 0.5, cex = 0.9,
       x.intersp = -0.5, y.intersp = 0.1, bg = "#ffffff90")
legend(2, 2, "Haut-Haut", xjust = 0.5,  yjust = 0.5, cex = 0.9,
       x.intersp = -0.5, y.intersp = 0.1, bg = "#ffffff90")
legend(0, 0, "Non Sign.", xjust = 0.5,  yjust = 0.5, cex = 0.9,
       x.intersp = -0.5, y.intersp = 0.1, bg = "#ffffff90")

# Cartographie
mf_map(type = "typo", x = Moran, var = "q", val_order = levels(Moran$q),
       pal = unique(cols), border = NA, leg_val_cex = 0.7,
       leg_title = NA, leg_pos = "bottomleft")
mf_map(dep,
       col = "#000",
       add = TRUE)

















# CASD
# FIDELI 2022

decile <- paste0("d", 1:9)
revenus <- c(11839, 15204, 18023, 20404, 22713, 25305, 28384, 32591, 40498)

f <- data.frame(
  decile = decile, 
  rev_ann = revenus
)

f$rev_moi <- round(f$rev_ann / 12, 0)



# FILOCOM 2022

decile <- paste0("d", 1:9)
revenus <- c(7285, 11331, 14519, 17323, 19893, 22778, 26518, 31769, 42029)

g <- data.frame(
  decile = decile, 
  rev_ann = revenus
)

g$rev_moi <- round(g$rev_ann / 12, 0)
