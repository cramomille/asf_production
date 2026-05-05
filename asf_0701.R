
#                                               ASF - SCRIPT DE LA PLANCHE 0701                                     
#                                                          ENTRE-SOI DES RICHES
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
                 maille = "COMr2_CODE",
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
                       by.x = "COMr2_CODE", by.y = "comar")


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
Moran$COMr2_CODE <- substr(Moran$COMr2_CODE, 1, 5)
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



# CASD ------------------------------------------------------------------------
# Fond geographique
mar <- asf_mar(
  md = "iris_xxxx", 
  ma = "iris_r2", 
  geom = TRUE, 
  dir = "input/mar/"
)

tabl <- mar$tabl
geom <- mar$geom

fond <- asf_fond(geom, tabl, by = "IRISF_CODE", maille = "IRISrD_CODE")
fond <- asf_drom(fond)
fond_05 <- asf_simplify(fond, keep = 0.5)
fond_09 <- asf_simplify(fond, keep = 0.9)

z <- asf_zoom(f = fond_09, 
              places = c("5", "4", "Dijon", "Reims", "Rouen"), 
              # coords = c(2.584, 49.207), labels = "Senlis", 
              nb_cols = 7)


# CARTO 1 ----
x <- readRDS("input/asf_0701/tx_80100.rds")

c <- asf_fondata(f = fond_05, z = z[[1]], d = x, by = "IRISrD_CODE")

sum(nchar(as.character(c$class_tx_c80100_2022)), na.rm = TRUE) - 
  sum(nchar(as.character(x$class_tx_c80100_2022)), na.rm = TRUE)

palette <- c("#878787", 
             "#a8a8a7", 
             "#c6c6c6", 
             "#e3e3e3", 
             "#ffefaf", 
             "#ffdb7c", 
             "#fbbf69", 
             "#f28b52", 
             "#eb5e50", 
             "#d72738")

mf_map(c, var = "class_tx_c80100_2022", type = "typo", pal = palette, border = NA, 
       val_order = c("0-5", "5-10", "10-15", "15-20", "20-25", "25-30", "30-35", 
                     "35-40", "40-60", "60-77.322"))

zoom <- z[[1]]
zoom$COM <- substr(zoom$IRISrD_CODE, 1, 5)
com <- asf_borders(zoom, by = "COM", keep = 0.5)
mf_map(com, col = "#fff", add = TRUE)

mf_label(z[[2]], var = "label")


# CARTO 2 ----
x <- readRDS("input/asf_0701/riche_ql_2022_secret.rds")

class <- function(x) {
  cut(x,
      breaks = c(min(x, na.rm = TRUE), 0.6, 1, 1.5, max(x, na.rm = TRUE)),
      labels = c("j", "l", "m", "h"),
      include.lowest = TRUE)
}

x$v1_class <- class(x$ql_c80_90)
x$v2_class <- class(x$ql_c90_100)

x$v1_v2_class <- paste0(x$v1_class, x$v2_class)

c <- asf_fondata(f = fond_05, z = z[[1]], d = x, by = "IRISrD_CODE")

palette <- c(
  "jj" = "#e9e5ec", 
  "lj" = "#ded8de", 
  "mj" = "#f5b3bd", 
  "hj" = "#ed6c77", 
  
  "jl" = "#ded8de", 
  "ll" = "#ded8de", 
  "ml" = "#f5b3bd", 
  "hl" = "#ed6c77", 
  
  "jm" = "#a5d7d5", 
  "lm" = "#a5d7d5", 
  "mm" = "#b3a6af", 
  "hm" = "#c2435e", 
  
  "jh" = "#5abeb9", 
  "lh" = "#5abeb9", 
  "mh" = "#00889d", 
  "hh" = "#564770"
)

mf_map(c, "v1_v2_class", type = "typo", pal = palette, border = NA, 
       val_order = c("jj", "lj", "mj", "hj",
                     "jl", "ll", "ml", "hl",
                     "jm", "lm", "mm", "hm",
                     "jh", "lh", "mh", "hh"))


# CARTO 3 ----
x <- readRDS("input/asf_0701/riche_ql_2015_secret.rds")
y <- readRDS("input/asf_0701/riches_2015_2022_c80100.rds")

x <- merge(x[, c(1, 2, 16)], y[, c(1, 6)], by = "IRISrD_CODE", all = TRUE)

c <- asf_fondata(f = fond_05, z = z[[1]], d = x, by = "IRISrD_CODE")

sum(is.na(c$ql_c80_100))
sum(is.na(c$diff_2022_2015))


# Variation ----
mf_distr(c$diff_2022_2015)
pal <- c("#6071b5", "#8e9ed1", "#bccdeb", "#dfeaf8", "#feebdc", "#fbceb4", "#f28d65", "#dc0d15")

mf_map(c, var = "diff_2022_2015", type = "choro",
       breaks = c(min(c$diff_2022_2015, na.rm = TRUE),
                  -6, -4, -2, 0, 2, 4, 6,
                  max(c$diff_2022_2015, na.rm = TRUE)),
       pal = pal, border = NA)


# Typologie ----
mf_distr(c$ql_c80_100)
mf_distr(c$diff_2022_2015)

quantile(c$ql_c80_100, probs = c(1/3, 2/3), na.rm = TRUE)
quantile(c$diff_2022_2015, probs = c(1/3, 2/3), na.rm = TRUE)

class_ql <- function(x) {
  cut(x,
      breaks = c(min(x, na.rm = TRUE), 0.8, 1.2, max(x, na.rm = TRUE)),
      labels = c("l", "m", "h"),
      include.lowest = TRUE)
}

class_var <- function(x) {
  cut(x,
      breaks = c(min(x, na.rm = TRUE), -2, 2, max(x, na.rm = TRUE)),
      labels = c("l", "m", "h"),
      include.lowest = TRUE)
}

c$typo_ql <- class_ql(c$ql_c80_100)
c$typo_var <- class_var(c$diff_2022_2015)

c$typo_class <- paste0(c$typo_ql, c$typo_var) 

pal <- c(
  "ll" = "#6bbfa3", 
  "ml" = "#cabdb1", 
  "hl" = "#f5b3bd", 
  
  "lm" = "#c1deba", 
  "mm" = "#f3dece", 
  "hm" = "#ed6c77", 
  
  "lh" = "#ffda6f", 
  "mh" = "#f9b122", 
  "hh" = "#c14352"
)

mf_map(c, "typo_class", 
       type = "typo", 
       pal = pal, 
       border = NA, 
       val_order = c("ll", "ml", "hl", "lm", "mm", "hm", "lh", "mh", "hh"))


# Repartition ----

tmp <- merge(c, tabl[!duplicated(tabl$IRISrD_CODE), c(3, 16, 18)], by = "IRISrD_CODE", all.x = TRUE)
tmp <- tmp[!(tmp$typo_class %in% c("lNA", "NANA")), ]

# Les menages
asf_plot_vars(d = tmp, vars = "TOT", typo = "typo_class", 
              order.t = c("ll", "lm", "ml", "mm", "lh", "mh", "hl", "hm", "hh"), 
              pal = pal, eff = TRUE)

asf_plot_vars(d = tmp, vars = "TOT", typo = c("CATEAAV2020", "typo_class"), 
              order.t = c("ll", "lm", "ml", "mm", "lh", "mh", "hl", "hm", "hh"), 
              pal = pal, eff = TRUE)

asf_plot_vars(d = tmp, vars = "TOT", typo = c("TAAV2017", "typo_class"), 
              order.t = c("ll", "lm", "ml", "mm", "lh", "mh", "hl", "hm", "hh"), 
              pal = pal, eff = FALSE)

# Les IRIS
asf_plot_typo(d = tmp, vars = "typo_class", typo = c("CATEAAV2020"), 
              order.v = c("ll", "lm", "ml", "mm", "lh", "mh", "hl", "hm", "hh"),
              pal = pal)

asf_plot_typo(d = tmp, vars = "typo_class", typo = c("TAAV2017"), 
              order.v = c("ll", "lm", "ml", "mm", "lh", "mh", "hl", "hm", "hh"),
              pal = pal)

asf_plot_typo(d = tmp, vars = "typo_class", typo = c("TAAV2017", "CATEAAV2020"), 
              order.v = c("ll", "lm", "ml", "mm", "lh", "mh", "hl", "hm", "hh"),
              pal = pal)
asf_plot_typo(d = tmp, vars = "typo_class", typo = c("TAAV2017", "CATEAAV2020"), 
              order.v = c("ll", "lm", "ml", "mm", "lh", "mh", "hl", "hm", "hh"),
              pal = pal, eff = TRUE)
