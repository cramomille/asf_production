
#                              ASF - SCRIPT DE LA PLANCHE SUR L'AGE DES MENAGES
#
#                                                                antoine beroud
#                                                                  jean riviere

library(sf)
library(asf)
library(mapsf)

library(readxl)

###############################################################################
########################################################## FONDS D'ALIETTE ROUX

# Lecture du fichier
irisr_e <- st_read("input/IRISrE/AR04b_sf_IRISrE.shp")
irisr_e <- st_transform(irisr_e, crs = "EPSG:2154")

# Repositionnement des DROM
irisr_e <- asf_drom(f = irisr_e, id = "IRISE_C")

# Creation de zooms
z <- asf_zoom(f = irisr_e, 
              places = c("5", "4"), 
              r = 10000)

zoom <- z$zooms
label <- z$labels
point <- z$point

# Simplification des geometries du fond principal
irisr_e_simply <- asf_simplify(f = irisr_e, keep = 0.1)

# Lecture des donnees
data <- read_xlsx("input/DataDemo.xlsx")

# Ajout des zeros manquants dans les identifiants
data$IRISrE_CODE <- ifelse(nchar(data$IRISrE_CODE) < 9,
                           paste0(strrep("0", 9 - nchar(data$IRISrE_CODE)), data$IRISrE_CODE),
                           data$IRISrE_CODE)

# Jointure entre le fond, les zooms et les donnees
fondata <- asf_fondata(f = irisr_e_simply, 
                       z = zoom, 
                       d = data, 
                       by.x = "IRISE_C", 
                       by.y = "IRISrE_CODE")

# Creation des limites departementales
dep <- fondata
dep$DEP_CODE <- substr(dep$IRISE_C, 1, 2)
dep <- asf_borders(f = dep, by = "DEP_CODE")

# Choix d'une palette
pal <- c(
  "#e84249",
  "#ed6ea7",
  "#fdc533",
  "#81c185",
  "#78b7e5",
  "#8779b7",
  
  "#f28a48",
  "#a8589e",
  "#00a984",
  
  "#f2908e",
  "#774a59",
  "#5c79bb"
)

mf_map(fondata, 
       var = "Clust6", 
       type = "typo", 
       pal = pal,
       border = NA)

mf_map(dep, 
       col = "white", 
       lwd = 1, 
       add = TRUE)

mf_label(label, 
         var = "label", 
         col = "#000000", 
         font = 1)

###############################################################################
############################################################## TYPO DES CLASSES

x <- fondata

# Tableau ---------------------------------------------------------------------
# Conservation des colonnes utiles
vars <- x[, 2:(ncol(x) - 4)]
vars$geometry <- NULL

var_names <- names(vars)

typologie <- x[[22]]

# Calcul des moyennes pour la France entiere
moy_glob <- colMeans(vars, na.rm = TRUE)

# Calcul des moyennes par classe
moy_typo <- aggregate(vars, by = list(Classe = typologie), FUN = mean, na.rm = TRUE)

# Calcul de l'ecart a la moyenne nationale
ecart_moy <- as.data.frame(t(apply(moy_typo[, -1], 1, function(x) x - moy_glob)))
names(ecart_moy) <- paste0(var_names, "_ecart")

# Calcul de l'ecart-type global (pour chaque tranche d’age)
ecart_type <- apply(vars, 2, sd, na.rm = TRUE)

# Calcul de l'ecart standardise (z-score)
z_score <- as.data.frame(t(apply(moy_typo[, -1], 1, function(x) (x - moy_glob) / ecart_type)))
names(z_score) <- paste0(var_names, "_zscore")

# Fusion des resultats
result <- cbind(moy_typo, ecart_moy, z_score)


# Graphiques ------------------------------------------------------------------

# Ouvrir un fichier PDF
pdf("output/graph_clust12.pdf", width = 12, height = 7)

# Boucle sur chaque classe
for (i in 1:nrow(result)) {

  # Traitements pour la classe i
  moyennes <- as.numeric(result[i, var_names])
  ecarts   <- as.numeric(result[i, paste0(var_names, "_ecart")])
  zscores  <- as.numeric(result[i, paste0(var_names, "_zscore")])
  
  # Construction d'un vecteur avec les 3 groupes
  bar_values <- c(moyennes, ecarts, zscores)
  
  # Noms des barres
  group_labels <- c(
    paste0("Moy_", var_names),
    paste0("Écart_", var_names),
    paste0("Z_", var_names)
  )
  
  # Creation du barplot
  classe_nom <- as.character(result$Classe[i])
  par(mar = c(10, 4, 4, 2))  # marges pour les noms en abscisse
  barplot(bar_values,
          names.arg = group_labels,
          las = 2, # orientation verticale des labels
          col = rep(c("steelblue", "darkorange", "darkred"), each = length(var_names)),
          border = NA,
          main = paste("Classe", classe_nom, ": Moyennes, ecarts et z-scores par tranche d'age"),
          ylab = "Valeur")
  
  abline(h = 0, lty = 2)
}

# Fermer le PDF
dev.off()

###############################################################################
################################################################## CARTES EXPLO

# Boucle pour realiser toutes les cartes et les exporter en PDF
for (i in 2:(length(names(fondata)) - 4)) {

  # Nom de la variable
  varname <- names(fondata)[i]

  # Ouvrir un fichier PDF
  pdf(file = paste0("carte_", varname, ".pdf"), width = 8, height = 8)

  # Palette
  pal <- asf_palette(pal = "peche", nb = 6)

  # Seuils
  q6 <- quantile(fondata[[i]], probs = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1), na.rm = TRUE)

  # Carte choroplethe
  mf_map(fondata,
         var = varname,
         type = "choro",
         breaks = q6,
         pal = pal,
         border = NA)

  # Contours departements
  mf_map(dep,
         col = "white",
         lwd = 1,
         add = TRUE)

  # Labels
  mf_label(label,
           var = "label",
           col = "#000000",
           font = 1)

  # Fermer le PDF
  dev.off()
}
