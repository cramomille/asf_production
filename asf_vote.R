
#                                        ASF - SCRIPT DE LA PLANCHE SUR LE VOTE
#
#                                                                antoine beroud
#                                                                  jean riviere

library(sf)
library(asf)
library(mapsf)
library(readxl)


# IMPORT DU FOND D'ALIETTE ROUX -----------------------------------------------
# Lecture du fichier
irisr_e <- st_read("input/asf_vote/IRISrE/AR04b_sf_IRISrE.shp")

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
data <- read_xlsx("input/asf_vote/data2022.xlsx")

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


# CREATION DE CARTES EXPLORATOIRES --------------------------------------------
# Creation des limites departementales
dep <- fondata
dep$DEP_CODE <- substr(dep$IRISE_C, 1, 2)
dep <- asf_borders(f = dep, by = "DEP_CODE")

# Boucle pour realiser toutes les cartes et les exporter en PDF
# Ouverture d'un fichier PDF
pdf(file = paste0("output/asf_vote/explo_decile.pdf"), width = 8, height = 8)

for (i in 7:21) {
  
  # Nom de la variable
  varname <- names(fondata)[i]
  
  # Palette
  pal <- c(
    "#95254c",
    "#e73458",
    "#f08590",
    "#f8c7c8",
    "#fce4e0",
    "#ecf4e3",
    "#cde4c4",
    "#8dc9a4",
    "#009c79",
    "#006757"
  )
  
  # Seuils
  d <- quantile(fondata[[i]], probs = seq(0, 1, 0.1), na.rm = TRUE)
  
  # Carte choroplethe
  mf_map(fondata,
         var = varname,
         type = "choro",
         breaks = d,
         pal = rev(pal),
         border = NA)
  
  # Contours departements
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


# CREAION DE CARTES SUR LES AXES
pal <- asf_palette("div", nb = 8)

var <- "Dim.1"
breaks <- c(-9, -6, -3, -1, 0, 1, 3, 6)

var <- "Dim.2"
breaks = c(-7, -6, -3, -1, 0, 1, 3, 6)

var <- "Dim.3"
breaks = c(-7, -6, -3, -1, 0, 1, 3, 6, 13)

mf_map(fondata, 
       var = var, 
       type = "choro",
       breaks = breaks,
       pal = pal,
       border = NA)
mf_map(dep, 
       col = "#ffffff", 
       lwd = 1, 
       add = TRUE)
mf_label(label, 
         var = "label", 
         col = "#000000", 
         font = 1)


# CREATION DE CARTES SUR LES CLASSES
var <- "Clust17"
nb <- length(unique(fondata[[var]]))
pal <- grDevices::rainbow(nb)

pal <- colorRampPalette(brewer.pal(12, "Paired"))(nb)

mf_map(fondata, 
       var = var, 
       type = "typo", 
       pal = pal,
       border = NA)
mf_map(dep, 
       col = "#ffffff", 
       lwd = 1, 
       add = TRUE)
mf_label(label, 
         var = "label", 
         col = "#000000", 
         font = 1)


# TYPOLOGIE DES CLASSES -------------------------------------------------------

x <- fondata

# Tableau ---------------------------------------------------------------------
# Conservation des colonnes utiles
vars <- x[, c(7:21)]
vars$geometry <- NULL

var_names <- names(vars)

typologie <- x[[27]]

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
# Ouverture d'un fichier PDF
pdf("output/graph_clust17.pdf", width = 12, height = 7)

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

# Fermeture du fichier PDF
dev.off()
