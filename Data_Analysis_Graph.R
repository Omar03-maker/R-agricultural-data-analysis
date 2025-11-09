# Installation du package fmsb si nécessaire
if (!requireNamespace("fmsb", quietly = TRUE)) {
  install.packages("fmsb")
}
library(fmsb)

# Lecture du fichier CSV avec encodage approprié
donnees <- read.csv("C:/R_Skills/Agricole.csv", 
                    sep = ";", 
                    dec = ",", 
                    header = TRUE,
                    fileEncoding = "Latin1",
                    stringsAsFactors = FALSE)

# S'assurer que les noms de colonnes sont corrects
# Si nécessaire, renommer les colonnes
names(donnees) <- c("Variete", "Famille", "Rendement", "Besoin_eau", 
                    "Resistance_maladies", "Teneur_proteines", 
                    "Cycle_jours", "Apport_NPK")

# Vérification des noms de colonnes après renommage
print(names(donnees))

# Conversion des colonnes en numérique une par une pour éviter les erreurs
donnees$Rendement <- as.numeric(as.character(donnees$Rendement))
donnees$Besoin_eau <- as.numeric(as.character(donnees$Besoin_eau))
donnees$Resistance_maladies <- as.numeric(as.character(donnees$Resistance_maladies))
donnees$Teneur_proteines <- as.numeric(as.character(donnees$Teneur_proteines))
donnees$Cycle_jours <- as.numeric(as.character(donnees$Cycle_jours))
donnees$Apport_NPK <- as.numeric(as.character(donnees$Apport_NPK))

# Extraction des variables numériques
variables_numeriques <- donnees[, c("Rendement", "Besoin_eau", "Resistance_maladies", 
                                    "Teneur_proteines", "Cycle_jours", "Apport_NPK")]

# Création d'une matrice avec les noms de variétés en rownames
radar_data <- as.data.frame(variables_numeriques)
rownames(radar_data) <- donnees$Variete

# Normalisation pour placer toutes les variables sur une échelle comparable
normaliser <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) * 100
}
radar_data <- as.data.frame(lapply(radar_data, normaliser))

# Ajout des valeurs min et max requises par la fonction radarchart
radar_data <- rbind(rep(100, ncol(radar_data)),  # Max pour chaque variable
                    rep(0, ncol(radar_data)),    # Min pour chaque variable
                    radar_data)

# Définition des couleurs pour chaque variété - couleurs spécifiques pour correspondre à l'exemple
couleurs <- c("#E57373", "#66BB6A", "#42A5F5", "#78909C")  # Rouge, Vert, Cyan, Violet
couleurs_transparentes <- adjustcolor(couleurs, alpha.f = 0.3)

# Création du graphique radar
par(mar = c(2, 2, 2, 2))  # Ajustement des marges
radarchart(
  radar_data,
  pcol = couleurs,       # Couleurs des lignes
  pfcol = couleurs_transparentes,  # Couleurs de remplissage
  plwd = 2,              # Épaisseur des lignes
  plty = 1,              # Type de ligne: 1 pour solide (sans pointillés)
  cglcol = "grey",       # Couleur de la grille
  cglty = 1,             # Type de ligne de la grille
  axislabcol = "grey30", # Couleur des étiquettes d'axes
  caxislabels = c("0%", "25%", "50%", "75%", "100%"),  # Étiquettes des cercles
  title = "Caractéristiques des cultures")

# Récupération des vrais noms des cultures pour la légende
noms_cultures <- donnees$Variete

# Ajout d'une légende avec les vrais noms au lieu des numéros
legend(
  "topright",
  legend = noms_cultures,  # Utiliser les noms réels des cultures
  col = couleurs,
  lty = 1,
  lwd = 2,
  pch = 20,
  bty = "n",
  cex = 0.8)


