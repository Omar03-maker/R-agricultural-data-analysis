# =============================================================================
# ANALYSE COMPLÈTE DES DONNÉES AGRONOMIQUES - VERSION AMÉLIORÉE
# =============================================================================

# Chargement des bibliothèques nécessaires
install.packages("PerformanceAnalytics" , dependencies = TRUE)
install.packages("ggcorrplot" , dependencies = TRUE)
install.packages("metan" , dependencies = TRUE)
install.packages("moments" , dependencies = TRUE)
install.packages("agricolae" , dependencies = TRUE)
install.packages("VIM" , dependencies = TRUE)
install.packages("multcomp" , dependencies = TRUE)
library(tidyverse)
library(readxl)
library(multcompView)
library(PerformanceAnalytics)
library(FactoMineR)
library(factoextra)
library(corrplot)
library(ggcorrplot)
library(metan)
library(moments)
library(agricolae)
library(plotly)
library(DT)
library(VIM)
library(car)
library(emmeans)
library(broom)
library(patchwork)
library(multcomp)

# =============================================================================
# 1. GÉNÉRATION DES DONNÉES D'EXEMPLE
# =============================================================================

set.seed(123)  # Pour la reproductibilité

# Définition des facteurs
regions <- c("Nord", "Sud")
varietes <- paste0("V", 1:9)
sites <- c("Kane_kane", "Keur_ngalgou", "Koki", "Madina", "Merina")
n_rep <- 3

# Création du design expérimental
A <- expand.grid(Region = regions,
                 Sites = sites,
                 Variete = varietes,
                 REP = 1:n_rep) %>%
  # Ajout d'effets spécifiques aux régions et variétés
  mutate(
    # Longueur de gousse (LGS) - cm
    LGS = case_when(Region == "Nord" ~ rnorm(n(), mean = 15.5, sd = 1.8),
                    Region == "Sud" ~ rnorm(n(), mean = 17.2, sd = 2.1)) + 
      case_when(Variete %in% c("V1", "V3", "V7") ~ rnorm(n(), mean = 2, sd = 0.5),
                Variete %in% c("V2", "V5") ~ rnorm(n(), mean = -1.5, sd = 0.5),
                TRUE ~ rnorm(n(), mean = 0, sd = 0.5)),
    
    # Poids total de graines par gousse (PTGS) - g
    PTGS = pmax(0.5, LGS * 6.2 + rnorm(n(), mean = 0, sd = 15)),
    
    # Nombre de graines par gousse (NgrGS)
    NgrGS = pmax(4, round(LGS * 0.8 + rnorm(n(), mean = 0, sd = 1.5))),
    
    # Poids d'une graine (Pgr) - g
    Pgr = pmax(0.05, PTGS / NgrGS + rnorm(n(), mean = 0, sd = 0.02)),
    
    # Poids de 100 graines (P100gr) - g
    P100gr = Pgr * 100 + rnorm(n(), mean = 0, sd = 2),
    
    # Date de 50% floraison (FLO50) - jours
    FLO50 = case_when(Region == "Nord" ~ rnorm(n(), mean = 45, sd = 3),
                      Region == "Sud" ~ rnorm(n(), mean = 39, sd = 2.5)) + 
      case_when(Variete %in% c("V1", "V2") ~ rnorm(n(), mean = -3, sd = 1),
                Variete %in% c("V8", "V9") ~ rnorm(n(), mean = 4, sd = 1),
                TRUE ~ rnorm(n(), mean = 0, sd = 1)),
    
    # Maturité à 100% (Mat100) - jours
    Mat100 = FLO50 + rnorm(n(), mean = 25, sd = 5),
    
    # Rendement (Rdt) - kg/ha
    Rdt = pmax(50, P100gr * 35 + 
                 ifelse(Region == "Sud", 200, -150) + 
                 rnorm(n(), mean = 0, sd = 100)),
    
    # Poids total de graines (PTgraine) - identique à PTGS pour cohérence
    PTgraine = PTGS) %>%
  # Arrondir les valeurs pour plus de réalisme
  mutate(across(c(LGS, PTGS, Pgr, P100gr, PTgraine, Rdt), ~ round(.x, 2)),
         across(c(NgrGS, FLO50, Mat100), ~ round(.x, 0)))

# Conversion en facteurs
A$Region <- as.factor(A$Region)
A$Variete <- as.factor(A$Variete)
A$Sites <- as.factor(A$Sites)
A$REP <- as.factor(A$REP)

# Affichage des premières lignes
cat("=== APERÇU DES DONNÉES ===\n")
print(head(A))

# =============================================================================
# 2. STATISTIQUES DESCRIPTIVES AMÉLIORÉES
# =============================================================================

cat("\n=== STATISTIQUES DESCRIPTIVES ===\n")

params_to_analyze <- c("LGS", "PTGS", "NgrGS", "Pgr", "P100gr", "Rdt", "FLO50", "Mat100")

# Fonction pour calculer les statistiques complètes
calc_descriptives <- function(data, vars) {
  # Calculer les statistiques une par une pour éviter les problèmes de nommage
  results <- data.frame(Parametre = vars,
                        n = sapply(vars, function(v) sum(!is.na(data[[v]]))),
                        Moyenne = sapply(vars, function(v) mean(data[[v]], na.rm = TRUE)),
                        Mediane = sapply(vars, function(v) median(data[[v]], na.rm = TRUE)),
                        Ecart_type = sapply(vars, function(v) sd(data[[v]], na.rm = TRUE)),
                        Variance = sapply(vars, function(v) var(data[[v]], na.rm = TRUE)),
                        CV = sapply(vars, function(v) (sd(data[[v]], na.rm = TRUE) / mean(data[[v]], na.rm = TRUE)) * 100),
                        Minimum = sapply(vars, function(v) min(data[[v]], na.rm = TRUE)),
                        Q1 = sapply(vars, function(v) quantile(data[[v]], 0.25, na.rm = TRUE)),
                        Q3 = sapply(vars, function(v) quantile(data[[v]], 0.75, na.rm = TRUE)),
                        Maximum = sapply(vars, function(v) max(data[[v]], na.rm = TRUE)),
                        Asymetrie = sapply(vars, function(v) {
      if(require(moments, quietly = TRUE)) {
        moments::skewness(data[[v]], na.rm = TRUE)
      } else {
        NA
      }
    }),
    Aplatissement = sapply(vars, function(v) {
      if(require(moments, quietly = TRUE)) {
        moments::kurtosis(data[[v]], na.rm = TRUE)
      } else {
        NA
      }
    }),
    stringsAsFactors = FALSE)
  
  # Arrondir les résultats
  results[, -1] <- round(results[, -1], 3)
  
  return(results)
}

# Statistiques globales
desc_stats_global <- calc_descriptives(A, params_to_analyze)
print(desc_stats_global)

# Statistiques par région
desc_stats_region <- A %>%
  group_by(Region) %>%
  calc_descriptives(params_to_analyze) %>%
  ungroup()

cat("\n=== STATISTIQUES PAR RÉGION ===\n")
print(desc_stats_region)

# =============================================================================
# 3. ANALYSE DE LA VARIANCE AMÉLIORÉE
# =============================================================================

cat("\n=== ANALYSE DE LA VARIANCE ===\n")

# Fonction pour effectuer l'ANOVA et extraire les résultats
perform_anova <- function(data, response_var, factors = "Region * Variete") {
  formula_str <- paste(response_var, "~", factors)
  model <- aov(as.formula(formula_str), data = data)
  
  # Résumé ANOVA
  anova_summary <- summary(model)
  
  # Tests de normalité et homoscédasticité
  shapiro_p <- shapiro.test(residuals(model))$p.value
  levene_p <- car::leveneTest(residuals(model) ~ data$Region * data$Variete)$`Pr(>F)`[1]
  
  # Coefficients de détermination
  r_squared <- summary(lm(as.formula(formula_str), data = data))$r.squared
  adj_r_squared <- summary(lm(as.formula(formula_str), data = data))$adj.r.squared
  
  return(list(summary = anova_summary,
              model = model,
              shapiro_p = shapiro_p,
              levene_p = levene_p,
              r_squared = r_squared,
              adj_r_squared = adj_r_squared))}

# Analyse pour chaque variable
anova_results <- map(params_to_analyze, ~{
  cat(paste("\n*** ANOVA pour", .x, "***\n"))
  result <- perform_anova(A, .x)
  print(result$summary)
  cat(paste("R² =", round(result$r_squared, 3), 
            "| R² ajusté =", round(result$adj_r_squared, 3), "\n"))
  cat(paste("Test normalité (Shapiro) p =", round(result$shapiro_p, 4), "\n"))
  cat(paste("Test homoscédasticité (Levene) p =", round(result$levene_p, 4), "\n"))
  return(result)
}) %>% set_names(params_to_analyze)

# =============================================================================
# 4. TESTS POST-HOC ET COMPARAISONS MULTIPLES
# =============================================================================

cat("\n=== TESTS POST-HOC ===\n")

# Fonction simplifiée pour les comparaisons post-hoc
perform_posthoc_simple <- function(data, response_var, factor_var) {
  formula_str <- paste(response_var, "~", factor_var)
  model <- aov(as.formula(formula_str), data = data)
  
  # Test de Tukey
  tukey_result <- TukeyHSD(model)
  
  # Test HSD de Tukey avec agricolae (plus robuste)
  hsd_result <- agricolae::HSD.test(model, factor_var, group = TRUE)
  
  return(list(tukey = tukey_result,
              hsd = hsd_result,
              groups = hsd_result$groups))}

# Exemple pour le rendement
cat("Test de Tukey pour le rendement par variété :\n")
rdt_posthoc <- perform_posthoc_simple(A, "Rdt", "Variete")
rdt_posthoc
print(rdt_posthoc$groups)

# Tests post-hoc pour d'autres variables importantes
cat("\nTest de Tukey pour P100gr par variété :\n")
p100_posthoc <- perform_posthoc_simple(A, "P100gr", "Variete")
print(p100_posthoc$groups)

# Comparaisons par région
cat("\nComparaison des rendements entre régions :\n")
region_posthoc <- perform_posthoc_simple(A, "Rdt", "Region")
print(region_posthoc$groups)

# =============================================================================
# 5. ANALYSE MULTIVARIÉE AVANCÉE
# =============================================================================

cat("\n=== ANALYSE MULTIVARIÉE ===\n")

# Préparation des données numériques (vérifier les noms de colonnes disponibles)
cat("Colonnes disponibles dans A :\n")
print(colnames(A))

# Sélectionner uniquement les colonnes qui existent
available_params <- intersect(params_to_analyze, colnames(A))
cat("Variables numériques disponibles :\n")
print(available_params)

numeric_data <- A[, available_params, drop = FALSE]
numeric_data <- na.omit(numeric_data)

cat("Dimensions des données numériques :", dim(numeric_data), "\n")

# Matrice de corrélation améliorée
cor_matrix <- cor(numeric_data)

# Visualisation de la corrélation
p1 <- ggcorrplot(cor_matrix, 
                 hc.order = TRUE, 
                 type = "lower",
                 lab = TRUE, 
                 lab_size = 3, 
                 colors = c("#6D9EC1", "white", "#E46726"),
                 title = "Matrice de corrélation des variables")

print(p1)

# Test de significativité des corrélations
cor_test <- cor.mtest(numeric_data, conf.level = 0.95)

# ACP (Analyse en Composantes Principales)
pca_result <- PCA(numeric_data, scale.unit = TRUE, graph = FALSE)

# Résumé de l'ACP
cat("\nRésumé de l'ACP :\n")
print(pca_result$eig[1:5,])

# Contribution des variables
cat("\nContribution des variables aux axes principaux :\n")
print(pca_result$var$contrib[,1:3])

# Visualisations ACP
p2 <- fviz_pca_var(pca_result, col.var = "contrib",
                   gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                   repel = TRUE, title = "Variables - ACP")
print(p2)

p3 <- fviz_pca_biplot(pca_result, repel = TRUE, col.var = "#2E9FDF",
                      col.ind = "#696969", title = "Biplot ACP")
print(p3)

# =============================================================================
# 6. ANALYSE GGE BIPLOT AMÉLIORÉE
# =============================================================================

cat("\n=== ANALYSE GGE BIPLOT ===\n")

# Modèle GGE pour l'interaction Génotype × Environnement
gge_model <- gge(A, env = Sites, gen = Variete, resp = Rdt, verbose = FALSE)

# Performance moyenne et stabilité
p4 <- plot(gge_model, type = 2, 
           title = "Performance moyenne et stabilité des génotypes",
           col.gen = "red", col.env = "blue")
print(p4)

# Évaluation des génotypes dans les environnements
p5 <- plot(gge_model, type = 3, 
           title = "Quel génotype a gagné où ?",
           col.gen = "darkgreen", col.env = "orange")
print(p5)

# =============================================================================
# 7. ANALYSES SUPPLÉMENTAIRES PROPOSÉES
# =============================================================================

cat("\n=== ANALYSES SUPPLÉMENTAIRES ===\n")

# 7.1 Analyse de clusters (classification des variétés)
cluster_data <- numeric_data
cluster_result <- kmeans(scale(cluster_data), centers = 3, nstart = 25)

A$Cluster <- factor(cluster_result$cluster[match(paste(A$Region, A$Variete, A$Sites, A$REP),
                                                 paste(A$Region, A$Variete, A$Sites, A$REP))])

p6 <- fviz_cluster(cluster_result, data = scale(cluster_data),
                   palette = c("#2E9FDF", "#00AFBB", "#E7B800"),
                   geom = "point", ellipse.type = "convex",
                   ggtheme = theme_bw(), title = "Classification des observations")
print(p6)

# 7.2 Indice de sélection multi-traits
# Calcul d'indices économiques pondérés
# =============================================================================
# 7.2 Indice de sélection multi-traits - VERSION FINALE CORRIGÉE
# =============================================================================

# Calcul d'indices économiques pondérés
weights <- c(Rdt = 0.4, P100gr = 0.2, FLO50 = -0.2, Mat100 = -0.1, LGS = 0.1)

# SOLUTION 1 : Pipeline complet corrigé
selection_index <- A %>%
  group_by(Variete, Region) %>%
  summarise(Rdt_mean = mean(Rdt, na.rm = TRUE),
            P100gr_mean = mean(P100gr, na.rm = TRUE),
            FLO50_mean = mean(FLO50, na.rm = TRUE),
            Mat100_mean = mean(Mat100, na.rm = TRUE),
            LGS_mean = mean(LGS, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(
    std_Rdt = (Rdt_mean - mean(Rdt_mean)) / sd(Rdt_mean),
    std_P100gr = (P100gr_mean - mean(P100gr_mean)) / sd(P100gr_mean),
    std_FLO50 = (FLO50_mean - mean(FLO50_mean)) / sd(FLO50_mean),
    std_Mat100 = (Mat100_mean - mean(Mat100_mean)) / sd(Mat100_mean),
    std_LGS = (LGS_mean - mean(LGS_mean)) / sd(LGS_mean),
    Index_Selection = std_Rdt * 0.4 + 
      std_P100gr * 0.2 + 
      std_FLO50 * (-0.2) + 
      std_Mat100 * (-0.1) + 
      std_LGS * 0.1
  )

# Utiliser explicitement dplyr::select pour éviter les conflits
selection_index_final <- selection_index %>%
  dplyr::select(Variete, Region, Index_Selection) %>%
  arrange(desc(Index_Selection))

# Affichage du résultat
cat("Classement des variétés selon l'indice de sélection :\n")
print(selection_index_final)

# 7.3 Analyse de la stabilité (coefficient de variation)
stability_analysis <- A %>%
  group_by(Variete) %>%
  summarise(Rdt_moyen = mean(Rdt, na.rm = TRUE),
            Rdt_cv = (sd(Rdt, na.rm = TRUE) / mean(Rdt, na.rm = TRUE)) * 100,
            P100gr_moyen = mean(P100gr, na.rm = TRUE),
            P100gr_cv = (sd(P100gr, na.rm = TRUE) / mean(P100gr, na.rm = TRUE)) * 100,
            .groups = "drop"
  ) %>%
  arrange(Rdt_cv)

cat("\nAnalyse de stabilité (CV%) :\n")
print(stability_analysis)

# 7.4 Régression multiple pour prédiction du rendement
regression_model <- lm(Rdt ~ LGS + P100gr + FLO50 + Mat100 + Region, data = A)
cat("\nModèle de régression pour prédire le rendement :\n")
print(summary(regression_model))

# 7.5 Analyse de la variabilité génétique (héritabilité)
if(require(variability, quietly = TRUE)) {
  heritability <- gen.var(A[params_to_analyze], A$Variete, A$REP)
  cat("\nEstimation de l'héritabilité :\n")
  print(heritability)}

# =============================================================================
# 8. VISUALISATIONS AVANCÉES
# =============================================================================

# Graphique en radar des performances moyennes par variété
if(require(ggradar, quietly = TRUE)) {
  radar_data <- A %>%
    group_by(Variete) %>%
    summarise(across(c(Rdt, P100gr, LGS, NgrGS), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
    mutate(across(-Variete, ~ scales::rescale(.x, to = c(0, 1))))
  
  p7 <- ggradar(radar_data, grid.label.size = 3, axis.label.size = 3,
                group.point.size = 2, title = "Profil des variétés")}
print(p7)

# Heatmap des performances
p8 <- A %>%
  group_by(Variete, Sites) %>%
  summarise(Rdt_moyen = mean(Rdt, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = Sites, y = Variete, fill = Rdt_moyen)) +
  geom_tile() +
  scale_fill_gradient2(low = "red", mid = "white", high = "darkgreen", 
                       midpoint = mean(A$Rdt, na.rm = TRUE)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Heatmap des rendements par variété et site",
       fill = "Rendement\n(kg/ha)")

print(p8)

# =============================================================================
# 9. EXPORT DES RÉSULTATS
# =============================================================================

# Sauvegarde des données
write.csv(A, "donnees_exemple_agronomiques.csv", row.names = FALSE)
write.csv(desc_stats_global, "statistiques_descriptives.csv", row.names = FALSE)
write.csv(selection_index, "indice_selection.csv", row.names = FALSE)
write.csv(stability_analysis, "analyse_stabilite.csv", row.names = FALSE)

cat("\n=== ANALYSE TERMINÉE ===\n")
cat("Fichiers exportés :\n")
cat("- donnees_exemple_agronomiques.csv\n")
cat("- statistiques_descriptives.csv\n")
cat("- indice_selection.csv\n")
cat("- analyse_stabilite.csv\n")

# Résumé des principales conclusions
cat("\n=== RÉSUMÉ DES PRINCIPALES CONCLUSIONS ===\n")
cat("1. Variété la plus performante (indice de sélection) :", 
    selection_index$Variete[1], "\n")
cat("2. Variété la plus stable (CV rendement) :", 
    stability_analysis$Variete[1], "\n")
cat("3. R² du modèle de prédiction du rendement :", 
    round(summary(regression_model)$r.squared, 3), "\n")

# =============================================================================
# FIN DU SCRIPT
# =============================================================================