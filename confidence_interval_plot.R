########################################################################################################################################
#Script for drawing a graph with datapoints + 95% CI 
########################################################################################################################################
# Le fichier d'entrée contient 3 colonnes. 
# 1ere colonne : les noms des lignées des plantes étudiées (plant_line) (text)
# 2eme colonne : le facteur de groupage (ex : condition de culture, jour de la manip....) (grouping_factor)(text)
# 3eme colonne : la valeur mesurée (mesured_value)(numeric)


########################################################################################################################################
# 1. Données à analyser
########################################################################################################################################
# Choisir le répertoire de travail
setwd("PATH/TO/FILES")

# Initialisation de la variable DATA par le fichier contenant les données à analyser
DATA <- "file.xlsx"


########################################################################################################################################
# 2. Variables de personnalisation du graphique
########################################################################################################################################
# Définir la couleur des points
COULEUR <- "#d9c396"

# Palette de couleur pour les intervalles de confiance
PALETTE <- "Paired"

# Orientationd des étiquettes de l'axe X.
# Pour des étiquettes horizontales : angle = 0, vjust = 0, hjust=0.
# Pour des étiquettes tournées de 90° : angle = 90, vjust = 0.5, hjust=1
orientation_xlabels <- theme(axis.text.x = element_text(angle = 0, vjust = 0, hjust=0.5))

# Position et aspect des marqueurs de groupe
# Les valeurs par défaut produise un graph ou les parties du graph sont séparées avec le facteur
# de groupement dans un cadre à l'intérieur du graph.
# On peut modifier ainsi :
# parties jointives : panel.spacing = unit(0, "lines")
# Pas de cadre autour du facteur de groupement : strip.background = element_rect(colour=NA, fill=NA
# facteur de groupement à l'extérieur du graph :strip.placement = "outside" 
strip_pos <- theme(panel.spacing = unit(0.3, "lines"), 
                  strip.background = element_rect(colour="black", fill=NA),
                  strip.placement = "inside")


########################################################################################################################################
# 3. Installation et appel des paquets
########################################################################################################################################


if (!require(ggbeeswarm)) { install.packages("ggbeeswarm") }
if (!require(Rmisc)) { install.packages("Rmisc") }
if (!require(ggplot2)) { install.packages("ggplot2") }
if (!require(RColorBrewer)) { install.packages("RColorBrewer") }
if (!require(dplyr)) { install.packages("dplyr") }
if (!require(rcompanion)) { install.packages("rcompanion") }
if (!require(rstatix)) { install.packages("rstatix", repos = "https://cloud.r-project.org") }
if (!require(readxl)) { install.packages("readxl") }


library(ggplot2) # Pour faire le graph
library(ggbeeswarm) # pour la fonction geom_quasirandom
library(Rmisc) # pour la commande summarySE
library(RColorBrewer) # pour la définition des couleurs
library(rstatix)  # pour les tests statistiques
library(rcompanion) #pour le calcul de l'intervalle de confiance pour les données non paramétriques
library(dplyr) # pour les fonction %>%, group_by, summarise, select
library(readxl) # Pour charger les données au format excel


########################################################################################################################################
# 4. Appel des fonctions utilisées dans le script
########################################################################################################################################

#=======================================================================================================================================
# Fait le test de Kruskal-Wallis
# Retourne TRUE si au moins une médianes est significativement différente
#=======================================================================================================================================
check_kruskal <- function(kruskal_pval) {
    flag_kruskal <- FALSE
    
    for (i in 1 : nrow(kruskal_pval)) {
        if (kruskal_pval$p[i] < 0.05) {
            print (paste0("Le test de Kruskall Wallis compare les médianes, la pvalue pour le groupe ", kruskal_pval$grouping_factor[i] , " est < 0.05 ce qui indique qu’au moins 1 des médianes est différentes des autres, on réalise un test post hoc de Dunn"))
            flag_kruskal <- TRUE
        } else {
            print (paste0("Le test de Kruskall Wallis compare les médianes, la pvalue pour le groupe ", kruskal_pval$grouping_factor[i] , " est > 0.05 ce qui indique qu’il n'y a pas de différence entre les médianes"))
        }
    }    

    return(flag_kruskal)
}

#=======================================================================================================================================
# Fait le test de Dunn
# retourne les pvalue dans un dataframe
#=======================================================================================================================================
test_dunn <- function() {
    pval <- as.data.frame(df %>% group_by(grouping_factor) %>% dunn_test(mesured_value ~ plant_line, p.adjust.method = "BH"))
    print(df %>% group_by(grouping_factor) %>% dunn_test(mesured_value ~ plant_line, p.adjust.method = "BH"))
    return(pval)
}

#=======================================================================================================================================
# Réalise le graphique lorsque les données suivent une loi normale. L'intervalle de confiance est 
# construit autour de la moyenne
#=======================================================================================================================================
plot_normal <- function(df, my_colours, real_colname_df, my_summary) {
    p <- ggplot(data=df, aes(x=plant_line, y=mesured_value)) +
        geom_quasirandom(dodge.width=0.8,alpha = 0.6, colour=COULEUR) +
        geom_pointrange(data=my_summary, 
                        aes(ymin=mesured_value - ci, ymax=mesured_value + ci, color=plant_line),
                        position=position_dodge(width=0.8)) + 
        scale_colour_manual(values=my_colours) +
        scale_x_discrete(name = real_colname_df[1]) +
        scale_y_continuous(name = real_colname_df[3],
                           limits=c(0, max(df$mesured_value) + 0.1 * (max(df$mesured_value))),
                           expand = c(0, 0)) +
        facet_wrap(~ grouping_factor, strip.position = "bottom", scales = "free_x") +
        theme_classic() + 
        strip_pos +
        orientation_xlabels +
        theme(legend.title=element_blank())
    print(p)
}


#=======================================================================================================================================
# Réalise le graphique lorsque les données ne suivent pas une loi normale. L'intervalle de confiance est 
# construit autour de la médiane
#=======================================================================================================================================
plot_not_normal <- function(df, my_colours, real_colname_df, conf_int) {
    # La colonne "median" doit porter le nom "mesured_value" pour le ggplot
    names(conf_int)[4] <- "mesured_value"
    
    p <- ggplot(data=df, aes(x=plant_line, y=mesured_value)) +
        geom_quasirandom(dodge.width=0.8, alpha = 0.6, colour=COULEUR) +
        geom_pointrange(data=conf_int, aes(ymin=Percentile.lower, ymax=Percentile.upper, 
                                           color=plant_line), position=position_dodge(width=0.8)) +
        scale_colour_manual(values=my_colours) +
        scale_x_discrete(name = real_colname_df[1]) +
        scale_y_continuous(name = real_colname_df[3],
                           limits=c(0, max(df$mesured_value) + 0.1 * (max(df$mesured_value))),
                           expand = c(0, 0)) +
        facet_wrap(~ grouping_factor, strip.position = "bottom", scales = "free") +
        theme_classic() +  
        strip_pos +
        orientation_xlabels +
        theme(legend.title=element_blank())
    print(p)
}


#=======================================================================================================================================
# Vérifie la normalité des données. Sort de la boucle si au moins un des groupes de données ne suit 
# pas une loi normale.
#Retourne TRUE si les données suivent une loi normale
#=======================================================================================================================================
check_normality <- function(shapiro_df) {
    # On suppose que les données sont normales
    flag_normal <- TRUE
    
    for (i in 1 : nrow(shapiro_df)) {
        if(shapiro_df[i, 4] > 0.05) {
            # print(paste0("les données ",shapiro_df$grouping_factor[i],"-", 
            # shapiro_df$plant_line[i], " suivent une loi normale"), quote = FALSE)
            
        } else {
            # print(paste0("les données ",shapiro_df$grouping_factor[i],"-", 
            #          shapiro_df$plant_line[i], " ne suivent pas une loi normale"), quote = FALSE)
            
            # En fait les données ne sont pas normales, pas besoin d'aller plus loin
            flag_normal <- FALSE
            break
        }
    }
    return(flag_normal)
}


########################################################################################################################################
# 5. Corps du script
########################################################################################################################################

#Teste le type de données (txt ou xlsx) et réalise l'import en fonction

if(grepl("\\.txt$", DATA)) { 
    df <- read.table(DATA, header = TRUE, stringsAsFactors = TRUE, sep = '\t')
} else { 
    df <- read_excel(DATA, col_types = c("text", "text", "numeric"))
    df[,1:2] <- lapply(df[,1:2], factor)
}


# Compte le nombre de colonnes et affiche un erreur s'il est différent de 3
if (ncol(df)!=3) {
    print ("ERREUR : le nombre de colonnes n'est pas égal à 3")
    stop()
}

# Sauvegarde des noms des colonnes du fichier d'entrée
real_colname_df <- colnames(df)

# Remplacement des nom des colonnes par les noms génériques
colnames(df) <- c("plant_line", "grouping_factor", "mesured_value")

# Define color
my_colours = brewer.pal(n = 9, PALETTE)[9:3]

# Determining data normality status
shapiro_df <- df %>%group_by(plant_line, grouping_factor)%>%
    summarise(statistic = shapiro.test(mesured_value)$statistic, 
              p.value = shapiro.test(mesured_value)$p.value)

flag_normal <- check_normality(shapiro_df)

# Data treatement according to normality status
if(flag_normal == TRUE) {
    print("Les données suivent une loi normale")
    
    # Summary
    my_summary <- summarySE(df, measurevar="mesured_value", groupvars=c("plant_line", "grouping_factor"))
    
    # Plot
    plot_normal(df, my_colours, real_colname_df, my_summary)
    
    # Stats
    anova_results <- df %>% group_by(grouping_factor) %>%  anova_test(mesured_value ~ plant_line)
    tukey_results <- df %>%  group_by(grouping_factor) %>%  tukey_hsd(mesured_value ~ plant_line)
    
    # Sauver les fichiers
    write.table(my_summary, file = "Summary.txt", 
                quote = FALSE, row.names = FALSE, sep = '\t')
    write.table(anova_results, file = "Anova.txt", 
                quote = FALSE, row.names = FALSE, sep = '\t')
    write.table(tukey_results[, c(1,3,4, 9, 10)], file = "Tukey.txt", 
                quote = FALSE, row.names = FALSE, sep = '\t')
    #ggsave("Plot.svg", width=4, height=5)
    
    
} else {
    print("Les données ne suivent pas une loi normale")
    
    # Summary
    my_summary <- groupwiseMedian(data = df,
                                var = "mesured_value",
                                group = c("grouping_factor", "plant_line"),
                                conf       = 0.95,
                                R          = 5000,
                                percentile = TRUE,
                                bca        = FALSE,
                                digits     = 3)
    
    # Plot
    plot_not_normal(df, my_colours, real_colname_df, my_summary)
    
    # Stats
    kruskal_pval <- (df %>% group_by(grouping_factor)%>%kruskal_test(mesured_value ~ plant_line)) %>% select(grouping_factor, p)
    
    flag_kruskal <- check_kruskal(kruskal_pval)
    if (flag_kruskal == TRUE) { pval_dunn <- test_dunn() }
    
    # Sauver les fichiers
    write.table(my_summary, file = "Summary.txt", 
                quote = FALSE, row.names = FALSE, sep = '\t')
    write.table(kruskal_pval, file = "Kruskal.txt", 
                quote = FALSE, row.names = FALSE, sep = '\t')
   write.table(pval_dunn[, c(1, 3, 4, 8, 9, 10)], file = "Dunn.txt", 
                quote = FALSE, row.names = FALSE, sep = '\t')
    #ggsave("Plot.svg", width=4, height=5)
}

#Environnement
if (!require(devtools)) { install.packages("devtools") }

InfoSession <- devtools::session_info()

sink("InfoSession.txt")
print(InfoSession)
sink()
