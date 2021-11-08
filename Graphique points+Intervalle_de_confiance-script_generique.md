# Graphique points + Intervalle de confiance - script générique

Le script `confidence_interval_plot.R` est un script générique qui :

- Détermine la distribution des données (parmétriques ou non paramétriques)
- Fait les tests statistiques adaptés (Anova + Tukey ou Kruskal-Wallis + Dunn)
- Représente l'ensemble des points et l'intervalle de confiance centré sur la moyenne ou la médiane selon la nature des données.

## Structure des données

Les données doivent être au format `.txt` ou `.xlsx ` et  comporter 3 colonnes dans l'ordre suivant : lignées (bactéries, plantes...), facteur de groupement (traitement, jour de la manip...) et valeur mesurée.

Exemple :

| Line  | Treatment | Value |
| ----- | --------- | ----- |
| line1 | A         | 0.733 |
| line2 | A         | 0.750 |
| line3 | A         | 0.726 |
| line1 | B         | 0.829 |
| line2 | B         | 0.806 |
| line3 | B         | 0.795 |

Si le nombre de colonne n'est pas égal à 3, le message suivant va s'afficher et le script s'arrêter :

```
"ERREUR : le nombre de colonnes n'est pas égal à 3"
```

## Les variables de personnalisation du graphique

### Définir la couleur des points

```R
COULEUR <- "#d9c396"
```

Le code hexadécimal des couleurs est utilisé. 

Pour modifier la couleur, taper le code (ex : "#d9c396") dans la barre de recherche Google. La page de résultat commence par un pavé interactif pour le choix des couleurs.

<img src=".images/Graphique points + Intervalle de confiance - script générique/image-20210517161736266.png" alt="image-20210517161736266" style="zoom:50%;" />

1 - Choisissez la nuance sur la barre "Arc en ciel"

2 - Déplacez le rond blanc sur la zone de votre choix

3 - Le code hexadécimal de la couleur apparait dans le cadre HEX

### Choisir la palette de couleur pour les intervalles de confiance

```R
PALETTE <- "Paired"
```

Ci dessous la liste des palettes compatibles avec les daltoniens;

<img src=".images/Graphique points + Intervalle de confiance - script générique/colopicker.png" alt="colopicker" style="zoom:50%;" />

Vous pouvez aussi faire apparaitre la liste en tapant :

```R
display.brewer.all(colorblindFriendly = TRUE)
```

1. **Palettes séquentielles** (première liste de couleurs), qui sont adaptées aux données ordonnées qui évoluent de bas en haut (gradient). Les noms des palettes sont : Blues, BuGn, BuPu, GnBu, Greens, Greys, Oranges, OrRd, PuBu, PuBuGn, PuRd, Purples, RdPu, Reds, YlGn, YlGnBu YlOrBr, YlOrRd.
2. **Palettes qualitatives** (deuxième liste de couleurs), qui conviennent le mieux pour représenter des données nominales ou catégorielles. Ils n’impliquent pas de différences d’ampleur entre les groupes. Les noms des palettes sont : Dark2, Paired, Set2.
3. **Palettes divergentes** (troisième liste de couleurs), qui mettent autant l’accent sur les valeurs critiques du milieu et les extrêmes aux deux extrémités de la plage de données. Les palettes divergentes sont : BrBG, PiYG, PRGn, PuOr, RdBu, RdYlBu

La commande (ligne 177) indique que les couleurs 3 à 9 de la palette choisie seront utilisées en commençant par le couleur 9. Libre à vous de modifier ce choix, voire de l'enlever complètement.

```R
my_colours = brewer.pal(n = 9, PALETTE)[9:3]
```

### Choisir l'aspect du graph

- <u>Orientation des étiquettes de l'axe des abscisses</u>

```R
orientation_xlabels <- theme(axis.text.x = element_text(angle = 0, vjust = 0, hjust=0.5))
```

Les étiquettes de l'axe peuvent être orientée horizontalement ou tournées de 90°.

La valeur par défaut est : horizontale.

Pour modifier ce paramètre il faut modifier les valeur de la variable `orientation_xlabels` (ligne 18). 

Les valeurs par défaut sont : `angle = 0, vjust = 0, hjust=0`

Pour tourner les étiquettes : `angle = 90, vjust = 0.5, hjust=1`

- <u>Position et aspect des éléments de groupement</u>

    ```R
    strip_pos <- theme(panel.spacing = unit(0.3, "lines"), 
                      strip.background = element_rect(colour="black", fill=NA),
                      strip.placement = "inside")
    ```

    Par défaut le graph présente n (nombre de niveaux du facteur de groupement) panneaux séparés, avec mention du facteur de groupement correspondnat dans un cadre à l'intérieur du graph.

    Les valeurs par défaut sont : 

    - Pour l'espacement entre les panneaux

    `panel.spacing = unit(0.3, "lines")`

    La valeur peut être diminuée (0) ou augmentée selon que l'on souhaite plus ou moins espacer les panneaux

    - Pour l'aspect du "strip" contenant le facteur de groupement

    `strip.background = element_rect(colour="black", fill=NA)`

    Pour supprimer le cadre il faut remplacer `"black"` par `NA`

    - Pour la position du  "strip" contenant le facteur de groupement

    `strip.placement = "inside"`

    Pour positionner le strip à l'extérieru du graph remplacer `"inside"` par `"outside"`

## Exécution du script

Seule la première partie du script nécessite des modifications. 

Deux variables doivent impérativement être modifiées , il s'agit du répertoire de travail et du nom du fichier à analyser.

Les autres variables concernent l'aspect du graphique et ont des valeurs par défaut.

### 1. Données à analyser

- Choisir le répertoire de travail

```R
setwd("PATH/TO/FILES")
```

Le répertoire de travail peut être sélectionner graphiquement dans l'onglet `Files` du cadre en bas à droite de l'interface R-studio.

<img src=".images/Graphique points + Intervalle de confiance - script générique/image-20210511173330669.png" alt="image-20210511173330669" style="zoom:50%;" />

- Initailiser la variable `DATA`

```R
DATA <- "my_file.txt"
```

ou 

```R
DATA <- "my_file.xlsx"
```



### 2. Variables de personnalisation du graphique

Dans cette section vous trouverez les variables permettant de personnaliser le graphique. Il s'agit des variables `COULEUR`, `PALETTE`,  `orientation_xlabels` et `strip_pos` qui sont décrite en détails dans la section précédente (Les variables de personnalisation du graphique). 

Toutes ces variables ont des valeurs par défaut, il n'est pas obligatoire de les modifier.

```R
COULEUR <- "#d9c396"
PALETTE <- "Paired"
orientation_xlabels <- theme(axis.text.x = element_text(angle = 0, vjust = 0, hjust=0.5))
strip_pos <- theme(panel.spacing = unit(0.3, "lines"), 
                  strip.background = element_rect(colour="black", fill=NA),
                  strip.placement = "inside")
```



### 3. Installation et appel des paquets

La section 3 permet d'installer dans votre système les paquets nécessaire à l'éxécution du script. L'installation ne se fera que si elle est nécessaire. Les paquets sont ensuite appeler pour fonctionner dans le script. Le commentaire (après le #) à coté de l'appel de chaque paquet décrit le role des paquets.

```R
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
```



### 4. Appel des fonctions utilisées dans le script

Le script utilise des fonctions "maison". Pour qu'elles soient correctement utilisées il faut les appeler. Si les fonctions n'apparaissent pas dans votre environnement (cadre en haut à droite) vous ne pourrez pas exécuter le script.

```R
#=============================================================================================
# Fait le test de Kruskal-Wallis
# Retourne TRUE si au moins une médianes est significativement différente
#=============================================================================================
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

#=============================================================================================
# Fait le test de Dunn
# retourne les pvalue dans un dataframe
#=============================================================================================
test_dunn <- function() {
    pval <- as.data.frame(df %>% group_by(grouping_factor) %>% dunn_test(mesured_value ~ plant_line, p.adjust.method = "BH"))
    print(df %>% group_by(grouping_factor) %>% dunn_test(mesured_value ~ plant_line, p.adjust.method = "BH"))
    return(pval)
}

#=============================================================================================
# Réalise le graphique lorsque les données suivent une loi normale. L'intervalle de confiance 
# est construit autour de la moyenne
#=============================================================================================
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


#=============================================================================================
# Réalise le graphique lorsque les données ne suivent pas une loi normale. L'intervalle de 
# confiance est construit autour de la médiane
#=============================================================================================
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
        facet_wrap(~ grouping_factor, strip.position = "bottom", scales = "free_x") +
        theme_classic() + 
        strip_pos +
        orientation_xlabels +
        theme(legend.title=element_blank())
    print(p)
}


#=============================================================================================
# Vérifie la normalité des données. Sort de la boucle si au moins un des groupes de données ne 
# suit pas une loi normale.
#Retourne TRUE si données suivent une loi normale
#=============================================================================================
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
```



### 5. Corps du script

C'est la partie principale du script, là où l'analyse est faite.

- Vérifie l'extension du fichier de donnée et le charge selon ses caractéristiques

```R
if(grepl("\\.txt$", DATA)) { 
    df <- read.table(DATA, header = TRUE, stringsAsFactors = TRUE, sep = '\t')
} else { 
    df <- read_excel(DATA, col_types = c("text", "text", "numeric"))
}
```

- Vérifie que le nombre de colonne est bien égal à 3 et interrompt le script dans le cas contraire

```R
if (ncol(df)!=3) {
    print ("ERREUR : le nombre de colonnes n'est pas égal à 3")
    stop()
}
```

- Pour rendre le script générique, il faut que les nom de colonnes soient toujours identiques, pour cela on remplace les noms de colonnes de votre fichier par des noms génériques en conservant les noms originaux pour qu'ils apparaissent dans le rendu final.

```R
# Sauvegarde des noms des colonnes du fichier d'entrée
real_colname_df <- colnames(df)

# Remplacement des nom des colonnes par les noms génériques
colnames(df) <- c("plant_line", "grouping_factor", "mesured_value")

```

- Définition des couleurs

```R
my_colours = brewer.pal(n = 9, PALETTE)[9:3]
```

- Vérifie si la distribution des données est normale ou non

```R
shapiro_df <- df %>%group_by(grouping_factor, plant_line)%>%
    summarise(statistic = shapiro.test(mesured_value)$statistic, 
              p.value = shapiro.test(mesured_value)$p.value)

flag_normal <- check_normality(shapiro_df)
```

- Traitement des données en fonction de leur distribution

    - Données paramétriques

        ```R
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
        ```

    - Données non paramétriques

        ```R
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
        ```

    

### 6. Session R

La commande suivant pemet d'afficher toutes les informations concernant la session R qui a produit les données : outils, version ...

```R
InfoSession <- devtools::session_info()

# sauvegarde du fichier session
 write.table(InfoSession, file = "InfoSession.txt", 
                quote = FALSE, row.names = FALSE, sep = '\t')
```



## Enregistrement des données

Les fichiers sont enregistrés au format `.txt` avec la tabulation comme séparateur. Ainsi vous pouvez coller le resultat dans un fichier excel et le modifier à votre guise.

Pour les graphiques vous devez utiliser la fonction `Export` de R-studio. Cliquer sur `export `puis `Save as Image` pour sauver votre graphique. Une boite de dialogue s'ouvre.

Vous pouvez modifier les paramètre de longueur et largeur puis en utilisant `Update Preview` voir le résultat

<img src=".images/Graphique points + Intervalle de confiance - script générique/image-20210512171715660.png" alt="image-20210512171715660" style="zoom:50%;" />

Une alternative à l'utilisation de la fonction `Export`  est de dé-commenter (c'est à dire retirer `#`) la ligne `ggsave("Plot.svg", width=4, height=5)` qui se trouve à la fin de chaque bloc d'analyse , on aurait ainsi : 

- pour les données normales

    ```R
    ....
         # Sauver les fichiers
        write.table(my_summary, file = "Summary.txt", 
                    quote = FALSE, row.names = FALSE, sep = '\t')
        write.table(anova_results, file = "Anova.txt", 
                    quote = FALSE, row.names = FALSE, sep = '\t')
        write.table(tukey_results[, c(1,3,4, 9, 10)], file = "Tukey.txt", 
                    quote = FALSE, row.names = FALSE, sep = '\t')
        ggsave("Plot.svg", width=4, height=5)
    ```

- pour les données non paramétriques 

    ```R
    ....
        # Sauver les fichiers
        write.table(my_summary, file = "Summary.txt", 
                    quote = FALSE, row.names = FALSE, sep = '\t')
        write.table(kruskal_pval, file = "Kruskal.txt", 
                    quote = FALSE, row.names = FALSE, sep = '\t')
       write.table(pval_dunn[, c(1, 3, 4, 8, 9, 10)], file = "Dunn.txt", 
                    quote = FALSE, row.names = FALSE, sep = '\t')
        ggsave("Plot.svg", width=4, height=5)
    }
    ```

    

:warning: Dans ce cas le graphique ne s'affiche pas dans le cadran en bas à droit de l'interface R-studio

## Citations

R packages

> Clarke, Erik, and Scott Sherrill-Mix. 2017. *Ggbeeswarm: Categorical Scatter (Violin Point) Plots*. https://CRAN.R-project.org/package=ggbeeswarm.

> Hope, Ryan M. 2013. *Rmisc: Rmisc: Ryan Miscellaneous*. https://CRAN.R-project.org/package=Rmisc.

> Kassambara, Alboukadel. 2021. *Rstatix: Pipe-Friendly Framework for Basic Statistical Tests*. https://CRAN.R-project.org/package=rstatix.

> Mangiafico, Salvatore. 2021. *Rcompanion: Functions to Support Extension Education Program Evaluation*. https://CRAN.R-project.org/package=rcompanion.

> Neuwirth, Erich. 2014. *RColorBrewer: ColorBrewer Palettes*. https://CRAN.R-project.org/package=RColorBrewer.

> R Core Team. 2020. *R: A Language and Environment for Statistical Computing*. Vienna, Austria: R Foundation for Statistical Computing. https://www.R-project.org/.

> Wickham, Hadley. 2016. *Ggplot2: Elegant Graphics for Data Analysis*. Springer-Verlag New York. https://ggplot2.tidyverse.org.

> Wickham, Hadley, and Jennifer Bryan. 2019. *Readxl: Read Excel Files*. https://CRAN.R-project.org/package=readxl.

> Wickham, Hadley, Romain François, Lionel Henry, and Kirill Müller. 2021. *Dplyr: A Grammar of Data Manipulation*. https://CRAN.R-project.org/package=dplyr.

Script

> Cécile Lecampion. 2021. Script to perform statistical analysis according to data distribution and plot points and confidence interval. https://github.com/Cecile06/GO_analysis_and_graph/blob/main/README.md

