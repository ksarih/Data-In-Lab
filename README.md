# Projet DATA IN LAB  
Analyse statistique de l’essai clinique sur le bicarbonate de sodium

## Structure du projet

Le dossier contient les éléments suivants :

- `Data/`  
  Dossier contenant l’ensemble des fichiers CSV nécessaires à l’analyse.  
  Fichiers présents :
  - Data/RANDOMIZATION.csv
  - Data/VITAL_STATUS.csv
  - Data/SOFA.csv

- `Code.R`  
  Script principal au format R.
  
- `DATA_IN_LAB.Rmd`  
  Script principal au format R Markdown contenant tout le pipeline d’analyse, de la préparation des données jusqu’aux modèles statistiques avancés.

- `README.md`  
  Document explicatif décrivant le projet, les objectifs, les données manipulées, les analyses menées et la logique statistique suivie.

## Objectif du projet

Ce travail consiste à analyser les données d’un essai clinique évaluant l’impact d’un traitement par bicarbonate de sodium sur l’évolution clinique de patients de réanimation. L’enjeu principal consiste à mesurer l’effet du traitement sur un critère composite nommé Favorable.

Plusieurs axes d’analyse sont développés :

1. Construction des variables Sofa_ok et Favorable.
2. Mise en place d’un test Chi-deux global pour étudier l’effet du traitement.
3. Analyses stratifiées selon trois variables importantes :
   - présence de sepsis (INCL_SEPSIS_YN),
   - catégorie d’âge (AGE_CLASS),
   - score rénal AKIN (INCL_AKIN).
4. Application de tests statistiques adaptés aux effectifs (Chi2 ou Fisher).
5. Correction des p-values pour le risque d’erreurs multiples :
   - Bonferroni,
   - Holm,
   - Benjamini-Hochberg.
6. Analyse de sous-groupes obtenus en combinant deux variables de stratification, puis trois.
7. Construction de graphiques pour illustrer les proportions de patients favorables dans chaque sous-groupe.
8. Mise en place de modèles de régression logistique :
   - modèle simple,
   - modèle ajusté,
   - modèles avec interactions entre le traitement et les variables de stratification.

## Description des données

Les données sont issues de trois fichiers situés dans `Data/`.

### RANDOMIZATION.csv
Variables principales :
- SUBJID : identifiant patient
- INCL_SEPSIS_YN : sepsis à l’inclusion (0 = non, 1 = oui)
- AGE_CLASS : classe d’âge (0 = moins de 65 ans, 1 = 65 ans ou plus)
- INCL_AKIN : score d’insuffisance rénale (0 = AKIN 0–1, 1 = AKIN 2–3)
- ARM_NUM : bras de traitement (0 = contrôle, 1 = bicarbonate)

### VITAL_STATUS.csv
Variables :
- SUBJID
- VITAL_STATUS_D28 : statut vital à J28 (0 = décédé, 1 = vivant)

### SOFA.csv
Variables :
- SUBJID
- Scores SOFA par organe au jour 7 : respiratoire, cardiovasculaire, neurologique, rénal, hépatique et hématologique.

## Construction des variables clés

Deux variables sont définies :

1. Sofa_ok = 1 si tous les composants SOFA à J7 sont strictement inférieurs à 3.
2. Favorable = 1 si le patient est vivant à J28 et Sofa_ok = 1.

Ces variables servent de base à toutes les analyses effectuées.

## Résultats des analyses descriptives

Des statistiques simples permettent de décrire la cohorte :
- Nombre de favoris.
- Nombre de patients vivants à J28.
- Répartition des bras de traitement.
- Distribution selon les variables de stratification.

## Tests du Chi2 et de Fisher

Un test d’association ARM_NUM versus Favorable est appliqué globalement puis par sous-groupe.  
Selon les effectifs, un test de Chi2 ou de Fisher est automatiquement sélectionné.

L’interprétation se base sur :
- les p-values,
- les odds ratios,
- les intervalles de confiance.

## Correction des p-values

Dans les analyses stratifiées, six tests indépendants sont effectués.  
Pour contrôler le risque d’erreur globale, trois corrections sont appliquées :

- Bonferroni
- Holm
- Benjamini-Hochberg

Un tableau final récapitule :
- p-value brute,
- p-value corrigée Bonferroni,
- p-value corrigée Holm,
- p-value corrigée BH.

## Analyses combinées des variables de stratification

Deux types de combinaisons sont étudiées :

1. Age × Sepsis  
2. Age × AKIN  

Pour chaque combinaison, les tests du traitement sont réalisés et les p-values corrigées.

Une analyse descriptive et graphique permet de visualiser les proportions de Favorable selon le bras de traitement dans chaque sous-groupe.

## Analyse des combinaisons triples

Les trois facteurs de stratification sont combinés en huit groupes.  
Un graphique d’effectifs permet d’évaluer la possibilité d'appliquer un test statistique ; plusieurs groupes ayant des effectifs faibles, les tests globaux ne sont pas réalisés pour éviter des conclusions erronées.

## Régressions logistiques

Plusieurs modèles sont estimés.

### Modèle simple  
Favorable ~ ARM_NUM  
Permet d’estimer l’effet brut du traitement.

### Modèle ajusté  
Favorable ~ ARM_NUM + AGE_CLASS + INCL_AKIN + INCL_SEPSIS_YN  
Montre que l’effet du traitement reste significatif après ajustement.

### Modèles avec interactions  
Tests des interactions :
- ARM_NUM × AGE_CLASS
- ARM_NUM × INCL_AKIN
- ARM_NUM × INCL_SEPSIS_YN

Ces modèles explorent l’hétérogénéité de l’effet du traitement selon les sous-groupes.

## Conclusion générale

Les résultats convergent vers un effet favorable du traitement par bicarbonate, particulièrement chez :
- les patients de moins de 65 ans,
- les patients présentant une insuffisance rénale AKIN 2–3,
- les patients sans sepsis initial.

Les analyses montrent également une hétérogénéité de l’effet selon les profils cliniques.

Pour confirmer ces observations, une étude plus large serait nécessaire, notamment dans les sous-groupes où les effectifs sont très faibles.

---

