library(dplyr)
library(ggplot2)


df_VITAL_STATUS <- read.csv("Data/VITAL_STATUS.csv")
df_RANDOMIZATION <- read.csv("Data/RANDOMIZATION.csv")
df_SOFA <- read.csv("Data/SOFA.csv")

### Id les individus mort avant J28
Indiv_mort <- df_VITAL_STATUS %>%
  filter(VITAL_STATUS_D28 == 0) %>%
  select (SUBJID, SURNAME, NAME)
# 279 mort avant J28




### Création d'un df final 
# filtrage des modalités
df_RANDOMIZATION <- df_RANDOMIZATION %>%
  select(SUBJID,INCL_SEPSIS_YN,AGE_CLASS,INCL_AKIN,ARM_NUM)

df_VITAL_STATUS <- df_VITAL_STATUS %>%
  select(SUBJID,VITAL_STATUS_D28)

df_SOFA <- df_SOFA %>%
  select(SUBJID,SOFA_hepatic_D7,SOFA_cardiovasc_D7, SOFA_respiratory_D7, SOFA_neurologic_D7, SOFA_renal_D7, SOFA_haematologic_D7)


### Fusion finale
data <- df_RANDOMIZATION %>%
  left_join(df_VITAL_STATUS, by = "SUBJID") %>%
  left_join(df_SOFA, by = "SUBJID")




#calcul de l'estimateur / patient : Sofa < 3: (1) /Non (0)
#et calcul de l'estimateur / patient : Favorable(1) /Non (0)
#initialisation des nouvelles variables :
data$Sofa_ok<-rep(NA, 400)
data$Favorable<-rep(NA, 400)

for (i in 1:400) {
  data[i,"Sofa_ok"]<-if (max(data[i,c("SOFA_hepatic_D7","SOFA_cardiovasc_D7", "SOFA_respiratory_D7", "SOFA_neurologic_D7", "SOFA_renal_D7", "SOFA_haematologic_D7")]) < 3) 1 else 0 #Sofa_ok
  data[i,'Favorable']<-if(data[i,"VITAL_STATUS_D28"]== 1 & data[i,"Sofa_ok"]==1) 1 else 0 #Favorable 1/0
}



data <- data %>%
  select (SUBJID, INCL_SEPSIS_YN, AGE_CLASS, INCL_AKIN, VITAL_STATUS_D28, Sofa_ok, Favorable, ARM_NUM)


###### Exploration 
sum(data$Favorable) #67 Favorable
sum(data$Sofa_ok) # 193 SOfa OK
sum(data$VITAL_STATUS_D28) #121 Survivants 28J

sum(data$ARM_NUM) #199 traité au SB

a <- sum(data$Favorable == 1 & data$ARM_NUM == 1) #44
c <- sum(data$Favorable == 1 & data$ARM_NUM == 0) #23
b <- sum(data$Favorable == 0 & data$ARM_NUM == 1) #155
d <- sum(data$Favorable == 0 & data$ARM_NUM == 0) #178

Tab_contingence <- matrix(
  c(a, b, c, d),
  nrow = 2,
  byrow = TRUE,
  dimnames = list(
    ARM_NUM   = c("1_traitement_SB", "0_sans_SB"),
    Favorable = c("1_oui", "0_non")
  )
)

Tab_contingence


#######################

sum(data$AGE_CLASS == 1 & data$Favorable == 1  & data$ARM_NUM == 1)
sum(data$AGE_CLASS == 0 & data$Favorable == 1  & data$ARM_NUM == 1)
sum(data$AGE_CLASS == 1 & data$VITAL_STATUS_D28 == 1)


############# Test du chi2 #############

res_chi2 <- chisq.test(Tab_contingence, correct = FALSE)
res_chi2

res_chi2$expected #effectif attendue 
res_chi2$p.value


# p = 0.00428 < 0.05 donc on rejette HO donc une issue favorable dépend du traitement reçu 

#################### ODDS #########

# Odds ratio brut
OR <- (a * d) / (b * c)
OR

# Log(OR) et erreur standard
log_OR <- log(OR)
SE_log_OR <- sqrt(1/a + 1/b + 1/c + 1/d)
log_OR
SE_log_OR
# Intervalle de confiance à 95 %
IC_inf <- exp(log_OR - 1.96 * SE_log_OR)
IC_sup <- exp(log_OR + 1.96 * SE_log_OR)

OR
IC_inf
IC_sup


#stratification
table_sepsis <- table(data$Favorable, data$INCL_SEPSIS_YN)
table_sepsis

table_age <- table(data$Favorable, data$AGE_CLASS)
table_age
table_akin <- table(data$Favorable, data$INCL_AKIN)
table_akin
library(ggplot2)
df_plot <- data.frame(
  Groupe = c("Sans sepsis", "Avec sepsis",
             "< 65 ans", "≥ 65 ans",
             "AKIN 0–1", "AKIN 2–3"),
  Pourcentage = c(
    31/(31+133)*100, 36/(36+200)*100,
    25/(25+170)*100, 42/(42+163)*100,
    28/(28+183)*100, 39/(39+150)*100
  ),
  Type = c("Sepsis", "Sepsis",
           "Âge", "Âge",
           "AKIN", "AKIN")
)
ggplot(df_plot %>% filter(Type == "Sepsis"),
       aes(x = Groupe, y = Pourcentage)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  ylim(0, 30) +
  labs(title = "Proportion de Favorables selon le Sepsis",
       x = "",
       y = "Proportion de Favorables (%)") +
  theme_minimal(base_size = 14)

ggplot(df_plot %>% filter(Type == "Âge"),
       aes(x = Groupe, y = Pourcentage)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  ylim(0, 30) +
  labs(title = "Proportion de Favorables selon l'âge",
       x = "",
       y = "Proportion de Favorables (%)") +
  theme_minimal(base_size = 14)

ggplot(df_plot %>% filter(Type == "AKIN"),
       aes(x = Groupe, y = Pourcentage)) +
  geom_bar(stat = "identity", fill = "firebrick") +
  ylim(0, 30) +
  labs(title = "Proportion de Favorables selon le stade AKIN",
       x = "",
       y = "Proportion de Favorables (%)") +
  theme_minimal(base_size = 14)


# Sepsis = 0
tab_sepsis0 <- table(
  data$Favorable[data$INCL_SEPSIS_YN == 0],
  data$ARM_NUM[data$INCL_SEPSIS_YN == 0]
)
tab_sepsis0

p_sepsis0 <- if(any(tab_sepsis0 < 5)) {
  fisher.test(tab_sepsis0)$p.value
} else {
  chisq.test(tab_sepsis0)$p.value
}

# Sepsis = 1
tab_sepsis1 <- table(
  data$Favorable[data$INCL_SEPSIS_YN == 1],
  data$ARM_NUM[data$INCL_SEPSIS_YN == 1]
)
tab_sepsis1

p_sepsis1 <- if(any(tab_sepsis1 < 5)) {
  fisher.test(tab_sepsis1)$p.value
} else {
  chisq.test(tab_sepsis1)$p.value
}
# Age < 65
tab_age0 <- table(
  data$Favorable[data$AGE_CLASS == 0],
  data$ARM_NUM[data$AGE_CLASS == 0]
)
tab_age0

p_age0 <- if(any(tab_age0 < 5)) {
  fisher.test(tab_age0)$p.value
} else {
  chisq.test(tab_age0)$p.value
}

# Age ≥ 65
tab_age1 <- table(
  data$Favorable[data$AGE_CLASS == 1],
  data$ARM_NUM[data$AGE_CLASS == 1]
)
tab_age1

p_age1 <- if(any(tab_age1 < 5)) {
  fisher.test(tab_age1)$p.value
} else {
  chisq.test(tab_age1)$p.value
}


# AKIN 0–1
tab_akin0 <- table(
  data$Favorable[data$INCL_AKIN == 0],
  data$ARM_NUM[data$INCL_AKIN == 0]
)
tab_akin0

p_akin0 <- if(any(tab_akin0 < 5)) {
  fisher.test(tab_akin0)$p.value
} else {
  chisq.test(tab_akin0)$p.value
}

# AKIN 2–3
tab_akin1 <- table(
  data$Favorable[data$INCL_AKIN == 1],
  data$ARM_NUM[data$INCL_AKIN == 1]
)
tab_akin1

p_akin1 <- if(any(tab_akin1 < 5)) {
  fisher.test(tab_akin1)$p.value
} else {
  chisq.test(tab_akin1)$p.value
}



p_traitement_strat <- c(
  p_sepsis0, p_sepsis1,
  p_age0, p_age1,
  p_akin0, p_akin1
)

p_traitement_strat

# Correction Bonferroni (6 tests)
p_bonf_trait <- p.adjust(p_traitement_strat, method = "bonferroni")

# Correction Benjamini–Hochberg
p_bh_trait <- p.adjust(p_traitement_strat, method = "BH")
#correction holm
p_holm <- p.adjust(p_traitement_strat, method = "holm")
resultats_traitement_strat <- data.frame(
  Stratification = c("Sepsis=0", "Sepsis=1", "Âge<65", "Âge≥65", "AKIN 0–1", "AKIN 2–3"),
  p_brut = p_traitement_strat,
  p_bonferroni = p_bonf_trait,
  p_Holm = p_holm,
  p_BH = p_bh_trait
)

resultats_traitement_strat
#D'après l'analyse du tab :
#Le traitement fonctionne significativement chez les patients avec insuffisance rénale aiguë sévère (AKIN 2–3).
#Le traitement est significativement efficace chez les patients de moins de 65 ans.










##########################################################################################################################













#######age x sepsis
# on analyse les4 sous-groupes cliniques combinés d'ages et sepsis



data$AGE_SEPSIS <- interaction(data$AGE_CLASS, data$INCL_SEPSIS_YN)
table(data$AGE_SEPSIS)
p_values_age_sepsis <- c()
#Pour chaque groupe, tu testes l’effet du traitement SB
for (g in unique(data$AGE_SEPSIS)) {
  cat("\nGROUPE :", g, "\n")
  
  tab <- table(
    data$Favorable[data$AGE_SEPSIS == g],
    data$ARM_NUM[data$AGE_SEPSIS == g]
  )
  
  print(tab)
  
  p_values_age_sepsis <- c(
    p_values_age_sepsis,
    if(any(tab < 5)) fisher.test(tab)$p.value else chisq.test(tab)$p.value
  )
}
#Groupe 4 : 0.0 = <65 ans et sans sepsis montre un effet très fort du traitement.
p_values_age_sepsis
# 0.7985   0.2544   0.2694   0.00235
p.adjust(p_values_age_sepsis, method = "BH")
# 0.7985   0.3592   0.3592   0.0094
#donc que le groupe 4 est significatif
# DONC LE GROUPES DES PATIENTS MOINS DE 65 ANS ET SANS SEPSIS SONT BIEN RECEPTIVES AUX SB
############### age x akin
data$AGE_AKIN <- interaction(data$AGE_CLASS, data$INCL_AKIN)
table(data$AGE_AKIN)
p_values_age_akin <- c()

for (g in unique(data$AGE_AKIN)) {
  cat("\nGROUPE :", g, "\n")
  
  tab <- table(
    data$Favorable[data$AGE_AKIN == g],
    data$ARM_NUM[data$AGE_AKIN == g]
  )
  
  print(tab)
  
  p_values_age_akin <- c(
    p_values_age_akin,
    if(any(tab < 5)) fisher.test(tab)$p.value else chisq.test(tab)$p.value
  )
}
#énorme effet du traitement chez les jeunes avec insuffisance rénale sévère.
p_values_age_akin
# 1.000000e+00 1.000000e+00 7.461246e-05 1.465156e-01
p.adjust(p_values_age_akin, method = "BH")
# 1.0000000000 1.0000000000 0.0002984498 0.2930312911
#Le seul groupe avec un signal fort = 0.1 = <65 ans & AKIN 2–3
#choix du test selon les dim de la table pour eviter les bugs








####GRAPHES
library(ggplot2)
library(dplyr)

# 1. P-values BH (on aurait pu faire bonf revient au meme )
# Résumés par groupe pour les graphes Age × Sepsis et Age × AKIN

df_age_sepsis <- data %>%
  group_by(AGE_SEPSIS, ARM_NUM) %>%
  summarise(
    Favorables = sum(Favorable),
    Total = n(),
    Proportion = Favorables / Total * 100,
    .groups = "drop"
  )

df_age_akin <- data %>%
  group_by(AGE_AKIN, ARM_NUM) %>%
  summarise(
    Favorables = sum(Favorable),
    Total = n(),
    Proportion = Favorables / Total * 100,
    .groups = "drop"
  )

p_df_age_sepsis <- data.frame(
  AGE_SEPSIS = c("0.0","0.1","1.0","1.1"),
  pBH = p.adjust(p_values_age_sepsis, method = "BH")
)

# 2. IC WITH binom.test()
df_age_sepsis_IC <- df_age_sepsis %>%
  rowwise() %>%
  mutate(
    p = Favorables / Total,
    IC_low  = binom.test(Favorables, Total)$conf.int[1] * 100,
    IC_high = binom.test(Favorables, Total)$conf.int[2] * 100
  )

# 3. Merge LES 3
df_plot_age_sepsis <- df_age_sepsis_IC %>%
  left_join(p_df_age_sepsis, by = "AGE_SEPSIS") %>%
  mutate(
    Groupe = factor(AGE_SEPSIS,
                    levels = c("0.0","0.1","1.0","1.1"),
                    labels = c("<65 ans / sans sepsis",
                               "<65 ans / avec sepsis",
                               "≥65 ans / sans sepsis",
                               "≥65 ans / avec sepsis"))
  )

# 4. Graphique propre et fonctionnel
ggplot(df_plot_age_sepsis, aes(x = Groupe, y = Proportion, fill = factor(ARM_NUM))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  
 
  geom_text(aes(label = paste0("p(BH)=", round(pBH,4))),
            position = position_dodge(width = 0.8),
            vjust = -0.5, size = 3.2, color = "black") +
  
  scale_fill_manual(values = c("yellow", "green")) +
  
  labs(title = "Proportion de Favorables selon Âge × Sepsis",
       x = "",
       y = "Proportion (%)",
       fill = "Traitement (ARM_NUM)") +
  
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1),
        plot.title = element_text(size = 18))
# On remarque que toujours la proportions de fav avec SB sont tjrs sup a ceux sans
#mais le groupe>65 ans avec sepsis repondent bien eu SB avec une p-value 0.009
#  P-values BH pour Âge × AKIN
p_df_age_akin <- data.frame(
  AGE_AKIN = c("0.0","1.0","0.1","1.1"),
  pBH = p.adjust(p_values_age_akin, method = "BH")
)

# Calcul des IC exacts binomiaux
df_age_akin_IC <- df_age_akin %>%
  rowwise() %>%
  mutate(
    p = Favorables / Total,
    IC_low  = binom.test(Favorables, Total)$conf.int[1] * 100,
    IC_high = binom.test(Favorables, Total)$conf.int[2] * 100
  )

#  Fusion des p-values + ajout des labels propres
df_plot_age_akin <- df_age_akin_IC %>%
  left_join(p_df_age_akin, by = "AGE_AKIN") %>%
  mutate(
    Groupe = factor(AGE_AKIN,
                    levels = c("0.0","0.1","1.0","1.1"),
                    labels = c("<65 ans / AKIN 0–1",
                               "<65 ans / AKIN 2–3",
                               "≥65 ans / AKIN 0–1",
                               "≥65 ans / AKIN 2–3"))
  )

# 4. Graphique final publication-ready
ggplot(df_plot_age_akin, aes(x = Groupe, y = Proportion, fill = factor(ARM_NUM))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  
  
  
  geom_text(aes(label = paste0("p(BH)=", round(pBH,4))),
            position = position_dodge(width = 0.8),
            vjust = -0.5, size = 3.2, color = "black") +
  
  scale_fill_manual(values = c("red", "steelblue")) +
  
  labs(title = "Proportion de Favorables selon Âge × AKIN",
       x = "",
       y = "Proportion (%)",
       fill = "Traitement (ARM_NUM)") +
  
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1),
        plot.title = element_text(size = 18))
#le traitement de bicarbonate marche plus sur les moins de 65 ans et AKIN 2-3 (17 patients dans ce cas), 16 d entre eux ont recu le traitement  FAVORABLEE
#en plus la p-value est tres petite donc significative
# Sous-groupe  : Favorables + âge < 65 + AKIN 2–3
subset_sepsis_visual <- data %>%
  filter(Favorable == 1,
         AGE_CLASS == 0,     # < 65 ans
         INCL_AKIN == 1)     # AKIN 2–3
table(subset_sepsis_visual$INCL_SEPSIS_YN) #17 patients dans ce groupe 
#9 sans sepsis et 8 avec sepsis
library(ggplot2)

ggplot(subset_sepsis_visual, 
       aes(x = factor(INCL_SEPSIS_YN,
                      labels = c("Sans sepsis", "Avec sepsis")))) +
  geom_bar(fill = "steelblue") +
  labs(
    title = "Présence du sepsis chez les Favorables <65 ans et AKIN 2–3",
    x = "Sepsis à l'inclusion",
    y = "Nombre de patients"
  ) +
  theme_minimal(base_size = 14)

#donc on ne peut rien dire car on voulait expliquer pourquoi ils sont favorables alors que le AKIN est 2-3 et <65
table(subset_sepsis_visual$ARM_NUM)
#DONC 16/17 ONT RECU LE TRAITEMENT DONC CA EXPLIQUE POURQUOI ILS SONT FAVORABLES 
patient_sans_traitement <- subset_sepsis_visual %>%
  filter(ARM_NUM == 0)

patient_sans_traitement

#on essaie de voir l'effectif quand on les combine a 3 ces variables de stratificatifs pour voir si on peut faire un test a 3 ou non 

data$STRAT_COMBO <- interaction(
  data$INCL_SEPSIS_YN,
  data$AGE_CLASS,
  data$INCL_AKIN,
  sep = "_"
)
table(data$STRAT_COMBO)
data$STRAT_COMBO_LABEL <- factor(
  data$STRAT_COMBO,
  labels = c(
    "Sepsis=0, Age<65, AKIN0-1",
    "Sepsis=0, Age<65, AKIN2-3",
    "Sepsis=0, Age≥65, AKIN0-1",
    "Sepsis=0, Age≥65, AKIN2-3",
    "Sepsis=1, Age<65, AKIN0-1",
    "Sepsis=1, Age<65, AKIN2-3",
    "Sepsis=1, Age≥65, AKIN0-1",
    "Sepsis=1, Age≥65, AKIN2-3"
  )
)
library(ggplot2)

ggplot(data, aes(x = STRAT_COMBO_LABEL)) +
  geom_bar(fill = "steelblue") +
  labs(
    title = "Effectifs pour chaque combinaison Sepsis × Âge × AKIN",
    x = "Strate combinée",
    y = "Nombre de patients"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))
table(data$STRAT_COMBO, data$Favorable)
table_age_traitement <- table(data$AGE_CLASS, data$ARM_NUM)
table_age_traitement
#l'objectif de tester et de pouvoir comparer les combinaisons donc meme l'option de choisir qlqs combinaisons ou l'effectifs est raisonables et tester est non pris en compte 

