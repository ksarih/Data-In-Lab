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


####################


