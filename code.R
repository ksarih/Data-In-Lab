library(dplyr)


df_VITAL_STATUS <- read.csv("VITAL_STATUS.csv")
df_RANDOMIZATION <- read.csv("RANDOMIZATION.csv")
df_SOFA <- read.csv("SOFA.csv")

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



sum(data$Favorable) #67 Favorable
sum(data$Sofa_ok) # 193 SOfa OK
sum(data$VITAL_STATUS_D28) #121 Survivants 28J






