library("dplyr")
library("tidyr")
library("ggplot2")
library("ggpattern")
library("corrplot")

setwd("C:/Users/lwang03/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/Kansas_Roar")

Data <- read.csv("Soil1_20240520_LW Modified.csv", header = T)
Field_col <- c(E1 = "#543fbf", E2 = "#117733", E3 = "#44AA99", SS1 = "#BB9D05" , S2 = "#CC6677", S3 = "#88CCEE", S4 = "#AA4499" , W1 ="#882255" )
Season_col <- c(Spring = "#56B4E9", Fall = "#E69900")
#Selecting the variables that are connected to the Haney Test and Nitrogen, changing the names
Haney_Test <- Data %>% select(Season,
                              Field,
                              OM.,
                              Ace.Protein.g.Kg,
                              NO3_N_H2O,
                              OC_H2O_ppm,
                              ON_H2O_ppm,
                              CN_H2O,
                              NH4_N_H2O,
                              CO2,
                              N_H2O,
                              X.MAC,
                              WSA_Mod,
                              X..Clay,
                              X..Silt,
                              X..Sand,
                              CEC,
                              ON_release_ppm,
                              SoilHealth,
                              X2N.KCl.NO3.N.ppm.N,
                              KCl.NH4.N.ppm,
                              Biomass,
                              Total_Fungi_Biomass,
                              UndifferentiatedBiomass,
                              Protozoa_biomasss) %>% 
  rename(`Organic Matter (%)` = "OM.",
         `Water Extractable Nitrate Nitrogen (mg/kg)` = "NO3_N_H2O",
         `Water Extractable Organic Carbon (mg/kg)` = "OC_H2O_ppm",
         `Water Extractable Organic Nitrogen (mg/kg)` = "ON_H2O_ppm",
         `WEOC:WEON` = "CN_H2O",
         `Water Extractable Ammonium Nitrogen (mg/kg)` = "NH4_N_H2O",
         `24 hour CO2 Respiration (mg/kg)` = "CO2",
         `Microbially Active Carbon (%)` = "X.MAC",
         `Autoclavable Citrate Protein (g/kg)` = "Ace.Protein.g.Kg",
         `Organic Nitrogen Release (mg/kg)` = "ON_release_ppm",
         `Water Extractable Nitrogen (mg/kg)` = "N_H2O",
         `Clay (%)` = "X..Clay",
         `Silt (%)` ="X..Silt",
         `Sand (%)` ="X..Sand",
         `Aggregate Stability 1-2mm (%)` = "WSA_Mod",
         `1M KCl NO3-N (mg/kg)` = X2N.KCl.NO3.N.ppm.N,
         `1M KCl NH4-N (mg/kg` = KCl.NH4.N.ppm,
         `Biomass Microbial (ug/g)` = "Biomass",
         `Biomass Fungal (ug/g)` = "Total_Fungi_Biomass",
         `Biomass Undifferentiated (ug/g)` = "UndifferentiatedBiomass",
         `Protazoa Biomass (ug/g)` = "Protozoa_biomasss") %>%
  mutate( `Carbon (%)` = `Organic Matter (%)`/1.72) %>%
  filter(`WEOC:WEON` < 80) %>%
  mutate(`Water Extractable Ammonium Nitrogen (mg/kg)` = replace(`Water Extractable Ammonium Nitrogen (mg/kg)`, 
                                                                 `Water Extractable Ammonium Nitrogen (mg/kg)` == "<0.1", 0.1))
#Making everything numeric
Haney_Test2 <- Haney_Test %>% mutate_at(3:length(Haney_Test), as.numeric)

#Finding the correlations between spring and fall
Spring <- Haney_Test2 %>% subset(Season %in% "Spring")
Fall <- Haney_Test2 %>% subset(Season %in% "Fall") %>%
  select(-c(`Autoclavable Citrate Protein (g/kg)`,
            `1M KCl NO3-N (mg/kg)`, `1M KCl NH4-N (mg/kg`))


Spring2 <- Spring %>% select(-c(Season, Field))
Fall2 <- Fall %>% select(-c(Season, Field))
SpringCor <- cor(Spring2)
FallCor <- cor(Fall2)

pdf(file = "Kansas ROAR Spring Correlation Plot Spring_FallCompare.pdf", width = 26, height = 15)

par(mfrow=c(1,2))

corrplot(FallCor, method = 'number', tl.col = "black", type = "lower", order = 'AOE', title = "Kansas ROAR Fall Correlation", mar = c(0,0,1,0))
corrplot(SpringCor, method = 'number', tl.col = "black", type = "lower", order = 'AOE',  title = "Kansas ROAR Spring Correlation", mar = c(0,0,1,0))
dev.off()


#Grouping by Crop management type
Cropdiversity_Spring <- Spring %>% subset(Field %in% c("S3", "S4", "W1")) %>%
  select(-c(Season, Field))

Cropdiversity_Fall <- Fall %>% subset(Field %in% c("S3", "S4", "W1")) %>%
  select(-c(Season, Field))

Covercrop_Spring <- Spring %>% subset(Field %in% c("S2")) %>%
  select(-c(Season, Field))

Covercrop_Fall <- Fall %>% subset(Field %in% c("S2")) %>%
  select(-c(Season, Field))

Notill_Spring <- Spring %>% subset(Field %in% c("E1", "E2", "E3")) %>%
  select(-c(Season, Field))
Notill_Fall <- Fall %>% subset(Field %in% c("E1", "E2", "E3")) %>%
  select(-c(Season, Field))

Notill_Spring <- Spring %>% subset(Field %in% c("E1", "E2", "E3")) %>%
  select(-c(Season, Field))
Notill_Fall <- Fall %>% subset(Field %in% c("E1", "E2", "E3")) %>%
  select(-c(Season, Field))

Conventional_Spring <- Spring %>% subset(Field %in% c("SS1")) %>%
  select(-c(Season, Field))

Conventional_Fall <- Fall %>% subset(Field %in% c("SS1")) %>%
  select(-c(Season, Field))


CS_cor <- cor(Conventional_Spring)
NTS_cor <- cor(Notill_Spring)
CCS_cor <- cor(Covercrop_Spring)
CDS_cor <- cor(Cropdiversity_Spring)

pdf(file = "Kansas ROAR Spring Correlation Plot 2023-2024.pdf", width = 30, height = 25)

par(mfrow=c(2,2))

corrplot(CS_cor, method = 'number', tl.col = "black", type = "lower", order = 'alphabet', title = "Conventional Kansas ROAR Spring Correlation", mar = c(0,0,1,0))
corrplot(NTS_cor, method = 'number', tl.col = "black", type = "lower", order = 'alphabet', title = "No-Till Kansas ROAR Spring Correlation", mar = c(0,0,1,0))
corrplot(CCS_cor, method = 'number', tl.col = "black", type = "lower", order = 'alphabet', title = "No-Till & Cover Crop Kansas ROAR Spring Correlation", mar = c(0,0,1,0))
corrplot(CDS_cor, method = 'number', tl.col = "black", type = "lower", order = 'alphabet', title = "No-Till, Cover Crop & Crop Diversity Kansas ROAR Spring Correlation", mar = c(0,0,1,0))

dev.off()  

##Fall$$$
CF_cor <- cor(Conventional_Fall)
NTF_cor <- cor(Notill_Fall)
CCF_cor <- cor(Covercrop_Fall)
CDF_cor <- cor(Cropdiversity_Fall)

pdf(file = "Kansas ROAR Fall Correlation Plot 2023-2024.pdf", width = 30, height = 25)

par(mfrow=c(2,2))

corrplot(CF_cor, method = 'number', tl.col = "black", type = "lower", order = 'alphabet',  title = "Conventional Kansas ROAR Fall Correlation", mar = c(0,0,1,0))
corrplot(NTF_cor, method = 'number', tl.col = "black", type = "lower", order = 'alphabet', title = "No-Till Kansas ROAR Fall Correlation", mar = c(0,0,1,0))
corrplot(CCF_cor, method = 'number', tl.col = "black", type = "lower", order = 'alphabet', title = "No-Till & Cover Crop Kansas ROAR Fall Correlation", mar = c(0,0,1,0))
corrplot(CDF_cor, method = 'number', tl.col = "black", type = "lower", order = 'alphabet', title = "No-Till, Cover Crop & Crop Diversity Kansas ROAR Fall Correlation", mar = c(0,0,1,0))

dev.off()  


#Grouping by Field
S3_Spring <- Spring %>% subset(Field %in% c("S3")) %>%
  select(-c(Season, Field))

Cropdiversity_Fall <- Fall %>% subset(Field %in% c("S3", "S4", "W1")) %>%
  select(-c(Season, Field))

Covercrop_Spring <- Spring %>% subset(Field %in% c("S2")) %>%
  select(-c(Season, Field))

Covercrop_Fall <- Fall %>% subset(Field %in% c("S2")) %>%
  select(-c(Season, Field))

Notill_Spring <- Spring %>% subset(Field %in% c("E1", "E2", "E3")) %>%
  select(-c(Season, Field))
Notill_Fall <- Fall %>% subset(Field %in% c("E1", "E2", "E3")) %>%
  select(-c(Season, Field))

Notill_Spring <- Spring %>% subset(Field %in% c("E1", "E2", "E3")) %>%
  select(-c(Season, Field))
Notill_Fall <- Fall %>% subset(Field %in% c("E1", "E2", "E3")) %>%
  select(-c(Season, Field))

Conventional_Spring <- Spring %>% subset(Field %in% c("SS1")) %>%
  select(-c(Season, Field))

Conventional_Fall <- Fall %>% subset(Field %in% c("SS1")) %>%
  select(-c(Season, Field))


CS_cor <- cor(Conventional_Spring)
NTS_cor <- cor(Notill_Spring)
CCS_cor <- cor(Covercrop_Spring)
CDS_cor <- cor(Cropdiversity_Spring)

pdf(file = "Kansas ROAR Spring Correlation Plot 2023-2024.pdf", width = 30, height = 25)

par(mfrow=c(2,2))

CS_cor <- cor(Conventional_Spring)
corrplot(CS_cor, method = 'number', tl.col = "black", type = "lower", order = 'alphabet', title = "Conventional Kansas ROAR Spring Correlation", mar = c(0,0,1,0))

NTS_cor <- cor(Notill_Spring)
corrplot(NTS_cor, method = 'number', tl.col = "black", type = "lower", order = 'alphabet', title = "No-Till Kansas ROAR Spring Correlation", mar = c(0,0,1,0))

CCS_cor <- cor(Covercrop_Spring)
corrplot(CCS_cor, method = 'number', tl.col = "black", type = "lower", order = 'alphabet', title = "No-Till & Cover Crop Kansas ROAR Spring Correlation", mar = c(0,0,1,0))

CDS_cor <- cor(Cropdiversity_Spring)
corrplot(CDS_cor, method = 'number', tl.col = "black", type = "lower", order = 'alphabet', title = "No-Till, Cover Crop & Crop Diversity Kansas ROAR Spring Correlation", mar = c(0,0,1,0))

dev.off()  






