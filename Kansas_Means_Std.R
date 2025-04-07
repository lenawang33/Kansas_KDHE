library("dplyr")
library("tidyr")
library("ggplot2")
library("ggpattern")
library("ggpubr")

setwd("C:/Users/lwang03/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/Kansas_Roar")

Data <- read.csv("Soil1_20240520_LW Modified.csv", header = T)
Field_col <- c(E1 = "#543fbf", E2 = "#117733", E3 = "#44AA99", SS1 = "#BB9D05" , S2 = "#CC6677", S3 = "#88CCEE", S4 = "#AA4499" , W1 ="#882255" )
Season_col <- c(Spring = "#56B4E9", Fall = "#E69900")


#Selecting the variables that are connected to the Haney Test and Nitrogen, changing the names
Haney_Test <- Data %>% select(Crop.2023,
                              Tillage,
                              Cover.Crop,
                              Field,
                              Sample_ID,
                              Season,
                              OM.,
                              Ace.Protein.g.Kg,
                              NO3_N_H2O,
                              OC_H2O_mg.kg,
                              ON_H2O_mg.kg,
                              CN_H2O,
                              NH4_N_H2O,
                              CO2_24hr,
                              N_H2O,
                              X.MAC,
                              Aggstab1_2mm,
                              X..Clay,
                              X..Silt,
                              X..Sand,
                              CEC,
                              ON_release_ppm,
                              AA_Ca,
                              AA_Mg,
                              AA_K,
                              AA_Na,
                              X.Hsat,
                              X.Ksat,
                              X.Casat,
                              X.Mgsat,
                              X.Nasat,
                              H3A_NO3,
                              H3A_NH4,
                              H3A_P,
                              H3A_IP,
                              H3A_OP,
                              H3A_ICAPS,
                              H3A_ICAPK,
                              H3A_ICAPCa,
                              H3A_ICAPAl,
                              H3A_ICAPFe,
                              H3A_ICAPZn,
                              H3ACu,
                              H3A_ICAPMg,
                              H3A_ICAPNa,
                              SoilHealth,
                              Biomass,
                              Total_Fungi_Biomass,
                              UndifferentiatedBiomass,
                              TotalBacteriabiomass,
                              Fungi_Bacteria,
                              Protozoa_biomasss,
                              Salts,
                              Gram_neg_biomass,
                              gram_pos_biomass,
                              X.C,
                              X.13C,
                              X.N,
                              X.15N,
                              C.N
                              ) %>% 
  rename(`Organic Matter (%)` = "OM.",
         `Water Extractable Nitrate Nitrogen (mg/kg)` = "NO3_N_H2O",
         `Water Extractable Organic Carbon (mg/kg)` = "OC_H2O_mg.kg",
         `Water Extractable Organic Nitrogen (mg/kg)` = "ON_H2O_mg.kg",
         `WEOC:WEON` = "CN_H2O",
         `Calcium (mg/kg)` = "AA_Ca",
         `Potassium (mg/kg)` = "AA_K",
         `Magnesium (mg/kg)` = "AA_Mg",
         `Sodium (mg/kg)` = "AA_Na",
         `Water Extractable Ammonium Nitrogen (mg/kg)` = "NH4_N_H2O",
         `Microbially Active Carbon (%)` = "X.MAC",
         `Autoclavable Citrate Protein (g/kg)` = "Ace.Protein.g.Kg",
         `Organic Nitrogen Release (mg/kg)` = "ON_release_ppm",
         `H3A Extracted Nitrate (mg/kg)` = "H3A_NO3",
         `H3A Extracted Ammonium (mg/kg)` = "H3A_NH4",
         `H3A Extracted Phosphorous (mg/kg)` = "H3A_P",
         `H3A Extracted Inorganic Phosphorous (mg/kg)` = "H3A_IP",
         `H3A Extracted Organic Phosphorous (mg/kg)` = "H3A_OP",
         `H3A Extracted Sulfur (mg/kg)` = "H3A_ICAPS",
         `H3A Extracted Potassium (mg/kg)` = "H3A_ICAPK",
         `Water Extractable Nitrogen (mg/kg)` = "N_H2O",
         `H3A Extracted Calcium (mg/kg)` = "H3A_ICAPCa",
         `H3A Extracted Aluminum (mg/kg)` = "H3A_ICAPAl",
         `H3A Extracted Iron (mg/kg)` = "H3A_ICAPFe",
         `H3A Extracted Zinc (mg/kg)` = "H3A_ICAPZn",
         `H3A Extracted Copper (mg/kg)` = "H3ACu",
         `H3A Extracted Magnesium (mg/kg)` = "H3A_ICAPMg",
         `H3A Extracted Sodium (mg/kg)` = "H3A_ICAPNa",
         `Clay (%)` = "X..Clay",
         `Silt (%)` ="X..Silt",
         `Sand (%)` ="X..Sand",
         `Electrical Conductivity (mmhos/cm)` = "Salts",
         `Aggregate Stability 1-2 mm (%)` = "Aggstab1_2mm",
         `Microbial Biomass (ug/g)` = "Biomass",
         `Fungal Biomass (ug/g)` = "Total_Fungi_Biomass",
         `Undifferentiated Biomass (ug/g)` = "UndifferentiatedBiomass",
         `Bacteria Biomass (ug/g)` = "TotalBacteriabiomass",
         `Fungi:Bacteria (ug/g)` = "Fungi_Bacteria",
         `Gram(-) Biomass (ug/g)` = "Gram_neg_biomass",
         `Gram(+) Biomass (ug/g)` = "gram_pos_biomass",
         `% BS H` = X.Hsat,
         `% BS K` = X.Ksat,
         `% BS Mg` = X.Mgsat,
         `% BS Ca` = X.Casat,
         `% BS Na` = X.Nasat,
         `Protozoa Biomass (ug/g)` = "Protozoa_biomasss",
         `% Carbon` = "X.C",
         `δ13 Carbon` = "X.13C",
         `% Nitrogen` =  "X.N",
         `δ15 Nitrogen` = "X.15N",
         `Carbon:Nitrogen Ratio` = "C.N"
         ) %>%
  mutate( `Carbon (%)` = `Organic Matter (%)`/1.72,
          `24 hour CO2 Respiration (mg/kg)` = CO2_24hr/2.83) %>%
  filter(`WEOC:WEON` < 80)

Haney_Test[,7:length(Haney_Test)] <- as.numeric(unlist(Haney_Test[,7:length(Haney_Test)]))

Long_H <-  pivot_longer(Haney_Test, cols = c(7:length(Haney_Test)), names_to = "Variable", values_to = "Value")
  
#Solving for the means and standard deviation
Haney_Means <- Long_H %>%
  group_by(Field, Variable, Season) %>%
  mutate(Mean = mean(Value, na.rm = T),
         Sd = sd(Value, na.rm = T)) %>%
  distinct(Mean, Sd) %>% ungroup() %>%
  mutate(Max = Mean - Sd,
         Min = Mean + Sd) %>%
  mutate(Min = replace(Min, which(Min < 0), 0))

#Doing a separate dataframe for pH, because it needs to be unlogged to find a medium
pH <- Data %>% select(Crop.2023,
                      Tillage,
                      Cover.Crop,
                      Field,
                      Sample_ID, 
                      Season,
                      pH) 

#Long format makes it the same format as Haney_Means
pH_long <-  pivot_longer(pH, cols = c(7), names_to = "Variable", values_to = "Value")
#Means, Sd, Min and Max for pH is all calculated here
pH <- pH_long %>%
  mutate(Sd = sd(Value)) %>%
  mutate(pH = 10**(-Value)) %>%
  group_by(Field, Variable, Season) %>%
  mutate(Mean = mean(Value)) %>%
  mutate(pH = -log10(Value)) %>%
  distinct(Mean, Sd) %>% ungroup %>%
  mutate(Min = Mean - Sd,
         Max = Mean + Sd) %>%
  mutate(Min = replace(Min, which(Min < 0), 0))

#Bringing the two dataframes together
Haney_Means <- rbind(Haney_Means, pH)
         
#Limiting significant figures to 2 
Haney_Means$Mean <- signif(Haney_Means$Mean, digits = 2)

#write.csv(Haney_Means, "Kansas_Means_Sd.csv")

Haney_Means <- Haney_Means %>%
  mutate(Field = factor(Field, levels = c("SS1", "E2", "E3", "E1", "S2", "S3", "S4", "W1")))
#####
Sites <- data.frame(matrix(ncol = 0, nrow = 8))
Sites$Field <- c("SS1", "E2", "E3", "E1", "S2", "S3", "S4", "W1")
Sites$Cover_Crop <- c("No Cover Crop", "No Cover Crop", "No Cover Crop",
                      "Cover Crop", "Cover Crop", "Cover Crop", "Cover Crop", "Cover Crop")

Sites$Tillage <- c("Conventional Till", "No Till", "No Till",
                   "No Till", "No Till", "No Till", "No Till", "No Till")

Sites$Plant_Diversity <- c(NA, NA, NA, 
                           NA, NA, "High Plant Diversity", "High Plant Diversity", "High Plant Diversity")





#A barplot of all the variables read in, including error bars and the mean values
Haney_Plot <- ggplot(Haney_Means, aes(x = Field, y = Mean, fill = Season)) +
  geom_bar(position = "dodge", stat = "identity") + 
  geom_errorbar(aes(x = Field, ymax = Max, ymin = Min), position = position_dodge(width = 0.9), 
                width = 0.2) +
  geom_text(aes(label = Mean), size = 4, hjust = 1, vjust = 1, position = position_dodge(width = 0.9)) +
  facet_wrap(vars(Variable), scales = "free") + 
  scale_color_manual(values = Season_col) +
  scale_fill_manual(values = Season_col) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        text = element_text(size = 20))


setwd("C:/Users/lwang03/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/Kansas_Roar/Plots")
ggsave("Haney_Initial_Season_Barplot_isotopes.pdf", Haney_Plot, height = 35, width = 50, limitsize = F)




#Getting the Isotope Data
Isotope <- Long_H %>%
  subset(Variable %in% c("δ13 Carbon", "δ15 Nitrogen")) %>%
  group_by(Field, Variable, Season) %>%
  mutate(Mean = mean(Value, na.rm = T),
         Sd = sd(Value, na.rm = T)) %>%
  distinct(Mean, Sd, .keep_all = T) %>%
  mutate(`Field Season` = paste(Field, Season)) %>%
  mutate(`Field Season` = factor(`Field Season`, 
                                 levels = c("SS1 Fall", "SS1 Spring",
                                            "E2 Fall", "E2 Spring",
                                            "E3 Fall", "E3 Spring",
                                            "E1 Fall", "E1 Spring",
                                            "S2 Fall", "S2 Spring",
                                            "S3 Fall", "S3 Spring",
                                            "S4 Fall", "S4 Spring",
                                            "W1 Fall", "W1 Spring")))

Carbon_iso <- Isotope %>%
  subset(Variable %in% c("δ13 Carbon"))

Nitrogen_iso <- Isotope %>%
  subset(Variable %in% c("δ15 Nitrogen"))

Carboniso_Plot <- ggplot(Carbon_iso, aes(x = `Field Season`, y = Mean, color = Season)) +
  geom_point(size = 4) +
  geom_errorbar(aes(x = `Field Season`, ymax = Mean + Sd, ymin = Mean - Sd), linewidth = 1.2, width = 0.7) +
  scale_color_manual(values = Season_col) +
  labs(y = quote(paste(delta^13, Carbon))) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        text = element_text(size = 20))

Nitrogeniso_Plot <- ggplot(Nitrogen_iso, aes(x = `Field Season`, y = Mean, color = Season)) +
  geom_point(size = 5) +
  geom_errorbar(aes(x = `Field Season`, ymax = Mean + Sd, ymin = Mean - Sd), linewidth = 1.8, width = 0.7) +
  scale_color_manual(values = Season_col) +
  labs(y = quote(paste(delta^15, Nitrogen))) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        text = element_text(size = 25))

Isotope_Plots <- ggarrange(Carboniso_Plot, Nitrogeniso_Plot, common.legend = T)
setwd("C:/Users/lwang03/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/Kansas_Roar/Plots")
ggsave("Haney_Initial_Season_Barplot_isotopes2.pdf", Isotope_Plots, height = 5, width = 15, limitsize = F)

##Getting the Texture Plot
Texture <- Haney_Means %>%
  subset(Variable %in% c("Clay (%)", "Silt (%)", "Sand (%)" )) %>%
  drop_na(Mean)


Clay <- Texture %>%
  subset(Variable %in% c("Clay (%)")) %>%
  group_by(Field) %>%
  summarize(Sd = sum(Sd),
            Min = sum(Min),
            Max = sum(Max)) %>%
  mutate(Variable = "Clay (%)",
         Season = "Spring") %>%
  ungroup()


Silt <- Texture %>%
  subset(Variable %in% c("Clay (%)", "Silt (%)")) %>%
  group_by(Field) %>%
  summarize(Sd = sum(Sd),
            Min = sum(Min),
            Max = sum(Max)) %>%
  mutate(Variable = "Silt (%)",
         Season = "Spring") %>%
  ungroup()

Sand <- Texture %>%
  group_by(Field) %>%
  summarize(Sd = sum(Sd),
            Min = sum(Min),
            Max = sum(Max)) %>%
  mutate(Variable = "Sand (%)",
         Season = "Spring") %>%
  ungroup()

Texture_means <- Texture %>%
  select(Field, Variable, Mean)

Texture_f <- rbind(Clay, Silt, Sand)

Texture_p <- left_join(Texture_f, Texture_means) %>%
  mutate(Variable = factor(Variable, levels = c("Sand (%)", "Silt (%)", "Clay (%)")))

Texture_Col <- c("Sand (%)" = "#CC6677", "Silt (%)" = "#44AA99", "Clay (%)" = "#88CCEE")

Texture_Plot <- ggplot(Texture_p, aes(x = Field, y = Mean, fill = Variable)) +
  geom_bar(position = "stack", stat = "identity") + 
  geom_errorbar(aes(x = Field, ymax = Max, ymin = Min), position = position_dodge(width = 0.9), 
                width = 0.2) + 
  scale_fill_manual(values = Texture_Col) +
  scale_color_manual(values = Texture_Col) + 
  theme_bw() +
  ylab("Texture (%)") + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        text = element_text(size = 25))



#Nitrogen
Nitrogen <- Haney_Means %>%
  subset(Variable %in% c("Water Extractable Organic Nitrogen (mg/kg)",
                         "Water Extractable Nitrate Nitrogen (mg/kg)",
                         "Water Extractable Ammonium Nitrogen (mg/kg)")) %>%
  mutate(Variable = recode(Variable,
                           "Water Extractable Organic Nitrogen (mg/kg)" = "Organic Nitrogen (mg/kg)",
                           "Water Extractable Nitrate Nitrogen (mg/kg)" = "Nitrate (mg/kg)",
                           "Water Extractable Ammonium Nitrogen (mg/kg)" = "Ammonium (mg/kg)"))


Nitrate <- Nitrogen %>%
  subset(Variable %in% c("Nitrate (mg/kg)")) %>%
  group_by(Field, Season) %>%
  summarize(Sd = sum(Sd),
            Min = sum(Min),
            Max = sum(Max)) %>%
  mutate(Variable = "Nitrate (mg/kg)")


Ammonium <- Nitrogen %>%
  subset(Variable %in% c("Nitrate (mg/kg)",
                         "Ammonium (mg/kg)")) %>%
  group_by(Field, Season) %>%
  summarize(Sd = sum(Sd),
            Min = sum(Min),
            Max = sum(Max)) %>%
  mutate(Variable = "Ammonium (mg/kg)")

Organic <- Nitrogen %>%
  group_by(Field, Season) %>%
  summarize(Sd = sum(Sd),
            Min = sum(Min),
            Max = sum(Max)) %>%
  mutate(Variable = "Organic Nitrogen (mg/kg)")

Nitrogen_Means <- Nitrogen %>%
  select(Field, Variable, Season, Mean)

Nitrogen_sd <- rbind(Nitrate, Ammonium, Organic)

Nitrogen2 <- left_join(Nitrogen_sd, Nitrogen_Means) %>%
  mutate(Variable = factor(Variable, levels = c("Organic Nitrogen (mg/kg)",
                                                "Ammonium (mg/kg)",
                                                "Nitrate (mg/kg)"))) %>%
  rename(`Water Extractable Nitrogen (mg/kg)` = "Mean")


Nitrogen_Col <- c("Nitrate (mg/kg)" = "#CC6677",
                 "Ammonium (mg/kg)" = "#44AA99",
                 "Organic Nitrogen (mg/kg)" = "#88CCEE")

Nitrogen_Plot <- ggplot(Nitrogen2, aes(x = Field, y = `Water Extractable Nitrogen (mg/kg)`, fill = Variable)) +
  geom_bar(position = "stack", stat = "identity") + 
  geom_errorbar(aes(x = Field, ymax = Max, ymin = Min), position = position_dodge(width = 0.9), 
                width = 0.2) + 
  scale_fill_manual(values = Nitrogen_Col) +
  scale_color_manual(values = Nitrogen_Col) + 
  facet_grid(~ Season) + 
  theme_bw()+ 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        text = element_text(size = 25))

#Nitrogen Plot as Percentages:
NP <- Haney_Test %>% 
  select(Crop.2023,
         Tillage,
         Cover.Crop,
         Field,
         Sample_ID,
         Season,
         `Water Extractable Nitrate Nitrogen (mg/kg)`,
         `Water Extractable Organic Nitrogen (mg/kg)`,
         `Water Extractable Ammonium Nitrogen (mg/kg)`,
         `Water Extractable Nitrogen (mg/kg)`)

NP_long <- pivot_longer(NP, cols = c(7:9), names_to = "Variable", values_to = "Value") %>%
  mutate(Percent = Value/`Water Extractable Nitrogen (mg/kg)` * 100) %>%
  group_by(Field, Variable, Season) %>%
  mutate(Mean = mean(Percent, na.rm = T),
         Sd = sd(Percent, na.rm = T)) %>%
  distinct(Mean, Sd) %>% ungroup() %>%
  mutate(Min = Mean - Sd,
         Max = Mean + Sd) %>%
  mutate(Min = replace(Min, which(Min < 0), 0))


N <- NP_long %>%
  subset(Variable %in% c("Water Extractable Organic Nitrogen (mg/kg)",
                         "Water Extractable Nitrate Nitrogen (mg/kg)",
                         "Water Extractable Ammonium Nitrogen (mg/kg)")) %>%
  mutate(Variable = recode(Variable,
                           "Water Extractable Organic Nitrogen (mg/kg)" = "Organic Nitrogen (%)",
                           "Water Extractable Nitrate Nitrogen (mg/kg)" = "Nitrate (%)",
                           "Water Extractable Ammonium Nitrogen (mg/kg)" = "Ammonium (%)"))


Nitrate <- N %>%
  subset(Variable %in% c("Nitrate (%)")) %>%
  group_by(Field, Season) %>%
  summarize(Sd = sum(Sd),
            Min = sum(Min),
            Max = sum(Max)) %>%
  mutate(Variable = "Nitrate (%)")


Ammonium <- N %>%
  subset(Variable %in% c("Nitrate (%)",
                         "Ammonium (%)")) %>%
  group_by(Field, Season) %>%
  summarize(Sd = sum(Sd),
            Min = sum(Min),
            Max = sum(Max)) %>%
  mutate(Variable = "Ammonium (%)")

Organic <- N %>%
  group_by(Field, Season) %>%
  summarize(Sd = sum(Sd),
            Min = sum(Min),
            Max = sum(Max)) %>%
  mutate(Variable = "Organic Nitrogen (%)")

N_means <- N %>%
  select(Field, Variable, Season, Mean)

N_sd <- rbind(Nitrate, Ammonium, Organic)

N2 <- left_join(N_sd, N_means) %>%
  mutate(Variable = factor(Variable, levels = c("Organic Nitrogen (%)",
                                                "Ammonium (%)",
                                                "Nitrate (%)"))) %>%
  rename(`Water Extractable Nitrogen (%)` = "Mean")


Nitrogen_Col <- c("Nitrate (%)" = "#CC6677",
                  "Ammonium (%)" = "#44AA99",
                  "Organic Nitrogen (%)" = "#88CCEE")

Nitrogen_Plot <- ggplot(N2, aes(x = Field, y = `Water Extractable Nitrogen (%)`, fill = Variable)) +
  geom_bar(position = "stack", stat = "identity") + 
  geom_errorbar(aes(x = Field, ymax = Max, ymin = Min), position = position_dodge(width = 0.9), 
                width = 0.2) + 
  scale_fill_manual(values = Nitrogen_Col) +
  scale_color_manual(values = Nitrogen_Col) + 
  facet_grid(~ Season) + 
  theme_bw()+ 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        text = element_text(size = 25))





#K, Mg, and Ca, which are used to sum up CEC
CEC <- Haney_Means %>%
  subset(Variable %in% c("Calcium (mg/kg)", 
                         "Magnesium (mg/kg)",
                         "Potassium (mg/kg)",
                         "Sodium (mg/kg)",
                         "CEC"))


Ca <- CEC %>%
  subset(Variable %in% c("Calcium (mg/kg)")) %>%
  group_by(Field, Season) %>%
  summarize(Sd = sum(Sd),
            Min = sum(Min),
            Max = sum(Max)) %>%
  mutate(Variable = "Calcium (mg/kg)")


Mg <- CEC %>%
  subset(Variable %in% c("Calcium (mg/kg)",
                         "Magnesium (mg/kg)")) %>%
  group_by(Field, Season) %>%
  summarize(Sd = sum(Sd),
            Min = sum(Min),
            Max = sum(Max)) %>%
  mutate(Variable = "Magnesium (mg/kg)")

Na <- CEC %>%
  subset(Variable %in% c("Calcium (mg/kg)",
                         "Magnesium (mg/kg)",
                         "Sodium (mg/kg)")) %>%
  group_by(Field, Season) %>%
  summarize(Sd = sum(Sd),
            Min = sum(Min),
            Max = sum(Max)) %>%
  mutate(Variable = "Sodium (mg/kg)")


K <- CEC %>%
  group_by(Field, Season) %>%
  subset(Variable %in% c("Calcium (mg/kg)",
                         "Magnesium (mg/kg)",
                         "Sodium (mg/kg)",
                         "Potassium (mg/kg)")) %>%
  summarize(Sd = sum(Sd),
            Min = sum(Min),
            Max = sum(Max)) %>%
  mutate(Variable = "Potassium (mg/kg)")

CEC_Means <- CEC %>%
  select(Field, Variable, Season, Mean)

CEC_Sd <- rbind(K, Na, Mg, Ca)

CEC2 <- left_join(CEC_Sd, CEC_Means) %>%
  mutate(Variable = factor(Variable, levels = c("Potassium (mg/kg)",
                                                "Sodium (mg/kg)",
                                                "Magnesium (mg/kg)",
                                                "Calcium (mg/kg)"))) %>%
  rename(`Cations (mg/kg)` = "Mean")

CEC_Col <- c("Potassium (mg/kg)" = "#CC6677",
             "Sodium (mg/kg)" = "#f7c545",
                  "Magnesium (mg/kg)" = "#44AA99",
                  "Calcium (mg/kg)" = "#88CCEE")

CEC_Plot <- ggplot(CEC2, aes(x = Field, y = `Cations (mg/kg)`, fill = Variable)) +
  geom_bar(position = "stack", stat = "identity") + 
  geom_errorbar(aes(x = Field, ymax = Max, ymin = Min), position = position_dodge(width = 0.9), 
                width = 0.2) + 
  scale_fill_manual(values = CEC_Col) +
  scale_color_manual(values = CEC_Col) + 
  facet_grid(~ Season) + 
  theme_bw()+ 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        text = element_text(size = 25))

###Base Saturation graph
Base_sat <- Haney_Means %>%
  subset(Variable %in% c("% BS H","% BS K", "% BS Mg", 
         "% BS Ca", "% BS Na"))



H <- Base_sat %>%
  subset(Variable %in% c("% BS H")) %>%
  group_by(Field, Season) %>%
  summarize(Sd = sum(Sd),
            Min = sum(Min),
            Max = sum(Max)) %>%
  mutate(Variable = "% BS H")

Ca <- Base_sat %>%
  subset(Variable %in% c("% BS H",
                         "% BS Ca")) %>%
  group_by(Field, Season) %>%
  summarize(Sd = sum(Sd),
            Min = sum(Min),
            Max = sum(Max)) %>%
  mutate(Variable = "% BS Ca")


Mg <- Base_sat %>%
  subset(Variable %in% c("% BS H",
                         "% BS Ca",
                         "% BS Mg")) %>%
  group_by(Field, Season) %>%
  summarize(Sd = sum(Sd),
            Min = sum(Min),
            Max = sum(Max)) %>%
  mutate(Variable = "% BS Mg")

Na <- Base_sat %>%
  subset(Variable %in% c("% BS H",
                         "% BS Ca",
                         "% BS Mg",
                         "% BS Na")) %>%
  group_by(Field, Season) %>%
  summarize(Sd = sum(Sd),
            Min = sum(Min),
            Max = sum(Max)) %>%
  mutate(Variable = "% BS Na")


K <- Base_sat %>%
  group_by(Field, Season) %>%
  subset(Variable %in% c("% BS H",
                         "% BS Ca",
                         "% BS Mg",
                         "% BS Na",
                         "% BS K")) %>%
  summarize(Sd = sum(Sd),
            Min = sum(Min),
            Max = sum(Max)) %>%
  mutate(Variable = "% BS K")

Base_Means <- Base_sat %>%
  select(Field, Variable, Season, Mean)

Base_Sd <- rbind(H, K, Na, Mg, Ca)

Base2 <- left_join(Base_Sd, Base_Means) %>%
  mutate(Variable = factor(Variable, levels = c("% BS K",
                                                "% BS Na",
                                                "% BS Mg",
                                                "% BS Ca",
                                                "% BS H"))) %>%
  rename(`Base Saturation (%)` = "Mean")

Base_Col <- c("% BS K" = "#CC6677",
             "% BS Na" = "#f7c545",
             "% BS Mg" = "#44AA99",
             "% BS Ca" = "#88CCEE",
             "% BS H" = "#8b5ef2")

Base_Plot <- ggplot(Base2, aes(x = Field, y = `Base Saturation (%)`, fill = Variable)) +
  geom_bar(position = "stack", stat = "identity") + 
  geom_errorbar(aes(x = Field, ymax = Max, ymin = Min), position = position_dodge(width = 0.9), 
                width = 0.2) + 
  scale_fill_manual(values = Base_Col) +
  scale_color_manual(values = Base_Col) + 
  facet_grid(~ Season) + 
  theme_bw()+ 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        text = element_text(size = 25))



#Getting the microbial biomass
PLFA <- Haney_Test %>% 
  select(Crop.2023,
         Tillage,
         Cover.Crop,
         Field,
         Sample_ID,
         Season,
         `Protozoa Biomass (ug/g)`,
         `Fungal Biomass (ug/g)`, 
         `Bacteria Biomass (ug/g)`,
         `Undifferentiated Biomass (ug/g)`,
         `Microbial Biomass (ug/g)`,
         `Gram(-) Biomass (ug/g)`,
         `Gram(+) Biomass (ug/g)`) %>%
  mutate(`Gram(+):Gram(-)` = `Gram(+) Biomass (ug/g)`/`Gram(-) Biomass (ug/g)`)

PLFA_long <- pivot_longer(PLFA, cols = c(7:13), names_to = "Variable", values_to = "Value") %>%
  group_by(Field, Variable, Season) %>%
  mutate(Mean = mean(Value, na.rm = T),
         Sd = sd(Value, na.rm = T)) %>%
  distinct(Mean, Sd) %>% ungroup() %>%
  mutate(Min = Mean - Sd,
         Max = Mean + Sd) %>%
  mutate(Min = replace(Min, which(Min < 0), 0))


P <- PLFA_long %>%
  subset(Variable %in% c("Protozoa Biomass (ug/g)",
                         "Fungal Biomass (ug/g)",
                         "Gram(-) Biomass (ug/g)",
                         "Gram(+) Biomass (ug/g)",
                         "Undifferentiated Biomass (ug/g)"))

UB <- P %>%
  subset(Variable %in% c("Undifferentiated Biomass (ug/g)")) %>%
  group_by(Field, Season) %>%
  summarize(Sd = sum(Sd),
            Min = sum(Min),
            Max = sum(Max)) %>%
  mutate(Variable = "Undifferentiated Biomass (ug/g)")


Gramneg <- P %>%
  subset(Variable %in% c("Undifferentiated Biomass (ug/g)",
                         "Gram(-) Biomass (ug/g)")) %>%
  group_by(Field, Season) %>%
  summarize(Sd = sum(Sd),
            Min = sum(Min),
            Max = sum(Max)) %>%
  mutate(Variable = "Gram(-) Biomass (ug/g)")

Grampos <- P %>%
  subset(Variable %in% c("Undifferentiated Biomass (ug/g)",
                         "Gram(-) Biomass (ug/g)",
                         "Gram(+) Biomass (ug/g)")) %>%
  group_by(Field, Season) %>%
  summarize(Sd = sum(Sd),
            Min = sum(Min),
            Max = sum(Max)) %>%
  mutate(Variable = "Gram(+) Biomass (ug/g)")

FB <- P %>%
  subset(Variable %in% c("Undifferentiated Biomass (ug/g)",
                         "Fungal Biomass (ug/g)",
                         "Gram(-) Biomass (ug/g)",
                         "Gram(+) Biomass (ug/g)")) %>%
  group_by(Field, Season) %>%
  summarize(Sd = sum(Sd),
            Min = sum(Min),
            Max = sum(Max)) %>%
  mutate(Variable = "Fungal Biomass (ug/g)")

PB <- P %>%
  group_by(Field, Season) %>%
  summarize(Sd = sum(Sd),
            Min = sum(Min),
            Max = sum(Max)) %>%
  mutate(Variable = "Protozoa Biomass (ug/g)")

PLFA_means <- P %>%
  select(Field, Variable, Season, Mean)


P_sd <- rbind(UB, FB, Grampos, Gramneg, PB)

P2 <- left_join(P_sd, PLFA_means) %>%
  mutate(Variable = factor(Variable, levels = c("Protozoa Biomass (ug/g)",
                                                "Fungal Biomass (ug/g)",
                                                "Gram(+) Biomass (ug/g)",
                                                "Gram(-) Biomass (ug/g)",
                                                "Undifferentiated Biomass (ug/g)"))) %>%
  rename("Microbial Biomass (ug/g)" =  "Mean") %>%
  mutate(Field = factor(Field, levels = c("SS1", "E2", "E3", "E1", "S2", "S3", "S4", "W1")))


PLFA_Col <- c("Protozoa Biomass (ug/g)" = "#CC6677",
              "Fungal Biomass (ug/g)" = "#44AA99",
              "Gram(+) Biomass (ug/g)" = "#88CCEE",
              "Gram(-) Biomass (ug/g)" = "#f7c545",
              "Undifferentiated Biomass (ug/g)" = "#d3d3d3")

PLFA_Plot <- ggplot(P2, aes(x = Field, y = `Microbial Biomass (ug/g)`, fill = Variable)) +
  geom_bar(position = "stack", stat = "identity") + 
  #geom_errorbar(aes(x = Field, ymax = Max, ymin = Min), position = position_dodge(width = 0.9), 
                #width = 0.2) + 
  scale_fill_manual(values = PLFA_Col) +
  scale_color_manual(values = PLFA_Col) + 
  facet_grid(~ Season) + 
  theme_bw()+ 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        text = element_text(size = 25))

PLFA_ratio <- PLFA %>%
  group_by(Field, Season) %>%
  mutate(Mean = mean(`Gram(+):Gram(-)`),
         Sd = sd(`Gram(+):Gram(-)`)) %>%
  distinct(Mean, Sd, Field, Season)

PLFA_ratio_plot <- ggplot(PLFA_ratio, aes(x = Field, y = Mean, fill = Season)) +
  geom_bar(position = "dodge", stat = "identity") + 
  geom_errorbar(aes(x = Field, ymax = Mean + Sd , ymin = Mean - Sd), position = position_dodge(width = 0.9), 
                width = 0.2) +
  facet_wrap(vars(Season), scales = "free") + 
  scale_color_manual(values = Season_col) +
  scale_fill_manual(values = Season_col) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        text = element_text(size = 20))





#Getting Microbial Biomass as a percentage
PLFA <- Haney_Test %>% 
  select(Crop.2023,
         Tillage,
         Cover.Crop,
         Field,
         Sample_ID,
         Season,
         `Protozoa Biomass (ug/g)`,
         `Fungal Biomass (ug/g)`, 
         `Bacteria Biomass (ug/g)`,
         `Undifferentiated Biomass (ug/g)`,
         `Microbial Biomass (ug/g)`)

PLFA_long <- pivot_longer(PLFA, cols = c(7:10), names_to = "Variable", values_to = "Value") %>%
  mutate(Percent = Value/`Microbial Biomass (ug/g)` * 100) %>%
  group_by(Field, Variable, Season) %>%
  mutate(Mean = mean(Percent, na.rm = T),
         Sd = sd(Percent, na.rm = T)) %>%
  distinct(Mean, Sd) %>% ungroup() %>%
  mutate(Min = Mean - Sd,
         Max = Mean + Sd) %>%
  mutate(Min = replace(Min, which(Min < 0), 0))


P <- PLFA_long %>%
  subset(Variable %in% c("Protozoa Biomass (ug/g)",
                         "Fungal Biomass (ug/g)",
                         "Bacteria Biomass (ug/g)",
                         "Undifferentiated Biomass (ug/g)"))

UB <- P %>%
  subset(Variable %in% c("Undifferentiated Biomass (ug/g)")) %>%
  group_by(Field, Season) %>%
  summarize(Sd = sum(Sd),
            Min = sum(Min),
            Max = sum(Max)) %>%
  mutate(Variable = "Undifferentiated Biomass (%)")


FB <- P %>%
  subset(Variable %in% c("Undifferentiated Biomass (ug/g)",
                         "Fungal Biomass (ug/g)")) %>%
  group_by(Field, Season) %>%
  summarize(Sd = sum(Sd),
            Min = sum(Min),
            Max = sum(Max)) %>%
  mutate(Variable = "Fungal Biomass (%)")

BB <- P %>%
  subset(Variable %in% c("Undifferentiated Biomass (ug/g)",
                         "Fungal Biomass (ug/g)",
                         "Bacteria Biomass (ug/g)")) %>%
  group_by(Field, Season) %>%
  summarize(Sd = sum(Sd),
            Min = sum(Min),
            Max = sum(Max)) %>%
  mutate(Variable = "Bacteria Biomass (%)")

PB <- P %>%
  group_by(Field, Season) %>%
  summarize(Sd = sum(Sd),
            Min = sum(Min),
            Max = sum(Max)) %>%
  mutate(Variable = "Protozoa Biomass (%)")

PLFA_means <- P %>%
  select(Field, Variable, Season, Mean) %>%
  mutate(Variable = recode(Variable,
                           "Protozoa Biomass (ug/g)" = "Protozoa Biomass (%)",
                           "Fungal Biomass (ug/g)" = "Fungal Biomass (%)",
                           "Bacteria Biomass (ug/g)" = "Bacteria Biomass (%)",
                           "Undifferentiated Biomass (ug/g)" = "Undifferentiated Biomass (%)"))


P_sd <- rbind(UB, FB, BB, PB)

P2 <- left_join(P_sd, PLFA_means) %>%
  mutate(Variable = factor(Variable, levels = c("Protozoa Biomass (%)",
                                                "Fungal Biomass (%)",
                                                "Bacteria Biomass (%)",
                                                "Undifferentiated Biomass (%)"))) %>%
  rename("Microbial Biomass (%)" = Mean)


PLFA_Col <- c("Bacteria Biomass (%)" = "#CC6677",
                  "Fungal Biomass (%)" = "#44AA99",
                  "Protozoa Biomass (%)" = "#88CCEE",
                  "Undifferentiated Biomass (%)" = "#d3d3d3")

PLFA_Plot <- ggplot(P2, aes(x = Field, y = `Microbial Biomass (%)`, fill = Variable)) +
  geom_bar(position = "stack", stat = "identity") + 
  geom_errorbar(aes(x = Field, ymax = Max, ymin = Min), position = position_dodge(width = 0.9), 
                width = 0.2) + 
  scale_fill_manual(values = PLFA_Col) +
  scale_color_manual(values = PLFA_Col) + 
  facet_grid(~ Season) + 
  theme_bw()+ 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        text = element_text(size = 25))

