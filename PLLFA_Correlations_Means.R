library("dplyr")
library("tidyr")
library("ggplot2")
library("stringr")
library("data.table")
library("scales")
library("lubridate")

setwd("C:/Users/lwang03/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/Kansas_Roar")

Data <- read.csv("Soil1_20231127_LW Modified.csv", header = T)

Field_col <- c(E1 = "#332288", E2 = "#117733", E3 = "#44AA99", SS1 = "#BB9D05" , S2 = "#CC6677", S3 = "#88CCEE", S4 = "#AA4499" , W1 ="#882255" )

Crop_col <- c("Soybeans" = "#332288", "Heavy Crop Mix" = "#117733", "Corn" = "#BB9D05")

Tillage_Cover <- c("Cover Crop No-Till" = 17, "No Cover Crop No-Till" = 15, "No Cover Crop Conventional Till" = 0, "Cover Crop Conventional Till" = 2,
                   "NA NA" = 19)

#Selecting the variables that are connected to the Haney Test and Nitrogen, changing the names
Haneys <- Data %>% select(Cash_Crop,
                              Tillage,
                              Cover.Crop,
                              Field,
                              Sample.ID,
                              NO3_N_H2O,
                              pH,
                              OM.,
                              OC_H2O_ppm,
                              ON_H2O_ppm,
                              CN_H2O,
                              NH4_N_H2O,
                              CO2,
                              N_H2O,
                              X.MAC,
                              ON_release_ppm,
                              H3A_NO3,
                              H3A_NH4,
                              SoilHealth,
                              #H3A_IN,
                              Biomass,
                              Fungi_Bacteria
) %>% 
  rename(`Organic Matter (%)` = "OM.",
         `WEN-NO3 (mg/L)` = "NO3_N_H2O",
         `WEOC (mg/L)` = "OC_H2O_ppm",
         `WEON (mg/L)` = "ON_H2O_ppm",
         `WEOC:WEON` = "CN_H2O",
         `WEN-NH4 (mg/L)` = "NH4_N_H2O",
         `CO2 Respiration (mg/L)` = "CO2",
         `MAC (%)` = "X.MAC",
         `ON Release (mg/L)` = "ON_release_ppm",
         `H3A-NO3(mg/L)` = "H3A_NO3",
         `H3A-NH4 (mg/L)` = "H3A_NH4",
         `WEN (mg/L)` = "N_H2O",
         `Fungi:Bacteria` = "Fungi_Bacteria")


#This section solves for the means 
Haney_Test <- Haneys %>% select(-c(pH))
Haney_Test[,6:18] <- as.numeric(unlist(Haney_Test[,6:18]))

Long_H <-  pivot_longer(Haney_Test, cols = c(6:18), names_to = "Variable", values_to = "Value")

#Solving for the means and standard deviation
Haney_Means <- Long_H %>%
  group_by(Field, Variable) %>%
  mutate(Sd = sd(Value, na.rm = T),
         Mean = mean(Value, na.rm = T),) %>%
  distinct(Sd, Mean) %>% ungroup() %>%
  mutate(Min = Mean - Sd,
         Max = Mean + Sd) %>%
  mutate(Min = replace(Min, which(Min < 0), 0))

PLFA_Means <- Haney_Test %>% select(c(Field, `Fungi:Bacteria`, Biomass, Cash_Crop, Tillage, Cover.Crop)) %>%
  group_by(Field, Cash_Crop, Tillage, Cover.Crop) %>%
  mutate(`Fungi:Bacteria means` = mean(`Fungi:Bacteria`, na.rm = T),
         FB_Sd = sd(`Fungi:Bacteria`, na.rm = T),
         `Biomass means` = mean(Biomass, na.rm = T),
         B_Sd = sd(Biomass, na.rm = T)) %>%
  distinct(`Fungi:Bacteria means`, FB_Sd, `Biomass means`, B_Sd) %>%
  ungroup()


pH <- Haneys %>% select(c(Field, pH)) %>%
  group_by(Field) %>%
  mutate(Variable = "pH") %>%
  mutate(Sd = sd(pH, na.rm = T)) %>%
  mutate(pH = 10**(-pH)) %>%
  mutate(Mean = mean(pH, na.rm = T)) %>%
  mutate(Mean = -log10(Mean)) %>%
  select(-c(pH)) %>%
  distinct(Field, Variable,Sd, Mean) %>% ungroup() %>%
  mutate(Min = Mean - Sd,
         Max = Mean + Sd) %>%
  mutate(Min = replace(Min, which(Min < 0), 0))

Haney_Means <- rbind(Haney_Means, pH)



Means_Joined <- left_join(Haney_Means, PLFA_Means)
Means_Joined$`Cover & Till` <- paste(Means_Joined$Cover.Crop, Means_Joined$Tillage)

Biomass_Correlations <- ggplot(Means_Joined) + 
  geom_point(aes(x = `Biomass means`, y = Mean, color = Cash_Crop, shape = `Cover & Till`), size = 6) + 
  geom_errorbar(aes(x = `Biomass means`, ymax = Max, ymin = Min)) +
  geom_errorbarh(aes(y = Mean, 
                     xmax = `Biomass means` + B_Sd, 
                     xmin = `Biomass means` - B_Sd)) +
  facet_wrap(vars(Variable), scales = "free") +
  scale_color_manual(values = Crop_col) +
  scale_shape_manual(values = Tillage_Cover) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        text = element_text(size = 25),
        aspect.ratio = 1)


setwd("C:/Users/lwang03/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/Kansas_Roar/Plots")
ggsave("Biomass_Correlations_Means.pdf", Biomass_Correlations, height = 40, width = 40)



Fungi_Bacteria_Correlations <- ggplot(Means_Joined) + 
  geom_point(aes(x = `Fungi:Bacteria means`, y = Mean, color = Cash_Crop, shape = `Cover & Till`), size = 6) + 
  geom_errorbar(aes(x = `Fungi:Bacteria means`, ymax = Max, ymin = Min)) +
  geom_errorbarh(aes(y = Mean, 
                     xmax = `Fungi:Bacteria means` + FB_Sd, 
                     xmin = `Fungi:Bacteria means` - FB_Sd)) +
  facet_wrap(vars(Variable), scales = "free") +
  scale_color_manual(values = Crop_col) +
  scale_shape_manual(values = Tillage_Cover) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        text = element_text(size = 25),
        aspect.ratio = 1)


setwd("C:/Users/lwang03/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/Kansas_Roar/Plots")
ggsave("Fungi_Bacteria_Correlations_Means.pdf", Fungi_Bacteria_Correlations, height = 40, width = 40)
