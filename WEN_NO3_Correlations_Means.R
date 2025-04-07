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
Haney_Test[,7:20] <- as.numeric(unlist(Haney_Test[,7:20]))

Long_H <-  pivot_longer(Haney_Test, cols = c(7:20), names_to = "Variable", values_to = "Value")

#Solving for the means and standard deviation
Haney_Means <- Long_H %>%
  group_by(Field, Variable) %>%
  mutate(Sd = sd(Value, na.rm = T),
         Mean = mean(Value, na.rm = T),) %>%
  distinct(Sd, Mean) %>% ungroup() %>%
  mutate(Min = Mean - Sd,
         Max = Mean + Sd) %>%
  mutate(Min = replace(Min, which(Min < 0), 0))

PLFA_Means <- Haney_Test %>% select(c(Field, `WEN-NO3 (mg/L)`, Cash_Crop, Tillage, Cover.Crop)) %>%
  group_by(Field, Cash_Crop, Tillage, Cover.Crop) %>%
  mutate(WEN_m = mean(`WEN-NO3 (mg/L)`, na.rm = T),
         WEN_Sd = sd(`WEN-NO3 (mg/L)`, na.rm = T)) %>%
  distinct(WEN_m, WEN_Sd) %>%
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

WEN_NO3_Correlations <- ggplot(Means_Joined) + 
  geom_point(aes(x = `Mean`, y = WEN_m, color = Cash_Crop, shape = `Cover & Till`), size = 6) + 
  geom_errorbarh(aes(y = WEN_m, xmax = Max, xmin = Min)) +
  geom_errorbar(aes(x = Mean, 
                     ymax = WEN_m + WEN_Sd, 
                     ymin = WEN_m - WEN_Sd)) +
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
ggsave("WEN_NO3_Correlations_Means.pdf", WEN_NO3_Correlations, height = 40, width = 40)
