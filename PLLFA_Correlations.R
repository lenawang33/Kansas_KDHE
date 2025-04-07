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



Crop_col <- c("Soybeans" = "#332288", "Heavy Crop Mix" = "#117733", "Corn" = "#BB9D05", "NA" = )

Tillage_Cover <- c("Cover Crop No-Till" = 17, "No Cover Crop No-Till" = 15, "No Cover Crop Conventional Till" = 0, "Cover Crop Conventional Till" = 2,
                   "NA NA" = 19)

#Selecting the variables that are connected to the Haney Test and Nitrogen, changing the names
Haney_Test <- Data %>% select(Cash_Crop,
                              Tillage,
                              Cover.Crop,
                              Field,
                              Sample.ID,
                              OM.,
                              NO3_N_H2O,
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

Haney_Test[,6:17] <- as.numeric(unlist(Haney_Test[,6:17]))

Long_H <-  pivot_longer(Haney_Test, cols = c(6:17), names_to = "Variable", values_to = "Value")

Long_H$`Cover & Till` <- paste(Long_H$Cover.Crop, Long_H$Tillage)

Fungi_Bacteria_Correlations <- ggplot(Long_H) + 
  geom_point(aes(x = `Fungi:Bacteria`, y = Value, color = Cash_Crop, shape = `Cover & Till`), size = 8) + 
  facet_grid(Variable ~ Field, scales = "free") +
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
ggsave("Fungi_Bacteria_Correlations_No_Mean.pdf", Fungi_Bacteria_Correlations, height = 40, width = 40, limitsize = F)

Biomass_Correlations <- ggplot(Long_H) + 
  geom_point(aes(x = `Biomass`, y = Value, color = Cash_Crop, shape = `Cover & Till`), size = 8) + 
  facet_grid(Variable ~ Field, scales = "free") +
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
ggsave("Biomass_Correlations_No_Mean.pdf", Biomass_Correlations, height = 40, width = 40, limitsize = F)

